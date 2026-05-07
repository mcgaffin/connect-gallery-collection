# Copy `template_dir`'s contents into a fresh tempdir and write
# collection.json next to them. Returns the staged dir path.
stage_bundle <- function(template_dir, config) {
  if (!dir.exists(template_dir)) {
    stop(sprintf("stage_bundle: template directory not found: %s", template_dir))
  }
  staged <- tempfile("collection-bundle-")
  dir.create(staged)
  files <- list.files(template_dir, full.names = TRUE, all.files = TRUE,
                      no.. = TRUE)
  file.copy(files, staged, recursive = TRUE)
  jsonlite::write_json(
    config,
    file.path(staged, "collection.json"),
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  staged
}

# One-time per-process rsconnect setup. Idempotent.
setup_rsconnect <- function(connect_server, connect_api_key,
                            server_name = "connect",
                            account_name = "configurator") {
  rsconnect::addServer(
    url   = paste0(connect_server, "/__api__/"),
    name  = server_name,
    quiet = TRUE
  )
  rsconnect::connectApiUser(
    account = account_name,
    server  = server_name,
    apiKey  = connect_api_key
  )
  invisible(list(server = server_name, account = account_name))
}

# Spawn deploy in a background process. Returns a callr handle.
# On CREATE, pass appId = NULL — the child generates an appName via marker+uuid.
# On UPDATE, pass appId = <guid> — the existing name is preserved by Connect.
launch_deploy <- function(staged_dir, app_id, app_title,
                          connect_server, connect_api_key,
                          marker = COLLECTION_NAME_MARKER) {
  callr::r_bg(
    func = function(staged_dir, app_id, app_title,
                    connect_server, connect_api_key, marker) {
      rsconnect::addServer(
        url   = paste0(connect_server, "/__api__/"),
        name  = "connect",
        quiet = TRUE
      )
      rsconnect::connectApiUser(
        account = "configurator",
        server  = "connect",
        apiKey  = connect_api_key
      )

      app_name <- if (is.null(app_id)) {
        paste0(marker, "-", uuid::UUIDgenerate())
      } else {
        NULL
      }

      # rsconnect calls `quarto inspect` during deploy; on Connect the
      # binary often isn't on the Shiny process's PATH. Probe common
      # locations, set both RSCONNECT_QUARTO and prepend to PATH so
      # findQuarto() and any later Sys.which() both see it.
      versioned <- sort(Sys.glob("/opt/quarto/*/bin/quarto"), decreasing = TRUE)
      candidates <- c(
        unname(Sys.which("quarto")),
        Sys.getenv("QUARTO_PATH"),
        Sys.getenv("RSCONNECT_QUARTO"),
        versioned,
        "/opt/quarto/bin/quarto",
        "/usr/local/bin/quarto",
        "/usr/lib/rstudio-server/bin/quarto/bin/quarto"
      )
      candidates <- candidates[nzchar(candidates)]
      quarto_bin <- ""
      for (p in candidates) {
        if (file.exists(p)) { quarto_bin <- p; break }
      }
      if (!nzchar(quarto_bin)) {
        stop(sprintf(
          "Quarto binary not found. Tried: %s. Set QUARTO_PATH or RSCONNECT_QUARTO as an env var on the configurator's content settings.",
          paste(candidates, collapse = ", ")
        ))
      }
      Sys.setenv(RSCONNECT_QUARTO = unname(quarto_bin))
      Sys.setenv(PATH = paste(dirname(quarto_bin), Sys.getenv("PATH"), sep = .Platform$path.sep))
      message("Using quarto: ", quarto_bin)

      ok <- rsconnect::deployApp(
        appDir         = staged_dir,
        appId          = app_id,
        appName        = app_name,
        appTitle       = app_title,
        server         = "connect",
        account        = "configurator",
        forceUpdate    = TRUE,
        launch.browser = FALSE,
        logLevel       = "normal"
      )
      if (!isTRUE(ok)) stop("deployApp returned a non-TRUE result")

      # rsconnect writes a deployment record into staged_dir/rsconnect/.
      # Read the most recent record to get the real Connect URL and GUID.
      records <- rsconnect::deployments(appPath = staged_dir)
      if (nrow(records) == 0) {
        stop("deployApp succeeded but no deployment record was written")
      }
      latest <- records[nrow(records), ]

      # Resolve the GUID with explicit NULL/NA-safe fallbacks because the
      # callr child does not have R/config.R's %||% in scope.
      guid <- if (!is.null(latest$appGuid) && nchar(latest$appGuid) > 0) {
        latest$appGuid
      } else if (!is.null(latest$appId) && nchar(latest$appId) > 0) {
        latest$appId
      } else {
        NA_character_
      }

      list(
        url  = as.character(latest$url),
        guid = as.character(guid),
        name = if (!is.null(latest$name) && nchar(latest$name) > 0)
          as.character(latest$name) else app_name
      )
    },
    args = list(
      staged_dir       = staged_dir,
      app_id           = app_id,
      app_title        = app_title,
      connect_server   = connect_server,
      connect_api_key  = connect_api_key,
      marker           = marker
    ),
    supervise = TRUE
  )
}
