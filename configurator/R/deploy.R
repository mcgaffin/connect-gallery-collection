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
                          marker = "__content-collection__") {
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

      url <- rsconnect::deployApp(
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

      list(url = as.character(url), name = app_name)
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
