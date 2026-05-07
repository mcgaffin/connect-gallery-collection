library(shiny)
library(shinyjs)
library(bslib)
library(httr2)
library(jsonlite)

# Source helper modules into the global environment so the server function
# and the UI builders can see them.
local({
  helpers <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  for (f in helpers) source(f, local = FALSE)
})

connect_server  <- Sys.getenv("CONNECT_SERVER", "http://localhost:3939")
connect_api_key <- Sys.getenv("CONNECT_API_KEY", "")

# Configure rsconnect once at startup if a key is present.
if (nzchar(connect_api_key)) {
  tryCatch(
    setup_rsconnect(connect_server, connect_api_key),
    error = function(e) message("setup_rsconnect: ", e$message)
  )
}

# ---------- UI ----------
ui <- page_fillable(
  title = "Collection Configurator",
  theme = bs_theme(preset = "shiny"),
  shinyjs::useShinyjs(),
  tags$head(
    tags$style("
      @media (prefers-reduced-motion: reduce) { .spinner-border { animation: none; } }
      .modal-dialog { max-width: 760px; }
    ")
  ),
  uiOutput("home_ui")
)

# ---------- Server ----------
server <- function(input, output, session) {
  # ---- reactive state ----
  view             <- reactiveVal("home")            # "home" | "wizard"
  wizard_step      <- reactiveVal(1)                 # 1..4
  editing_guid     <- reactiveVal(NULL)              # NULL = create
  search_results   <- reactiveVal(list())
  preview_html     <- reactiveVal("")
  preview_busy     <- reactiveVal(FALSE)
  all_tags         <- reactiveVal(list())
  collections      <- reactiveVal(list())
  deploy_handle    <- reactiveVal(NULL)
  deploy_progress  <- reactiveVal(NULL)
  staged_dir       <- reactiveVal(NULL)

  wizard_state <- reactiveValues(
    title = "", description = "", intro_markdown = "",
    theme = "minimal", source_type = "manual",
    guids = character(0), source_tag = ""
  )

  notify <- function(msg, type = "default") {
    showNotification(msg, type = type,
                     duration = if (type == "error") NULL else 5)
  }

  reset_wizard_state <- function() {
    wizard_state$title          <- ""
    wizard_state$description    <- ""
    wizard_state$intro_markdown <- ""
    wizard_state$theme          <- "minimal"
    wizard_state$source_type    <- "manual"
    wizard_state$guids          <- character(0)
    wizard_state$source_tag     <- ""
    search_results(list())
    preview_html("")
  }

  # ---- initial loads ----
  observe({
    collections(fetch_my_collections(connect_server, connect_api_key))
  })
  observe({
    all_tags(get_tags(connect_server, connect_api_key))
  })

  # ---- home view ----
  output$home_ui <- renderUI({
    if (view() == "home") home_view(collections())
    else NULL    # wizard is rendered into a modal via showModal
  })

  observeEvent(input$new_collection, {
    reset_wizard_state()
    editing_guid(NULL)
    wizard_step(1)
    view("wizard")
    show_wizard()
  })

  # Edit buttons in the home list
  observe({
    for (coll in collections()) {
      local({
        c_guid <- coll$guid
        observeEvent(input[[paste0("edit_", c_guid)]], {
          load_existing(c_guid)
        }, ignoreInit = TRUE, once = FALSE)
      })
    }
  })

  load_existing <- function(guid) {
    info <- get_content(connect_server, connect_api_key, guid)
    bundle_path <- download_active_bundle(connect_server, connect_api_key, guid)
    cfg <- if (!is.null(bundle_path)) extract_collection_json(bundle_path) else NULL

    if (is.null(cfg)) {
      reset_wizard_state()
      if (!is.null(info)) {
        wizard_state$title       <- info$title       %||% ""
        wizard_state$description <- info$description %||% ""
      }
      notify("This collection has no saved settings yet. Saving will publish a fresh configuration.",
             type = "warning")
    } else {
      parsed <- parse_config(cfg)
      wizard_state$title          <- parsed$title
      wizard_state$description    <- parsed$description
      wizard_state$intro_markdown <- parsed$intro_markdown
      wizard_state$theme          <- parsed$theme
      wizard_state$source_type    <- parsed$source_type
      wizard_state$guids          <- parsed$guids
      wizard_state$source_tag     <- parsed$source_tag
    }
    editing_guid(guid)
    wizard_step(1)
    view("wizard")
    show_wizard()
  }

  # ---- wizard render ----
  show_wizard <- function() {
    step  <- wizard_step()
    mode  <- if (is.null(editing_guid())) "create" else "edit"
    body  <- step_body_for(step)
    showModal(wizard_modal_dialog(step = step, mode = mode,
                                  state = isolate(reactiveValuesToList(wizard_state)),
                                  body = body))
  }

  step_body_for <- function(step) {
    s <- isolate(reactiveValuesToList(wizard_state))
    switch(as.character(step),
      "1" = step_select_ui(state = s,
                           search_query = isolate(input$search_query) %||% "",
                           search_results = search_results(),
                           all_tags = all_tags()),
      "2" = step_describe_ui(state = s),
      "3" = step_theme_ui(state = s),
      "4" = step_preview_ui(html_string = preview_html(),
                            source_type = s$source_type,
                            busy = preview_busy())
    )
  }

  # ---- bindings: keep wizard_state in sync with inputs ----
  observeEvent(input$collection_title,       { wizard_state$title          <- input$collection_title       }, ignoreInit = TRUE)
  observeEvent(input$collection_description, { wizard_state$description    <- input$collection_description }, ignoreInit = TRUE)
  observeEvent(input$collection_intro,       { wizard_state$intro_markdown <- input$collection_intro       }, ignoreInit = TRUE)
  observeEvent(input$source_type,            { wizard_state$source_type    <- input$source_type             }, ignoreInit = TRUE)
  observeEvent(input$tag_select,             { wizard_state$source_tag     <- input$tag_select %||% ""      }, ignoreInit = TRUE)

  # Theme button clicks
  observe({
    lapply(names(THEME_COLORS), function(id) {
      observeEvent(input[[paste0("theme_", id)]], {
        wizard_state$theme <- id
        show_wizard()  # re-render to update selected state
      }, ignoreInit = TRUE)
    })
  })

  # Search button / query change
  observeEvent(input$search_query, {
    q <- input$search_query
    if (!is.null(q) && nchar(trimws(q)) > 0) {
      search_results(search_content(connect_server, connect_api_key, q))
      show_wizard()
    } else {
      search_results(list())
    }
  }, ignoreInit = TRUE)

  # Result-row checkbox toggles via custom message handler (HTML checkboxes)
  observeEvent(input$select_all, {
    results <- search_results()
    all_guids <- vapply(results, function(r) r$guid %||% "", character(1))
    if (isTRUE(input$select_all)) {
      wizard_state$guids <- unique(c(wizard_state$guids, all_guids))
    } else {
      wizard_state$guids <- setdiff(wizard_state$guids, all_guids)
    }
    show_wizard()
  }, ignoreInit = TRUE)

  observe({
    for (item in search_results()) {
      local({
        guid <- item$guid
        observeEvent(input[[paste0("result_", guid)]], {
          if (guid %in% wizard_state$guids) {
            wizard_state$guids <- setdiff(wizard_state$guids, guid)
          } else {
            wizard_state$guids <- c(wizard_state$guids, guid)
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # ---- wizard navigation ----
  observeEvent(input$wizard_cancel, {
    removeModal()
    view("home")
    collections(fetch_my_collections(connect_server, connect_api_key))
  })

  observeEvent(input$wizard_back, {
    wizard_step(max(1, wizard_step() - 1))
    show_wizard()
  })

  observeEvent(input$wizard_next, {
    valid <- validate_step(wizard_step())
    if (!isTRUE(valid$ok)) { notify(valid$msg, "warning"); return() }
    new_step <- wizard_step() + 1
    wizard_step(new_step)
    if (new_step == 4) refresh_preview()
    show_wizard()
  })

  validate_step <- function(step) {
    s <- reactiveValuesToList(wizard_state)
    if (step == 1) {
      if (identical(s$source_type, "manual") && length(s$guids) == 0) {
        return(list(ok = FALSE, msg = "Select at least one item to continue."))
      }
      if (identical(s$source_type, "tag") && !nzchar(s$source_tag)) {
        return(list(ok = FALSE, msg = "Pick a tag to continue."))
      }
    }
    if (step == 2 && !nzchar(trimws(s$title))) {
      return(list(ok = FALSE, msg = "Title is required."))
    }
    list(ok = TRUE, msg = "")
  }

  refresh_preview <- function() {
    s <- reactiveValuesToList(wizard_state)
    preview_busy(TRUE)
    items <- if (identical(s$source_type, "tag") && nzchar(s$source_tag)) {
      fetch_content_by_tag(connect_server, connect_api_key, s$source_tag)
    } else {
      lapply(s$guids, function(g) get_content(connect_server, connect_api_key, g))
    }
    items <- Filter(Negate(is.null), items)
    preview_html(build_collection_html(s, items, THEME_COLORS,
                                       connect_server = connect_server))
    preview_busy(FALSE)
  }

  # ---- publish ----
  observeEvent(input$wizard_publish, { trigger_publish() })
  trigger_publish <- function() {
    cfg <- build_config(
      title          = wizard_state$title,
      description    = wizard_state$description,
      intro_markdown = wizard_state$intro_markdown,
      theme          = wizard_state$theme,
      source_type    = wizard_state$source_type,
      guids          = wizard_state$guids,
      tag            = wizard_state$source_tag
    )
    staged <- tryCatch(
      stage_bundle("dashboard_template", cfg),
      error = function(e) { notify(paste("Bundle staging failed:", e$message), "error"); NULL }
    )
    if (is.null(staged)) return()
    handle <- tryCatch(
      launch_deploy(staged_dir = staged,
                    app_id = editing_guid(),
                    app_title = cfg$title,
                    connect_server = connect_server,
                    connect_api_key = connect_api_key),
      error = function(e) { notify(paste("Deploy launch failed:", e$message), "error"); NULL }
    )
    if (is.null(handle)) return()
    progress_id <- showNotification(
      tags$div(tags$span(class = "spinner-border spinner-border-sm me-2"),
               "Publishing your collection..."),
      type = "message", duration = NULL
    )
    deploy_handle(handle); deploy_progress(progress_id); staged_dir(staged)
  }

  observe({
    handle <- deploy_handle()
    req(handle)
    invalidateLater(2000)
    if (handle$is_alive()) return()

    p <- deploy_progress()
    if (!is.null(p)) removeNotification(p)
    sd <- staged_dir(); if (!is.null(sd) && dir.exists(sd)) unlink(sd, recursive = TRUE)

    if (isTRUE(handle$get_exit_status() == 0)) {
      result <- tryCatch(handle$get_result(), error = function(e) e)
      if (inherits(result, "error")) {
        notify(paste("Publish failed:", conditionMessage(result)), "error")
      } else {
        url <- result$url %||% connect_server
        showNotification(
          tags$div(tags$p("Your collection is ready!"),
                   tags$a(href = url, target = "_blank",
                          class = "btn btn-sm btn-outline-primary mt-2",
                          "Open Collection")),
          type = "message", duration = 15
        )
        removeModal()
        view("home")
        collections(fetch_my_collections(connect_server, connect_api_key))
      }
    } else {
      err <- tryCatch(handle$read_error_lines(), error = function(e) character(0))
      msg <- if (length(err) > 0) paste(tail(err, 6), collapse = "\n")
             else sprintf("Deploy exited with status %s", handle$get_exit_status())
      notify(paste("Publish failed:\n", msg), "error")
    }
    deploy_handle(NULL); deploy_progress(NULL); staged_dir(NULL)
  })
}

shinyApp(ui, server)
