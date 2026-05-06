library(shiny)
library(shinyjs)
library(bslib)
library(httr2)
library(jsonlite)

# Source helper modules. file.path resolves relative to the app's working dir
# both during local dev (via shiny::runApp) and on Connect.
local({
  helpers <- list.files("R", pattern = "\\.R$", full.names = TRUE)
  for (f in helpers) source(f, local = FALSE)
})

connect_server <- Sys.getenv("CONNECT_SERVER", "http://localhost:3939")
connect_api_key <- Sys.getenv("CONNECT_API_KEY", "")

# Configure rsconnect so deployApp() targets this Connect by name="connect".
# Idempotent; safe to call on every app start.
if (nzchar(connect_api_key)) {
  tryCatch(
    setup_rsconnect(connect_server, connect_api_key),
    error = function(e) message("setup_rsconnect: ", e$message)
  )
}

# Theme definitions
themes <- list(
  "warm" = list(label = "Warm", bg = "#fffbeb", accent = "#d97706"),
  "cool" = list(label = "Cool", bg = "#eff6ff", accent = "#2563eb"),
  "minimal" = list(label = "Minimal", bg = "#fafafa", accent = "#737373"),
  "fun" = list(label = "Fun", bg = "#fdf2f8", accent = "#db2777"),
  "bold" = list(label = "Bold", bg = "#eef2ff", accent = "#4338ca"),
  "earth" = list(label = "Earth", bg = "#f0fdf4", accent = "#15803d")
)

# UI
ui <- page_sidebar(
  title = "Collection Configurator",
  theme = bs_theme(preset = "shiny"),
  shinyjs::useShinyjs(),
  tags$head(tags$style("
    @media (prefers-reduced-motion: reduce) {
      .spinner-border {
        animation: none;
      }
    }
  ")),

  sidebar = sidebar(
    width = 380,
    title = "Configuration",

    # Compact spacing CSS
    tags$style("
      .sidebar .form-group { margin-bottom: 0.5rem; }
      .sidebar hr { margin: 0.5rem 0; }
      .sidebar .control-label { margin-bottom: 0.25rem; }

      /* Clean up tab styling */
      .bslib-card > .card-header > .nav-tabs .nav-link {
        font-size: 0.875rem;
      }

      /* Disabled state for config wrapper */
      #sidebar-config-wrapper.disabled-overlay {
        opacity: 0.4;
        pointer-events: none;
        user-select: none;
      }

      /* Loading overlay */
      #sidebar-config-wrapper { position: relative; }
      #sidebar-loading-overlay {
        position: absolute;
        top: 0; left: 0; right: 0; bottom: 0;
        background: rgba(255, 255, 255, 0.85);
        display: flex;
        align-items: flex-start;
        justify-content: center;
        padding-top: 3rem;
        z-index: 10;
        border-radius: 0.25rem;
        font-size: 0.875rem;
        color: #666;
      }
    "),

    # Target dashboard
    selectizeInput("dashboard_guid", "Collection Dashboard",
      choices = c("Loading collections..." = ""),
      options = list(placeholder = "Select a collection dashboard...")
    ),

    # Config fields wrapped with loading overlay (starts disabled)
    tags$div(id = "sidebar-config-wrapper", class = "disabled-overlay",
      shinyjs::hidden(
        tags$div(id = "sidebar-loading-overlay",
          tags$span(class = "spinner-border spinner-border-sm me-2", role = "status", `aria-label` = "Loading"),
          "Loading configuration..."
        )
      ),

    hr(),

    # Metadata
    textInput("collection_title", "Title", placeholder = "My Collection"),
    textInput("collection_description", "Description", placeholder = "A short description"),
    textAreaInput("collection_intro", "Introduction (Markdown)",
      rows = 3, placeholder = "Write an intro. Markdown supported."
    ),

    hr(),

    # Theme
    tags$label("Theme", class = "control-label"),
    uiOutput("theme_picker"),

    hr(),

    # Source type
    radioButtons("source_type", "Content Source",
      choices = c("Select content" = "manual", "Use a tag" = "tag"),
      selected = "manual"
    ),

    # Tag selection (shown when source_type == "tag")
    conditionalPanel(
      condition = "input.source_type == 'tag'",
      selectInput("tag_select", "Select Tag", choices = c("Loading..." = ""))
    ),

    # Content search (shown when source_type == "manual")
    conditionalPanel(
      condition = "input.source_type == 'manual'",
      textInput("search_query", "Search Content", placeholder = "Search by title..."),
      actionButton("search_btn", "Search", class = "btn-sm btn-primary mb-1")
    ),

    hr(),

    # Save
    actionButton("save_config", "Save & Publish", class = "btn-primary btn-lg w-100")
    ) # close sidebar-config-wrapper
  ),

  # Main content area
  navset_card_tab(
    id = "main_tabs",

    nav_panel("Search Results",
      conditionalPanel(
        condition = "input.source_type == 'manual'",
        tags$div(id = "search_results",
          uiOutput("search_results_ui")
        )
      ),
      conditionalPanel(
        condition = "input.source_type == 'tag'",
        tags$div(
          style = "display: flex; align-items: center; justify-content: center; min-height: 300px; padding-bottom: 80px;",
          tags$p(class = "text-muted", "Content will be dynamically included based on the selected tag.")
        )
      )
    ),

    nav_panel("Selected Items",
      tags$div(class = "mt-3",
        uiOutput("selected_items_ui")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  selected_guids <- reactiveVal(character(0))
  search_results <- reactiveVal(list())
  has_searched <- reactiveVal(FALSE)
  selected_theme <- reactiveVal("minimal")
  notify <- function(msg, type = "default") {
    showNotification(msg, type = type, duration = if (type == "error") NULL else 5)
  }
  all_tags <- reactiveVal(list())

  # Load collection dashboards on startup
  observe({
    dashboards <- fetch_collection_dashboards(connect_server, connect_api_key)
    choices <- c("Select a collection..." = "", "Create new collection..." = "__new__")
    for (d in dashboards) {
      label <- d$title %||% d$name %||% d$guid
      choices[label] <- d$guid
    }
    updateSelectizeInput(session, "dashboard_guid", choices = choices)
  })

  # Load tags on startup
  observe({
    tags_data <- get_tags(connect_server, connect_api_key)
    all_tags(tags_data)

    # Build tag choices: only child tags (those with parent_id)
    choices <- c("Select a tag..." = "")
    for (tag in tags_data) {
      if (!is.null(tag$parent_id)) {
        choices[tag$name] <- tag$name
      }
    }
    updateSelectInput(session, "tag_select", choices = choices)
  })

  # Theme picker grid
  output$theme_picker <- renderUI({
    current <- selected_theme()
    theme_buttons <- lapply(names(themes), function(id) {
      t <- themes[[id]]
      is_selected <- id == current
      actionButton(
        paste0("theme_", id),
        t$label,
        class = "btn btn-sm w-100",
        style = sprintf(
          "background: %s; color: %s; border: 2px solid %s; font-weight: 500; %s",
          t$bg, t$accent,
          if (is_selected) t$accent else "#dee2e6",
          if (is_selected) sprintf("box-shadow: 0 0 0 1px %s, 0 0 0 4px %s;", t$accent, t$bg) else ""
        )
      )
    })
    tags$div(
      style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 0.5rem;",
      theme_buttons
    )
  })

  # Theme button click handlers
  observe({
    lapply(names(themes), function(id) {
      observeEvent(input[[paste0("theme_", id)]], {
        selected_theme(id)
      }, ignoreInit = TRUE)
    })
  })

  # Search content
  observeEvent(input$search_btn, {
    req(input$search_query)
    shinyjs::disable("search_btn")
    shinyjs::html("search_btn", "Searching...")
    on.exit({
      shinyjs::enable("search_btn")
      shinyjs::html("search_btn", "Search")
    })
    results <- search_content(connect_server, connect_api_key, input$search_query)
    search_results(results)
    has_searched(TRUE)
  })

  # Load config when a dashboard is selected
  observeEvent(input$dashboard_guid, {
    req(input$dashboard_guid)
    selection <- trimws(input$dashboard_guid)
    if (!nzchar(selection)) return()

    # CREATE flow: clear the form to defaults, enable the sidebar, no fetch.
    if (identical(selection, "__new__")) {
      updateTextInput(session, "collection_title", value = "")
      updateTextInput(session, "collection_description", value = "")
      updateTextAreaInput(session, "collection_intro", value = "")
      selected_theme("minimal")
      updateRadioButtons(session, "source_type", selected = "manual")
      updateSelectInput(session, "tag_select", selected = "")
      selected_guids(character(0))
      shinyjs::removeClass("sidebar-config-wrapper", "disabled-overlay")
      return()
    }

    # UPDATE flow: download the active bundle, extract collection.json.
    shinyjs::show("sidebar-loading-overlay")
    on.exit({
      shinyjs::hide("sidebar-loading-overlay")
      shinyjs::removeClass("sidebar-config-wrapper", "disabled-overlay")
    })

    guid <- selection

    # Pre-fill title/description from Connect content metadata as a fallback.
    content <- get_content(connect_server, connect_api_key, guid)
    if (!is.null(content)) {
      updateTextInput(session, "collection_title", value = content$title %||% "")
      updateTextInput(session, "collection_description", value = content$description %||% "")
    }

    bundle_path <- download_active_bundle(connect_server, connect_api_key, guid)
    cfg <- if (!is.null(bundle_path)) extract_collection_json(bundle_path) else NULL

    if (is.null(cfg)) {
      notify(
        "This collection has no saved settings yet. Saving will publish a fresh configuration.",
        type = "warning"
      )
      return()
    }

    parsed <- parse_config(cfg)
    updateTextInput(session, "collection_title", value = parsed$title)
    updateTextInput(session, "collection_description", value = parsed$description)
    updateTextAreaInput(session, "collection_intro", value = parsed$intro_markdown)
    selected_theme(parsed$theme)
    updateRadioButtons(session, "source_type", selected = parsed$source_type)
    updateSelectInput(session, "tag_select", selected = parsed$source_tag)
    selected_guids(parsed$guids)

    notify("Loaded existing configuration.", type = "message")
  })

  # Render search results with select buttons
  output$search_results_ui <- renderUI({
    results <- search_results()
    if (length(results) == 0) {
      msg <- if (has_searched()) {
        "No content matches your search."
      } else {
        "Search for content to add to your collection."
      }
      return(tags$div(
        style = "display: flex; align-items: center; justify-content: center; min-height: 300px; padding-bottom: 80px;",
        tags$p(class = "text-muted", msg)
      ))
    }

    current_selected <- selected_guids()

    result_cards <- lapply(seq_along(results), function(i) {
      item <- results[[i]]
      guid <- item$guid %||% ""
      title <- item$title %||% item$name %||% "Untitled"
      description <- item$description %||% ""
      if (nchar(description) > 80) {
        description <- paste0(substr(description, 1, 80), "...")
      }
      app_mode <- item$app_mode %||% ""
      is_selected <- guid %in% current_selected
      btn_id <- paste0("toggle_", i)

      tags$div(class = paste("card mb-2", if (is_selected) "border-primary"),
        tags$div(class = "card-body py-2 px-3 d-flex align-items-center gap-2",
          tags$div(class = "flex-grow-1",
            tags$div(class = "fw-medium small", title),
            tags$div(class = "text-muted", style = "font-size: 0.75rem;",
              paste(app_mode, if (nchar(description) > 0) paste0(" - ", description))
            )
          ),
          actionButton(btn_id,
            if (is_selected) "Remove" else "Add",
            class = if (is_selected) "btn-sm btn-outline-danger" else "btn-sm btn-outline-primary"
          )
        )
      )
    })

    tagList(
      tags$p(class = "text-muted small", paste(length(results), "result(s)")),
      result_cards
    )
  })

  # Handle toggle buttons
  observe({
    results <- search_results()
    lapply(seq_along(results), function(i) {
      btn_id <- paste0("toggle_", i)
      observeEvent(input[[btn_id]], {
        guid <- results[[i]]$guid
        current <- selected_guids()
        if (guid %in% current) {
          selected_guids(setdiff(current, guid))
        } else {
          selected_guids(c(current, guid))
        }
      }, ignoreInit = TRUE)
    })
  })

  # Render selected items
  output$selected_items_ui <- renderUI({
    guids <- selected_guids()
    if (length(guids) == 0) {
      return(tags$div(
        style = "display: flex; align-items: center; justify-content: center; min-height: 300px; padding-bottom: 80px;",
        tags$p(class = "text-muted", "No items selected yet.")
      ))
    }

    item_cards <- lapply(guids, function(guid) {
      item <- get_content(connect_server, connect_api_key, guid)
      title <- if (!is.null(item)) (item$title %||% item$name %||% guid) else guid

      tags$div(class = "card mb-2",
        tags$div(class = "card-body py-2 px-3 d-flex align-items-center",
          tags$span(class = "flex-grow-1 small fw-medium", title),
          tags$span(class = "text-muted small me-2", substr(guid, 1, 8)),
          actionButton(paste0("remove_", which(guids == guid)),
            "Remove", class = "btn-sm btn-outline-danger"
          )
        )
      )
    })

    tagList(
      tags$p(class = "fw-medium", paste(length(guids), "item(s) selected")),
      item_cards
    )
  })

  # Background deploy state
  deploy_handle <- reactiveVal(NULL)
  deploy_progress_id <- reactiveVal(NULL)

  observeEvent(input$save_config, {
    req(input$dashboard_guid)
    selection <- trimws(input$dashboard_guid)
    if (!nzchar(selection)) return()

    shinyjs::disable("save_config")
    shinyjs::html("save_config", "Publishing...")

    cfg <- build_config(
      title          = input$collection_title,
      description    = input$collection_description,
      intro_markdown = input$collection_intro,
      theme          = selected_theme(),
      source_type    = input$source_type,
      guids          = selected_guids(),
      tag            = input$tag_select
    )

    # CREATE if sentinel; UPDATE otherwise.
    app_id <- if (identical(selection, "__new__")) NULL else selection

    staged <- tryCatch(
      stage_bundle(template_dir = "dashboard_template", config = cfg),
      error = function(e) { notify(paste("Bundle staging failed:", e$message), "error"); NULL }
    )
    if (is.null(staged)) {
      shinyjs::enable("save_config")
      shinyjs::html("save_config", "Save & Publish")
      return()
    }

    handle <- tryCatch(
      launch_deploy(
        staged_dir       = staged,
        app_id           = app_id,
        app_title        = cfg$title,
        connect_server   = connect_server,
        connect_api_key  = connect_api_key
      ),
      error = function(e) { notify(paste("Deploy launch failed:", e$message), "error"); NULL }
    )
    if (is.null(handle)) {
      shinyjs::enable("save_config")
      shinyjs::html("save_config", "Save & Publish")
      return()
    }

    progress_id <- showNotification(
      ui = tags$div(
        tags$span(class = "spinner-border spinner-border-sm me-2",
                  role = "status", `aria-label` = "Loading"),
        "Publishing your collection..."
      ),
      type = "message",
      duration = NULL
    )

    deploy_handle(handle)
    deploy_progress_id(progress_id)
  })

  # Poll background deploy
  observe({
    handle <- deploy_handle()
    req(handle)
    invalidateLater(2000)

    if (handle$is_alive()) return()

    progress_id <- deploy_progress_id()
    if (!is.null(progress_id)) removeNotification(progress_id)

    exit_status <- handle$get_exit_status()
    if (isTRUE(exit_status == 0)) {
      result <- tryCatch(handle$get_result(), error = function(e) NULL)
      url <- result$url %||% connect_server
      showNotification(
        ui = tags$div(
          tags$p("Your collection is ready!"),
          tags$a(
            href = url, target = "_blank",
            class = "btn btn-sm btn-outline-primary mt-2",
            "Open Collection"
          )
        ),
        type = "message",
        duration = 15
      )
    } else {
      err_lines <- tryCatch(handle$read_error_lines(), error = function(e) character(0))
      msg <- if (length(err_lines) > 0) {
        paste(tail(err_lines, 6), collapse = "\n")
      } else {
        sprintf("Deploy exited with status %s", exit_status)
      }
      showNotification(paste("Publish failed:\n", msg), type = "error", duration = NULL)
    }

    deploy_handle(NULL)
    deploy_progress_id(NULL)
    shinyjs::enable("save_config")
    shinyjs::html("save_config", "Save & Publish")
  })
}

shinyApp(ui, server)
