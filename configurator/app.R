library(shiny)
library(shinyjs)
library(bslib)
library(httr2)
library(jsonlite)
library(pins)
library(connectapi)

# Connect API helpers
connect_server <- Sys.getenv("CONNECT_SERVER", "http://localhost:3939")
connect_api_key <- Sys.getenv("CONNECT_API_KEY", "")

api_headers <- function(req) {
  if (nchar(connect_api_key) > 0) {
    req |> req_headers(Authorization = paste("Key", connect_api_key))
  } else {
    req
  }
}

search_content <- function(query) {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/search/content")) |>
      api_headers() |>
      req_url_query(
        q = paste("published:true", query),
        include = "owner",
        page_size = 20
      ) |>
      req_perform()
    result <- resp_body_json(resp)
    result$results %||% list()
  }, error = function(e) {
    message("Search error: ", e$message)
    list()
  })
}

get_tags <- function() {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/tags")) |>
      api_headers() |>
      req_perform()
    resp_body_json(resp)
  }, error = function(e) {
    message("Tags error: ", e$message)
    list()
  })
}

get_content <- function(guid) {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/content/", guid)) |>
      api_headers() |>
      req_url_query(include = "owner") |>
      req_perform()
    resp_body_json(resp)
  }, error = function(e) NULL)
}

# Pin board for storing collection configs
get_pin_board <- function() {
  board_connect()
}

pin_name_for <- function(guid) {
  paste0("collection_config_", guid)
}

read_collection_pin <- function(guid) {
  tryCatch({
    board <- get_pin_board()
    pin_read(board, pin_name_for(guid))
  }, error = function(e) NULL)
}

write_collection_pin <- function(guid, config) {
  board <- get_pin_board()
  pin_write(board, config, name = pin_name_for(guid),
    type = "json",
    title = paste("Collection config:", config$title %||% guid))
}

update_content <- function(guid, title, description) {
  request(paste0(connect_server, "/__api__/v1/content/", guid)) |>
    api_headers() |>
    req_method("PATCH") |>
    req_body_json(list(title = title, description = description)) |>
    req_perform()
}

# Cookie jar file for maintaining session affinity across render + poll requests
cookie_jar <- tempfile(fileext = ".txt")

render_content <- function(guid) {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/content/", guid, "/render")) |>
      api_headers() |>
      req_method("POST") |>
      req_cookie_preserve(cookie_jar) |>
      req_perform()
    result <- resp_body_json(resp)
    task_id <- result$task_id %||% NULL
    message("Render triggered for ", guid, ", task_id: ", task_id)
    task_id
  }, error = function(e) {
    message("Render error: ", e$message)
    NULL
  })
}

get_task_status <- function(task_id) {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/tasks/", task_id)) |>
      api_headers() |>
      req_url_query(wait = 1, first = 0) |>
      req_cookie_preserve(cookie_jar) |>
      req_perform()
    resp_body_json(resp)
  }, error = function(e) {
    message("Task poll error for ", task_id, ": ", e$message)
    list(finished = TRUE, code = -1, error = e$message)
  })
}

fetch_collection_dashboards <- function() {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/search/content")) |>
      api_headers() |>
      req_url_query(
        q = "published:true tag:_collection_",
        include = "owner",
        page_size = 100
      ) |>
      req_perform()
    result <- resp_body_json(resp)
    result$results %||% list()
  }, error = function(e) {
    message("Fetch collections error: ", e$message)
    list()
  })
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
    actionButton("save_config", "Save & Render", class = "btn-primary btn-lg w-100")
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
    dashboards <- fetch_collection_dashboards()
    choices <- c("Select a collection..." = "")
    for (d in dashboards) {
      label <- d$title %||% d$name %||% d$guid
      choices[label] <- d$guid
    }
    updateSelectizeInput(session, "dashboard_guid", choices = choices)
  })

  # Load tags on startup
  observe({
    tags_data <- get_tags()
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
    results <- search_content(input$search_query)
    search_results(results)
    has_searched(TRUE)
  })

  # Load config when a dashboard is selected
  observeEvent(input$dashboard_guid, {
    req(input$dashboard_guid)
    guid <- trimws(input$dashboard_guid)
    if (nchar(guid) == 0) return()

    shinyjs::show("sidebar-loading-overlay")
    on.exit({
      shinyjs::hide("sidebar-loading-overlay")
      shinyjs::removeClass("sidebar-config-wrapper", "disabled-overlay")
    })

    # Fetch content info for title/description
    content <- get_content(guid)
    if (!is.null(content)) {
      updateTextInput(session, "collection_title", value = content$title %||% "")
      updateTextInput(session, "collection_description", value = content$description %||% "")
    }

    # Read full config from pin
    config <- read_collection_pin(guid)
    if (!is.null(config)) {
      if (!is.null(config$intro_markdown)) {
        updateTextAreaInput(session, "collection_intro", value = config$intro_markdown)
      }
      if (!is.null(config$theme)) {
        selected_theme(config$theme)
      }
      if (!is.null(config$source_type)) {
        updateRadioButtons(session, "source_type", selected = config$source_type)
      }
      if (!is.null(config$source_tag)) {
        updateSelectInput(session, "tag_select", selected = config$source_tag)
      }
      if (!is.null(config$guids)) {
        selected_guids(config$guids)
      }
      notify("Loaded existing configuration.", type = "message")
    } else {
      notify("No existing config found. Title and description loaded from dashboard.", type = "warning")
    }
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
      item <- get_content(guid)
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

  # Render task polling state
  render_task_id <- reactiveVal(NULL)
  render_dashboard_guid <- reactiveVal(NULL)
  render_progress_id <- reactiveVal(NULL)

  # Save and render
  observeEvent(input$save_config, {
    req(input$dashboard_guid)
    shinyjs::disable("save_config")
    shinyjs::html("save_config", "Saving...")
    guid <- trimws(input$dashboard_guid)

    # Build config
    config <- list(
      title = input$collection_title,
      description = input$collection_description,
      intro_markdown = input$collection_intro,
      theme = selected_theme(),
      source_type = input$source_type
    )

    if (input$source_type == "tag") {
      config$source_tag <- input$tag_select
    } else {
      config$guids <- selected_guids()
    }

    tryCatch({
      # Update title and description on the content item
      update_content(guid, input$collection_title, input$collection_description)

      # Write config to pin
      write_collection_pin(guid, config)

      shinyjs::html("save_config", "Rendering...")

      # Trigger re-render
      task_id <- render_content(guid)

      if (!is.null(task_id)) {
        # Show in-progress toast
        progress_id <- showNotification(
          ui = tags$div(
            tags$span(class = "spinner-border spinner-border-sm me-2", role = "status", `aria-label` = "Loading"),
            "Rendering your collection..."
          ),
          type = "message",
          duration = NULL  # Stays until we dismiss it
        )
        render_task_id(task_id)
        render_dashboard_guid(guid)
        render_progress_id(progress_id)
      } else {
        notify("Configuration saved but render could not be triggered.", type = "warning")
      }
    }, error = function(e) {
      notify(paste("Error:", e$message), type = "error")
    })

    shinyjs::enable("save_config")
    shinyjs::html("save_config", "Save & Render")
  })

  # Poll render task status
  observe({
    task_id <- render_task_id()
    req(task_id)

    # Poll every 2 seconds
    invalidateLater(2000)

    status <- get_task_status(task_id)

    if (isTRUE(status$finished)) {
      # Dismiss the progress toast
      progress_id <- render_progress_id()
      if (!is.null(progress_id)) {
        removeNotification(progress_id)
      }

      guid <- render_dashboard_guid()
      dashboard_url <- paste0(connect_server, "/content/", guid, "/")

      if (is.null(status$code) || status$code == 0) {
        # Success
        showNotification(
          ui = tags$div(
            tags$p("Your collection is ready!"),
            tags$a(
              href = dashboard_url,
              target = "_blank",
              class = "btn btn-sm btn-outline-primary mt-2",
              "Open Collection"
            )
          ),
          type = "message",
          duration = 15
        )
      } else {
        # Render failed
        error_msg <- status$error %||% "Unknown error"
        notify(paste("Render failed:", error_msg), type = "error")
      }

      # Clear polling state
      render_task_id(NULL)
      render_dashboard_guid(NULL)
      render_progress_id(NULL)
    }
  })
}

shinyApp(ui, server)
