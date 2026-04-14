library(shiny)
library(bslib)
library(httr2)
library(jsonlite)

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
    resp <- request(paste0(connect_server, "/__api__/v1/content")) |>
      api_headers() |>
      req_url_query(
        search = query,
        include = "owner"
      ) |>
      req_perform()
    resp_body_json(resp)
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

get_content_env <- function(guid) {
  tryCatch({
    resp <- request(paste0(connect_server, "/__api__/v1/content/", guid, "/environment")) |>
      api_headers() |>
      req_perform()
    resp_body_json(resp)
  }, error = function(e) list())
}

set_content_env <- function(guid, env_vars) {
  request(paste0(connect_server, "/__api__/v1/content/", guid, "/environment")) |>
    api_headers() |>
    req_method("PATCH") |>
    req_body_json(env_vars) |>
    req_perform()
}

update_content <- function(guid, title, description) {
  request(paste0(connect_server, "/__api__/v1/content/", guid)) |>
    api_headers() |>
    req_method("PATCH") |>
    req_body_json(list(title = title, description = description)) |>
    req_perform()
}

render_content <- function(guid) {
  tryCatch({
    request(paste0(connect_server, "/__api__/v1/content/", guid, "/render")) |>
      api_headers() |>
      req_method("POST") |>
      req_perform()
    TRUE
  }, error = function(e) {
    message("Render error: ", e$message)
    FALSE
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

  sidebar = sidebar(
    width = 380,
    title = "Configuration",

    # Target dashboard
    textInput("dashboard_guid", "Dashboard GUID",
      placeholder = "Paste the GUID of your collection dashboard"
    ),
    actionButton("load_existing", "Load Existing Config", class = "btn-sm btn-outline-secondary mb-3"),

    hr(),

    # Metadata
    textInput("collection_title", "Title", placeholder = "My Collection"),
    textInput("collection_description", "Description", placeholder = "A short description"),
    textAreaInput("collection_intro", "Introduction (Markdown)",
      rows = 4, placeholder = "Write an intro. Markdown supported."
    ),

    hr(),

    # Theme
    tags$label("Theme", class = "control-label"),
    radioButtons("theme", NULL,
      choices = setNames(names(themes), sapply(themes, function(t) t$label)),
      selected = "minimal",
      inline = TRUE
    ),

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
      actionButton("search_btn", "Search", class = "btn-sm btn-primary mb-2")
    ),

    hr(),

    # Save
    actionButton("save_config", "Save & Render", class = "btn-primary btn-lg w-100")
  ),

  # Main content area
  navset_card_tab(
    title = "Collection Preview",

    nav_panel("Search Results",
      conditionalPanel(
        condition = "input.source_type == 'manual'",
        tags$div(id = "search_results",
          uiOutput("search_results_ui")
        )
      ),
      conditionalPanel(
        condition = "input.source_type == 'tag'",
        tags$p(class = "text-muted mt-3",
          "Content will be dynamically included based on the selected tag."
        )
      )
    ),

    nav_panel("Selected Items",
      tags$div(class = "mt-3",
        uiOutput("selected_items_ui")
      )
    ),

    nav_panel("Status",
      tags$div(class = "mt-3",
        uiOutput("status_ui")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  selected_guids <- reactiveVal(character(0))
  search_results <- reactiveVal(list())
  status_message <- reactiveVal("Ready. Enter a dashboard GUID and configure your collection.")
  all_tags <- reactiveVal(list())

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

  # Search content
  observeEvent(input$search_btn, {
    req(input$search_query)
    results <- search_content(input$search_query)
    search_results(results)
  })

  # Load existing config
  observeEvent(input$load_existing, {
    req(input$dashboard_guid)
    guid <- trimws(input$dashboard_guid)

    # Fetch content info
    content <- get_content(guid)
    if (!is.null(content)) {
      updateTextInput(session, "collection_title", value = content$title %||% "")
      updateTextInput(session, "collection_description", value = content$description %||% "")
    }

    # Fetch env vars
    env <- get_content_env(guid)
    for (e in env) {
      if (e$name == "COLLECTION_CONFIG") {
        config <- tryCatch(fromJSON(e$value), error = function(err) list())
        if (!is.null(config$intro_markdown)) {
          updateTextAreaInput(session, "collection_intro", value = config$intro_markdown)
        }
        if (!is.null(config$theme)) {
          updateRadioButtons(session, "theme", selected = config$theme)
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
        status_message("Loaded existing configuration.")
        break
      }
    }
  })

  # Render search results with select buttons
  output$search_results_ui <- renderUI({
    results <- search_results()
    if (length(results) == 0) {
      return(tags$p(class = "text-muted", "No results yet. Search for content above."))
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
      return(tags$p(class = "text-muted", "No items selected yet."))
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

  # Save and render
  observeEvent(input$save_config, {
    req(input$dashboard_guid)
    guid <- trimws(input$dashboard_guid)

    # Build config
    config <- list(
      title = input$collection_title,
      description = input$collection_description,
      intro_markdown = input$collection_intro,
      theme = input$theme,
      source_type = input$source_type
    )

    if (input$source_type == "tag") {
      config$source_tag <- input$tag_select
    } else {
      config$guids <- selected_guids()
    }

    config_json <- toJSON(config, auto_unbox = TRUE)

    status_message("Saving configuration...")

    tryCatch({
      # Update title and description on the content item
      update_content(guid, input$collection_title, input$collection_description)

      # Set the environment variable
      env_payload <- list(list(name = "COLLECTION_CONFIG", value = as.character(config_json)))
      set_content_env(guid, env_payload)

      status_message("Configuration saved. Triggering render...")

      # Trigger re-render
      success <- render_content(guid)

      if (success) {
        dashboard_url <- paste0(connect_server, "/content/", guid, "/")
        status_message(paste0(
          "Collection updated and rendering! ",
          "View it at: ", dashboard_url
        ))
      } else {
        status_message("Configuration saved but render could not be triggered. You may need to manually re-render the dashboard.")
      }
    }, error = function(e) {
      status_message(paste("Error:", e$message))
    })
  })

  # Status output
  output$status_ui <- renderUI({
    msg <- status_message()
    tags$div(class = "alert alert-info", msg)
  })
}

shinyApp(ui, server)
