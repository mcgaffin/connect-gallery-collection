.beta_callout <- function() {
  shiny::tags$div(
    class = "alert alert-secondary",
    style = "background-color:#eef2ff; color:#3730a3; border-color:#c7d2fe;",
    shiny::tags$div(class = "fw-medium",
                    "Collections is an experimental feature"),
    shiny::tags$div(class = "small mt-1",
                    "While in beta, please note these limits:"),
    shiny::tags$ul(class = "small mb-2 mt-1",
      shiny::tags$li("Limited theming options"),
      shiny::tags$li("Sharing a collection only shares the collection itself — recipients still need access to each item inside it")
    ),
    shiny::tags$div(class = "small",
      "Have feedback? ",
      shiny::tags$a(href = "https://forum.posit.co/", target = "_blank",
                    class = "alert-link", "Tell us on Posit Community ↗")
    )
  )
}

.result_row <- function(item, is_selected) {
  guid  <- item$guid %||% ""
  title <- item$title %||% item$name %||% "Untitled"
  mode  <- item$app_mode %||% ""
  icon  <- content_icon_path(mode)
  label <- content_type_label(mode)
  shiny::tags$div(
    class = paste("d-flex align-items-center gap-3 py-2 px-3 border-top",
                  if (is_selected) "bg-light" else ""),
    shiny::tags$input(type = "checkbox",
      id = paste0("result_", guid),
      class = "form-check-input result-checkbox",
      `data-guid` = guid,
      checked = if (is_selected) "checked" else NULL),
    shiny::tags$img(src = icon, width = "28", height = "28",
                    style = "flex-shrink:0;"),
    shiny::tags$div(class = "flex-grow-1",
      shiny::tags$div(class = "fw-medium", title),
      shiny::tags$div(class = "text-muted small", label)
    )
  )
}

step_select_ui <- function(state, search_query, search_results, all_tags) {
  source_type <- state$source_type %||% "manual"
  selected_guids <- state$guids %||% character(0)

  # Tag choices: only child tags (those with parent_id set)
  tag_choices <- c("Select a tag..." = "")
  for (t in all_tags) {
    if (!is.null(t$parent_id)) tag_choices[t$name] <- t$name
  }

  # Manual-mode body
  manual_body <- shiny::tagList(
    shiny::tags$div(class = "input-group",
      shiny::textInput("search_query", label = NULL,
                       value = search_query %||% "",
                       placeholder = "Search for content to add...",
                       width = "100%"),
      shiny::tags$span(class = "input-group-text",
                       sprintf("%d selected", length(selected_guids)))
    ),
    if (length(search_results) == 0) {
      shiny::tags$div(class = "text-center text-muted my-4 p-4",
        style = "border:1px dashed #ced4da; border-radius:0.5rem;",
        if (nzchar(search_query %||% ""))
          "No content matches your search."
        else
          "Start typing to find content you've published or have access to"
      )
    } else {
      shiny::tagList(
        shiny::tags$div(class = "py-2 px-3 border-top border-bottom",
          shiny::actionButton("select_all",
                              sprintf("Select all %d", length(search_results)),
                              class = "btn-link p-0")
        ),
        shiny::tags$div(class = "result-list",
          lapply(search_results, function(item) {
            .result_row(item, (item$guid %||% "") %in% selected_guids)
          })
        )
      )
    }
  )

  # Tag-mode body
  tag_body <- shiny::tagList(
    shiny::selectInput("tag_select", "Select Tag",
                       choices = tag_choices,
                       selected = state$source_tag %||% ""),
    shiny::tags$p(class = "form-text",
      "Content tagged with this tag will be included automatically each time the collection renders. Newly tagged content shows up on the next render.")
  )

  shiny::tagList(
    shiny::tags$div(class = "wizard-step-body",
      .beta_callout(),
      shiny::tags$div(class = "segmented-radio mb-3",
        shiny::radioButtons("source_type", label = NULL,
          choices = c("Select content" = "manual", "Use a tag" = "tag"),
          selected = source_type, inline = TRUE)
      ),
      if (identical(source_type, "manual")) manual_body else tag_body
    )
  )
}
