step_preview_ui <- function(html_string, source_type, busy) {
  body <- if (isTRUE(busy)) {
    shiny::tags$div(class = "text-center py-5",
      shiny::tags$span(class = "spinner-border me-2", role = "status"),
      "Generating preview…"
    )
  } else {
    shiny::HTML(html_string %||% "")
  }
  shiny::tagList(
    shiny::tags$div(class = "wizard-step-body",
      shiny::tags$h2(class = "h5", "Preview"),
      shiny::tags$p(class = "text-muted small",
        "This is what your collection will look like when published."),
      shiny::tags$div(
        style = "border:1px solid #dee2e6; border-radius:0.5rem; max-height:60vh; overflow:auto; background:#f8f9fa; padding:1rem;",
        body
      ),
      if (identical(source_type, "tag") && !isTRUE(busy)) {
        shiny::tags$p(class = "text-muted small mt-2",
          "This is a snapshot from now. The deployed collection always shows currently-tagged content.")
      }
    )
  )
}
