WIZARD_STEP_TITLES <- c("Select content", "Describe", "Theme", "Preview")

.wizard_breadcrumb <- function(step) {
  parts <- lapply(seq_along(WIZARD_STEP_TITLES), function(i) {
    label <- sprintf("%d. %s", i, WIZARD_STEP_TITLES[i])
    cls <- if (i == step) "fw-bold text-dark" else "text-muted"
    shiny::tags$span(class = cls, label)
  })
  sep <- shiny::tags$span(class = "mx-2 text-muted", "›")
  interleaved <- list()
  for (i in seq_along(parts)) {
    interleaved[[length(interleaved) + 1]] <- parts[[i]]
    if (i < length(parts)) {
      interleaved[[length(interleaved) + 1]] <- sep
    }
  }
  shiny::tags$div(class = "border-bottom pb-2 mb-3",
                  do.call(shiny::tagList, interleaved))
}

.wizard_footer <- function(step, mode) {
  primary_label <- if (step < 4) "Next" else if (mode == "edit") "Update" else "Publish"
  primary_id    <- if (step < 4) "wizard_next" else "wizard_publish"
  shiny::tags$div(
    style = "display:flex; align-items:center; justify-content:space-between;",
    shiny::tags$a(href = "https://forum.posit.co/",
                  target = "_blank",
                  class = "text-muted small",
                  "Share feedback ↗"),
    shiny::tags$div(class = "d-flex gap-2",
      if (step > 1) shiny::actionButton("wizard_back", "Back",
                                        class = "btn-outline-secondary"),
      shiny::actionButton("wizard_cancel", "Cancel",
                          class = "btn-outline-secondary"),
      shiny::actionButton(primary_id, primary_label,
                          class = "btn-primary")
    )
  )
}

wizard_modal_dialog <- function(step, mode, state, body) {
  title_text <- if (mode == "edit") {
    paste0("Edit collection: ", state$title %||% "")
  } else {
    "Add a content collection"
  }
  shiny::modalDialog(
    title = shiny::tags$div(class = "d-flex align-items-center gap-2",
      shiny::tags$span(title_text),
      shiny::tags$span(class = "badge bg-info text-dark", "BETA")
    ),
    .wizard_breadcrumb(step),
    body,
    footer = .wizard_footer(step, mode),
    size = "l",
    easyClose = FALSE,
    fade = FALSE
  )
}
