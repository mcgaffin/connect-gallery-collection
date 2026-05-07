.format_home_date <- function(dt) {
  if (is.null(dt) || !nzchar(dt)) return("")
  d <- tryCatch(as.Date(dt), error = function(e) NA)
  if (is.na(d)) "" else format(d, "%b %d, %Y")
}

.home_row <- function(coll) {
  guid  <- coll$guid %||% ""
  title <- coll$title %||% coll$name %||% guid
  date  <- .format_home_date(coll$last_deployed_time)
  shiny::tags$div(class = "d-flex align-items-center py-3 px-3 border-bottom",
    shiny::tags$div(class = "flex-grow-1",
      shiny::tags$div(class = "fw-medium", title),
      shiny::tags$div(class = "text-muted small",
        if (nzchar(date)) paste("Last published:", date) else "")
    ),
    shiny::actionButton(paste0("edit_", guid), "Edit",
                        class = "btn-sm btn-outline-primary")
  )
}

home_view <- function(collections) {
  shiny::tagList(
    shiny::tags$div(class = "container py-4",
      shiny::tags$div(class = "d-flex align-items-center justify-content-between mb-4",
        shiny::tags$h1(class = "h3 mb-0", "Content Collections"),
        shiny::actionButton("new_collection", "+ New collection",
                            class = "btn-primary")
      ),
      if (length(collections) == 0) {
        shiny::tags$div(class = "text-center text-muted py-5",
          style = "border:1px dashed #ced4da; border-radius:0.5rem;",
          "You haven't created any collections yet. Click 'New collection' to get started."
        )
      } else {
        shiny::tags$div(class = "border rounded",
          lapply(collections, .home_row)
        )
      },
      shiny::tags$div(class = "mt-4 text-end",
        shiny::tags$a(href = "https://forum.posit.co/",
                      target = "_blank",
                      class = "text-muted small",
                      "Share feedback ↗")
      )
    )
  )
}
