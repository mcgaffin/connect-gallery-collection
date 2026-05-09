THEME_COLORS <- list(
  warm    = list(label = "Warm",    bg = "#fffbeb", accent = "#d97706", border = "#fde68a", text = "#92400e"),
  cool    = list(label = "Cool",    bg = "#eff6ff", accent = "#2563eb", border = "#bfdbfe", text = "#1e40af"),
  minimal = list(label = "Minimal", bg = "#fafafa", accent = "#525252", border = "#e5e5e5", text = "#404040"),
  fun     = list(label = "Fun",     bg = "#fdf2f8", accent = "#db2777", border = "#fbcfe8", text = "#9d174d"),
  bold    = list(label = "Bold",    bg = "#eef2ff", accent = "#4338ca", border = "#c7d2fe", text = "#3730a3"),
  earth   = list(label = "Earth",   bg = "#f0fdf4", accent = "#15803d", border = "#bbf7d0", text = "#166534")
)

# Internal helpers
.format_date <- function(dt) {
  if (is.null(dt) || !nzchar(dt)) return("")
  d <- tryCatch(as.Date(dt), error = function(e) NA)
  if (is.na(d)) "" else format(d, "%m/%d/%Y")
}

.owner_name <- function(item) {
  owner <- item$owner
  if (is.list(owner)) {
    first <- owner$first_name %||% ""
    last  <- owner$last_name %||% ""
  } else {
    first <- item$owner_first_name %||% ""
    last  <- item$owner_last_name %||% ""
  }
  trimws(paste(first, last))
}

.content_url <- function(connect_server, guid) {
  paste0(connect_server %||% "", "/content/", guid, "/")
}

# Connect content descriptions sometimes contain stray HTML (from prior
# tooling, or copy-pasted markup). Strip tags so cards show plain text only.
.strip_html <- function(text) {
  if (is.null(text) || !nzchar(text)) return("")
  # Remove tags and collapse whitespace.
  out <- gsub("<[^>]*>", " ", text)
  out <- gsub("\\s+", " ", out)
  trimws(out)
}

# Render a collection to a single HTML string.
# `items` is a list of lists with at least: guid, title, description,
# app_mode, last_deployed_time, owner (or owner_first/last_name fields).
# `connect_server` is used to build per-item links; pass "" or NULL if unknown.
build_collection_html <- function(config, items, theme_colors,
                                  connect_server = NULL) {
  colors <- theme_colors[[config$theme %||% "minimal"]] %||% theme_colors$minimal

  esc <- htmltools::htmlEscape

  # Style block
  style_html <- sprintf('<style>
body { background-color: %s; font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif; }
.collection-header { max-width: 960px; margin: 0 auto; padding: 2rem 2rem 0; }
.collection-title { font-size: 1.75rem; font-weight: 700; color: #111; margin: 0; }
.collection-description { margin-top: 0.25rem; color: #666; font-size: 1rem; }
.collection-panel { max-width: 960px; margin: 1.5rem auto; background: white; border-radius: 0.5rem; padding: 1.25rem; border: 1px solid %s; }
.collection-intro { border-left: 4px solid %s; font-size: 0.9rem; line-height: 1.6; }
.collection-intro h1, .collection-intro h2, .collection-intro h3 { margin-top: 0.75rem; margin-bottom: 0.5rem; font-weight: 600; }
.collection-intro p { margin-bottom: 0.5rem; }
.collection-intro a { color: %s; }
.collection-count { font-size: 0.875rem; font-weight: 500; color: #666; margin-bottom: 1rem; }
.collection-grid { display: grid; grid-template-columns: repeat(auto-fill, minmax(280px, 1fr)); gap: 1rem; }
.collection-card { display: flex; flex-direction: column; padding: 1rem; background: white; border: 1px solid %s; border-radius: 0.5rem; text-decoration: none; color: #111; transition: box-shadow 0.15s ease, border-color 0.15s ease; }
.collection-card:hover { box-shadow: 0 2px 8px rgba(0,0,0,0.08); border-color: %s; }
.collection-card__title { font-size: 0.9375rem; font-weight: 600; color: %s; }
.collection-card__description { margin-top: 0.375rem; font-size: 0.8125rem; color: #666; display: -webkit-box; -webkit-line-clamp: 3; -webkit-box-orient: vertical; overflow: hidden; flex: 1; }
.collection-card__meta { margin-top: 0.75rem; display: flex; justify-content: space-between; align-items: center; font-size: 0.75rem; color: #999; }
.collection-card__type { background: %s; padding: 0.125rem 0.5rem; border-radius: 0.25rem; font-size: 0.6875rem; }
.collection-card__owner { font-size: 0.75rem; color: #999; }
#quarto-header, #quarto-footer, .quarto-title-block, #title-block-header { display: none !important; }
/* Strip default styling from Quarto cell wrappers so empty cells (from the
   include:false setup chunk and from results:asis output containers) do not
   render as visible empty boxes. */
.cell, .cell-output, .cell-output-stdout, .cell-output-display, .quarto-layout-row {
  margin: 0 !important;
  padding: 0 !important;
  border: 0 !important;
  background: transparent !important;
  box-shadow: none !important;
}
.cell:empty, .cell-output:empty { display: none !important; }
</style>',
    colors$bg, colors$border, colors$accent, colors$accent,
    colors$border, colors$accent, colors$accent, colors$bg)

  parts <- character(0)
  parts <- c(parts, style_html)

  # Header
  parts <- c(parts, '<div class="collection-header">')
  parts <- c(parts, sprintf('<h1 class="collection-title">%s</h1>',
                            esc(config$title %||% "")))
  desc <- config$description %||% ""
  if (nzchar(desc)) {
    parts <- c(parts, sprintf('<p class="collection-description">%s</p>',
                              esc(desc)))
  }
  parts <- c(parts, '</div>')

  # Intro panel
  intro <- config$intro_markdown %||% ""
  if (nzchar(intro)) {
    intro_html <- markdown::markdownToHTML(text = intro, fragment.only = TRUE)
    parts <- c(parts, sprintf(
      '<div class="collection-panel collection-intro">%s</div>', intro_html))
  }

  # Items panel
  parts <- c(parts, '<div class="collection-panel">')
  parts <- c(parts, sprintf('<div class="collection-count">%d item(s)</div>',
                            length(items)))
  parts <- c(parts, '<div class="collection-grid">')

  for (item in items) {
    guid <- item$guid %||% ""
    title <- item$title %||% item$name %||% "Untitled"
    description <- .strip_html(item$description %||% "")
    if (nchar(description) > 120) {
      description <- paste0(substr(description, 1, 120), "...")
    }
    app_mode <- item$app_mode %||% ""
    date <- .format_date(item$last_deployed_time)
    owner <- .owner_name(item)
    url <- .content_url(connect_server, guid)
    type <- content_type_label(app_mode)

    parts <- c(parts, sprintf(
      '<a class="collection-card" href="%s" target="_blank">
         <div class="collection-card__title">%s</div>
         <div class="collection-card__description">%s</div>
         <div class="collection-card__meta">
           <span class="collection-card__type">%s</span>
           <span class="collection-card__owner">%s</span>
           <span>%s</span>
         </div>
       </a>',
      esc(url), esc(title), esc(description),
      esc(type), esc(owner), esc(date)))
  }

  parts <- c(parts, '</div></div>')
  paste(parts, collapse = "\n")
}
