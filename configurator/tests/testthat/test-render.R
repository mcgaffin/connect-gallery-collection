test_that("THEME_COLORS exposes all six themes with required color keys", {
  expect_setequal(names(THEME_COLORS),
                  c("warm", "cool", "minimal", "fun", "bold", "earth"))
  for (name in names(THEME_COLORS)) {
    t <- THEME_COLORS[[name]]
    expect_true(all(c("label", "bg", "accent", "border", "text") %in% names(t)),
                info = sprintf("theme %s missing keys", name))
  }
})

test_that("build_collection_html returns non-empty HTML with the title", {
  cfg <- list(title = "My Collection", description = "",
              intro_markdown = "", theme = "minimal",
              source_type = "manual", guids = character(0))
  html <- build_collection_html(cfg, items = list(), theme_colors = THEME_COLORS)
  expect_type(html, "character")
  expect_gt(nchar(html), 100)
  expect_match(html, "My Collection", fixed = TRUE)
})

test_that("build_collection_html escapes HTML in the title", {
  cfg <- list(title = "<script>alert(1)</script>", description = "",
              intro_markdown = "", theme = "minimal",
              source_type = "manual", guids = character(0))
  html <- build_collection_html(cfg, items = list(), theme_colors = THEME_COLORS)
  expect_no_match(html, "<script>alert", fixed = TRUE)
  expect_match(html, "&lt;script&gt;", fixed = TRUE)
})

test_that("build_collection_html renders one card per item", {
  cfg <- list(title = "T", description = "", intro_markdown = "",
              theme = "minimal", source_type = "manual",
              guids = c("a", "b"))
  items <- list(
    list(guid = "a", title = "Alpha", description = "first item",
         app_mode = "shiny", last_deployed_time = "2026-01-01T00:00:00Z"),
    list(guid = "b", title = "Beta",  description = "second item",
         app_mode = "quarto-static", last_deployed_time = "2026-02-02T00:00:00Z")
  )
  html <- build_collection_html(cfg, items = items, theme_colors = THEME_COLORS)
  expect_match(html, "Alpha", fixed = TRUE)
  expect_match(html, "Beta",  fixed = TRUE)
  expect_match(html, "2 item", fixed = TRUE)
})

test_that("build_collection_html renders intro markdown when present", {
  cfg <- list(title = "T", description = "", intro_markdown = "# Hello\n\nWorld",
              theme = "minimal", source_type = "manual", guids = character(0))
  html <- build_collection_html(cfg, items = list(), theme_colors = THEME_COLORS)
  expect_match(html, "<h1", fixed = TRUE)
  expect_match(html, "Hello", fixed = TRUE)
})

test_that("build_collection_html applies theme background color", {
  cfg <- list(title = "T", description = "", intro_markdown = "",
              theme = "warm", source_type = "manual", guids = character(0))
  html <- build_collection_html(cfg, items = list(), theme_colors = THEME_COLORS)
  expect_match(html, THEME_COLORS$warm$bg, fixed = TRUE)
})
