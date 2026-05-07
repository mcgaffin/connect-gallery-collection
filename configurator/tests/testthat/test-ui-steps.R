test_that("step_describe_ui returns a tag containing all three input ids", {
  ui <- step_describe_ui(state = list(title = "", description = "",
                                      intro_markdown = ""))
  html <- as.character(ui)
  expect_match(html, 'id="collection_title"',       fixed = TRUE)
  expect_match(html, 'id="collection_description"', fixed = TRUE)
  expect_match(html, 'id="collection_intro"',       fixed = TRUE)
})

test_that("step_describe_ui prefills inputs from state", {
  ui <- step_describe_ui(state = list(title = "My title",
                                      description = "Short",
                                      intro_markdown = "# Hi"))
  html <- as.character(ui)
  expect_match(html, "My title", fixed = TRUE)
  expect_match(html, "Short",    fixed = TRUE)
  expect_match(html, "# Hi",     fixed = TRUE)
})

test_that("step_theme_ui renders one button per theme", {
  ui <- step_theme_ui(state = list(theme = "minimal"))
  html <- as.character(ui)
  for (name in names(THEME_COLORS)) {
    expect_match(html, sprintf('id="theme_%s"', name), fixed = TRUE,
                 info = sprintf("missing theme button: %s", name))
  }
})

test_that("step_theme_ui marks the selected theme with an aria pressed attribute", {
  ui <- step_theme_ui(state = list(theme = "warm"))
  html <- as.character(ui)
  expect_match(html, 'aria-pressed="true"[^>]*id="theme_warm"',
               perl = TRUE)
  expect_match(html, 'aria-pressed="false"[^>]*id="theme_minimal"',
               perl = TRUE)
})

test_that("step_select_ui renders the source-type toggle and beta callout", {
  ui <- step_select_ui(
    state = list(source_type = "manual", source_tag = "",
                 guids = character(0)),
    search_query = "", search_results = list(), all_tags = list()
  )
  html <- as.character(ui)
  expect_match(html, 'id="source_type"',     fixed = TRUE)
  expect_match(html, "Select content",       fixed = TRUE)
  expect_match(html, "Use a tag",            fixed = TRUE)
  expect_match(html, "experimental feature", fixed = TRUE)
  expect_match(html, "Posit Community",      fixed = TRUE)
})

test_that("step_select_ui shows search input + empty hint in manual mode with no results", {
  ui <- step_select_ui(
    state = list(source_type = "manual", source_tag = "",
                 guids = character(0)),
    search_query = "", search_results = list(), all_tags = list()
  )
  html <- as.character(ui)
  expect_match(html, 'id="search_query"', fixed = TRUE)
  expect_match(html, "Start typing",      fixed = TRUE)
})

test_that("step_select_ui renders one row per result with checkbox and icon", {
  results <- list(
    list(guid = "g1", title = "First", description = "Hello",
         app_mode = "quarto-static"),
    list(guid = "g2", title = "Second", description = "",
         app_mode = "shiny")
  )
  ui <- step_select_ui(
    state = list(source_type = "manual", source_tag = "",
                 guids = c("g1")),
    search_query = "x", search_results = results, all_tags = list()
  )
  html <- as.character(ui)
  expect_match(html, "First",  fixed = TRUE)
  expect_match(html, "Second", fixed = TRUE)
  expect_match(html, "Quarto", fixed = TRUE)
  expect_match(html, "Shiny",  fixed = TRUE)
  expect_match(html, "icons/quarto.svg", fixed = TRUE)
  expect_match(html, "icons/shiny.svg",  fixed = TRUE)
  # The "Select all" checkbox uses a fixed input id
  expect_match(html, 'id="select_all"', fixed = TRUE)
})

test_that("step_select_ui shows tag selector in tag mode", {
  ui <- step_select_ui(
    state = list(source_type = "tag", source_tag = "",
                 guids = character(0)),
    search_query = "", search_results = list(),
    all_tags = list(list(name = "favorites", parent_id = "1"),
                    list(name = "research", parent_id = "1"))
  )
  html <- as.character(ui)
  expect_match(html, 'id="tag_select"', fixed = TRUE)
  expect_match(html, "favorites",       fixed = TRUE)
  expect_match(html, "research",        fixed = TRUE)
})
