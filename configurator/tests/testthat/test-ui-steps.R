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
