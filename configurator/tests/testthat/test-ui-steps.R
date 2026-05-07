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
