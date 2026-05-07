test_that("home_view shows the New collection button", {
  ui <- home_view(collections = list())
  html <- as.character(ui)
  expect_match(html, 'id="new_collection"', fixed = TRUE)
  expect_match(html, "New collection",      fixed = TRUE)
})

test_that("home_view shows the empty-state when there are no collections", {
  ui <- home_view(collections = list())
  html <- as.character(ui)
  expect_match(html, "haven't created any collections", fixed = TRUE)
})

test_that("home_view renders one row per collection", {
  collections <- list(
    list(guid = "g1", title = "Coll A", last_deployed_time = "2026-01-01T00:00:00Z"),
    list(guid = "g2", title = "Coll B", last_deployed_time = "2026-02-02T00:00:00Z")
  )
  ui <- home_view(collections = collections)
  html <- as.character(ui)
  expect_match(html, "Coll A", fixed = TRUE)
  expect_match(html, "Coll B", fixed = TRUE)
  expect_match(html, 'id="edit_g1"', fixed = TRUE)
  expect_match(html, 'id="edit_g2"', fixed = TRUE)
})
