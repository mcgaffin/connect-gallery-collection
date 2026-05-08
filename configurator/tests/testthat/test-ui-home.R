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

test_that("home_view renders an Open link per row pointing at the deployed URL", {
  collections <- list(
    list(guid = "g1", title = "Coll A", last_deployed_time = "2026-01-01T00:00:00Z")
  )
  ui <- home_view(collections = collections,
                   connect_server = "https://connect.example.com")
  html <- as.character(ui)
  expect_match(html, ">Open</a>", fixed = TRUE)
  expect_match(html, 'href="https://connect.example.com/content/g1/"', fixed = TRUE)
})

test_that("home_view renders a Share-this-collection link with copy_<guid> id", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  ui <- home_view(collections = collections)
  html <- as.character(ui)
  expect_match(html, 'id="copy_g1"', fixed = TRUE)
  expect_match(html, "Share this collection", fixed = TRUE)
  # SVG clipboard icon is inlined alongside the label.
  expect_match(html, "<svg", fixed = TRUE)
})

test_that("home_view shows a loading placeholder when metadata is missing", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  ui <- home_view(collections = collections)
  html <- as.character(ui)
  expect_match(html, "Loading details", fixed = TRUE)
})

test_that("home_view shows item count for search-based collections", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  meta <- list(g1 = list(source_type = "manual", n_items = 5))
  ui <- home_view(collections = collections, collection_meta = meta)
  html <- as.character(ui)
  expect_match(html, ">5 items<", fixed = TRUE)
  # The old "Search-based" prefix is gone.
  expect_no_match(html, "Search-based")
})

test_that("home_view singularizes item count when there is one item", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  meta <- list(g1 = list(source_type = "manual", n_items = 1))
  ui <- home_view(collections = collections, collection_meta = meta)
  html <- as.character(ui)
  expect_match(html, ">1 item<", fixed = TRUE)
})

test_that("home_view shows tag name as 'Tag: <name>'", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  meta <- list(g1 = list(source_type = "tag", source_tag = "finance"))
  ui <- home_view(collections = collections, collection_meta = meta)
  html <- as.character(ui)
  expect_match(html, "Tag: finance", fixed = TRUE)
  expect_no_match(html, "Tag-based:")
})

test_that("home_view renders the collection description when present", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  meta <- list(g1 = list(source_type = "manual", n_items = 0,
                          description = "A short description."))
  ui <- home_view(collections = collections, collection_meta = meta)
  html <- as.character(ui)
  expect_match(html, "A short description.", fixed = TRUE)
})

test_that("home_view truncates long descriptions with an ellipsis", {
  long <- paste(rep("a", 200), collapse = "")
  collections <- list(list(guid = "g1", title = "Coll A"))
  meta <- list(g1 = list(source_type = "manual", n_items = 0,
                          description = long))
  ui <- home_view(collections = collections, collection_meta = meta)
  html <- as.character(ui)
  expect_match(html, paste0(paste(rep("a", 120), collapse = ""), "…"),
               fixed = TRUE)
})

test_that("home_view falls back to the Connect content description when meta is missing", {
  collections <- list(list(guid = "g1", title = "Coll A",
                           description = "from Connect"))
  ui <- home_view(collections = collections)
  html <- as.character(ui)
  expect_match(html, "from Connect", fixed = TRUE)
})

test_that("home_view renders thumbnail_url when present (relative path)", {
  collections <- list(list(guid = "g1", title = "Coll A",
                           thumbnail_url = "/content/g1/__icon__/abc"))
  ui <- home_view(collections = collections,
                   connect_server = "https://connect.example.com")
  html <- as.character(ui)
  expect_match(html,
    'src="https://connect.example.com/content/g1/__icon__/abc"',
    fixed = TRUE)
})

test_that("home_view passes through absolute thumbnail URLs unchanged", {
  collections <- list(list(guid = "g1", title = "Coll A",
                           thumbnail_url = "https://cdn.example.com/x.png"))
  ui <- home_view(collections = collections,
                   connect_server = "https://connect.example.com")
  html <- as.character(ui)
  expect_match(html, 'src="https://cdn.example.com/x.png"', fixed = TRUE)
})

test_that("home_view falls back to icons/collection.svg when no thumbnail_url", {
  collections <- list(list(guid = "g1", title = "Coll A"))
  ui <- home_view(collections = collections)
  html <- as.character(ui)
  expect_match(html, 'src="icons/collection.svg"', fixed = TRUE)
})

test_that("home_view also uses collection.svg as the broken-image fallback", {
  collections <- list(list(guid = "g1", title = "Coll A",
                           thumbnail_url = "/content/g1/__icon__/abc"))
  ui <- home_view(collections = collections,
                   connect_server = "https://connect.example.com")
  html <- as.character(ui)
  expect_match(html, "icons/collection.svg", fixed = TRUE)
})
