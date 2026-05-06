test_that("stage_bundle copies template files and writes collection.json", {
  tmpl <- tempfile(); dir.create(tmpl)
  writeLines("---\ntitle: x\n---", file.path(tmpl, "index.qmd"))
  writeLines("dummy renv", file.path(tmpl, "renv.lock"))

  cfg <- list(title = "T", theme = "warm", source_type = "manual",
              guids = c("a", "b"))

  staged <- stage_bundle(template_dir = tmpl, config = cfg)
  on.exit(unlink(staged, recursive = TRUE), add = TRUE)

  expect_true(file.exists(file.path(staged, "index.qmd")))
  expect_true(file.exists(file.path(staged, "renv.lock")))
  expect_true(file.exists(file.path(staged, "collection.json")))

  written <- jsonlite::fromJSON(file.path(staged, "collection.json"),
                                simplifyVector = FALSE)
  expect_equal(written$title, "T")
  expect_equal(written$theme, "warm")
  expect_equal(written$guids, list("a", "b"))
})

test_that("stage_bundle errors clearly when template dir does not exist", {
  expect_error(
    stage_bundle(template_dir = "/nope/does/not/exist",
                 config = list(title = "T")),
    "template directory not found"
  )
})
