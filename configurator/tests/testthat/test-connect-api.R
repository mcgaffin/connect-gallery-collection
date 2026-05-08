test_that("share_url uses vanity_url when present", {
  url <- share_url("https://connect.example.com",
                   list(guid = "g1", vanity_url = "/team/dashboard/"))
  expect_equal(url, "https://connect.example.com/team/dashboard/")
})

test_that("share_url adds a leading slash to vanity_url if missing", {
  url <- share_url("https://connect.example.com",
                   list(guid = "g1", vanity_url = "team/dash"))
  expect_equal(url, "https://connect.example.com/team/dash")
})

test_that("share_url falls back to /content/<guid> when vanity_url is empty", {
  url <- share_url("https://connect.example.com",
                   list(guid = "g1", vanity_url = ""))
  expect_equal(url, "https://connect.example.com/content/g1")
})

test_that("share_url falls back when vanity_url is missing", {
  url <- share_url("https://connect.example.com", list(guid = "g1"))
  expect_equal(url, "https://connect.example.com/content/g1")
})

test_that("share_url strips a trailing slash from connect_server", {
  url <- share_url("https://connect.example.com/",
                   list(guid = "g1"))
  expect_equal(url, "https://connect.example.com/content/g1")
})
