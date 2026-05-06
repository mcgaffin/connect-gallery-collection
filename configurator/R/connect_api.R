# Connect API helpers. All functions take connect_server / connect_api_key as
# explicit arguments so they're easy to test or stub.

api_request <- function(connect_server, connect_api_key, path) {
  req <- httr2::request(paste0(connect_server, path))
  if (nzchar(connect_api_key)) {
    req <- httr2::req_headers(req, Authorization = paste("Key", connect_api_key))
  }
  req
}

search_content <- function(connect_server, connect_api_key, query) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key, "/__api__/v1/search/content") |>
      httr2::req_url_query(
        q = paste("published:true", query),
        include = "owner",
        page_size = 20
      ) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)$results %||% list()
  }, error = function(e) {
    message("search_content error: ", e$message)
    list()
  })
}

get_tags <- function(connect_server, connect_api_key) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key, "/__api__/v1/tags") |>
      httr2::req_perform()
    httr2::resp_body_json(resp)
  }, error = function(e) { message("get_tags error: ", e$message); list() })
}

get_content <- function(connect_server, connect_api_key, guid) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key,
                        paste0("/__api__/v1/content/", guid)) |>
      httr2::req_url_query(include = "owner") |>
      httr2::req_perform()
    httr2::resp_body_json(resp)
  }, error = function(e) NULL)
}

# Discovery: list all collection dashboards via the marker in `name`.
fetch_collection_dashboards <- function(connect_server, connect_api_key) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key, "/__api__/v1/content") |>
      httr2::req_url_query(name = COLLECTION_NAME_MARKER) |>
      httr2::req_perform()
    httr2::resp_body_json(resp) %||% list()
  }, error = function(e) {
    message("fetch_collection_dashboards error: ", e$message)
    list()
  })
}

# Download the content's currently-active source bundle to a tempfile.
# Returns the tempfile path, or NULL on failure.
download_active_bundle <- function(connect_server, connect_api_key, guid) {
  tryCatch({
    info <- get_content(connect_server, connect_api_key, guid)
    bundle_id <- info$bundle_id %||% NULL
    if (is.null(bundle_id)) return(NULL)

    out <- tempfile(fileext = ".tar.gz")
    api_request(connect_server, connect_api_key,
                paste0("/__api__/v1/content/", guid,
                       "/bundles/", bundle_id, "/download")) |>
      httr2::req_perform(path = out)
    out
  }, error = function(e) {
    message("download_active_bundle error: ", e$message)
    NULL
  })
}
