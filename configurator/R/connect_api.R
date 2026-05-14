# Connect API helpers. All functions take connect_server / connect_api_key as
# explicit arguments so they're easy to test or stub.

# Returns an API key scoped to the visitor making the current Shiny request,
# minted via `connectapi::connect()`'s OAuth token exchange. Requires that a
# "Posit Connect API" (Visitor API Key) OAuth integration is associated with
# the deployed Configurator content — see README.
#
# Falls back to `fallback_api_key` (the publisher's CONNECT_API_KEY) when no
# user-session-token is present (running locally, or no integration attached)
# or when the exchange fails. The publisher key keeps local development and
# any non-session code path working.
#
# Mint per-action rather than once per session: Connect-minted visitor keys
# are short-lived, so caching at session start would break long-lived
# Shiny sessions. The caller can layer its own session-scoped cache if the
# extra HTTP cost matters.
visitor_api_key <- function(session, connect_server, fallback_api_key) {
  token <- if (!is.null(session)) {
    session$request$HTTP_POSIT_CONNECT_USER_SESSION_TOKEN
  } else {
    NULL
  }
  if (is.null(token) || !nzchar(token)) {
    return(fallback_api_key)
  }

  audience <- Sys.getenv("CONNECT_VISITOR_INTEGRATION_GUID", "")
  audience <- if (nzchar(audience)) audience else NULL

  tryCatch({
    client <- connectapi::connect(
      server          = connect_server,
      api_key         = fallback_api_key,
      token           = token,
      audience        = audience,
      .check_is_fatal = FALSE
    )
    client$api_key
  }, error = function(e) {
    warning(sprintf(
      "visitor_api_key: token exchange failed (%s); using publisher key.",
      conditionMessage(e)
    ))
    fallback_api_key
  })
}

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
        q = paste("published:true locked:false", query),
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
  }, error = function(e) { message("get_content error: ", e$message); NULL })
}

# Discovery: list collection dashboards via the marker in `name`.
# Uses the search endpoint (broad text match) and filters client-side so
# we only return items where the marker is actually the name prefix.
fetch_collection_dashboards <- function(connect_server, connect_api_key) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key, "/__api__/v1/search/content") |>
      httr2::req_url_query(
        q = COLLECTION_NAME_MARKER,
        include = "owner",
        page_size = 100
      ) |>
      httr2::req_perform()
    result <- httr2::resp_body_json(resp)
    items <- result$results %||% list()
    matched <- Filter(function(d) {
      n <- d$name %||% ""
      startsWith(n, paste0(COLLECTION_NAME_MARKER, "-"))
    }, items)
    message(sprintf(
      "fetch_collection_dashboards: search returned %d items, %d matched marker prefix",
      length(items), length(matched)
    ))
    matched
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

# List collections owned by the current user via Connect's `owner:@me` token.
fetch_my_collections <- function(connect_server, connect_api_key) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key, "/__api__/v1/search/content") |>
      httr2::req_url_query(
        q = paste("published:true locked:false", "owner:@me", COLLECTION_NAME_MARKER),
        include = "owner",
        page_size = 100
      ) |>
      httr2::req_perform()
    items <- httr2::resp_body_json(resp)$results %||% list()
    Filter(function(d) {
      n <- d$name %||% ""
      startsWith(n, paste0(COLLECTION_NAME_MARKER, "-"))
    }, items)
  }, error = function(e) {
    message("fetch_my_collections error: ", e$message)
    list()
  })
}

# Build a shareable URL for a piece of Connect content. Prefers vanity_url
# (a path on the server) and falls back to the canonical /content/<guid> URL.
share_url <- function(connect_server, content) {
  server <- sub("/$", "", connect_server %||% "")
  vu <- content$vanity_url %||% ""
  if (nzchar(vu)) {
    if (!startsWith(vu, "/")) vu <- paste0("/", vu)
    paste0(server, vu)
  } else {
    paste0(server, "/content/", content$guid %||% "")
  }
}

# Search Connect for content matching the given tag.
fetch_content_by_tag <- function(connect_server, connect_api_key, tag_name) {
  tryCatch({
    resp <- api_request(connect_server, connect_api_key, "/__api__/v1/search/content") |>
      httr2::req_url_query(
        q = paste0("published:true locked:false tag:", tag_name),
        include = "owner",
        page_size = 100
      ) |>
      httr2::req_perform()
    httr2::resp_body_json(resp)$results %||% list()
  }, error = function(e) {
    message("fetch_content_by_tag error: ", e$message)
    list()
  })
}
