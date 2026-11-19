json_request <- function() httr2::request("https://api.example.com/things")

# Caching ---------------------------------------------------------------------

test_that("cache keys are valid for cachem and don't collide", {
  keys <- c(cache_key("wikipedia Côte d'Ivoire"), cache_key("wikipedia Cote d'Ivoire"), cache_key("A b"))

  expect_true(all(grepl("^[a-z0-9_-]+$", keys)))
  expect_equal(anyDuplicated(keys), 0)
})

test_that("cached() fetches once, then returns the cached value", {
  local_empty_cache()
  calls <- 0
  fetch <- function() {
    calls <<- calls + 1
    "value"
  }

  expect_equal(cached("thing", fetch), "value")
  expect_equal(cached("thing", fetch), "value")
  expect_equal(calls, 1)
})

test_that("errors are not cached, so the next call tries again", {
  local_empty_cache()
  fail <- TRUE
  fetch <- function() if (fail) stop(api_error("down")) else "value"

  expect_error(cached("thing", fetch), class = "api_error")
  fail <- FALSE
  expect_equal(cached("thing", fetch), "value")
})

# Daily disk cache ------------------------------------------------------------

# A copy of "thing" fetched `hours` ago
store_copy <- function(value, hours) {
  disk_cache()$set(cache_key("thing"), list(fetched_at = Sys.time() - hours * 3600, value = value))
}

test_that("cached_daily() fetches once a day, and the copy survives a restart", {
  local_empty_cache()
  calls <- 0
  fetch <- function() {
    calls <<- calls + 1
    "value"
  }

  expect_equal(cached_daily("thing", fetch), "value")
  # A new cache object on the same folder, as after an app restart
  expect_equal(cached_daily("thing", fetch, cache = disk_cache()), "value")
  expect_equal(calls, 1)
})

test_that("cached_daily() refreshes a copy that's over a day old", {
  local_empty_cache()
  store_copy("old", hours = 25)

  expect_equal(cached_daily("thing", function() "new"), "new")
  expect_equal(cached_daily("thing", function() stop("not called")), "new")
})

test_that("cached_daily() keeps using an old copy when a refresh fails", {
  local_empty_cache()
  store_copy("old", hours = 25)
  fetch <- function() stop(api_error("Too many requests."))

  expect_message(value <- cached_daily("thing", fetch), "Too many requests.*using the copy from")
  expect_equal(value, "old")
})

test_that("cached_daily() errors when there's no copy and the fetch fails", {
  local_empty_cache()
  fetch <- function() stop(api_error("Too many requests."))

  expect_error(cached_daily("thing", fetch), "Too many requests.", class = "api_error")
})

# Errors ----------------------------------------------------------------------

test_that("perform_json returns the parsed body", {
  httr2::local_mocked_responses(function(req) httr2::response_json(body = list(a = 1)))

  expect_equal(perform_json(json_request(), "the Example API"), list(a = 1))
})

test_that("API error messages are passed on", {
  httr2::local_mocked_responses(function(req) {
    httr2::response_json(status_code = 400, body = list(errors = list(list(message = "Bad region."))))
  })

  expect_error(perform_json(json_request(), "the Example API"), "Bad region.", class = "api_error")
})

test_that("rate limits and rejected keys get friendly messages", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 429))
  expect_error(perform_json(json_request(), "the Example API"), "Too many requests to the Example API", class = "api_error")

  httr2::local_mocked_responses(function(req) httr2::response(status_code = 401))
  expect_error(perform_json(json_request(), "the Example API"), "The Example API rejected the API key", class = "api_error")
})

test_that("other HTTP errors say which API and status", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 503))

  err <- expect_error(perform_json(json_request(), "the Example API"), class = "api_error")
  expect_match(conditionMessage(err), "(HTTP 503)", fixed = TRUE)
  expect_equal(err$status, 503)
})

test_that("a page that isn't JSON (e.g. a maintenance page) is an api_error", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html"),
      body = charToRaw("<html><body>Down for maintenance</body></html>")
    )
  })

  expect_error(
    perform_json(json_request(), "the Example API"),
    "The Example API isn't responding normally right now.",
    class = "api_error"
  )
})

test_that("as_api_error turns unexpected errors into api_errors, logging the original", {
  err <- expect_error(
    expect_message(as_api_error(stop("subscript out of bounds"), "Couldn't read it."), "subscript out of bounds"),
    class = "api_error"
  )
  expect_equal(conditionMessage(err), "Couldn't read it.")

  # api_errors keep their own message
  expect_error(as_api_error(stop(api_error("Busy.")), "Couldn't read it."), "^Busy\\.$", class = "api_error")
  expect_equal(as_api_error(1 + 1, "unused"), 2)
})
