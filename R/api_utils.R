# Shared helpers for the app's web API clients: caches and errors with
# user-facing messages.

# Caching ---------------------------------------------------------------------

# Shared by all sessions. The data behind these APIs changes at most daily,
# so an hour keeps requests low without showing stale data for long.
api_cache <- cachem::cache_mem(max_age = 60 * 60)

# World Bank indicators change yearly, and a cold request to the API can take
# tens of seconds, so keep them for a day
wb_cache <- cachem::cache_mem(max_age = 24 * 60 * 60)

# cachem keys may only use lowercase letters, digits, "_" and "-". Encode
# each character as hex so different keys (e.g. accented titles) can't
# collide.
cache_key <- function(key) {
  paste(sprintf("%x", utf8ToInt(enc2utf8(key))), collapse = "-")
}

# Return the cached value for `key`, or call `fetch()` and cache its result.
# Errors are not cached, so a failed request is retried next time.
cached <- function(key, fetch, cache = api_cache) {
  key <- cache_key(key)
  value <- cache$get(key)
  if (!cachem::is.key_missing(value)) {
    return(value)
  }
  value <- fetch()
  cache$set(key, value)
  value
}

# Daily disk cache ------------------------------------------------------------
#
# The country list behind every page takes several slow requests to build
# (the World Bank, then a Wikidata query that can take tens of seconds), so
# it's kept on disk: it survives app restarts and is shared by every R
# process on the machine. Set WORLDDATA_CACHE_DIR to store it somewhere
# else (the tests use a temporary folder).

disk_cache_dir <- function() {
  Sys.getenv("WORLDDATA_CACHE_DIR", tools::R_user_dir("worlddata", "cache"))
}

# No max_age: cached_daily() decides when an entry is out of date, so an
# old copy is still there if a refresh fails
disk_cache <- function() {
  cachem::cache_disk(disk_cache_dir(), max_size = 50 * 1024^2)
}

# Like cached(), but refetches a value once it's older than `max_age`
# seconds, and keeps using the old copy if that fails (e.g. the API is
# down). Errors only when there's no copy at all.
cached_daily <- function(
  key,
  fetch,
  cache = disk_cache(),
  max_age = 24 * 60 * 60
) {
  key <- cache_key(key)
  entry <- cache$get(key)
  have_copy <- !cachem::is.key_missing(entry)
  age <- if (have_copy) {
    as.numeric(difftime(Sys.time(), entry$fetched_at, units = "secs"))
  }

  if (have_copy && age < max_age) {
    return(entry$value)
  }

  value <- tryCatch(fetch(), error = function(e) {
    if (!have_copy) {
      stop(e)
    }
    message(sprintf(
      "Couldn't refresh cached data (%s); using the copy from %s.",
      conditionMessage(e),
      format(entry$fetched_at, "%Y-%m-%d %H:%M")
    ))
    NULL
  })
  if (is.null(value)) {
    return(entry$value)
  }

  cache$set(key, list(fetched_at = Sys.time(), value = value))
  value
}

# Errors ----------------------------------------------------------------------

# Error with a user-facing message, shown in the UI instead of R error text.
# `status` is the HTTP status when there is one.
api_error <- function(message, status = NA_integer_) {
  structure(
    class = c("api_error", "error", "condition"),
    list(message = message, call = NULL, status = status)
  )
}

# Wikimedia (Wikipedia, Wikidata) asks API clients to identify themselves
# with contact details
wikimedia_user_agent <- paste(
  "WorldDataDashboard/0.1",
  "(https://github.com/etbrand/worlddata; demo app for the carbonite R package)"
)

# Perform `req` and return the parsed JSON body, or signal a
# `api_error` with a message suitable for the UI. `timeout` is in seconds.
perform_json <- function(req, source, timeout = 10) {
  resp <- tryCatch(
    req |>
      httr2::req_timeout(timeout) |>
      httr2::req_error(is_error = \(resp) FALSE) |>
      httr2::req_perform(),
    error = function(e) {
      stop(api_error(
        sprintf(
          "Couldn't reach %s. Check your connection and try again.",
          source
        )
      ))
    }
  )

  status <- httr2::resp_status(resp)
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)

  if (status >= 400) {
    # Either {errors: [{message}]} or {message}
    detail <- if (!is.null(body$errors)) {
      body$errors[[1]]$message
    } else {
      body$message
    }

    message <- if (status == 429) {
      sprintf(
        "Too many requests to %s. Wait a few seconds and try again.",
        source
      )
    } else if (status %in% c(401, 403)) {
      sprintf("%s rejected the API key.", tools::toTitleCase(source))
    } else if (!is.null(detail)) {
      detail
    } else {
      sprintf(
        "%s returned an error (HTTP %d).",
        tools::toTitleCase(source),
        status
      )
    }
    stop(api_error(message, status))
  }

  # Not JSON, e.g. a maintenance page served while an API is down
  if (is.null(body)) {
    stop(api_error(
      sprintf(
        "%s isn't responding normally right now. Try again later.",
        tools::toTitleCase(source)
      ),
      status
    ))
  }

  body
}

# Run `expr`, turning any error that isn't already an api_error into one
# with `message` (logging the original), so a response in an unexpected
# shape shows a message in the UI rather than R error text or ending the
# session
as_api_error <- function(expr, message) {
  tryCatch(expr, error = function(e) {
    if (inherits(e, "api_error")) {
      stop(e)
    }
    message(sprintf("Unexpected error (%s): %s", message, conditionMessage(e)))
    stop(api_error(message))
  })
}
