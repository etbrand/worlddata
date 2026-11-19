# The app when the country list can't be loaded (e.g. the World Bank API is
# down and there's no copy on disk)

countries_ok <- function() {
  data.frame(
    name = c("Aland", "Borduria"), region = "East Asia & Pacific", region_id = "EAS",
    iso3 = c("ALA", "BOR"), wikipedia = NA_character_, flag = NA_character_,
    capital = NA_character_, population = 1:2, area = 1:2, languages = NA_character_,
    gini = NA_real_
  )
}

test_that("load_countries returns an api_error instead of failing", {
  local_mocked_bindings(fetch_data = function(region) stop(api_error("Couldn't reach the World Bank API.")))
  expect_s3_class(load_countries("EAS"), "api_error")
  expect_equal(conditionMessage(load_countries("EAS")), "Couldn't reach the World Bank API.")
})

test_that("load_countries turns any other error into an api_error with a plain message", {
  local_mocked_bindings(fetch_data = function(region) stop("subscript out of bounds"))
  expect_message(result <- load_countries("EAS"), "subscript out of bounds")
  expect_s3_class(result, "api_error")
  expect_equal(conditionMessage(result), countries_unavailable_message)
})

test_that("every page shows a notice while the country list can't be loaded, and recovers on retry", {
  down <- TRUE
  calls <- 0
  local_mocked_bindings(
    fetch_data = function(region) {
      calls <<- calls + 1
      if (down) stop(api_error("Couldn't reach the World Bank API.")) else countries_ok()
    },
    # The At a glance callouts and the Comparisons page also fetch
    # indicators; keep them offline
    fetch_wb_indicator = function(key) stop(api_error("offline")),
    fetch_wb_history = function(key, ...) stop(api_error("offline"))
  )

  shiny::testServer(server, {
    session$setInputs(region = "EAS", columns = "name")

    # The session keeps running: whatever reads the country list stops
    # quietly instead of failing
    expect_error(fetched_data(), class = "shiny.silent.error")
    for (id in data_status_pages) {
      html <- output[[paste0(id, "-status")]]$html
      expect_match(html, "Couldn't load the country data", fixed = TRUE)
      expect_match(html, "Couldn't reach the World Bank API.", fixed = TRUE)
      expect_match(html, "Try again", fixed = TRUE)
    }

    # Still down: trying again refetches and keeps the notice
    before <- calls
    session$setInputs(`status_social-retry` = 1)
    expect_gt(calls, before)
    expect_match(output[["status_social-status"]]$html, "Couldn't load the country data", fixed = TRUE)

    # Back up: the notice goes and the pages fill in
    down <<- FALSE
    session$setInputs(`status_economic-retry` = 1)
    expect_equal(fetched_data()$name, c("Aland", "Borduria"))
    for (id in data_status_pages) {
      expect_null(output[[paste0(id, "-status")]]$html)
    }
    expect_match(output$table$html, "Aland", fixed = TRUE)
  })
})
