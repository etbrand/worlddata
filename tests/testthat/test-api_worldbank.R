wb_body <- list(
  list(page = 1, pages = 1, per_page = 1000, total = 4),
  list(
    list(countryiso3code = "IND", date = "2025", value = 2702.48),
    list(countryiso3code = "CHN", date = "2025", value = 13861.97),
    list(countryiso3code = "", date = "2025", value = 5000),     # aggregate without a code
    list(countryiso3code = "JPN", date = "2024", value = NULL)    # no data
  )
)

wb_error_body <- list(list(message = list(list(
  id = "120", key = "Invalid value", value = "The provided parameter value is not valid"
))))

test_that("indicator rows parse, dropping aggregates without a code and missing values", {
  out <- parse_wb_indicator(wb_body)

  expect_equal(out$iso3, c("IND", "CHN"))
  expect_equal(out$year, c(2025L, 2025L))
  expect_equal(out$value, c(2702.48, 13861.97))
})

test_that("errors sent with HTTP 200 become api_errors", {
  expect_error(
    parse_wb_indicator(wb_error_body),
    "The World Bank API returned an error: The provided parameter value is not valid",
    class = "api_error"
  )
})

test_that("an indicator is requested for all countries, most recent value, and cached", {
  local_empty_cache()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    expect_match(req$url, "/v2/country/all/indicator/NY.GDP.PCAP.CD\\?format=json&mrnev=1&per_page=1000")
    httr2::response_json(body = wb_body)
  })

  fetch_wb_indicator("gdp_per_capita")
  out <- fetch_wb_indicator("gdp_per_capita")

  expect_equal(nrow(out), 2)
  expect_equal(calls, 1)
})

test_that("unknown indicators are rejected without a request", {
  expect_error(fetch_wb_indicator("nope"), "Unknown indicator", class = "api_error")
})

countries <- data.frame(
  name = c("India", "China", "Abkhazia", "Taiwan"),
  iso3 = c("IND", "CHN", NA, "TWN"),
  population = c(1.4e9, 1.4e9, 2e5, 2.3e7)
)
gdp <- data.frame(iso3 = c("IND", "CHN", "TWN"), year = 2025L, value = c(2700, 13900, 33000))
life <- data.frame(iso3 = c("IND", "CHN"), year = 2023L, value = c(72, 78))

test_that("indicator_scatter_data keeps every country with a code, in a stable order", {
  out <- indicator_scatter_data(countries, gdp, life)

  # Abkhazia has no code; Taiwan stays, with no life expectancy
  expect_equal(out$name, c("China", "India", "Taiwan"))
  expect_equal(out$x, c(13900, 2700, 33000))
  expect_equal(out$y, c(78, 72, NA))
  expect_equal(has_both(out), c(TRUE, TRUE, FALSE))
})

test_that("points keep their order and leave out values a log axis can't show", {
  d <- indicator_scatter_data(countries, gdp, life)
  d$x[1] <- 0

  points <- indicator_scatter_points(d, "gdp_per_capita", "life_expectancy")

  expect_equal(vapply(points, `[[`, "", "id"), c("CHN", "IND", "TWN"))
  expect_true(is.na(points[[1]]$value[[1]]))   # 0 on a log axis
  expect_true(is.na(points[[3]]$value[[2]]))   # missing
  expect_equal(points[[2]]$value[[3]], 1.4e9)  # population for the tooltip
})

test_that("the full chart and the proxy update share points and axes", {
  d <- indicator_scatter_data(countries, gdp, life)

  opts <- indicator_scatter_chart(d, "gdp_per_capita", "life_expectancy", chart_id = "x-chart")$x$opts
  update <- indicator_scatter_update(d, "life_expectancy", "gdp_per_capita")

  expect_equal(opts$xAxis[[1]]$type, "log")
  expect_equal(opts$yAxis[[1]]$type, "value")
  expect_equal(opts$series[[1]]$data, indicator_scatter_points(d, "gdp_per_capita", "life_expectancy"))
  expect_match(as.character(opts$tooltip$formatter), "getElementById('x-chart')", fixed = TRUE)

  expect_equal(update$xAxis[[1]], list(type = "value", name = "Life expectancy (years)"))
  expect_equal(update$yAxis[[1]], list(type = "log", name = "GDP per capita (US$)"))
  expect_length(update$series[[1]]$data, 3)
})

test_that("the indicators module joins data and reports API errors", {
  local_mocked_bindings(fetch_wb_indicator = function(key) {
    if (key == "internet") stop(api_error("The World Bank API returned an error."))
    data.frame(iso3 = c("IND", "CHN"), year = 2024L, value = if (key == "gdp_per_capita") c(2700, 13900) else c(72, 78))
  })
  countries <- shiny::reactiveVal(data.frame(
    name = c("India", "China"), iso3 = c("IND", "CHN"), population = c(1.4e9, 1.4e9)
  ))

  shiny::testServer(mod_indicators_server, args = list(countries = countries), {
    session$setInputs(x = "gdp_per_capita", y = "life_expectancy")
    expect_equal(sum(has_both(scatter_data())), 2)
    expect_true(can_draw())

    session$setInputs(y = "internet")
    expect_s3_class(scatter_data(), "api_error")
    expect_false(can_draw())
  })
})
