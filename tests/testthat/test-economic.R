countries <- data.frame(
  name = c("Aland", "Borduria", "Carpania", "Abkhazia"),
  iso3 = c("ALA", "BOR", "CAR", NA),
  population = c(1e6, 5e7, 2e7, 2e5)
)

# Yearly values, plus an aggregate (WLD) that isn't in the region
history <- data.frame(
  iso3 = c("ALA", "ALA", "BOR", "BOR", "CAR", "CAR", "WLD"),
  year = c(2004L, 2023L, 2004L, 2023L, 2023L, 2024L, 2023L),
  value = c(1, 3, -2, 5, 10, 300, 99)
)

# Indicator choices ------------------------------------------------------------

test_that("economic-only indicators aren't offered in the scatter", {
  choices <- wb_indicator_choices()
  expect_false(any(c("gdp", "gdp_growth", "inflation") %in% choices))
  expect_true(all(c("gdp_per_capita", "unemployment") %in% choices))
})

# Data ----------------------------------------------------------------------------

test_that("gdp_shares gives each country's share of the region, largest first", {
  out <- gdp_shares(countries, history)

  expect_equal(out$name, c("Carpania", "Borduria", "Aland"))
  expect_equal(out$value, c(300, 5, 3))
  expect_equal(out$year, c(2024L, 2023L, 2023L))
  expect_equal(sum(out$share), 1)
  expect_equal(out$share[1], 300 / 308)
})

test_that("yearly_spread gives quartiles for years with enough countries", {
  out <- yearly_spread(countries, history, min_countries = 2)

  # 2024 has only one country; the aggregate isn't counted
  expect_equal(out$spread$year, c(2004L, 2023L))
  expect_equal(out$spread$n, c(2L, 3L))
  expect_equal(out$spread$median, c(-0.5, 5))
  expect_equal(out$spread$low, c(-1.25, 4))
  expect_equal(out$spread$high, c(0.25, 7.5))
  expect_equal(out$countries, 3)
})

test_that("yearly_spread handles no data", {
  out <- yearly_spread(countries, history[history$iso3 == "WLD", ])

  expect_equal(nrow(out$spread), 0)
  expect_equal(out$countries, 0)
})

test_that("yearly_grid keeps years from from_year, by country name", {
  out <- yearly_grid(countries, history, from_year = 2020)

  expect_equal(out$name, c("Aland", "Borduria", "Carpania", "Carpania"))
  expect_equal(out$year, c(2023L, 2023L, 2023L, 2024L))
})

test_that("yearly_grid shows the most populous countries when capped", {
  out <- yearly_grid(countries, history, from_year = 2000, max_countries = 2)
  expect_equal(unique(out$name), c("Borduria", "Carpania"))
})

# Charts --------------------------------------------------------------------------

test_that("treemap colors run from dark to bright blue with GDP", {
  colors <- treemap_colors(c(1e12, 1e9, 1e6))
  expect_equal(colors, treemap_blues[c(5, 3, 1)])
  expect_equal(treemap_colors(5), treemap_blues[5])
})

test_that("gdp_treemap has a rectangle per country", {
  opts <- gdp_treemap(gdp_shares(countries, history))$x$opts
  series <- opts$series[[1]]

  # No axes: echarts4r's default y axis stops a treemap from drawing
  expect_null(opts$xAxis)
  expect_null(opts$yAxis)
  expect_equal(series$type, "treemap")
  expect_equal(vapply(series$data, `[[`, "", "name"), c("Carpania", "Borduria", "Aland"))
})

test_that("spread_band_chart stacks the band on the 25th percentile", {
  spread <- yearly_spread(countries, history, min_countries = 2)$spread
  series <- spread_band_chart(spread, "%")$x$opts$series

  expect_equal(unlist(series[[1]]$data), spread$low)
  expect_equal(unlist(series[[2]]$data), spread$high - spread$low)
  expect_equal(series[[1]]$stack, series[[2]]$stack)
  # Stacks on negative values too
  expect_equal(series[[2]]$stackStrategy, "all")
  expect_equal(vapply(series[[3]]$data, `[[`, 0, "value"), spread$median)
})

test_that("growth_heatmap has a cell per country and year, sized to its rows", {
  grid <- yearly_grid(countries, history, from_year = 2000)
  e <- growth_heatmap(grid, "Growth")
  opts <- e$x$opts

  expect_equal(unlist(opts$xAxis[[1]]$data), as.character(2004:2024))
  expect_equal(unlist(opts$yAxis[[1]]$data), c("Aland", "Borduria", "Carpania"))
  expect_length(opts$series[[1]]$data, nrow(grid))
  # Carpania in 2024: column 20, row 2 (zero-based)
  expect_equal(opts$series[[1]]$data[[6]]$value, list(20, 2, 300))
  expect_match(render_hook(e, "el.style.height"), sprintf("'%dpx'", rows_chart_height(3, extra = 32)), fixed = TRUE)
})

test_that("ranked_bar_chart has a bar per country, largest at the top", {
  data <- ranked_latest(countries, history)
  opts <- ranked_bar_chart(data, "%")$x$opts

  expect_equal(unlist(opts$yAxis[[1]]$data), c("Aland", "Borduria", "Carpania"))
  expect_equal(vapply(opts$series[[1]]$data, `[[`, 0, "value"), c(3, 5, 300))
})

# Module --------------------------------------------------------------------------

test_that("the economic page draws every chart for the region", {
  local_mocked_bindings(fetch_wb_history = function(key, ...) history)
  region <- shiny::reactiveVal(countries)

  shiny::testServer(mod_economic_server, args = list(countries = region), {
    for (key in names(economic_charts)) {
      expect_s3_class(output[[paste0(key, "_chart")]], "json")
      expect_match(output[[paste0(key, "_caption")]]$html, "World Bank")
    }
    expect_match(output$growth_caption$html, "3 of 4 countries and territories have data. 2023–2024.", fixed = TRUE)
  })
})

test_that("the economic page shows an empty state when the API fails", {
  local_mocked_bindings(fetch_wb_history = function(key, ...) {
    if (key == "inflation") stop(api_error("The World Bank API is busy.")) else history
  })

  shiny::testServer(mod_economic_server, args = list(countries = shiny::reactiveVal(countries)), {
    expect_match(output$inflation_status$html, "The World Bank API is busy.")
    expect_null(output$gdp_status$html)
  })
})
