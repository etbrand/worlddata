countries <- data.frame(
  name = c("Small", "Unknown", "Large", "Medium"),
  population = c(10, NA, 1000, 100),
  area = c(5, 50, NA, 500),
  languages = c("English, French", NA, "English", "French, English, Arabic")
)

# Charts with a row per country -----------------------------------------------

test_that("rows_chart_height grows with the number of countries", {
  expect_lt(rows_chart_height(27), rows_chart_height(60))
  # The largest region (~60 countries) should be under 900px
  expect_equal(rows_chart_height(60), 888)
  expect_match(resize_to_rows_js(60), "el.style.height = '888px'", fixed = TRUE)
})

test_that("charts with few rows get a minimum height", {
  expect_equal(rows_chart_height(3), row_layout$min_height)
  expect_equal(rows_chart_height(8, extra = 32), row_layout$min_height + 32)
})

test_that("long country names are truncated on the axis", {
  width <- country_label_width(c("Saint Helena, Ascension and Tristan da Cunha", "Chad"))
  label <- country_axis_label(width)

  expect_equal(width, ceiling(row_layout$max_label_chars * row_layout$char_width))
  expect_equal(label$overflow, "truncate")
  expect_equal(label$width, width)
  # Short names don't get more room than they need
  expect_lt(country_label_width(c("Chad", "Peru")), width)
})


# language_cloud_data ---------------------------------------------------------

test_that("language_cloud_data counts countries per language, most common first", {
  out <- language_cloud_data(countries)

  expect_equal(out$language, c("English", "French", "Arabic"))
  expect_equal(out$countries, c(3L, 2L, 1L))
})

test_that("language_cloud_data handles no language data", {
  out <- language_cloud_data(countries[2, ])

  expect_equal(nrow(out), 0)
})

test_that("language_cloud builds a word cloud series", {
  opts <- language_cloud(countries)$x$opts

  expect_equal(opts$series[[1]]$type, "wordCloud")
  expect_length(opts$series[[1]]$data, 3)
})

# Theme colors --------------------------------------------------------------------

test_that("chart colors are theme references, resolved when the chart renders", {
  e <- language_cloud(countries)

  expect_true(startsWith(plot_colors$text, "var(--cds-"))
  expect_equal(plot_colors$bar, "light-dark(#0f62fe, #4589ff)")
  expect_equal(e$x$opts$tooltip$textStyle$color, "var(--cds-text-primary)")
  theme <- render_hook(e, "light-dark")
  expect_match(theme, "getPropertyValue(token[1])", fixed = TRUE)
  expect_match(theme, "prefers-color-scheme: dark", fixed = TRUE)
})

test_that("no chart uses a fixed hex color for text, gridlines or tooltips", {
  opts <- trend_chart(yearly_trends(
    data.frame(name = c("A", "B", "C"), iso3 = c("AAA", "BBB", "CCC")),
    data.frame(iso3 = rep(c("AAA", "BBB", "CCC"), 2), year = rep(c(2000L, 2020L), each = 3), value = 1:6)
  ), "%")$x$opts

  expect_equal(opts$xAxis[[1]]$axisLabel$color, "var(--cds-text-secondary)")
  expect_equal(opts$yAxis[[1]]$splitLine$lineStyle$color, "var(--cds-border-subtle-01)")
  expect_equal(opts$tooltip$backgroundColor, "var(--cds-layer-01)")
})

test_that("light/dark pairs can't contain commas, which would break them", {
  expect_equal(light_dark("#0f62fe2e", "#4589ff40"), "light-dark(#0f62fe2e, #4589ff40)")
  expect_error(light_dark("rgba(15, 98, 254, 0.18)", "#4589ff40"), "commas")
})
