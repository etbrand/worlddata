countries <- data.frame(
  name = c("Aland", "Borduria", "Carpania", "Abkhazia"),
  iso3 = c("ALA", "BOR", "CAR", NA),
  population = c(1e6, 5e7, 2e7, 2e5)
)

# Yearly values: Aland has data from 2001, Borduria from 2000, Carpania only
# a recent value (so no change since 2000), and an aggregate not in the region
history <- data.frame(
  iso3 = c("ALA", "ALA", "ALA", "BOR", "BOR", "BOR", "CAR", "WLD"),
  year = c(2001L, 2010L, 2023L, 2000L, 2010L, 2024L, 2022L, 2024L),
  value = c(10, 40, 80, 2, 30, 60, 1.2, 70)
)

# History fetch ---------------------------------------------------------------

wb_row <- function(iso3, year, value) list(countryiso3code = iso3, date = as.character(year), value = value)

test_that("history is requested for a range of years, for all countries, and cached", {
  local_empty_cache()
  calls <- 0
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1
    expect_match(req$url, "/country/all/indicator/IT.NET.USER.ZS\\?format=json&date=2000%3A2024&per_page=20000&page=1")
    httr2::response_json(body = list(
      list(page = 1, pages = 1, total = 2),
      list(wb_row("ALA", 2000, 5), wb_row("ALA", 2001, NULL))
    ))
  })

  out <- fetch_wb_history("internet", start = 2000, end = 2024)
  fetch_wb_history("internet", start = 2000, end = 2024)

  expect_equal(out, data.frame(iso3 = "ALA", year = 2000L, value = 5))
  expect_equal(calls, 1)
})

test_that("history follows further pages if the API splits the response", {
  local_empty_cache()
  pages_requested <- integer()
  httr2::local_mocked_responses(function(req) {
    page <- as.integer(regmatches(req$url, regexpr("(?<=page=)[0-9]+$", req$url, perl = TRUE)))
    pages_requested[[length(pages_requested) + 1]] <<- page
    httr2::response_json(body = list(
      list(page = page, pages = 2),
      list(wb_row("ALA", 2000 + page, page))
    ))
  })

  out <- fetch_wb_history("urban", start = 2000, end = 2024)

  expect_equal(pages_requested, c(1L, 2L))
  expect_equal(out$year, c(2001L, 2002L))
})

# Data helpers ----------------------------------------------------------------

test_that("latest values are each country's most recent year", {
  out <- latest_values(history)
  expect_equal(out$year[out$iso3 == "ALA"], 2023L)
  expect_equal(out$value[out$iso3 == "BOR"], 60)
})

test_that("ranked_latest keeps the region's countries, smallest first", {
  out <- ranked_latest(countries, history)

  expect_equal(out$name, c("Carpania", "Borduria", "Aland"))
  expect_equal(out$value, c(1.2, 60, 80))
  expect_false("WLD" %in% out$iso3)
})

test_that("per-country charts keep only the most populous countries when capped", {
  out <- ranked_latest(countries, history, max_countries = 2)
  expect_setequal(out$name, c("Borduria", "Carpania"))
})

test_that("change_since uses the first value from around 2000 and the latest", {
  out <- change_since(countries, history)

  # Carpania only has a recent value, so no change to show
  expect_equal(out$name, c("Borduria", "Aland"))
  expect_equal(out$start_year, c(2000L, 2001L))
  expect_equal(out$start, c(2, 10))
  expect_equal(out$end, c(60, 80))
})

test_that("change_since leaves out countries whose latest value is too old to count as now", {
  stale <- rbind(history, data.frame(iso3 = "CAR", year = 2001L, value = 0.5))
  stale <- stale[!(stale$iso3 == "CAR" & stale$year == 2022L), ]
  stale <- rbind(stale, data.frame(iso3 = "CAR", year = 2004L, value = 0.9))

  expect_false("Carpania" %in% change_since(countries, stale)$name)
  expect_true("Carpania" %in% change_since(countries, stale, min_end_year = 2000)$name)
})

test_that("binned_latest counts countries per bin, keeping empty bins", {
  fertility <- data.frame(
    iso3 = c("ALA", "BOR", "CAR"),
    year = 2023L,
    value = c(1.2, 1.4, 2.7)
  )
  out <- binned_latest(countries, fertility, width = 0.5)

  expect_equal(out$from, c(1, 1.5, 2, 2.5))
  expect_equal(out$count, c(2L, 0L, 0L, 1L))
  expect_equal(out$countries[1], "Aland, Borduria")
  expect_equal(nrow(binned_latest(countries, history[0, ])), 0)
})

test_that("yearly_trends gives each country's series and the median where enough countries report", {
  out <- yearly_trends(countries, history, min_countries = 2)

  expect_setequal(unique(out$series$name), c("Aland", "Borduria", "Carpania"))
  # 2010 is the only year with at least two countries
  expect_equal(out$median, data.frame(year = 2010L, value = 35))
})

test_that("year_span describes a range of years", {
  expect_equal(year_span(c(2024, 2021, 2023)), "2021–2024")
  expect_equal(year_span(2024), "2024")
})

# Charts ----------------------------------------------------------------------

test_that("the lollipop chart ranks countries with stems and dots", {
  opts <- lollipop_chart(ranked_latest(countries, history), "Years")$x$opts

  expect_equal(unlist(opts$yAxis[[1]]$data), c("Carpania", "Borduria", "Aland"))
  expect_equal(vapply(opts$series, `[[`, "", "type"), c("bar", "scatter"))
  expect_equal(opts$series[[2]]$data[[3]]$year, 2023L)
})

test_that("the dumbbell chart joins each country's two values", {
  d <- change_since(countries, history)
  opts <- dumbbell_chart(d, "% of population")$x$opts

  types <- vapply(opts$series, `[[`, "", "type")
  expect_equal(types, c("bar", "bar", "scatter", "scatter"))
  # Connector: from the smaller value, spanning the change
  expect_equal(unlist(opts$series[[1]]$data), pmin(d$start, d$end))
  expect_equal(unlist(opts$series[[2]]$data), abs(d$end - d$start))
  expect_equal(opts$series[[4]]$data[[1]]$value[[1]], d$end[1])
})

test_that("the histogram labels bins and lists their countries", {
  bins <- data.frame(from = c(1, 1.5), to = c(1.5, 2), label = c("1–1.5", "1.5–2"),
                     count = c(12L, 0L), countries = c(paste("C", 1:12, collapse = ", "), ""))
  opts <- histogram_chart(bins, "Births per woman")$x$opts

  expect_equal(unlist(opts$xAxis[[1]]$data), c("1–1.5", "1.5–2"))
  expect_match(opts$series[[1]]$data[[1]]$countries, "and 2 more$")
})

test_that("the trend chart has a line per country and the regional median", {
  trends <- yearly_trends(countries, history, min_countries = 2)
  opts <- trend_chart(trends, "% of population")$x$opts

  names <- vapply(opts$series, `[[`, "", "name")
  expect_equal(names, c("Aland", "Borduria", "Carpania", "Regional median"))
  expect_equal(opts$series[[4]]$lineStyle$width, 3)
})

# Module ----------------------------------------------------------------------

test_that("the social page builds each chart and reports API errors", {
  local_mocked_bindings(fetch_wb_history = function(key, ...) {
    if (key == "fertility") stop(api_error("The World Bank API returned an error."))
    history
  })
  cs <- shiny::reactiveVal(countries)

  shiny::testServer(mod_social_server, args = list(countries = cs), {
    expect_match(output$life_caption$html, "3 of 4 countries and territories have data.", fixed = TRUE)
    expect_match(output$internet_caption$html, "2000–2024", fixed = TRUE)
    expect_match(output$fertility_status$html, "The World Bank API returned an error.", fixed = TRUE)
    expect_match(output$urban_caption$html, "Source:", fixed = TRUE)
  })
})

test_that("captions say when a world chart shows only the most populous countries", {
  many <- data.frame(name = paste("C", 1:100), iso3 = sprintf("C%02d", 1:100), population = 1:100)

  capped <- as.character(indicator_caption(55, many, 2024, latest = TRUE, per_country = TRUE))
  expect_match(capped, "The 60 most populous of 100 countries and territories; 55 have data.", fixed = TRUE)

  histogram <- as.character(indicator_caption(90, many, NULL, latest = TRUE, per_country = FALSE))
  expect_match(histogram, "90 of 100 countries and territories have data. Each country's most recent value.", fixed = TRUE)
})

# Urban population: lines or band ---------------------------------------------

# `n` countries, each with a value for 2000 and 2020
many_countries <- function(n) {
  codes <- sprintf("C%02d", seq_len(n))
  list(
    countries = data.frame(name = paste("Country", seq_len(n)), iso3 = codes, population = seq_len(n)),
    history = data.frame(
      iso3 = rep(codes, 2),
      year = rep(c(2000L, 2020L), each = n),
      value = c(seq_len(n), seq_len(n) + 10)
    )
  )
}

test_that("a few countries get a line each, more get the median and band", {
  few <- many_countries(trend_max_lines)
  lines <- trend_or_spread(few$countries, few$history)
  expect_equal(lines$kind, "lines")
  expect_length(unique(lines$trends$series$name), trend_max_lines)

  more <- many_countries(trend_max_lines + 1)
  band <- trend_or_spread(more$countries, more$history)
  expect_equal(band$kind, "band")
  expect_equal(band$spread$countries, trend_max_lines + 1)
  expect_equal(band$spread$spread$year, c(2000L, 2020L))
})

test_that("the urban caption says how to read the chart", {
  for (n in c(3, 20)) {
    data <- many_countries(n)
    local_mocked_bindings(fetch_wb_history = function(key, ...) data$history)

    shiny::testServer(mod_social_server, args = list(countries = shiny::reactiveVal(data$countries)), {
      caption <- output$urban_caption$html
      if (n <= trend_max_lines) {
        expect_match(caption, "Each grey line is a country", fixed = TRUE)
      } else {
        expect_match(caption, "the band covers the middle half of countries", fixed = TRUE)
        expect_match(caption, sprintf("%d of %d countries and territories have data.", n, n), fixed = TRUE)
      }
    })
  }
})

# Ranked charts: highest and lowest ----------------------------------------------

ranked_of <- function(n) {
  data.frame(name = sprintf("Country %02d", seq_len(n)), iso3 = sprintf("C%02d", seq_len(n)),
             year = 2024L, value = seq_len(n) + 50)
}

test_that("a long ranking shows only the highest and lowest, with headings and a gap", {
  rows <- split_rows(ranked_of(30), extremes = 10)

  expect_equal(nrow(rows), 24)
  # From the bottom of the chart up
  expect_equal(rows$name[1:10], sprintf("Country %02d", 1:10))
  expect_equal(rows$name[11:13], c("Lowest", " ", "  "))
  expect_equal(rows$name[14:23], sprintf("Country %02d", 21:30))
  expect_equal(rows$name[24], "Highest")
  expect_equal(which(rows$heading), c(11L, 24L))
  expect_true(all(is.na(rows$value[c(11, 12, 13, 24)])))
  expect_false(anyDuplicated(rows$name) > 0)
})

test_that("a short ranking shows every row", {
  rows <- split_rows(ranked_of(20), extremes = 10)
  expect_equal(rows$name, sprintf("Country %02d", 1:20))
  expect_false(any(rows$heading))
})

test_that("the split lollipop has headings, a median line and fits its rows", {
  e <- lollipop_chart(ranked_of(30), "Years", extremes = 10, median = 65.5)
  opts <- e$x$opts

  expect_length(opts$series[[2]]$data, 20)
  expect_match(opts$yAxis[[1]]$axisLabel$formatter, "'Lowest', 'Highest'", fixed = TRUE)
  expect_equal(opts$series[[2]]$markLine$data[[1]]$xAxis, 65.5)
  expect_equal(opts$series[[2]]$markLine$label$formatter, "Median 65.5")
  expect_match(render_hook(e, "el.style.height"), sprintf("'%dpx'", rows_chart_height(24)), fixed = TRUE)
})

test_that("the life expectancy caption says what's shown", {
  data <- many_countries(30)
  local_mocked_bindings(fetch_wb_history = function(key, ...) data$history)

  shiny::testServer(mod_social_server, args = list(countries = shiny::reactiveVal(data$countries)), {
    expect_match(output$life_caption$html, "Showing the highest and lowest 10; the dashed line is the median of all 30.", fixed = TRUE)
    expect_match(output$life_caption$html, "30 of 30 countries and territories have data.", fixed = TRUE)
  })
})

test_that("a split lollipop has a tear at its gap row; a full one doesn't", {
  split <- lollipop_chart(ranked_of(30), "Years", extremes = 10)
  tear <- render_hook(split, "tear-gap")
  # The gap is rows 12 and 13 from the bottom, indices 11 and 12
  expect_match(tear, "[11, 12].map(row => chart.convertToPixel({ yAxisIndex: 0 }, row))", fixed = TRUE)
  # Drawn in the theme's colors
  expect_match(tear, "--cds-layer-01", fixed = TRUE)
  expect_match(tear, "--cds-border-strong-01", fixed = TRUE)

  full <- lollipop_chart(ranked_of(20), "Years", extremes = 10)
  expect_null(render_hook(full, "tear-gap"))
})

test_that("the headings use the theme's primary text color, not a fixed one", {
  e <- lollipop_chart(ranked_of(30), "Years", extremes = 10)

  expect_equal(e$x$opts$yAxis[[1]]$axisLabel$rich$heading$color, "var(--cds-text-primary)")
  # Resolved for the current theme when the chart renders
  expect_false(is.null(render_hook(e, "light-dark")))
})

test_that("split rows keep the data's columns, blank for headings and the gap", {
  d <- data.frame(name = sprintf("C%02d", 1:30), start_year = 2000L, start = 1:30, end_year = 2024L, end = 31:60)
  rows <- split_rows(d, extremes = 10)

  expect_named(rows, c(names(d), "heading"))
  expect_true(all(is.na(rows$start[c(11, 12, 13, 24)])))
  expect_type(rows$start_year, "integer")
  expect_equal(split_gap(rows), c(12L, 13L))
  expect_length(split_gap(split_rows(d[1:20, ], extremes = 10)), 0)
})

# `n` countries' internet use around 2000 and now, sorted by now
changes_of <- function(n) {
  data.frame(name = sprintf("Country %02d", seq_len(n)), iso3 = sprintf("C%02d", seq_len(n)),
             start_year = 2000L, start = seq_len(n) / 10, end_year = 2024L, end = seq_len(n) + 50)
}

test_that("a long dumbbell chart shows only the highest and lowest, with a tear and median", {
  e <- dumbbell_chart(changes_of(30), "% of population", extremes = 10, median = 65.5)
  opts <- e$x$opts

  expect_equal(unlist(opts$yAxis[[1]]$data)[c(11, 24)], c("Lowest", "Highest"))
  expect_match(opts$yAxis[[1]]$axisLabel$formatter, "'Lowest', 'Highest'", fixed = TRUE)
  # A dot of each kind per country shown; connectors for every row
  expect_length(opts$series[[3]]$data, 20)
  expect_length(opts$series[[4]]$data, 20)
  expect_length(opts$series[[1]]$data, 24)
  expect_equal(opts$series[[4]]$data[[20]]$value[[1]], 80)
  expect_equal(opts$series[[4]]$markLine$data[[1]]$xAxis, 65.5)
  expect_match(render_hook(e, "tear-gap"), "[11, 12].map(", fixed = TRUE)
  expect_match(render_hook(e, "el.style.height"), sprintf("'%dpx'", rows_chart_height(24, extra = 20)), fixed = TRUE)

  full <- dumbbell_chart(changes_of(20), "% of population", extremes = 10)
  expect_length(full$x$opts$series[[4]]$data, 20)
  expect_null(render_hook(full, "tear-gap"))
  expect_null(full$x$opts$series[[4]]$markLine)
})

test_that("the internet chart ranks every country and says what's shown", {
  # More than the 60 most populous, which the chart used to stop at
  data <- many_countries(70)
  local_mocked_bindings(fetch_wb_history = function(key, ...) data$history)

  shiny::testServer(mod_social_server, args = list(countries = shiny::reactiveVal(data$countries)), {
    expect_match(
      output$internet_caption$html,
      "Showing the highest and lowest 10; the dashed line is the median latest value of all 70.",
      fixed = TRUE
    )
    expect_no_match(output$internet_caption$html, "most populous", fixed = TRUE)
  })
})

test_that("ranked_note says when only the highest and lowest are shown", {
  expect_equal(ranked_note(30), "Showing the highest and lowest 10; the dashed line is the median of all 30.")
  expect_equal(ranked_note(20), "The dashed line is the median.")
  expect_equal(ranked_note(5, median = "the median latest value"), "The dashed line is the median latest value.")
})
