# A country in East Asia & Pacific, plus history for every regional
# aggregate and the world. South Asia has no recent value.
regions_countries <- data.frame(name = "Aland", iso3 = "ALA", population = 1e6)

regions_history <- local({
  codes <- c("EAS", "ECS", "LCN", "MEA", "NAC", "SSF", "WLD", "ALA")
  rbind(
    data.frame(iso3 = codes, year = 2000L, value = seq_along(codes)),
    data.frame(iso3 = codes, year = 2023L, value = seq_along(codes) * 10),
    data.frame(iso3 = "SAS", year = 2000L, value = 5)
  )
})

# Data --------------------------------------------------------------------------

test_that("region_aggregates lists the World Bank's regions by short name, and the world", {
  out <- region_aggregates()

  expect_equal(out$iso3, c(names(wb_regions), "WLD"))
  expect_equal(out$name[out$iso3 == "MEA"], "Middle East & North Africa")
  expect_equal(tail(out$name, 1), "World")
  expect_equal(nrow(region_aggregates(world = FALSE)), 7)
})

test_that("region data uses the aggregates' rows of a history", {
  ranked <- ranked_latest(region_aggregates(), regions_history)

  # South Asia's only value is from 2000, so it's ranked on that
  expect_equal(nrow(ranked), 8)
  expect_equal(tail(ranked$name, 1), "World")
  expect_equal(regions_shown(ranked$name), 7)
})

test_that("region_trends gives a line per region, with the world as the reference", {
  out <- region_trends(regions_history)

  expect_equal(sort(unique(out$series$name)), sort(unname(region_short_names)))
  expect_false("World" %in% out$series$name)
  expect_equal(out$median, data.frame(year = c(2000L, 2023L), value = c(7, 70)))
})

test_that("region captions name the regions and the world", {
  caption <- as.character(region_caption(7, c(2023, 2024), latest = TRUE))
  expect_match(caption, "World Bank estimates for its 7 regions and the world. Each region's most recent value (2023–2024).", fixed = TRUE)

  caption <- as.character(region_caption(6, 2000:2023, world = FALSE))
  expect_match(caption, "World Bank estimates for 6 of its 7 regions. 2000–2023.", fixed = TRUE)
})

# Charts ------------------------------------------------------------------------

test_that("ranked charts highlight the world", {
  data <- ranked_latest(region_aggregates(), regions_history)

  points <- lollipop_chart(data, "Years", highlight = "World")$x$opts$series[[2]]$data
  colors <- vapply(points, \(p) if_null(p$itemStyle$color, ""), "")
  expect_equal(colors[vapply(points, `[[`, "", "name") == "World"], highlight_color)
  expect_equal(sum(nzchar(colors)), 1)

  bars <- ranked_bar_chart(data, "%", highlight = "World")$x$opts$series[[1]]$data
  expect_equal(bars[[which(data$name == "World")]]$itemStyle$color, highlight_color)
})

test_that("a colored trend chart has a colored line and legend entry per region", {
  opts <- trend_chart(region_trends(regions_history), "%", reference = "World", colors = series_colors)$x$opts

  names <- vapply(opts$series, `[[`, "", "name")
  expect_equal(tail(names, 1), "World")
  expect_equal(unlist(opts$legend$data), names)
  expect_equal(opts$series[[1]]$lineStyle$color, series_colors[1])
  expect_equal(tail(opts$series, 1)[[1]]$lineStyle$color, highlight_color)
})

test_that("the GDP treemap's tooltip names what the shares are of", {
  data <- gdp_shares(region_aggregates(world = FALSE), regions_history)
  e <- gdp_treemap(data, share_of = "the world")

  expect_match(e$x$opts$tooltip$formatter, "'the world'", fixed = TRUE)
})

# Modules -----------------------------------------------------------------------

test_that("the Regions view applies only while the region is World", {
  local_mocked_bindings(fetch_wb_history = function(key, ...) regions_history)
  region <- shiny::reactiveVal("World")

  shiny::testServer(mod_social_server, args = list(countries = shiny::reactiveVal(regions_countries), region = region), {
    session$setInputs(view = "countries")
    expect_match(output$life_caption$html, "1 of 1 countries and territories", fixed = TRUE)

    session$setInputs(view = "regions")
    expect_match(output$life_caption$html, "World Bank estimates for its 7 regions and the world", fixed = TRUE)
    expect_match(output$urban_caption$html, "World Bank estimates for its 7 regions and the world. 2000–2023.", fixed = TRUE)

    # A single region shows its countries, whatever the switcher says
    region("EAS")
    session$flushReact()
    expect_match(output$life_caption$html, "1 of 1 countries and territories", fixed = TRUE)
  })
})

test_that("the economic page has a Regions view of every chart", {
  local_mocked_bindings(fetch_wb_history = function(key, ...) regions_history)

  shiny::testServer(mod_economic_server, args = list(
    countries = shiny::reactiveVal(regions_countries), region = shiny::reactiveVal("World")
  ), {
    session$setInputs(view = "regions")
    for (key in names(economic_charts)) {
      expect_match(output[[paste0(key, "_caption")]]$html, "World Bank estimates for", fixed = TRUE)
    }
    # The treemap splits the world between regions, without the world itself
    expect_match(output$gdp_caption$html, "its 7 regions.", fixed = TRUE)
  })
})

test_that("the switcher appears only for the whole world", {
  html <- as.character(mod_social_ui("social"))

  expect_match(html, "data-display-if=\"input.region === &#39;World&#39;\"", fixed = TRUE)
  expect_match(html, "<cds-content-switcher[^>]*id=\"social-view\"")
})

# Comparisons ---------------------------------------------------------------------

test_that("the Comparisons page plots the regions as bubbles in the Regions view", {
  # Latest values for one country and every regional aggregate
  codes <- c("ALA", names(wb_regions), "WLD")
  latest <- function(values) data.frame(iso3 = codes, year = 2024L, value = values)
  local_mocked_bindings(fetch_wb_indicator = function(key) {
    switch(key,
      gdp_per_capita = latest(seq(1000, by = 1000, length.out = length(codes))),
      life_expectancy = latest(seq(60, by = 2, length.out = length(codes))),
      population = latest(seq(1e8, by = 1e8, length.out = length(codes)))
    )
  })
  region <- shiny::reactiveVal("World")

  shiny::testServer(mod_indicators_server, args = list(countries = shiny::reactiveVal(regions_countries), region = region), {
    session$setInputs(x = "gdp_per_capita", y = "life_expectancy", view = "countries")
    expect_equal(scatter_data()$name, "Aland")
    expect_match(output$caption$html, "1 of 1 countries and territories", fixed = TRUE)

    session$setInputs(view = "regions")
    d <- scatter_data()
    expect_setequal(d$iso3, names(wb_regions))
    expect_false("WLD" %in% d$iso3)
    # Sized by the World Bank's population figure for each region
    expect_equal(d$population[d$iso3 == "EAS"], 2e8)
    expect_match(output$caption$html, "World Bank estimates for all 7 of its regions", fixed = TRUE)

    # A single region shows its countries, whatever the switcher says
    region("EAS")
    session$flushReact()
    expect_equal(scatter_data()$name, "Aland")
  })
})

test_that("region bubbles still draw if the population request fails", {
  local_mocked_bindings(fetch_wb_indicator = function(key) stop(api_error("down")))

  bubbles <- region_bubbles()
  expect_equal(nrow(bubbles), 7)
  expect_true(all(is.na(bubbles$population)))
})

test_that("region bubbles are labelled with their names; country bubbles aren't", {
  d <- data.frame(name = c("East Asia & Pacific", "South Asia"), iso3 = c("EAS", "SAS"),
                  population = c(2.4e9, 2e9), x = c(14000, 2500), y = c(77, 72),
                  x_year = 2024L, y_year = 2024L)

  labelled <- indicator_scatter_chart(d, "gdp_per_capita", "life_expectancy", chart_id = "c", labels = TRUE)$x$opts
  expect_true(labelled$series[[1]]$label$show)
  expect_equal(labelled$series[[1]]$label$formatter, "{b}")
  expect_equal(labelled$series[[1]]$labelLayout$moveOverlap, "shiftY")
  expect_equal(vapply(labelled$series[[1]]$data, `[[`, "", "name"), d$name)

  plain <- indicator_scatter_chart(d, "gdp_per_capita", "life_expectancy", chart_id = "c")$x$opts
  expect_null(plain$series[[1]]$label)
})
