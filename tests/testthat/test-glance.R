glance_all <- data.frame(
  name = c("China", "South Korea", "Chad", "Japan"),
  iso3 = c("CHN", "KOR", "TCD", "JPN"),
  region_id = c("EAS", "EAS", "SSF", "EAS"),
  population = c(1.4e9, 5e7, 1.8e7, 1.2e8)
)
glance_east_asia <- glance_all[glance_all$region_id == "EAS", ]

# Latest values, with the East Asia & Pacific (EAS) and world (WLD) aggregates
latest_values_of <- function(...) {
  v <- c(...)
  data.frame(iso3 = names(v), year = 2024L, value = unname(v))
}
glance_gdp <- latest_values_of(CHN = 1.8e13, KOR = 1.7e12, JPN = 4.1e12, TCD = 1.3e10, EAS = 3.35e13, WLD = 1.1e14)
glance_life <- latest_values_of(CHN = 78.6, KOR = 83.5, JPN = 84.7, TCD = 55.2, EAS = 76.94, WLD = 73.33)
glance_population <- latest_values_of(CHN = 1.4e9, KOR = 5e7, JPN = 1.2e8, TCD = 1.8e7)

callouts_for <- function(region, countries, gdp = glance_gdp, life = glance_life) {
  callouts <- glance_callouts(region, countries, glance_all, gdp, life, glance_population)
  stats::setNames(callouts, c("countries", "population", "economy", "life"))
}

# Figures -------------------------------------------------------------------------

test_that("a region's callouts compare it with the world", {
  c <- callouts_for("EAS", glance_east_asia)

  expect_equal(c$countries$value, "3")
  expect_equal(c$countries$note, "Of 4 worldwide")

  expect_equal(c$population$value, "1.57")
  expect_equal(c$population$unit, "billion")
  expect_equal(c$population$note, "99% of the world's people · Largest: China")
  expect_equal(c$population$year, "2024")

  expect_equal(c$economy$value, "$33.5")
  expect_equal(c$economy$unit, "trillion")
  expect_equal(c$economy$note, "30% of world GDP · Largest: China")

  expect_equal(c$life$value, "76.9")
  expect_equal(c$life$unit, "years")
  expect_equal(c$life$note, "World: 73.3 years")
})

test_that("the world's callouts name the largest and highest countries", {
  c <- callouts_for(world_region, glance_all)

  expect_equal(c$countries$value, "4")
  expect_equal(c$countries$note, "Across the World Bank's 7 regions")
  expect_equal(c$population$note, "Largest: China")
  expect_equal(c$economy$value, "$110")
  expect_equal(c$economy$note, "Largest: China")
  expect_equal(c$life$value, "73.3")
  expect_equal(c$life$note, "Highest: Japan")
})

test_that("a failed World Bank request shows a dash, not an error", {
  c <- callouts_for("EAS", glance_east_asia, gdp = api_error("down"), life = api_error("down"))

  expect_equal(c$economy$value, missing_label)
  expect_match(c$economy$note, "Couldn't load")
  expect_equal(c$life$value, missing_label)
  # Figures from the country list still show
  expect_equal(c$population$value, "1.57")
})

test_that("a region without an aggregate figure shows a dash", {
  c <- callouts_for("SSF", glance_all[glance_all$region_id == "SSF", ])

  expect_equal(c$economy$value, missing_label)
  expect_equal(c$life$value, missing_label)
  expect_equal(c$population$note, "1% of the world's people · Largest: Chad")
})

test_that("numbers are compact, with three significant figures", {
  expect_equal(format_compact(2.384e9), list(value = "2.38", unit = "billion"))
  expect_equal(format_compact(1.1e14), list(value = "110", unit = "trillion"))
  expect_equal(format_compact(46029), list(value = "46,029", unit = NULL))
  expect_equal(format_share(0.287), "29%")
  expect_equal(format_share(0.004), "Under 1%")
})

# Tiles and module ----------------------------------------------------------------

test_that("a callout tile shows the label, number, unit, note and year", {
  html <- as.character(callout_tile(list(
    label = "Population", value = "2.38", unit = "billion", note = "Largest: China", year = "2024"
  )))

  expect_match(html, "Population</p>", fixed = TRUE)
  expect_match(html, '<span class="cds--type-heading-06">2.38</span>', fixed = TRUE)
  expect_match(html, ">billion</span>", fixed = TRUE)
  expect_match(html, "Largest: China</p>", fixed = TRUE)
  expect_match(html, "2024</p>", fixed = TRUE)
})

test_that("the callouts follow the region", {
  local_mocked_bindings(
    fetch_wb_indicator = function(key) {
      switch(key, gdp = glance_gdp, life_expectancy = glance_life, population = glance_population)
    },
    fetch_data = function(region) glance_all
  )
  countries <- shiny::reactiveVal(glance_east_asia)
  region <- shiny::reactiveVal("EAS")

  shiny::testServer(mod_glance_server, args = list(countries = countries, region = region), {
    expect_match(output$callouts$html, "30% of world GDP", fixed = TRUE)

    countries(glance_all)
    region(world_region)
    session$flushReact()
    expect_match(output$callouts$html, "Highest: Japan", fixed = TRUE)
  })
})
