# A row of the World Bank's country list
wb_country <- function(iso3, name, region_id, region, capital = "") {
  list(id = iso3, name = name, region = list(id = region_id, value = region), capitalCity = capital)
}

wb_country_list <- list(
  list(page = 1, pages = 1, per_page = 1000, total = 4),
  list(
    wb_country("KOR", "Korea, Rep.", "EAS", "East Asia & Pacific", "Seoul"),
    wb_country("EAS", "East Asia & Pacific", "NA", "Aggregates"),
    wb_country("TCD", "Chad", "SSF", "Sub-Saharan Africa ", "N'Djamena"),
    wb_country("ASM", "American Samoa", "EAS", "East Asia & Pacific")
  )
)

wb_values <- function(...) {
  values <- list(...)
  list(
    list(page = 1, pages = 1),
    lapply(names(values), \(iso3) list(countryiso3code = iso3, date = "2024", value = values[[iso3]]))
  )
}

wikidata_row <- function(item, iso3, article, flag = NULL, languages = "") {
  row <- list(
    item = list(value = paste0("http://www.wikidata.org/entity/Q", item)),
    iso3 = list(value = iso3),
    article = list(value = article),
    languages = list(value = languages)
  )
  if (!is.null(flag)) row$flag <- list(value = flag)
  row
}

wikidata_body <- list(results = list(bindings = list(
  wikidata_row(884, "KOR", "https://en.wikipedia.org/wiki/South_Korea",
               "http://commons.wikimedia.org/wiki/Special:FilePath/Flag%20of%20South%20Korea.svg", "Korean"),
  wikidata_row(657, "TCD", "https://en.wikipedia.org/wiki/Chad", NULL, "French|Arabic")
)))

# Mocks the World Bank and Wikidata. Returns a function giving the URLs
# requested so far. `wikidata` is the Wikidata response, or a status code.
mock_country_apis <- function(wikidata = wikidata_body, env = parent.frame()) {
  urls <- character()
  httr2::local_mocked_responses(function(req) {
    url <- req$url
    urls[[length(urls) + 1]] <<- url
    if (grepl("wikidata", url)) {
      if (is.numeric(wikidata)) return(httr2::response(status_code = wikidata))
      return(httr2::response_json(body = wikidata))
    }
    body <- if (grepl("SP.POP.TOTL", url, fixed = TRUE)) {
      wb_values(KOR = 51e6, TCD = 18e6, EAS = 2.4e9)
    } else if (grepl("AG.SRF.TOTL.K2", url, fixed = TRUE)) {
      wb_values(KOR = 100410, TCD = 1284000)
    } else if (grepl("SI.POV.GINI", url, fixed = TRUE)) {
      wb_values(KOR = 31.4)
    } else {
      wb_country_list
    }
    httr2::response_json(body = body)
  }, env = env)
  function() urls
}

# Country list ------------------------------------------------------------------

test_that("the World Bank's country list leaves out aggregates and uses common names", {
  out <- parse_wb_countries(wb_country_list)

  expect_equal(out$name, c("American Samoa", "Chad", "South Korea"))
  expect_equal(out$iso3, c("ASM", "TCD", "KOR"))
  expect_equal(out$region_id, c("EAS", "SSF", "EAS"))
  expect_equal(out$region, c("East Asia & Pacific", "Sub-Saharan Africa", "East Asia & Pacific"))
  expect_equal(out$capital, c(NA, "N'Djamena", "Seoul"))
})

test_that("World Bank errors in the country list are reported", {
  body <- list(list(message = list(list(value = "Invalid format"))))
  expect_error(parse_wb_countries(body), "Invalid format", class = "api_error")
})

test_that("display_name only replaces formal names", {
  expect_equal(display_name(c("KOR", "TCD"), c("Korea, Rep.", "Chad")), c("South Korea", "Chad"))
})

test_that("every region choice is a World Bank region", {
  expect_setequal(region_choices$Regions, names(wb_regions))
})

# fetch_data --------------------------------------------------------------------

test_that("fetch_data joins World Bank and Wikidata details", {
  local_empty_cache()
  mock_country_apis()

  out <- fetch_data(world_region)

  expect_equal(names(out), c(
    "name", "region", "region_id", "iso3", "wikipedia", "flag",
    "capital", "population", "area", "languages", "gini"
  ))
  korea <- out[out$iso3 == "KOR", ]
  expect_equal(korea$population, 51e6)
  expect_equal(korea$area, 100410)
  expect_equal(korea$gini, 31.4)
  expect_equal(korea$wikipedia, "https://en.wikipedia.org/wiki/South_Korea")
  expect_equal(korea$languages, "Korean")
  expect_match(korea$flag, "^https://commons.wikimedia.org/.*\\?width=160$")

  chad <- out[out$iso3 == "TCD", ]
  expect_equal(chad$languages, "Arabic, French")
  expect_true(is.na(chad$flag))
  expect_true(is.na(chad$gini))
  # Not in Wikidata
  expect_true(is.na(out$wikipedia[out$iso3 == "ASM"]))
})

test_that("regions are filtered from one cached copy, without more requests", {
  local_empty_cache()
  urls <- mock_country_apis()

  fetch_data(world_region)
  n <- length(urls())
  east_asia <- fetch_data("EAS")
  africa <- fetch_data("SSF")

  # The country list, three indicators and Wikidata
  expect_equal(n, 5)
  expect_length(urls(), n)
  expect_equal(east_asia$name, c("American Samoa", "South Korea"))
  expect_equal(africa$name, "Chad")
  expect_equal(nrow(fetch_data("NAC")), 0)
})

test_that("countries still load if Wikidata fails, and Wikidata is retried", {
  local_empty_cache()
  urls <- mock_country_apis(wikidata = 503)

  out <- fetch_data("EAS")
  expect_equal(out$name, c("American Samoa", "South Korea"))
  expect_true(all(is.na(out$languages)))

  fetch_data("EAS")
  expect_equal(sum(grepl("wikidata", urls())), 2)
})

test_that("fetch_data reports World Bank errors with a friendly message", {
  local_empty_cache()
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 503))

  expect_error(fetch_data("EAS"), "World Bank API returned an error", class = "api_error")
})

# Wikidata ----------------------------------------------------------------------

test_that("the Wikidata query only includes valid ISO3 codes", {
  query <- wikidata_countries_query(c("KOR", "TCD", "KOR", '"} DROP', NA))

  expect_match(query, 'VALUES ?iso3 { "KOR" "TCD" }', fixed = TRUE)
  expect_no_match(query, "DROP")
})

test_that("Wikidata results keep one row per country, from its main item", {
  body <- list(results = list(bindings = list(
    wikidata_row(407199, "PSE", "https://en.wikipedia.org/wiki/Palestinian_territories", NULL, "Arabic"),
    wikidata_row(219060, "PSE", "https://en.wikipedia.org/wiki/Palestine", NULL, "Arabic"),
    wikidata_row(884, "KOR", "https://en.wikipedia.org/wiki/South_Korea")
  )))

  out <- parse_wikidata_countries(body)

  expect_equal(out$iso3, c("KOR", "PSE"))
  expect_equal(out$wikipedia[2], "https://en.wikipedia.org/wiki/Palestine")
  expect_true(is.na(out$languages[1]))
})

test_that("Wikidata handles no results", {
  expect_equal(nrow(parse_wikidata_countries(list(results = list(bindings = list())))), 0)
})

test_that("flags are small PNG thumbnails from Wikimedia Commons", {
  expect_equal(
    flag_thumbnail_url(c("http://commons.wikimedia.org/wiki/Special:FilePath/Flag%20of%20Chad.svg", NA, "")),
    c("https://commons.wikimedia.org/wiki/Special:FilePath/Flag%20of%20Chad.svg?width=160", NA, NA)
  )
})

test_that("languages are listed alphabetically", {
  expect_equal(format_languages("Spanish|Aymara|Spanish"), "Aymara, Spanish")
  expect_true(is.na(format_languages("")))
})
