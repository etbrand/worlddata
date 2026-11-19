page <- function(title, extract = paste(title, "is a country.")) {
  list(pageid = 1, ns = 0, title = title, extract = extract)
}

test_that("article titles come from Wikipedia URLs", {
  expect_equal(
    wikipedia_title(c(
      "https://en.wikipedia.org/wiki/C%C3%B4te_d%27Ivoire",
      "https://en.wikipedia.org/wiki/Georgia_(country)",
      NA,
      "https://example.com/not-wikipedia"
    )),
    c("Côte d'Ivoire", "Georgia (country)", NA, NA)
  )
})

test_that("summaries follow redirects and keep the requested order", {
  body <- list(query = list(
    redirects = list(list(from = "Côte d'Ivoire", to = "Ivory Coast")),
    pages = list(
      page("Ivory Coast"),
      page("Chad"),
      list(ns = 0, title = "Nowhereland", missing = TRUE)
    )
  ))

  out <- parse_wikipedia_summaries(body, c("Chad", "Côte d'Ivoire", "Nowhereland"))

  expect_equal(out$title, c("Chad", "Côte d'Ivoire", "Nowhereland"))
  expect_equal(out$page_title, c("Chad", "Ivory Coast", NA))
  expect_equal(out$extract[2], "Ivory Coast is a country.")
  expect_equal(out$url[2], "https://en.wikipedia.org/wiki/Ivory_Coast")
  expect_true(is.na(out$extract[3]))
})

test_that("summaries are fetched 20 titles at a time and cached per title", {
  local_empty_cache()
  requests <- list()
  httr2::local_mocked_responses(function(req) {
    titles <- strsplit(utils::URLdecode(sub(".*titles=([^&]*).*", "\\1", req$url)), "|", fixed = TRUE)[[1]]
    requests[[length(requests) + 1]] <<- titles
    expect_match(req$options$useragent, "WorldDataDashboard")
    # Text only: flags come from Wikidata
    expect_match(req$url, "prop=extracts&")
    httr2::response_json(body = list(query = list(pages = lapply(titles, page))))
  })
  titles <- paste("Country", 1:25)

  out <- fetch_wikipedia_summaries(c(titles, NA))

  expect_equal(lengths(requests), c(20, 5))
  expect_equal(out$extract[25], "Country 25 is a country.")
  expect_true(is.na(out$extract[26]))

  # Cached: a second call with one new title only asks for that title
  fetch_wikipedia_summaries(c(titles[1:3], "Country 99"))
  expect_equal(requests[[3]], "Country 99")
})

test_that("Wikipedia errors become api_errors", {
  local_empty_cache()
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 503))

  expect_error(fetch_wikipedia_summaries("Chad"), class = "api_error")
})

# Row details -------------------------------------------------------------------

summary_row <- function(...) {
  s <- empty_summaries("Chad")
  s$page_title <- "Chad"
  s$extract <- "Chad is a landlocked country."
  s$url <- "https://en.wikipedia.org/wiki/Chad"
  utils::modifyList(s, list(...))
}

test_that("details show the summary and a credited link, without an image", {
  html <- as.character(wikipedia_details(summary_row(), "Chad"))

  expect_match(html, "Chad is a landlocked country.", fixed = TRUE)
  expect_match(html, 'href="https://en.wikipedia.org/wiki/Chad"', fixed = TRUE)
  expect_match(html, "CC BY-SA 4.0", fixed = TRUE)
  expect_no_match(html, "<img", fixed = TRUE)
})

test_that("a country's details fetch its summary, and explain missing ones and errors", {
  chad <- data.frame(name = "Chad", wikipedia = "https://en.wikipedia.org/wiki/Chad")
  nowhere <- data.frame(name = "Nowhereland", wikipedia = NA_character_)

  local_mocked_bindings(fetch_wikipedia_summaries = function(titles) {
    if (identical(titles, "Chad")) summary_row() else empty_summaries(titles)
  })
  expect_match(as.character(country_details(chad)), "Chad is a landlocked country.", fixed = TRUE)
  expect_match(as.character(country_details(nowhere)), "No Wikipedia summary is available for Nowhereland.", fixed = TRUE)

  local_mocked_bindings(fetch_wikipedia_summaries = function(titles) stop(api_error("down")))
  expect_match(as.character(country_details(chad)), "couldn't be loaded", fixed = TRUE)
})

test_that("the table's rows have lazy Wikipedia details", {
  local_mocked_bindings(fetch_data = function(region) {
    data.frame(name = c("Chad", "Niger"), iso3 = c("TCD", "NER"), capital = NA, population = 1:2,
               area = 1:2, languages = NA, gini = NA,
               wikipedia = c("https://en.wikipedia.org/wiki/Chad", "https://en.wikipedia.org/wiki/Niger"))
  })
  fetched <- character()
  local_mocked_bindings(fetch_wikipedia_summaries = function(titles) {
    fetched <<- c(fetched, titles)
    empty_summaries(titles)
  })

  shiny::testServer(server, {
    session$setInputs(region = "SSF", columns = "population")
    html <- output$table$html

    # Nothing fetched to render the table: only placeholders
    expect_length(fetched, 0)
    expect_match(html, "data-lazy-details", fixed = TRUE)
    expect_match(html, "carbonite-details-loading", fixed = TRUE)
  })
})
