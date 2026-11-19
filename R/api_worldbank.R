# World Bank Indicators API (https://datahelpdesk.worldbank.org): yearly
# development indicators per country. No key needed.
#
# Each indicator is fetched for all countries at once (one cached request
# covers every region) and joined to the country list (R/api_countries.R)
# by ISO3 code. The responses include the World Bank's aggregates (regions,
# income groups, "WLD" for the world), which the joins leave out.

worldbank_api_url <- "https://api.worldbank.org/v2"
worldbank_api_name <- "the World Bank API"

# Indicators offered in the app. `log` puts the axis on a log scale, for
# indicators that span several orders of magnitude.
wb_indicators <- list(
  gdp_per_capita = list(
    id = "NY.GDP.PCAP.CD",
    label = "GDP per capita (US$)",
    log = TRUE
  ),
  life_expectancy = list(
    id = "SP.DYN.LE00.IN",
    label = "Life expectancy (years)",
    log = FALSE
  ),
  internet = list(
    id = "IT.NET.USER.ZS",
    label = "Internet users (% of population)",
    log = FALSE
  ),
  urban = list(
    id = "SP.URB.TOTL.IN.ZS",
    label = "Urban population (%)",
    log = FALSE
  ),
  co2_per_capita = list(
    id = "EN.GHG.CO2.PC.CE.AR5",
    label = "CO₂ emissions per person (tonnes)",
    log = TRUE
  ),
  fertility = list(
    id = "SP.DYN.TFRT.IN",
    label = "Fertility rate (births per woman)",
    log = FALSE
  ),
  unemployment = list(
    id = "SL.UEM.TOTL.ZS",
    label = "Unemployment (% of labor force)",
    log = FALSE
  ),
  # Economic indicators page only: `scatter = FALSE` leaves them out of the
  # Indicators scatter, where yearly swings or a country's size say little
  gdp = list(
    id = "NY.GDP.MKTP.CD",
    label = "GDP (US$)",
    log = TRUE,
    scatter = FALSE
  ),
  gdp_growth = list(
    id = "NY.GDP.MKTP.KD.ZG",
    label = "GDP growth (annual %)",
    log = FALSE,
    scatter = FALSE
  ),
  inflation = list(
    id = "FP.CPI.TOTL.ZG",
    label = "Inflation, consumer prices (annual %)",
    log = FALSE,
    scatter = FALSE
  ),
  # Columns of the country list (fetch_wb_countries())
  population = list(
    id = "SP.POP.TOTL",
    label = "Population",
    log = TRUE,
    scatter = FALSE
  ),
  area = list(
    id = "AG.SRF.TOTL.K2",
    label = "Surface area (km²)",
    log = TRUE,
    scatter = FALSE
  ),
  gini = list(
    id = "SI.POV.GINI",
    label = "Gini index",
    log = FALSE,
    scatter = FALSE
  )
)

# Named vector for the scatter's select inputs: label = key
wb_indicator_choices <- function() {
  shown <- Filter(\(x) !isFALSE(x$scatter), wb_indicators)
  stats::setNames(names(shown), vapply(shown, `[[`, "", "label"))
}

# The API answers with HTTP 200 even for errors, as [{message: [...]}];
# data comes as [metadata, rows]. Signals an api_error for the former.
check_wb_body <- function(body) {
  if (length(body) < 2 || !is.null(body[[1]]$message)) {
    detail <- body[[1]]$message[[1]]$value
    stop(api_error(paste0(
      "The World Bank API returned an error",
      if (!is.null(detail)) paste0(": ", detail) else "."
    )))
  }
  invisible(body)
}

parse_wb_indicator <- function(body) {
  check_wb_body(body)

  # Missing values are JSON null (or an empty object)
  field <- function(x, default) if (length(x) == 0) default else x
  rows <- body[[2]]
  out <- data.frame(
    iso3 = vapply(rows, \(x) field(x$countryiso3code, ""), ""),
    year = vapply(rows, \(x) as.integer(x$date), integer(1)),
    value = vapply(rows, \(x) as.numeric(field(x$value, NA_real_)), numeric(1))
  )
  # Aggregates like "Arab World" sometimes have no code
  out[nzchar(out$iso3) & !is.na(out$value), , drop = FALSE]
}

# Most recent available value of indicator `key` (a name of wb_indicators)
# for every country: data frame of iso3, year, value
fetch_wb_indicator <- function(key) {
  indicator <- wb_indicators[[key]]
  if (is.null(indicator)) {
    stop(api_error(sprintf("Unknown indicator \"%s\".", key)))
  }

  cached(paste("worldbank", indicator$id), cache = wb_cache, function() {
    httr2::request(worldbank_api_url) |>
      httr2::req_url_path_append("country", "all", "indicator", indicator$id) |>
      # mrnev = most recent non-empty value per country; ~270 countries and
      # aggregates fit on one page
      httr2::req_url_query(format = "json", mrnev = 1, per_page = 1000) |>
      perform_json(worldbank_api_name) |>
      parse_wb_indicator()
  })
}

# Yearly values of indicator `key` for every country from `start` to `end`:
# data frame of iso3, year, value (only years with a value). About 7,000
# rows for 2000-2025, which fit on one page; any further pages are fetched
# too.
fetch_wb_history <- function(key, start = 2000, end = as.integer(format(Sys.Date(), "%Y"))) {
  indicator <- wb_indicators[[key]]
  if (is.null(indicator)) {
    stop(api_error(sprintf("Unknown indicator \"%s\".", key)))
  }

  cached(paste("worldbank history", indicator$id, start, end), cache = wb_cache, function() {
    request_page <- function(page) {
      httr2::request(worldbank_api_url) |>
        httr2::req_url_path_append("country", "all", "indicator", indicator$id) |>
        httr2::req_url_query(
          format = "json",
          date = paste0(start, ":", end),
          per_page = 20000,
          page = page
        ) |>
        perform_json(worldbank_api_name)
    }

    first <- request_page(1)
    pages <- c(list(first), lapply(seq_len(max(0, wb_page_count(first) - 1)) + 1, request_page))
    do.call(rbind, lapply(pages, parse_wb_indicator))
  })
}

# Number of pages in a World Bank response, from its metadata
wb_page_count <- function(body) {
  pages <- suppressWarnings(as.integer(body[[1]]$pages))
  if (length(pages) == 1 && !is.na(pages)) pages else 1L
}

# Join two indicators to `countries` (which needs name, iso3 and population).
# Keeps every country with an ISO3 code, in name order, with NA where an
# indicator is missing: the chart's points then stay in the same order when
# the indicators change, so ECharts can animate each one to its new place.
indicator_scatter_data <- function(countries, x, y) {
  names(x) <- c("iso3", "x_year", "x")
  names(y) <- c("iso3", "y_year", "y")

  out <- countries[!is.na(countries$iso3), c("name", "iso3", "population"), drop = FALSE]
  out <- merge(out, x, by = "iso3", all.x = TRUE)
  out <- merge(out, y, by = "iso3", all.x = TRUE)
  out <- out[order(out$name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Rows with both indicators, i.e. the points that are drawn
has_both <- function(data) {
  !is.na(data$x) & !is.na(data$y)
}
