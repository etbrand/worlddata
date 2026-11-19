# The app's list of countries: the World Bank's 217 economies, with their
# region, capital, population, area and Gini index from the World
# Bank, and languages, flag and Wikipedia article from Wikidata.

# The Region select's value for every country (no region filter)
world_region <- "World"

# World Bank regions, by code. The codes are also those of the World Bank's
# regional aggregates (e.g. "EAS" is East Asia & Pacific as a whole).
wb_regions <- c(
  EAS = "East Asia & Pacific",
  ECS = "Europe & Central Asia",
  LCN = "Latin America & Caribbean",
  MEA = "Middle East, North Africa, Afghanistan & Pakistan",
  NAC = "North America",
  SAS = "South Asia",
  SSF = "Sub-Saharan Africa"
)

# The header select: "World" set apart from the regions. Labels are kept
# short to fit the header, so MEA leaves out "Afghanistan & Pakistan"
# (wb_regions has the full name, e.g. for the table title).
region_choices <- list(
  World = world_region,
  Regions = c(
    "East Asia & Pacific" = "EAS",
    "Europe & Central Asia" = "ECS",
    "Latin America & Caribbean" = "LCN",
    "Middle East & North Africa" = "MEA",
    "North America" = "NAC",
    "South Asia" = "SAS",
    "Sub-Saharan Africa" = "SSF"
  )
)

# Countries in `region` (a code of wb_regions), or every country for
# world_region. Every region is filtered from one copy of the whole list,
# so changing region makes no requests.
fetch_data <- function(region) {
  countries <- fetch_all_countries()
  if (identical(region, world_region)) {
    return(countries)
  }
  out <- countries[countries$region_id %in% region, , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Every country, in name order: name, region, region_id, iso3, wikipedia,
# flag, capital, population, area, languages, gini. Both parts are kept on
# disk for a day (see cached_daily()). If Wikidata fails and there's no
# copy yet, countries come without languages, flags or Wikipedia links, and
# Wikidata is tried again next time.
fetch_all_countries <- function() {
  countries <- cached_daily("worldbank countries", fetch_wb_countries)
  details <- tryCatch(
    cached_daily("wikidata countries", function() fetch_wikidata_countries(countries$iso3)),
    api_error = function(e) empty_wikidata_countries()
  )
  join_country_details(countries, details)
}

# Adds Wikidata's details (from fetch_wikidata_countries()) to the World
# Bank's countries, keeping their order
join_country_details <- function(countries, details) {
  i <- match(countries$iso3, details$iso3)
  countries$wikipedia <- details$wikipedia[i]
  countries$flag <- details$flag[i]
  countries$languages <- details$languages[i]
  countries[, c(
    "name", "region", "region_id", "iso3", "wikipedia", "flag",
    "capital", "population", "area", "languages", "gini"
  )]
}

# The World Bank's economies, with their latest population, area and Gini
# coefficient. All ~300 entries, aggregates included, fit on one page.
fetch_wb_countries <- function() {
  body <- httr2::request(worldbank_api_url) |>
    httr2::req_url_path_append("country") |>
    httr2::req_url_query(format = "json", per_page = 1000) |>
    perform_json(worldbank_api_name)

  countries <- parse_wb_countries(body)
  latest <- function(key) {
    values <- fetch_wb_indicator(key)
    values$value[match(countries$iso3, values$iso3)]
  }
  countries$population <- latest("population")
  countries$area <- latest("area")
  countries$gini <- latest("gini")
  countries
}

# The country list's rows -> name, region, region_id, iso3, capital, in
# name order. Aggregates (regions, income groups, "World") are left out.
parse_wb_countries <- function(body) {
  check_wb_body(body)
  rows <- Filter(function(x) !identical(trimws(x$region$value), "Aggregates"), body[[2]])

  field <- function(x, default = NA_character_) {
    x <- trimws(if_null(x, ""))
    if (nzchar(x)) x else default
  }
  out <- data.frame(
    iso3 = vapply(rows, \(x) field(x$id), ""),
    name = vapply(rows, \(x) field(x$name), ""),
    region_id = vapply(rows, \(x) field(x$region$id), ""),
    capital = vapply(rows, \(x) field(x$capitalCity), "")
  )
  out$name <- display_name(out$iso3, out$name)
  out$region <- unname(wb_regions[out$region_id])
  out <- out[order(out$name), c("name", "region", "region_id", "iso3", "capital")]
  rownames(out) <- NULL
  out
}

# The World Bank uses formal names for some economies ("Korea, Rep."), which
# are long on chart axes and sort oddly. Their common English names, by
# ISO3 code:
common_names <- c(
  BHS = "Bahamas",
  BRN = "Brunei",
  CIV = "Côte d'Ivoire",
  COD = "DR Congo",
  COG = "Republic of the Congo",
  CUW = "Curaçao",
  EGY = "Egypt",
  FSM = "Micronesia",
  GMB = "Gambia",
  HKG = "Hong Kong",
  IRN = "Iran",
  KGZ = "Kyrgyzstan",
  KNA = "Saint Kitts and Nevis",
  KOR = "South Korea",
  LAO = "Laos",
  LCA = "Saint Lucia",
  MAC = "Macao",
  MAF = "Saint Martin",
  PRI = "Puerto Rico",
  PRK = "North Korea",
  RUS = "Russia",
  SOM = "Somalia",
  SVK = "Slovakia",
  SXM = "Sint Maarten",
  SYR = "Syria",
  TUR = "Türkiye",
  VCT = "Saint Vincent and the Grenadines",
  VEN = "Venezuela",
  VIR = "U.S. Virgin Islands",
  VNM = "Vietnam",
  YEM = "Yemen"
)

display_name <- function(iso3, name) {
  common <- unname(common_names[iso3])
  ifelse(is.na(common), name, common)
}
