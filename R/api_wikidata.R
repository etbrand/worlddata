# Wikidata (https://www.wikidata.org): the details the World Bank doesn't
# have, i.e. each country's official languages, flag and English Wikipedia
# article. One SPARQL query covers every country, matched by ISO3 code. No
# key needed.

wikidata_sparql_url <- "https://query.wikidata.org/sparql"
wikidata_api_name <- "Wikidata"

# SPARQL query for the countries with ISO3 codes `iso3`. Countries that no
# longer exist (P576, "dissolved") are left out, as some reuse codes.
#   P298: ISO 3166-1 alpha-3 code   P41: flag image   P37: official language
wikidata_countries_query <- function(iso3) {
  # Codes go into the query text, so only allow real codes
  iso3 <- unique(iso3[grepl("^[A-Z]{3}$", iso3)])
  sprintf(
    'SELECT ?item ?iso3 ?article ?flag (GROUP_CONCAT(DISTINCT ?language; separator="|") AS ?languages)
WHERE {
  VALUES ?iso3 { %s }
  ?item wdt:P298 ?iso3 .
  FILTER NOT EXISTS { ?item wdt:P576 ?dissolved }
  OPTIONAL { ?item wdt:P41 ?flag }
  OPTIONAL { ?article schema:about ?item ; schema:isPartOf <https://en.wikipedia.org/> }
  OPTIONAL {
    ?item wdt:P37 ?official .
    ?official rdfs:label ?language .
    FILTER(LANG(?language) = "en")
  }
}
GROUP BY ?item ?iso3 ?article ?flag',
    paste0('"', iso3, '"', collapse = " ")
  )
}

empty_wikidata_countries <- function() {
  data.frame(
    iso3 = character(),
    wikipedia = character(),
    flag = character(),
    languages = character()
  )
}

# Languages, flag and Wikipedia URL for each of `iso3`: data frame of iso3,
# wikipedia, flag, languages. Countries Wikidata doesn't know are missing.
# The query can take tens of seconds, so callers should cache the result.
fetch_wikidata_countries <- function(iso3) {
  httr2::request(wikidata_sparql_url) |>
    # POST: the list of codes makes the query too long for a URL
    httr2::req_body_form(query = wikidata_countries_query(iso3)) |>
    httr2::req_headers(Accept = "application/sparql-results+json") |>
    httr2::req_user_agent(wikimedia_user_agent) |>
    perform_json(wikidata_api_name, timeout = 90) |>
    parse_wikidata_countries()
}

# SPARQL JSON results -> one row per country. A country can come back more
# than once: with two flags, or when two items share a code (e.g. "State of
# Palestine" and "Palestinian territories"). Keep the item with the lowest
# Q-number, Wikidata's oldest and usually main item, and its first flag.
parse_wikidata_countries <- function(body) {
  rows <- body$results$bindings
  if (length(rows) == 0) {
    return(empty_wikidata_countries())
  }

  value <- function(x, field) if (is.null(x[[field]])) NA_character_ else x[[field]]$value
  column <- function(field) vapply(rows, value, "", field)

  out <- data.frame(
    item = as.integer(sub("^.*/Q", "", column("item"))),
    iso3 = column("iso3"),
    wikipedia = column("article"),
    flag = flag_thumbnail_url(column("flag")),
    languages = vapply(column("languages"), format_languages, "", USE.NAMES = FALSE)
  )
  out <- out[order(out$iso3, out$item), , drop = FALSE]
  out <- out[!duplicated(out$iso3), c("iso3", "wikipedia", "flag", "languages"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# "Spanish|Aymara" -> "Aymara, Spanish"; NA when there are none
format_languages <- function(x) {
  languages <- sort(unique(strsplit(if_null(x, ""), "|", fixed = TRUE)[[1]]))
  languages <- languages[nzchar(languages)]
  if (length(languages) == 0) NA_character_ else paste(languages, collapse = ", ")
}

# A flag as a 160px-wide PNG: Wikidata links each flag's SVG on Wikimedia
# Commons, whose Special:FilePath redirects to a thumbnail of any width.
# Plenty for a table icon and the profile.
flag_thumbnail_url <- function(url, width = 160) {
  out <- paste0(sub("^http://", "https://", url), "?width=", width)
  out[is.na(url) | !nzchar(url)] <- NA_character_
  out
}
