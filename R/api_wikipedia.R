# Wikipedia summaries (intro text) for the expandable rows of the Summary
# table. Wikidata gives each country's English Wikipedia URL, so there's no
# guessing article titles from country names.
#
# Uses the MediaWiki Action API, which returns up to 20 articles' intros per
# request, rather than one request per country. Wikipedia text is CC BY-SA,
# so the UI links back to each article.

wikipedia_api_url <- "https://en.wikipedia.org/w/api.php"
wikipedia_api_name <- "Wikipedia"

# The article title from a Wikipedia URL, e.g.
# ".../wiki/C%C3%B4te_d%27Ivoire" -> "Côte d'Ivoire"
wikipedia_title <- function(url) {
  title <- sub("^.*/wiki/", "", url)
  title <- vapply(title, utils::URLdecode, "", USE.NAMES = FALSE)
  out <- gsub("_", " ", title)
  out[is.na(url) | !grepl("/wiki/", url)] <- NA_character_
  out
}

empty_summaries <- function(titles = character()) {
  n <- length(titles)
  data.frame(
    title = as.character(titles),
    page_title = rep(NA_character_, n),
    extract = rep(NA_character_, n),
    url = rep(NA_character_, n)
  )
}

# One row per requested title (in order), following Wikipedia's title
# normalisation and redirects (e.g. "Côte d'Ivoire" -> "Ivory Coast").
# Titles without an article get NAs.
parse_wikipedia_summaries <- function(body, titles) {
  query <- body$query
  out <- empty_summaries(titles)
  if (is.null(query$pages)) return(out)

  # requested title -> final page title
  final <- stats::setNames(titles, titles)
  for (step in list(query$normalized, query$redirects)) {
    for (m in step) final[final == m$from] <- m$to
  }

  pages <- query$pages
  page_titles <- vapply(pages, \(p) p$title, "")

  for (i in seq_along(titles)) {
    j <- match(final[[i]], page_titles)
    if (is.na(j)) next
    page <- pages[[j]]
    if (isTRUE(page$missing) || length(page$extract) == 0) next

    out$page_title[i] <- page$title
    out$extract[i] <- page$extract
    out$url[i] <- paste0("https://en.wikipedia.org/wiki/", utils::URLencode(gsub(" ", "_", page$title)))
  }
  out
}

wikipedia_request <- function(titles) {
  httr2::request(wikipedia_api_url) |>
    httr2::req_url_query(
      action = "query",
      format = "json",
      formatversion = 2,
      redirects = 1,
      prop = "extracts",
      # Plain-text intro, cut to a few sentences
      exintro = 1,
      explaintext = 1,
      exsentences = 3,
      exlimit = 20,
      titles = paste(titles, collapse = "|")
    ) |>
    httr2::req_user_agent(wikimedia_user_agent)
}

# Summaries for `titles` (NA allowed), one row each in the same order. Each
# title is cached on its own, so another region or a re-render only fetches
# what isn't cached yet, 20 titles per request.
fetch_wikipedia_summaries <- function(titles) {
  out <- empty_summaries(titles)
  wanted <- unique(titles[!is.na(titles)])
  if (!length(wanted)) return(out)

  key <- function(title) paste("wikipedia", title)
  found <- lapply(wanted, \(t) api_cache$get(cache_key(key(t))))
  missing <- wanted[vapply(found, cachem::is.key_missing, logical(1))]

  for (batch in split(missing, ceiling(seq_along(missing) / 20))) {
    summaries <- wikipedia_request(batch) |>
      perform_json(wikipedia_api_name) |>
      parse_wikipedia_summaries(batch)
    for (i in seq_len(nrow(summaries))) {
      api_cache$set(cache_key(key(batch[[i]])), summaries[i, , drop = FALSE])
    }
  }

  for (i in which(!is.na(titles))) {
    cached_row <- api_cache$get(cache_key(key(titles[[i]])))
    if (!cachem::is.key_missing(cached_row)) out[i, ] <- cached_row
  }
  out
}
