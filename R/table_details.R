# Content of the Country list table's expandable rows: each country's Wikipedia
# summary

# `summary` is one row of fetch_wikipedia_summaries(), or NULL if Wikipedia
# couldn't be reached. `name` is the country's name, for messages.
wikipedia_details <- function(summary, name) {
  if (is.null(summary) || is.na(summary$extract)) {
    return(htmltools::div(
      class = "wiki-details wiki-details--empty cds--type-body-01",
      if (is.null(summary)) {
        "The Wikipedia summary couldn't be loaded. Try again later."
      } else {
        sprintf("No Wikipedia summary is available for %s.", name)
      }
    ))
  }

  htmltools::div(
    class = "wiki-details",
    htmltools::div(
      class = "wiki-details__text",
      htmltools::p(class = "cds--type-body-01", summary$extract),
      htmltools::p(
        class = "wiki-details__source with-icon cds--type-helper-text-01",
        htmltools::a(
          class = "with-icon",
          href = summary$url,
          target = "_blank",
          rel = "noopener noreferrer",
          htmltools::span("Read more on Wikipedia"),
          carbonite::carbon_icon("launch", title = "Opens in a new tab")
        ),
        htmltools::span("· Text from Wikipedia, CC BY-SA 4.0")
      )
    )
  )
}

# Details for one country (a one-row data frame with name and wikipedia),
# fetching its Wikipedia summary. Called when the row is first expanded
# (carbon_table(lazy_details = TRUE)). A Wikipedia error shows a message in
# the row rather than failing.
country_details <- function(country) {
  summary <- tryCatch(
    fetch_wikipedia_summaries(wikipedia_title(country$wikipedia)),
    api_error = function(e) NULL
  )
  wikipedia_details(summary, country$name)
}
