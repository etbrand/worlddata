# Country profile, shown in a modal when a country's name is clicked in the
# Country list table. A first version: key facts, the flag and the Wikipedia
# summary. Charts can come later.

# Key facts for one country (a one-row data frame from fetch_data()), as
# label/value rows. Missing values show as a dash, like the table.
country_facts <- function(country) {
  facts <- c(
    "Region" = format_missing(country$region),
    "Capital" = format_missing(country$capital),
    "Population" = format_number(country$population),
    "Area" = if (is.na(country$area)) {
      missing_label
    } else {
      paste(format_number(country$area), "km²")
    },
    "Languages" = format_missing(country$languages),
    "Gini index" = format_missing(country$gini)
  )
  data.frame(fact = names(facts), value = unname(facts))
}

country_profile_modal <- function(country) {
  summary <- tryCatch(
    fetch_wikipedia_summaries(wikipedia_title(country$wikipedia)),
    api_error = function(e) NULL
  )

  carbonite::carbon_modal(
    title = country$name,
    label = "Country profile",
    size = "md",
    footer = carbonite::carbon_modal_close_button("Close", kind = "primary"),
    htmltools::div(
      class = "country-profile",
      if (!is.na(country$flag)) {
        htmltools::img(
          class = "country-profile__flag",
          src = country$flag,
          alt = sprintf("Flag of %s", country$name)
        )
      },
      carbonite::carbon_structured_list(
        country_facts(country),
        columns = c(fact = "Key facts", value = ""),
        condensed = TRUE
      ),
      htmltools::h3(
        "Overview",
        class = "country-profile__heading cds--type-heading-02"
      ),
      wikipedia_details(summary, country$name)
    )
  )
}
