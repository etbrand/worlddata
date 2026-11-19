# At a glance callouts: four headline figures for the selected region,
# above the Country list and Languages tabs (see R/glance.R for the figures)

mod_glance_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(class = "glance-callouts", shiny::uiOutput(ns("callouts")))
}

# `countries`: reactive data frame from fetch_data(); `region`: reactive
# header region (world_region or a code of wb_regions)
mod_glance_server <- function(id, countries, region) {
  shiny::moduleServer(id, function(input, output, session) {
    # Latest values for every country and aggregate, cached for a day and
    # shared with the other pages; an api_error if a request fails
    latest <- function(key) {
      shiny::reactive(tryCatch(fetch_wb_indicator(key), api_error = identity))
    }
    gdp <- latest("gdp")
    life <- latest("life_expectancy")
    population <- latest("population")

    output$callouts <- shiny::renderUI({
      shiny::req(countries(), region())
      callouts <- glance_callouts(
        region = region(),
        countries = countries(),
        all_countries = fetch_data(world_region),
        gdp = gdp(),
        life = life(),
        population = population()
      )
      # A row of four on wide screens, two by two on tablets, stacked on
      # phones; the same gap between rows as between columns (DESIGN.md)
      carbonite::carbon_grid(
        row_gap = 7,
        lapply(callouts, function(callout) {
          carbonite::carbon_column(
            width = c(sm = 4, md = 4, lg = 4),
            callout_tile(callout)
          )
        })
      )
    })
  })
}

# One callout as a tile: label, big number (with a smaller unit), note and
# the data's year
callout_tile <- function(callout) {
  carbonite::carbon_tile(
    class = "callout-tile",
    htmltools::p(
      class = "callout-tile__label cds--type-label-01",
      callout$label
    ),
    htmltools::p(
      class = "callout-tile__value",
      htmltools::span(class = "cds--type-heading-06", callout$value),
      if (!is.null(callout$unit)) {
        htmltools::span(
          class = "callout-tile__unit cds--type-body-02",
          callout$unit
        )
      }
    ),
    if (!is.null(callout$note)) {
      htmltools::p(
        class = "callout-tile__note cds--type-helper-text-01",
        callout$note
      )
    },
    if (!is.null(callout$year)) {
      htmltools::p(
        class = "callout-tile__year cds--type-helper-text-01",
        callout$year
      )
    }
  )
}
