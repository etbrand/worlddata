# Social indicators page: four World Bank indicators for the countries in the
# selected region, each with the chart type that fits its question.

social_charts <- list(
  life = list(
    indicator = "life_expectancy",
    title = "Life expectancy",
    question = "Where do people live longest?",
    icon = "health-cross"
  ),
  internet = list(
    indicator = "internet",
    title = "Internet use",
    question = "How has internet use grown since 2000?",
    icon = "wifi"
  ),
  fertility = list(
    indicator = "fertility",
    title = "Fertility rate",
    question = "How many children do women have, across the region?",
    icon = "chart--histogram"
  ),
  urban = list(
    indicator = "urban",
    title = "Urban population",
    question = "How quickly are people moving to cities?",
    icon = "building"
  )
)

mod_social_ui <- function(id) {
  ns <- shiny::NS(id)
  half <- c(sm = 4, md = 8, lg = 8)
  tile <- function(key, height = NULL) indicator_chart_tile(ns, social_charts, key, height)

  indicator_page(
    indicator_page_heading(
      ns,
      "Social indicators",
      "How long people live, how connected they are, family size and where ",
      "people live, for the countries in the selected region. Data from the ",
      "World Bank; hover over the charts for details."
    ),
    # Per-country charts: initial height only; they resize to fit their rows
    # (rows_chart_height())
    carbonite::carbon_column(width = half, tile("life", height = "400px")),
    carbonite::carbon_column(width = half, tile("internet", height = "400px")),
    carbonite::carbon_column(width = half, tile("fertility")),
    carbonite::carbon_column(width = half, tile("urban"))
  )
}

# `countries`: reactive data frame from fetch_data() (name, iso3, population)
# `region`: reactive header region, for the Countries/Regions switcher
mod_social_server <- function(id, countries, region = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # Each chart: a function from (countries, history) to its data, the
    # chart, how many countries it shows, the years its values are from,
    # and whether it has one row per country (so is capped for the world)
    specs <- list(
      # Every country, ranked; with many countries the chart shows the
      # highest and lowest 10, with the median of them all
      life = list(
        data = function(cs, h) ranked_latest(cs, h, max_countries = Inf),
        chart = function(d) {
          lollipop_chart(d, "Years", digits = 1, extremes = ranked_extremes, median = stats::median(d$value))
        },
        rows = function(d) nrow(d),
        years = function(d) d$year,
        latest = TRUE,
        per_country = FALSE,
        note = function(d) ranked_note(nrow(d))
      ),
      # Every country, ranked by its latest value; with many countries, the
      # highest and lowest 10, with the median latest value of them all
      internet = list(
        data = function(cs, h) change_since(cs, h, max_countries = Inf),
        chart = function(d) {
          dumbbell_chart(d, "% of population", digits = 0, extremes = ranked_extremes, median = stats::median(d$end))
        },
        rows = function(d) nrow(d),
        years = function(d) c(d$start_year, d$end_year),
        latest = FALSE,
        per_country = FALSE,
        note = function(d) ranked_note(nrow(d), median = "the median latest value")
      ),
      fertility = list(
        data = function(cs, h) binned_latest(cs, h, width = 0.5),
        chart = function(d) histogram_chart(d, "Births per woman"),
        rows = function(d) sum(d$count),
        years = function(d) NULL,
        latest = TRUE,
        per_country = FALSE
      ),
      # A line per country for a few countries; with more, the median and
      # the middle half of countries (see trend_or_spread())
      urban = list(
        data = function(cs, h) trend_or_spread(cs, h),
        chart = function(d) {
          if (d$kind == "lines") {
            trend_chart(d$trends, "% of population", digits = 1)
          } else {
            spread_band_chart(d$spread$spread, "% of population", digits = 1)
          }
        },
        rows = function(d) {
          if (d$kind == "lines") length(unique(d$trends$series$name)) else d$spread$countries
        },
        years = function(d) {
          if (d$kind == "lines") d$trends$series$year else d$spread$spread$year
        },
        latest = FALSE,
        per_country = FALSE,
        note = function(d) {
          if (d$kind == "lines") {
            "Each grey line is a country; blue is the regional median."
          } else {
            "The line is the median country; the band covers the middle half of countries."
          }
        }
      )
    )

    # Regions view: the World Bank's regional aggregates, with the world
    # highlighted. A histogram of seven values says little, so fertility is
    # ranked instead; the urban trend shows a line per region.
    specs$life$regions <- list(
      data = function(rs, h) ranked_latest(rs, h),
      chart = function(d) lollipop_chart(d, "Years", digits = 1, highlight = world_aggregate_name),
      rows = function(d) regions_shown(d$name),
      years = function(d) d$year,
      latest = TRUE,
      world = TRUE
    )
    specs$internet$regions <- list(
      data = function(rs, h) change_since(rs, h),
      chart = function(d) dumbbell_chart(d, "% of population", digits = 0),
      rows = function(d) regions_shown(d$name),
      years = function(d) c(d$start_year, d$end_year),
      latest = FALSE,
      world = TRUE
    )
    specs$fertility$regions <- list(
      data = function(rs, h) ranked_latest(rs, h),
      chart = function(d) lollipop_chart(d, "Births per woman", digits = 1, highlight = world_aggregate_name),
      rows = function(d) regions_shown(d$name),
      years = function(d) d$year,
      latest = TRUE,
      world = TRUE
    )
    specs$urban$regions <- list(
      data = function(rs, h) region_trends(h),
      chart = function(d) trend_chart(d, "% of population", digits = 1, reference = world_aggregate_name, colors = series_colors),
      rows = function(d) regions_shown(d$series$name),
      years = function(d) c(d$series$year, d$median$year),
      latest = FALSE,
      world = TRUE
    )

    view <- indicator_view(input, region)
    indicator_charts_server(social_charts, specs, countries, output, session, view)
  })
}
