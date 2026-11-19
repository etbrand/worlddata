# Economic indicators page: four World Bank indicators for the countries in
# the selected region, each with the chart type that fits its question (and
# none repeated from the Social indicators page).

economic_charts <- list(
  gdp = list(
    indicator = "gdp",
    title = "Size of the economy",
    question = "Which economies are biggest, and what share of the total do they make up?",
    icon = "chart--treemap"
  ),
  inflation = list(
    indicator = "inflation",
    title = "Inflation",
    question = "How fast have prices risen, and how much does that vary?",
    icon = "currency"
  ),
  growth = list(
    indicator = "gdp_growth",
    title = "Economic growth",
    question = "Which years were good or bad for growth?",
    icon = "growth"
  ),
  unemployment = list(
    indicator = "unemployment",
    title = "Unemployment",
    question = "Where is unemployment highest?",
    icon = "user--multiple"
  )
)

mod_economic_ui <- function(id) {
  ns <- shiny::NS(id)
  half <- c(sm = 4, md = 8, lg = 8)
  tile <- function(key, height = NULL) indicator_chart_tile(ns, economic_charts, key, height)

  indicator_page(
    indicator_page_heading(
      ns,
      "Economic indicators",
      "How big each economy is, how prices and output have changed over the ",
      "years, and how many people are out of work, for the countries in the ",
      "selected region. Data from the World Bank; hover over the charts for details."
    ),
    carbonite::carbon_column(width = half, tile("gdp", height = "400px")),
    carbonite::carbon_column(width = half, tile("inflation", height = "400px")),
    # Per-country charts: initial height only; they resize to fit their rows
    # (rows_chart_height())
    carbonite::carbon_column(width = half, tile("growth", height = "600px")),
    carbonite::carbon_column(width = half, tile("unemployment", height = "600px"))
  )
}

# `countries`: reactive data frame from fetch_data() (name, iso3, population)
# `region`: reactive header region, for the Countries/Regions switcher
mod_economic_server <- function(id, countries, region = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    # See indicator_charts_server() for what each spec holds
    specs <- list(
      gdp = list(
        data = function(cs, h) gdp_shares(cs, h),
        chart = function(d) gdp_treemap(d),
        rows = function(d) nrow(d),
        years = function(d) d$year,
        latest = TRUE,
        per_country = FALSE
      ),
      inflation = list(
        data = function(cs, h) yearly_spread(cs, h),
        chart = function(d) spread_band_chart(d$spread, "% per year", digits = 1),
        rows = function(d) d$countries,
        years = function(d) d$spread$year,
        latest = FALSE,
        per_country = FALSE
      ),
      growth = list(
        data = function(cs, h) yearly_grid(cs, h, from_year = 2005),
        chart = function(d) growth_heatmap(d, "Real GDP growth"),
        rows = function(d) length(unique(d$name)),
        years = function(d) d$year,
        latest = FALSE,
        per_country = TRUE
      ),
      unemployment = list(
        data = function(cs, h) ranked_latest(cs, h),
        chart = function(d) ranked_bar_chart(d, "% of labor force", digits = 1),
        rows = function(d) nrow(d),
        years = function(d) d$year,
        latest = TRUE,
        per_country = TRUE
      )
    )

    # Regions view: the World Bank's regional aggregates. The treemap splits
    # the world's GDP between the regions; a spread across seven regions
    # says little, so inflation shows a line per region and the world.
    specs$gdp$regions <- list(
      data = function(rs, h) gdp_shares(rs[rs$iso3 != world_aggregate, , drop = FALSE], h),
      chart = function(d) gdp_treemap(d, share_of = "the world"),
      rows = function(d) regions_shown(d$name),
      years = function(d) d$year,
      latest = TRUE,
      world = FALSE
    )
    specs$inflation$regions <- list(
      data = function(rs, h) region_trends(h),
      chart = function(d) trend_chart(d, "% per year", digits = 1, reference = world_aggregate_name, colors = series_colors),
      rows = function(d) regions_shown(d$series$name),
      years = function(d) c(d$series$year, d$median$year),
      latest = FALSE,
      world = TRUE
    )
    specs$growth$regions <- list(
      data = function(rs, h) yearly_grid(rs, h, from_year = 2005, max_countries = Inf),
      chart = function(d) growth_heatmap(d, "Real GDP growth"),
      rows = function(d) regions_shown(d$name),
      years = function(d) d$year,
      latest = FALSE,
      world = TRUE
    )
    specs$unemployment$regions <- list(
      data = function(rs, h) ranked_latest(rs, h),
      chart = function(d) ranked_bar_chart(d, "% of labor force", digits = 1, highlight = world_aggregate_name),
      rows = function(d) regions_shown(d$name),
      years = function(d) d$year,
      latest = TRUE,
      world = TRUE
    )

    view <- indicator_view(input, region)
    indicator_charts_server(economic_charts, specs, countries, output, session, view)
  })
}
