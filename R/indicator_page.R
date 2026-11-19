# Shared by the Social and Economic indicators pages: each is a grid of
# tiles with one World Bank chart per tile, for the countries in the
# selected region.
#
# `charts` describes each tile: a named list (by key) of lists with
# `indicator` (a name of wb_indicators), `title`, `question` and `icon`.
#
# `specs` says how to draw each chart, by the same keys: lists with
#   data(countries, history)  the chart's data
#   chart(data)               the echarts widget
#   rows(data)                how many countries it shows
#   years(data)               the years its values are from
#   latest, per_country       for the caption (see indicator_caption())
#   note(data)                optional: a sentence for the caption on how
#                             to read the chart
#   regions                   optional: the same for the Regions view, with
#                             `countries` being region_aggregates() and
#                             `rows` counting regions (see regions_shown())

indicator_chart_tile <- function(ns, charts, key, height = NULL) {
  chart <- charts[[key]]
  carbonite::carbon_tile(
    class = "indicator-chart",
    tile_title(chart$title, chart$icon, level = "h3"),
    htmltools::p(class = "indicator-chart__question cds--type-body-01", chart$question),
    shiny::uiOutput(ns(paste0(key, "_status"))),
    echarts4r::echarts4rOutput(ns(paste0(key, "_chart")), height = if_null(height, "360px")),
    shiny::uiOutput(ns(paste0(key, "_caption")))
  )
}

# An indicator page: its heading, then the chart tiles (carbon_columns of
# indicator_chart_tile()s) in a grid with the same gap between rows as
# between columns (DESIGN.md)
indicator_page <- function(heading, ...) {
  htmltools::tagList(
    heading,
    carbonite::carbon_grid(row_gap = 7, ...)
  )
}

# Page title and lead (`...`, the lead's text), then (only when the header's
# region is World) a switcher between comparing countries and comparing the
# World Bank's regions. `ns`: the module's namespace function.
indicator_page_heading <- function(ns, title, ...) {
  carbonite::carbon_grid(
    carbonite::carbon_column(
      width = 16,
      page_heading(
        title,
        htmltools::tagList(...),
        shiny::conditionalPanel(
          # `input.region` is the header select, outside the module
          condition = sprintf("input.region === '%s'", world_region),
          class = "view-switcher",
          carbonite::carbon_content_switcher(
            ns("view"),
            carbonite::carbon_switch("countries", "Countries"),
            carbonite::carbon_switch("regions", "Regions"),
            size = "sm"
          )
        )
      )
    )
  )
}

# The view the charts show: "regions" only while the region is World and
# the switcher is on Regions. Picking a single region resets the switcher,
# so World starts with countries again. `region`: reactive header region,
# or NULL (always countries). Call from inside a module server.
indicator_view <- function(input, region) {
  if (is.null(region)) {
    return(shiny::reactive("countries"))
  }
  shiny::observeEvent(region(), ignoreInit = TRUE, {
    if (!identical(region(), world_region) && identical(input$view, "regions")) {
      carbonite::update_carbon_content_switcher("view", "countries")
    }
  })
  shiny::reactive({
    if (identical(region(), world_region) && identical(input$view, "regions")) "regions" else "countries"
  })
}

# Outputs <key>_status, <key>_chart and <key>_caption for every chart. Call
# from inside a module server. `countries`: reactive data frame from
# fetch_data() (name, iso3, population). `view`: reactive "countries" or
# "regions" (from indicator_view()).
indicator_charts_server <- function(charts, specs, countries, output, session,
                                    view = shiny::reactive("countries")) {
  # One history per indicator, or an api_error. Cached for a day, shared
  # with other sessions, and independent of the region.
  history <- lapply(charts, function(chart) {
    shiny::reactive(tryCatch(fetch_wb_history(chart$indicator), api_error = identity))
  })

  for (key in names(charts)) {
    local({
      key <- key

      by_region <- shiny::reactive(view() == "regions" && !is.null(specs[[key]]$regions))
      spec_now <- function() if (by_region()) specs[[key]]$regions else specs[[key]]

      # The chart's data, or an api_error
      chart_data <- shiny::reactive({
        shiny::req(countries())
        h <- history[[key]]()
        if (inherits(h, "api_error")) return(h)
        if (by_region()) spec_now()$data(region_aggregates(), h) else spec_now()$data(countries(), h)
      })

      has_data <- function(d) !inherits(d, "api_error") && spec_now()$rows(d) > 0

      output[[paste0(key, "_status")]] <- shiny::renderUI({
        d <- chart_data()
        message <- if (inherits(d, "api_error")) {
          conditionMessage(d)
        } else if (!has_data(d) && by_region()) {
          "The World Bank has no regional figures for this indicator."
        } else if (!has_data(d)) {
          "None of the countries in this region have data for this indicator."
        }
        if (!is.null(message)) {
          htmltools::tagList(
            empty_state_tile("No chart", message, icon = "chart--line"),
            htmltools::tags$style(sprintf("#%s { display: none; }", session$ns(paste0(key, "_chart"))))
          )
        }
      })

      output[[paste0(key, "_chart")]] <- echarts4r::renderEcharts4r({
        d <- chart_data()
        shiny::req(has_data(d))
        spec_now()$chart(d)
      })

      output[[paste0(key, "_caption")]] <- shiny::renderUI({
        d <- chart_data()
        shiny::req(has_data(d))
        spec <- spec_now()
        if (by_region()) {
          region_caption(spec$rows(d), spec$years(d), latest = spec$latest, world = spec$world)
        } else {
          indicator_caption(
            spec$rows(d), countries(), spec$years(d),
            latest = spec$latest, per_country = spec$per_country,
            note = if (!is.null(spec$note)) spec$note(d)
          )
        }
      })
    })
  }
}

# e.g. "48 of 52 countries and territories have data. Each country's most
# recent value (2021–2024). Source: World Bank." Per-country charts of the
# whole world show only the most populous countries, and say so. `note`:
# an optional sentence about how to read the chart, before the source.
indicator_caption <- function(shown, countries, years, latest = FALSE, per_country = FALSE, note = NULL) {
  total <- nrow(countries)
  capped <- per_country && sum(!is.na(countries$iso3)) > social_max_countries
  coverage <- if (capped) {
    sprintf(
      "The %d most populous of %d countries and territories; %d have data.",
      social_max_countries, total, shown
    )
  } else {
    sprintf("%d of %d countries and territories have data.", shown, total)
  }
  when <- if (latest && length(years)) {
    sprintf(" Each country's most recent value (%s).", year_span(years))
  } else if (latest) {
    " Each country's most recent value."
  } else {
    sprintf(" %s.", year_span(years))
  }
  source_caption(paste0(coverage, when, if (!is.null(note)) paste0(" ", note)))
}

# Caption for the Regions view, e.g. "World Bank estimates for its 7
# regions and the world. Each region's most recent value (2024)."
# `shown`: regions in the chart, not counting the world; `world`: whether
# the world is in the chart too.
region_caption <- function(shown, years, latest = FALSE, world = TRUE) {
  total <- length(wb_regions)
  regions <- if (shown == total) sprintf("its %d regions", total) else sprintf("%d of its %d regions", shown, total)
  when <- if (latest && length(years)) {
    sprintf(" Each region's most recent value (%s).", year_span(years))
  } else if (latest) {
    " Each region's most recent value."
  } else {
    sprintf(" %s.", year_span(years))
  }
  source_caption(paste0(
    "World Bank estimates for ", regions, if (isTRUE(world)) " and the world", ".", when
  ))
}

# A chart caption: `text`, then the source
source_caption <- function(text) {
  htmltools::p(
    class = "chart-caption cds--type-helper-text-01",
    paste0(text, " Source: "),
    htmltools::a(
      "World Bank",
      href = "https://data.worldbank.org",
      target = "_blank",
      rel = "noopener noreferrer",
      .noWS = "after"
    ),
    "."
  )
}
