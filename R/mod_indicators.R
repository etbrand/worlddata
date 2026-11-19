# Comparisons page: World Bank indicators for the countries in the
# selected region (or, for the whole world, the World Bank's regions), as a
# bubble chart of one indicator against another

mod_indicators_ui <- function(id) {
  ns <- shiny::NS(id)
  choices <- wb_indicator_choices()

  htmltools::tagList(
    # Title and lead, then the Countries/Regions switcher for World
    indicator_page_heading(
      ns,
      "Comparisons",
      "Pick two World Bank indicators to see how they relate across the ",
      "countries in the selected region. Each bubble is a country, or for ",
      "the whole world, one of the World Bank's regions, sized by its ",
      "population; hover over a bubble for its values."
    ),
    carbonite::carbon_grid(
      carbonite::carbon_column(
        width = 16,
        carbonite::carbon_tile(
          htmltools::div(
            class = "indicator-controls",
            carbonite::carbon_select(
              ns("x"),
              label = "Horizontal axis",
              choices = choices,
              selected = "gdp_per_capita"
            ),
            carbonite::carbon_select(
              ns("y"),
              label = "Vertical axis",
              choices = choices,
              selected = "life_expectancy"
            )
          ),
          shiny::uiOutput(ns("status")),
          # The same-indicator warning goes over the chart, so the tile
          # keeps its size (see .indicator-chart-area in www/styles.css)
          htmltools::div(
            class = "indicator-chart-area",
            echarts4r::echarts4rOutput(ns("chart"), height = "560px"),
            shiny::uiOutput(ns("axes_notice"))
          ),
          shiny::uiOutput(ns("caption"))
        )
      )
    )
  )
}

# `countries`: reactive data frame from fetch_data() (name, iso3, population)
# `region`: reactive header region, for the Countries/Regions switcher
mod_indicators_server <- function(id, countries, region = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    view <- indicator_view(input, region)
    by_region <- shiny::reactive(view() == "regions")

    # Each indicator is fetched once for all countries and cached, so
    # switching region doesn't refetch it. The responses include the World
    # Bank's regional aggregates, which the Regions view plots.
    indicator <- function(key) {
      shiny::reactive({
        shiny::req(key())
        tryCatch(fetch_wb_indicator(key()), api_error = identity)
      })
    }
    x_data <- indicator(shiny::reactive(input$x))
    y_data <- indicator(shiny::reactive(input$y))

    # What the bubbles are: the region's countries, or the World Bank's seven
    # regions (without the world, whose bubble would dwarf them)
    bubbles <- shiny::reactive({
      if (by_region()) region_bubbles() else countries()
    })

    # Joined data, or an api_error
    scatter_data <- shiny::reactive({
      shiny::req(bubbles())
      for (d in list(x_data(), y_data())) {
        if (inherits(d, "api_error")) return(d)
      }
      indicator_scatter_data(bubbles(), x_data(), y_data())
    })

    # Whether there's anything to plot. A reactiveVal only notifies when its
    # value changes, so the chart below redraws when this flips, not every
    # time the data changes.
    # The same indicator on both axes: nothing to compare, so the chart is
    # hidden and a notification asks for two different ones
    same_indicator <- shiny::reactive({
      shiny::req(input$x, input$y)
      identical(input$x, input$y)
    })

    can_draw <- shiny::reactiveVal(FALSE)
    shiny::observe({
      if (same_indicator()) {
        can_draw(FALSE)
        return()
      }
      d <- scatter_data()
      can_draw(is.data.frame(d) && any(has_both(d)))
    })

    # Over the chart, which (with its caption) is made invisible but keeps
    # its space, so the tile doesn't change size
    output$axes_notice <- shiny::renderUI({
      shiny::req(same_indicator())
      htmltools::tagList(
        same_indicator_notice(input$x),
        htmltools::tags$style(sprintf(
          "#%s, #%s { visibility: hidden; }",
          session$ns("chart"), session$ns("caption")
        ))
      )
    })

    output$status <- shiny::renderUI({
      shiny::req(!same_indicator())
      d <- scatter_data()
      message <- if (inherits(d, "api_error")) {
        conditionMessage(d)
      } else if (!any(has_both(d)) && by_region()) {
        "The World Bank has no regional figures for both indicators."
      } else if (!any(has_both(d))) {
        "None of the countries in this region have data for both indicators."
      }
      if (!is.null(message)) {
        htmltools::tagList(
          empty_state_tile("No chart", message, icon = "chart--bubble"),
          htmltools::tags$style(sprintf(
            "#%s { display: none; }",
            session$ns("chart")
          ))
        )
      }
    })

    # Whether each axis is on a log scale. A reactiveVal only notifies when
    # its value changes, so the chart below redraws only when an axis
    # switches between log and linear: ECharts rebuilds an axis whose type
    # changes, and a proxy update can't resend its styling (the label
    # formatter is JavaScript), so the axis would lose it.
    axis_scales <- shiny::reactiveVal(NULL)
    scales_for <- function(x, y) c(isTRUE(wb_indicators[[x]]$log), isTRUE(wb_indicators[[y]]$log))
    shiny::observeEvent(list(input$x, input$y), once = TRUE, {
      shiny::req(input$x, input$y)
      axis_scales(scales_for(input$x, input$y))
    })

    # Full redraw for a new region or view (different bubbles), a change of
    # axis scale, or when there's something to plot again after an error
    output$chart <- echarts4r::renderEcharts4r({
      bubbles()
      axis_scales()
      shiny::req(can_draw())
      shiny::isolate(indicator_scatter_chart(
        scatter_data(),
        input$x,
        input$y,
        chart_id = session$ns("chart"),
        # Name the seven regions' bubbles; countries are too many to label
        labels = by_region()
      ))
    })

    # New indicators: merge the new points and axes into the existing chart,
    # so the bubbles move rather than the chart being redrawn. If an axis
    # changes scale, the chart redraws instead (see axis_scales above).
    shiny::observeEvent(list(input$x, input$y), ignoreInit = TRUE, {
      shiny::req(input$x, input$y)
      scales <- scales_for(input$x, input$y)
      if (!identical(scales, axis_scales())) {
        axis_scales(scales)
        return()
      }
      if (same_indicator()) {
        return()
      }
      d <- scatter_data()
      if (!is.data.frame(d) || !any(has_both(d))) {
        return()
      }

      # The proxy needs the namespaced id: it finds the chart by DOM id
      proxy <- echarts4r::echarts4rProxy(session$ns("chart"))
      proxy$chart <- list(
        x = list(opts = indicator_scatter_update(d, input$x, input$y))
      )
      echarts4r::e_merge(proxy)
    })

    # Still rendered for the same indicator on both axes (invisible, see
    # axes_notice), so its space is kept
    output$caption <- shiny::renderUI({
      d <- scatter_data()
      shiny::req(is.data.frame(d))
      d <- d[has_both(d), , drop = FALSE]
      shiny::req(nrow(d) > 0)
      years <- year_span(c(d$x_year, d$y_year))

      source_caption(if (by_region()) {
        total <- length(wb_regions)
        sprintf(
          paste(
            "World Bank estimates for %s with figures for both indicators.",
            "Bubble size shows population. Each region's most recent value is used (%s)."
          ),
          if (nrow(d) == total) sprintf("all %d of its regions", total) else sprintf("%d of its %d regions", nrow(d), total),
          years
        )
      } else {
        sprintf(
          paste(
            "%d of %d countries and territories have data for both indicators.",
            "Bubble size shows population. Each country's most recent value is used (%s)."
          ),
          nrow(d), nrow(countries()), years
        )
      })
    })
  })
}

# Warning shown in place of the chart when both axes have indicator `key`
same_indicator_notice <- function(key) {
  htmltools::div(
    class = "indicator-notice",
    carbonite::carbon_inline_notification(
      title = "Choose two different indicators",
      subtitle = sprintf(
        "Both axes show %s. Pick a different indicator for one of them to compare the two.",
        wb_indicators[[key]]$label
      ),
      kind = "warning",
      close_button = FALSE
    )
  )
}

# The World Bank's seven regions as bubbles (name, iso3, population), sized
# by the World Bank's population figure for each region. Without population
# (if that request fails), the bubbles are all the smallest size.
region_bubbles <- function() {
  regions <- region_aggregates(world = FALSE)
  population <- tryCatch(fetch_wb_indicator("population"), api_error = function(e) NULL)
  if (!is.null(population)) {
    regions$population <- population$value[match(regions$iso3, population$iso3)]
  }
  regions
}
