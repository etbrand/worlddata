column_labels <- c(
  name = "Name",
  capital = "Capital",
  population = "Population",
  area = "Area (km²)",
  languages = "Languages",
  gini = "Gini index",
  flag = "Flag"
)

missing_label <- "—"

format_number <- function(x) {
  out <- formatC(round(x), format = "d", big.mark = ",")
  out[is.na(x)] <- missing_label
  out
}

# Default for columns without a specific formatter: show values as-is, with
# missing values as an em dash instead of "NA"
format_missing <- function(x) {
  out <- as.character(x)
  out[is.na(x)] <- missing_label
  out
}

# Flag image URLs as small inline images. The images load as their rows
# come into view, so only the current page's flags are downloaded.
format_flag <- function(x) {
  lapply(x, function(url) {
    if (is.na(url)) {
      return(missing_label)
    }
    htmltools::img(
      class = "table-flag",
      src = url,
      alt = "Flag",
      height = 16,
      loading = "lazy"
    )
  })
}

column_formatters <- list(
  population = format_number,
  area = format_number,
  flag = format_flag
)

# Columns that can't be sorted (an image has nothing to sort by)
unsortable_columns <- "flag"

# `region`: world_region or a code of wb_regions
table_title <- function(region) {
  if (identical(region, world_region)) {
    return("Countries of the world")
  }
  paste("Countries in", wb_regions[[region]])
}

table_columns <- function(col_names) {
  lapply(stats::setNames(nm = col_names), \(col) {
    formatter <- column_formatters[[col]]
    if (is.null(formatter)) formatter <- format_missing

    carbonite::carbon_table_column(
      label = unname(column_labels[col]),
      sortable = if (col %in% unsortable_columns) FALSE,
      formatter = formatter,
      # An icon button after each country's name opens its profile
      # (input$table_cell_click)
      action_icon = if (col == "name") "launch",
      action_label = "Open profile"
    )
  })
}

# The table's columns: the country name always comes first, then the
# optional columns chosen in "Table columns" (which may be none)
table_column_names <- function(selected) {
  c("name", setdiff(as.character(selected), "name"))
}

server <- function(input, output, session) {
  fetched_data <- reactive({
    req(input$region)
    print2("FETCHED DATA")
    fetch_data(region = input$region)
  })

  table_data <- reactive({
    req(fetched_data())
    # input$columns is NULL when no optional columns are chosen
    cols <- table_column_names(input$columns)
    fetched_data()[, intersect(cols, names(fetched_data())), drop = FALSE]
  })

  output$table <- carbonite::render_carbon_table({
    data <- table_data()
    countries <- fetched_data()

    carbonite::carbon_table(
      data,
      columns = table_columns(names(data)),
      # Each country's Wikipedia summary, fetched only when its row is first
      # expanded. `i` is the row number: `data` has the same rows as
      # `countries` but not the Wikipedia URL.
      row_details = function(row, i) country_details(countries[i, , drop = FALSE]),
      lazy_details = TRUE,
      title = table_title(input$region),
      description = sprintf(
        "%d countries and territories · Open a country's profile with the button next to its name · Sources: World Bank, Wikidata",
        nrow(data)
      ),
      size = "sm",
      sortable = TRUE,
      toolbar = carbonite::carbon_table_toolbar(
        search = TRUE,
        search_placeholder = "Search countries"
      ),
      # The pagination bar includes the rows-per-page select. Keep the
      # user's choice when the table re-renders (columns, region).
      page_size = if_null(isolate(input$table_page_size), 10),
      page_sizes = c(10, 25, 50, 100)
    )
  })

  # A country's "Open profile" button was clicked: show its profile. `row` is the row number
  # in the table's data, which has the same rows as fetched_data().
  observeEvent(input$table_cell_click, {
    click <- input$table_cell_click
    countries <- fetched_data()
    i <- as.integer(click$row)
    req(identical(click$column, "name"), length(i) == 1, !is.na(i), i >= 1, i <= nrow(countries))

    carbonite::show_carbon_modal(country_profile_modal(countries[i, , drop = FALSE]))
  })

  output$language_cloud <- echarts4r::renderEcharts4r({
    req(fetched_data())
    language_cloud(fetched_data())
  })

  region <- reactive(input$region)
  mod_glance_server("glance", countries = fetched_data, region = region)
  mod_indicators_server("indicators", countries = fetched_data, region = region)
  mod_social_server("social", countries = fetched_data, region = region)
  mod_economic_server("economic", countries = fetched_data, region = region)
}
