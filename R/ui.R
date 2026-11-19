ui <- function() {
  shiny::addResourcePath("www", system.file("www", package = "worlddata"))
  carbonite::carbon_page(
    # The mark before the name (carbon_page() puts the title inside the
    # header's name link as-is)
    title = htmltools::tagList(app_logo(), "World Data Dashboard"),
    # The region applies to every tab of the Countries page, so it lives in
    # the header, with "World" set apart from the regions
    header_content = carbonite::carbon_select(
      inputId = "region",
      choices = region_choices,
      selected = world_region,
      size = "sm",
      `aria-label` = "Region",
      class = "header-region"
    ),
    sidebar = carbonite::carbon_sidebar(
      # The route stays "countries" so existing links keep working
      carbonite::carbon_nav_item(
        "at-a-glance",
        "At a glance",
        icon = "earth"
      ),
      carbonite::carbon_nav_item(
        "social",
        "Social indicators",
        icon = "events"
      ),
      carbonite::carbon_nav_item(
        "economic",
        "Economic indicators",
        icon = "finance"
      ),
      carbonite::carbon_nav_item(
        "compare",
        "Comparisons",
        icon = "chart--bubble"
      ),
      carbonite::carbon_nav_item(
        "about",
        "About",
        icon = "information"
      )
    ),
    carbonite::carbon_panel(
      "at-a-glance",
      # carbon_page() only accepts panels; Shiny moves this into <head>.
      # Assets go through the "www/" resource path registered above: Shiny
      # only serves the www folder at the site root when it's started on the
      # app's folder (shiny::runApp()), not from an app object or an IDE.
      htmltools::tags$head(
        # Bump `v` when styles.css changes, or browsers may keep using an
        # old cached copy
        htmltools::tags$link(rel = "stylesheet", href = "www/styles.css?v=15"),
        # Bump `v` when the icon changes; browsers cache favicons aggressively
        htmltools::tags$link(
          rel = "shortcut icon",
          href = "www/favicon.svg?v=6"
        ),
        htmltools::tags$link(
          rel = "icon",
          type = "image/svg+xml",
          href = "www/favicon.svg?v=6"
        ),
        htmltools::tags$link(
          rel = "apple-touch-icon",
          href = "www/apple-touch-icon.png?v=6"
        )
      ),
      carbonite::carbon_grid(
        carbonite::carbon_column(
          width = 16,
          page_heading(
            "At a glance",
            paste(
              "Headline figures for the selected region, a table of every",
              "country and territory, where you can open each one's profile,",
              "and the official languages they share."
            )
          )
        )
      ),
      # Its own grid: a grid inside a grid column would be indented
      mod_glance_ui("glance"),
      carbonite::carbon_grid(
        carbonite::carbon_column(
          width = 16,
          carbonite::carbon_tabs(
            "countries_tabs",
            carbonite::carbon_tab(
              "country_list",
              tab_label("Country list", "table"),
              # tab-content: space below the tabs (DESIGN.md)
              htmltools::div(
                class = "tab-content country-list-columns",
                # The country name is always the first column, so it isn't
                # an option here (see table_column_names())
                carbonite::carbon_multiselect(
                  inputId = "columns",
                  label = "Table columns",
                  choices = c(
                    "Capital" = "capital",
                    "Population" = "population",
                    "Area" = "area",
                    "Languages" = "languages",
                    "Gini index" = "gini",
                    # Last, so the flag is the right-most column
                    "Flag" = "flag"
                  ),
                  selected = c("capital", "population", "gini", "flag")
                )
              ),
              carbonite::carbon_table_output(
                "table",
                initial_columns = c(
                  "Name",
                  "Capital",
                  "Population",
                  "Gini index",
                  "Flag"
                ),
                size = "sm"
              )
            ),
            carbonite::carbon_tab(
              "languages",
              tab_label("Languages", "language"),
              htmltools::div(
                class = "tab-content",
                echarts4r::echarts4rOutput("language_cloud", height = "600px")
              )
            )
          )
        )
      )
    ),
    carbonite::carbon_panel(
      "social",
      mod_social_ui("social")
    ),
    carbonite::carbon_panel(
      "economic",
      mod_economic_ui("economic")
    ),
    carbonite::carbon_panel(
      "compare",
      mod_indicators_ui("indicators")
    ),
    about_panel()
  )
}
