carbonite_url <- "https://github.com/etbrand/carbonite"
worldbank_data_url <- "https://data.worldbank.org"

# Clickable tile linking to an external site
about_link_tile <- function(title, icon, description, href) {
  carbonite::carbon_tile(
    tile_title(title, icon, level = "h3"),
    htmltools::p(description),
    htmltools::p(
      class = "about-tile-link with-icon",
      htmltools::span(sub("^https://", "", href)),
      carbonite::carbon_icon("launch", title = "Opens in a new tab")
    ),
    href = href,
    target = "_blank",
    rel = "noopener noreferrer",
    class = "about-tile"
  )
}

external_link <- function(text, href) {
  htmltools::a(
    text,
    href = href,
    target = "_blank",
    rel = "noopener noreferrer"
  )
}

# The APIs the app calls, for the structured list on the About page
data_sources <- function() {
  data.frame(
    source = I(list(
      external_link("World Bank Indicators API", worldbank_data_url),
      external_link("Wikidata", "https://www.wikidata.org"),
      external_link("Wikipedia", "https://en.wikipedia.org")
    )),
    provides = c(
      paste(
        "The list of countries and their regions, capitals, population, area and",
        "Gini index, and every indicator on the Social indicators, Economic indicators",
        "and Comparisons pages"
      ),
      "Each country's official languages, flag and Wikipedia article",
      "Each country's summary, shown in its profile and when you expand a row of the Summary table"
    ),
    updates = c(
      "Yearly; the app keeps a copy for up to a day",
      "Edited continuously; the app keeps a copy for up to a day",
      "Live articles; the app keeps a copy for up to an hour"
    )
  )
}

# Static tile describing one aspect of the data
about_info_tile <- function(title, icon, ...) {
  carbonite::carbon_tile(
    tile_title(title, icon, level = "h3"),
    ...,
    class = "about-tile"
  )
}

about_panel <- function() {
  half <- c(sm = 4, md = 8, lg = 8)
  third <- c(sm = 4, md = 8, lg = 5)

  # Each part has its own grid, so section spacing and the gap between tile
  # rows don't add up (DESIGN.md)
  carbonite::carbon_panel(
    "about",
    # Intro -------------------------------------------------------------------
    carbonite::carbon_grid(
      carbonite::carbon_column(
        width = 16,
        page_heading(
          "World Data Dashboard",
          paste(
            "World Data Dashboard is a demo app that shows what the carbonite R",
            "package can do. carbonite brings IBM's Carbon Design System to",
            "Shiny, and every input, tab, table, chart container, tile and",
            "accordion in this app is a carbonite component. The data comes",
            "from the World Bank, Wikidata and Wikipedia."
          ),
          tags = carbonite::carbon_tag("Built with carbonite", type = "blue")
        )
      )
    ),
    carbonite::carbon_grid(
      row_gap = 7,
      carbonite::carbon_column(
        width = half,
        about_link_tile(
          "carbonite on GitHub",
          "logo--github",
          paste(
            "Source code, documentation and issue tracker for carbonite,",
            "the Carbon Design System bindings for Shiny that power this app."
          ),
          carbonite_url
        )
      ),
      carbonite::carbon_column(
        width = half,
        about_link_tile(
          "World Bank Open Data",
          "api",
          paste(
            "Free and open data about the world's economies, which supplies",
            "the countries, regions and every chart you see here."
          ),
          worldbank_data_url
        )
      )
    ),

    # Using the app -----------------------------------------------------------
    page_section(
      carbonite::carbon_column(
        width = 16,
        section_heading("Using the app"),
        carbonite::carbon_accordion(
          carbonite::carbon_accordion_item(
            "Region",
            open = TRUE,
            htmltools::p(
              "The region menu at the top right chooses which countries the ",
              "At a glance, Social indicators, Economic indicators and Comparisons pages show: ",
              htmltools::strong("World"),
              " for every country, or one of the World Bank's seven regions."
            ),
            htmltools::p(
              "The menu shortens one region's name: ",
              htmltools::strong("Middle East & North Africa"),
              " is the World Bank's Middle East, North Africa, Afghanistan & ",
              "Pakistan region."
            )
          ),
          carbonite::carbon_accordion_item(
            "At a glance",
            htmltools::p(
              "Four headline figures for the selected region: how many countries ",
              "and territories it has, its population, the size of its economy and ",
              "its life expectancy, each compared with the world. Below them are ",
              "the Country list table and the Languages word cloud."
            ),
            htmltools::p(
              "A region's GDP and life expectancy are the World Bank's figures for ",
              "the region as a whole, which include estimates for countries with ",
              "no figure of their own. Each callout shows the year its data is from."
            )
          ),
          carbonite::carbon_accordion_item(
            "Country list",
            htmltools::p(
              "A table with one row per country. Select the button next to a ",
              "country's name to open its profile, or expand a row for its Wikipedia summary. Use ",
              htmltools::strong("Table columns"),
              " to pick which other details to show. Columns appear in the same ",
              "order as in that list. Click a column header to sort by it."
            )
          ),
          carbonite::carbon_accordion_item(
            "Languages",
            htmltools::p(
              "A word cloud of the region's official languages. The bigger ",
              "the word, the more countries have that official language. Hover ",
              "over a word to see how many."
            ),
            htmltools::p(
              "Many countries have more than one official language, and every ",
              "one counts equally. Languages that are widely spoken but not ",
              "official aren't included."
            )
          ),
          carbonite::carbon_accordion_item(
            "Social indicators",
            htmltools::p(
              "Four World Bank indicators for the countries in the selected region, ",
              "each shown the way that best answers its question:"
            ),
            htmltools::tags$ul(
              class = "about-list",
              htmltools::tags$li(
                htmltools::strong("Life expectancy"),
                ": countries ranked by their latest value. For a region with more than 20, the highest and lowest 10, with a dashed line at the median of them all."
              ),
              htmltools::tags$li(
                htmltools::strong("Internet use"),
                ": each country's share around 2000 and now, ranked by the latest. For a region with more than 20, the highest and lowest 10, with a dashed line at the median latest value of them all."
              ),
              htmltools::tags$li(
                htmltools::strong("Fertility rate"),
                ": how many countries fall in each range."
              ),
              htmltools::tags$li(
                htmltools::strong("Urban population"),
                ": the trend since 2000. For a region with up to 10 countries, a line per ",
                "country (hover to see which), with the median highlighted; for more, the median with a band covering the middle half of countries."
              )
            )
          ),
          carbonite::carbon_accordion_item(
            "Countries or regions",
            htmltools::p(
              "When the region is ",
              htmltools::strong("World"),
              ", the Social indicators, Economic indicators and Comparisons pages have a switch between ",
              htmltools::strong("Countries"),
              " and ",
              htmltools::strong("Regions"),
              ". Regions compares the World Bank's seven regions, using the ",
              "World Bank's own figures for each region and for the world as a ",
              "whole, which is highlighted. These figures are weighted (for ",
              "example, by population) and can include estimates for countries ",
              "with no figure of their own."
            ),
            htmltools::p(
              "A few charts change to suit seven regions: fertility is ranked ",
              "rather than shown as a histogram, and urban population and ",
              "inflation show a line for each region."
            )
          ),
          carbonite::carbon_accordion_item(
            "Economic indicators",
            htmltools::p(
              "Four more World Bank indicators, about the region's economies:"
            ),
            htmltools::tags$ul(
              class = "about-list",
              htmltools::tags$li(
                htmltools::strong("Size of the economy"),
                ": a rectangle per country, sized by its GDP in US dollars, ",
                "labeled with its share of the region's total."
              ),
              htmltools::tags$li(
                htmltools::strong("Inflation"),
                ": the median yearly rise in consumer prices since 2000, with a ",
                "band covering the middle half of countries. Hover over a year to ",
                "see the numbers."
              ),
              htmltools::tags$li(
                htmltools::strong("Economic growth"),
                ": real GDP growth for each country and year since 2005, red when ",
                "the economy shrank and blue when it grew. Countries are ranked by ",
                "their average yearly growth, compounded over the period; for a ",
                "region with more than 20, the highest and lowest 10."
              ),
              htmltools::tags$li(
                htmltools::strong("Unemployment"),
                ": countries ranked by their latest unemployment rate (International ",
                "Labour Organization estimates). For a region with more than 20, the ",
                "highest and lowest 10, with a dashed line at the median of them all."
              )
            )
          ),
          carbonite::carbon_accordion_item(
            "Comparisons",
            htmltools::p(
              "A bubble chart of any two World Bank indicators, one on each axis, ",
              "with a bubble per country sized by its population. Choose the ",
              "indicators with ",
              htmltools::strong("Horizontal axis"),
              " and ",
              htmltools::strong("Vertical axis"),
              "; the bubbles move to their new places when you do. Each country ",
              "shows its most recent value of each indicator."
            )
          )
        )
      )
    ),

    # About the data ----------------------------------------------------------
    page_section(
      carbonite::carbon_column(
        width = 16,
        section_heading("About the data"),
        htmltools::p(
          class = "prose",
          "The list of countries and nearly all the numbers come from the ",
          htmltools::a(
            "World Bank",
            href = worldbank_data_url,
            target = "_blank",
            rel = "noopener noreferrer",
            .noWS = "after"
          ),
          ", so the table and every chart cover the same countries. Wikidata ",
          "adds the details the World Bank doesn't have. None of these ",
          "services needs an API key, and the app keeps a copy of the country ",
          "list for a day, so changing the region is instant."
        ),
        htmltools::h3(
          "Data sources",
          class = "subsection-heading cds--type-heading-03"
        ),
        carbonite::carbon_structured_list(
          data_sources(),
          columns = c(
            source = "Source",
            provides = "What the app uses it for",
            updates = "How current"
          )
        )
      )
    ),
    # A group within the section: a row of tiles below the list
    carbonite::carbon_grid(
      row_gap = 7,
      class = "page-group",
      carbonite::carbon_column(
        width = third,
        about_info_tile(
          "What the app uses",
          "list--bulleted",
          htmltools::tags$ul(
            class = "about-list",
            htmltools::tags$li("Country name and region"),
            htmltools::tags$li("Capital city"),
            htmltools::tags$li("Population (most recent value)"),
            htmltools::tags$li("Area in km²"),
            htmltools::tags$li("Official languages"),
            htmltools::tags$li("Gini index (most recent value)")
          )
        )
      ),
      carbonite::carbon_column(
        width = third,
        about_info_tile(
          "Missing values",
          "help",
          htmltools::p(
            "Not every country has every detail. For example, some ",
            "territories have no capital or no Gini index."
          ),
          htmltools::p(
            "Missing values show as a dash (—) in the table and are left out of the charts."
          )
        )
      ),
      carbonite::carbon_column(
        width = c(sm = 4, md = 8, lg = 6),
        about_info_tile(
          "Countries and territories",
          "earth",
          htmltools::p(
            "The list is the World Bank's 217 economies, which include some ",
            "territories, such as Hong Kong and Puerto Rico, as well as ",
            "countries. Regions and status follow the World Bank's data, with ",
            "some formal names shortened (e.g. “Korea, Rep.” is ",
            "South Korea), and aren't a statement by this app."
          )
        )
      )
    )
  )
}
