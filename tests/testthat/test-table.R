test_that("format_missing shows missing values as an em dash", {
  expect_equal(format_missing(c("Kabul", NA)), c("Kabul", "—"))
  expect_equal(format_missing(c(27.4, NA)), c("27.4", "—"))
})

test_that("format_number adds commas and shows missing values as an em dash", {
  expect_equal(format_number(c(1234567, NA)), c("1,234,567", "—"))
})

test_that("every table column has a formatter", {
  cols <- table_columns(c("name", "capital", "population", "languages", "gini"))

  expect_true(all(vapply(cols, \(col) is.function(col$formatter), logical(1))))
  expect_identical(cols$population$formatter, format_number)
  expect_identical(cols$gini$formatter, format_missing)
})

# Table rendering ---------------------------------------------------------------

fake_countries <- function(region) {
  data.frame(
    name = paste(region, 1:3),
    iso3 = NA_character_,
    capital = NA_character_,
    population = 1:3,
    area = 1:3,
    languages = NA_character_,
    gini = NA_real_
  )
}

test_that("the table has no row selection", {
  local_mocked_bindings(fetch_data = function(region) fake_countries(region))

  shiny::testServer(server, {
    session$setInputs(region = "EAS", columns = c("name", "population"))
    html <- output$table$html

    # carbonite marks a selectable table with `is-selectable` and gives each
    # row a `selection-name` for its checkbox
    expect_match(html, "<cds-table", fixed = TRUE)
    expect_no_match(html, "is-selectable", fixed = TRUE)
    expect_no_match(html, "selection-name", fixed = TRUE)
  })
})

test_that("the table keeps the user's rows per page when it re-renders", {
  local_mocked_bindings(fetch_data = function(region) fake_countries(region))

  shiny::testServer(server, {
    session$setInputs(region = "EAS", columns = c("name", "population"))
    expect_match(output$table$html, 'page-size="10"')

    session$setInputs(table_page_size = 25)
    session$setInputs(columns = c("name", "population", "gini"))
    expect_match(output$table$html, 'page-size="25"')
  })
})

test_that("the table title names the region in full", {
  expect_equal(table_title("SSF"), "Countries in Sub-Saharan Africa")
  expect_equal(table_title("MEA"), "Countries in Middle East, North Africa, Afghanistan & Pakistan")
  expect_equal(table_title(world_region), "Countries of the world")
})

# Flag column -------------------------------------------------------------------

test_that("flags show as small lazy-loaded images, with a dash when missing", {
  cells <- format_flag(c("https://flags.example/af.png", NA))

  html <- as.character(cells[[1]])
  expect_match(html, 'src="https://flags.example/af.png"', fixed = TRUE)
  expect_match(html, 'class="table-flag"', fixed = TRUE)
  expect_match(html, 'loading="lazy"', fixed = TRUE)
  expect_equal(cells[[2]], "—")
})

test_that("the flag column can't be sorted, and renders images in the table", {
  cols <- table_columns(c("name", "flag"))
  expect_false(cols$flag$sortable)
  expect_null(cols$name$sortable)

  data <- data.frame(name = c("Afghanistan", "Abkhazia"))
  data$flag <- c("https://flags.example/af.png", NA)
  html <- as.character(carbonite::carbon_table(data, columns = cols, sortable = TRUE))

  expect_match(html, '<img class="table-flag" src="https://flags.example/af.png"', fixed = TRUE)
  # The flag column is last, i.e. right-most
  headers <- regmatches(html, gregexpr(">[A-Za-z]+</cds-table-header-cell>", html))[[1]]
  expect_equal(headers, c(">Name</cds-table-header-cell>", ">Flag</cds-table-header-cell>"))
})

# Country name and profiles -----------------------------------------------------

test_that("the country name is always the first column", {
  expect_equal(table_column_names(c("capital", "area", "flag")), c("name", "capital", "area", "flag"))
  expect_equal(table_column_names(NULL), "name")
  # A stray "name" isn't duplicated
  expect_equal(table_column_names(c("name", "gini")), c("name", "gini"))
})

test_that("only the name column has an Open profile button, after the plain name", {
  cols <- table_columns(c("name", "capital"))
  expect_true(cols$name$action)
  expect_equal(cols$name$action_icon, "launch")
  expect_false(cols$capital$action)

  data <- data.frame(name = "Chad", capital = "N'Djamena")
  html <- as.character(carbonite::carbon_table(data, columns = cols))
  expect_match(html, "<span>Chad</span>", fixed = TRUE)
  expect_match(html, 'aria-label="Open profile: Chad"', fixed = TRUE)
})

profile_country <- data.frame(
  name = "Chad", region = "Sub-Saharan Africa", iso3 = "TCD",
  wikipedia = "https://en.wikipedia.org/wiki/Chad", flag = "https://flags.example/td.png",
  capital = "N'Djamena", population = 18000000, area = 1284000,
  languages = "Arabic, French", gini = NA_real_
)

test_that("country facts are formatted like the table", {
  facts <- country_facts(profile_country)

  expect_equal(facts$fact, c("Region", "Capital", "Population", "Area", "Languages", "Gini index"))
  expect_equal(facts$value[facts$fact == "Population"], "18,000,000")
  expect_equal(facts$value[facts$fact == "Area"], "1,284,000 km²")
  expect_equal(facts$value[facts$fact == "Gini index"], "—")
})

test_that("the profile modal shows the flag, facts and Wikipedia summary", {
  local_mocked_bindings(fetch_wikipedia_summaries = function(titles) {
    s <- empty_summaries(titles)
    s$page_title <- "Chad"
    s$extract <- "Chad is a landlocked country."
    s$url <- "https://en.wikipedia.org/wiki/Chad"
    s
  })

  html <- as.character(country_profile_modal(profile_country))

  expect_match(html, "<cds-modal", fixed = TRUE)
  expect_match(html, "Chad</cds-modal-heading>", fixed = TRUE)
  expect_match(html, 'src="https://flags.example/td.png"', fixed = TRUE)
  expect_match(html, "N&#39;Djamena|N'Djamena")
  expect_match(html, "Chad is a landlocked country.", fixed = TRUE)
})

test_that("clicking a country's name shows its profile", {
  local_mocked_bindings(fetch_data = function(region) {
    rbind(profile_country, transform(profile_country, name = "Niger", iso3 = "NER"))
  })
  shown <- list()
  local_mocked_bindings(
    show_carbon_modal = function(ui, session = NULL) shown[[length(shown) + 1]] <<- ui,
    .package = "carbonite"
  )
  local_mocked_bindings(country_profile_modal = function(country) country$name)

  shiny::testServer(server, {
    session$setInputs(region = "SSF", columns = "population")
    session$setInputs(table_cell_click = list(row = 2, key = "2", column = "name"))
    # Clicks on other columns, or rows that don't exist, are ignored
    session$setInputs(table_cell_click = list(row = 1, key = "1", column = "population"))
    session$setInputs(table_cell_click = list(row = 9, key = "9", column = "name"))

    expect_equal(shown, list("Niger"))
  })
})
