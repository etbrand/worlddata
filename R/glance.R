# Headline figures for the At a glance page: four callouts about the
# selected region (or the whole world), each a list of
#   label   what it is, e.g. "Population"
#   value   the number as text, e.g. "2.38"
#   unit    optional, shown smaller after the value, e.g. "billion"
#   note    context, e.g. "29% of the world's people · Largest: China"
#   year    the data's year(s), e.g. "2024", or NULL

# `region`: world_region or a code of wb_regions. `countries`: the region's
# countries and `all_countries` every country (from fetch_data()). `gdp`,
# `life` and `population`: latest values from fetch_wb_indicator() (iso3,
# year, value), which include the World Bank's regional aggregates; or an
# api_error if that request failed.
glance_callouts <- function(region, countries, all_countries, gdp, life, population) {
  world <- identical(region, world_region)
  list(
    countries_callout(world, countries, all_countries),
    population_callout(world, countries, all_countries, population),
    economy_callout(world, region, countries, gdp),
    life_callout(world, region, all_countries, life)
  )
}

countries_callout <- function(world, countries, all_countries) {
  list(
    label = "Countries and territories",
    value = format(nrow(countries), big.mark = ","),
    note = if (world) {
      sprintf("Across the World Bank's %d regions", length(wb_regions))
    } else {
      sprintf("Of %d worldwide", nrow(all_countries))
    }
  )
}

# Population from the country list (every country has one); its year from
# the population indicator, when that loaded
population_callout <- function(world, countries, all_countries, population) {
  total <- sum(countries$population, na.rm = TRUE)
  largest <- largest_name(countries, countries$population)
  share <- total / sum(all_countries$population, na.rm = TRUE)
  amount <- format_compact(total)

  list(
    label = "Population",
    value = amount$value,
    unit = amount$unit,
    note = join_notes(
      if (!world) sprintf("%s of the world's people", format_share(share)),
      if (!is.na(largest)) paste("Largest:", largest)
    ),
    year = if (!inherits(population, "api_error")) {
      year_label(population$year[population$iso3 %in% countries$iso3])
    }
  )
}

# The region's GDP is the World Bank's figure for the region as a whole
# (its aggregate), so countries without a figure are still estimated
economy_callout <- function(world, region, countries, gdp) {
  label <- "Economy (GDP)"
  if (inherits(gdp, "api_error")) {
    return(unavailable_callout(label))
  }
  code <- if (world) world_aggregate else region
  total <- aggregate_value(gdp, code)
  if (is.null(total)) {
    return(unavailable_callout(label))
  }

  world_total <- aggregate_value(gdp, world_aggregate)
  in_region <- gdp[gdp$iso3 %in% countries$iso3, , drop = FALSE]
  largest <- largest_name(countries, in_region$value[match(countries$iso3, in_region$iso3)])
  amount <- format_compact(total$value)

  list(
    label = label,
    value = paste0("$", amount$value),
    unit = amount$unit,
    note = join_notes(
      if (!world && !is.null(world_total)) {
        sprintf("%s of world GDP", format_share(total$value / world_total$value))
      },
      if (!is.na(largest)) paste("Largest:", largest)
    ),
    year = as.character(total$year)
  )
}

# The World Bank's life expectancy for the region as a whole, weighted by
# population, next to the world's; for the world, the highest country
life_callout <- function(world, region, all_countries, life) {
  label <- "Life expectancy"
  if (inherits(life, "api_error")) {
    return(unavailable_callout(label))
  }
  value <- aggregate_value(life, if (world) world_aggregate else region)
  if (is.null(value)) {
    return(unavailable_callout(label))
  }

  note <- if (world) {
    countries <- life[life$iso3 %in% all_countries$iso3, , drop = FALSE]
    highest <- largest_name(all_countries, countries$value[match(all_countries$iso3, countries$iso3)])
    if (!is.na(highest)) paste("Highest:", highest)
  } else {
    world_value <- aggregate_value(life, world_aggregate)
    if (!is.null(world_value)) sprintf("World: %s years", format(round(world_value$value, 1), nsmall = 1))
  }

  list(
    label = label,
    value = format(round(value$value, 1), nsmall = 1),
    unit = "years",
    note = note,
    year = as.character(value$year)
  )
}

unavailable_callout <- function(label) {
  list(label = label, value = missing_label, note = "Couldn't load this figure from the World Bank.")
}

# Helpers ---------------------------------------------------------------------

# The row of an aggregate (e.g. "EAS", "WLD") in latest values, or NULL
aggregate_value <- function(values, code) {
  row <- values[values$iso3 == code & !is.na(values$value), , drop = FALSE]
  if (nrow(row) == 0) NULL else list(value = row$value[1], year = row$year[1])
}

# Name of the country with the largest of `values` (aligned with
# `countries`), or NA when none has a value
largest_name <- function(countries, values) {
  if (length(values) == 0 || all(is.na(values))) {
    return(NA_character_)
  }
  countries$name[which.max(values)]
}

# 2.38e9 -> list(value = "2.38", unit = "billion"): three significant
# figures, so numbers stay short enough for a callout
format_compact <- function(x) {
  sizes <- c(trillion = 1e12, billion = 1e9, million = 1e6)
  for (unit in names(sizes)) {
    if (abs(x) >= sizes[[unit]]) {
      return(list(value = format(signif(x / sizes[[unit]], 3)), unit = unit))
    }
  }
  list(value = format(round(x), big.mark = ","), unit = NULL)
}

# 0.287 -> "29%"; shares under 1% would round to "0%"
format_share <- function(share) {
  if (share < 0.01) "Under 1%" else paste0(round(share * 100), "%")
}

join_notes <- function(...) {
  notes <- unlist(list(...))
  if (length(notes)) paste(notes, collapse = " · ")
}

# "2024", or "2022–2024" when countries' latest years differ
year_label <- function(years) {
  if (length(years) == 0) NULL else year_span(years)
}
