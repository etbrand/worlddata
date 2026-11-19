# The World Bank's regional aggregates, for the Regions view of the
# indicator pages (shown when the region is "World"). The indicator
# histories from fetch_wb_history() already include them: each region's
# code (e.g. "EAS") and "WLD" for the world are rows like any country's.

world_aggregate <- "WLD"
world_aggregate_name <- "World"

# Short region names, as in the header select, by code: chart axes have
# little room
region_short_names <- stats::setNames(names(region_choices$Regions), region_choices$Regions)

# The aggregates as a countries-like data frame (name, iso3, population),
# so the data functions in R/social.R and R/economic.R work on them
# unchanged. `world`: include the world as a whole.
region_aggregates <- function(world = TRUE) {
  out <- data.frame(
    name = unname(region_short_names),
    iso3 = names(region_short_names),
    population = NA_real_
  )
  if (world) {
    out <- rbind(out, data.frame(name = world_aggregate_name, iso3 = world_aggregate, population = NA_real_))
  }
  out
}

# Yearly values for each region (name, year, value) and for the world
# (year, value), in the shape of yearly_trends() so trend_chart() can draw
# them: the world takes the place of the median
region_trends <- function(history) {
  series <- merge(region_aggregates(world = FALSE)[, c("name", "iso3")], history, by = "iso3")
  series <- series[order(series$name, series$year), c("name", "year", "value"), drop = FALSE]
  rownames(series) <- NULL

  world <- history[history$iso3 == world_aggregate, c("year", "value"), drop = FALSE]
  world <- world[order(world$year), , drop = FALSE]
  rownames(world) <- NULL

  list(series = series, median = world)
}

# Number of regions (not counting the world) in a chart's data
regions_shown <- function(names) {
  length(setdiff(unique(names), world_aggregate_name))
}
