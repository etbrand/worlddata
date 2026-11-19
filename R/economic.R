# Data for the Economic indicators page. Like R/social.R, each function takes
# the region's countries (from fetch_data(): name, iso3, population) and an
# indicator's history from fetch_wb_history() (iso3, year, value).

# Each country's latest GDP and its share of the region's total, largest
# first: name, iso3, year, value, share (0-1). Every country is included:
# small economies are small rectangles in the treemap, so there's no need to
# leave them out. Countries' latest years can differ by a year or two.
gdp_shares <- function(countries, history) {
  out <- ranked_latest(countries, history, max_countries = Inf)
  out <- out[out$value > 0, , drop = FALSE]
  out <- out[order(-out$value, out$name), , drop = FALSE]
  out$share <- out$value / sum(out$value)
  rownames(out) <- NULL
  out
}

# The spread of yearly values across the region's countries: for each year
# with at least `min_countries` values, the median and the 25th and 75th
# percentiles. Quartiles rather than the full range, so one country's
# hyperinflation doesn't hide everyone else. Returns a list: `spread` (year,
# low, median, high, n) and `countries`, the number of countries with a
# value in any of those years.
yearly_spread <- function(countries, history, min_countries = 3) {
  region <- countries[!is.na(countries$iso3), "iso3", drop = FALSE]
  values <- merge(region, history, by = "iso3")

  years <- sort(unique(values$year))
  years <- years[vapply(years, \(y) sum(values$year == y), integer(1)) >= min_countries]
  quantiles <- lapply(years, \(y) stats::quantile(values$value[values$year == y], c(0.25, 0.5, 0.75), names = FALSE))

  out <- data.frame(
    year = years,
    low = vapply(quantiles, `[`, numeric(1), 1),
    median = vapply(quantiles, `[`, numeric(1), 2),
    high = vapply(quantiles, `[`, numeric(1), 3),
    n = vapply(years, \(y) sum(values$year == y), integer(1))
  )
  list(spread = out, countries = length(unique(values$iso3[values$year %in% years])))
}

# Each country's growth over the years of `grid` (from yearly_grid(), with
# yearly growth in %), compounded: `total` (% over its years with data) and
# `annual` (the average yearly rate, in %, that compounds to the same
# total). Ranked by `annual`, smallest first, as countries can be missing
# years: name, first, last (years), years (with data), total, annual.
growth_ranking <- function(grid) {
  by_country <- split(grid, grid$name)
  out <- do.call(rbind, lapply(by_country, function(d) {
    factor <- prod(1 + d$value / 100)
    data.frame(
      name = d$name[1],
      first = min(d$year),
      last = max(d$year),
      years = nrow(d),
      total = (factor - 1) * 100,
      annual = (factor^(1 / nrow(d)) - 1) * 100
    )
  }))
  if (is.null(out)) {
    return(data.frame(name = character(), first = integer(), last = integer(), years = integer(),
                      total = numeric(), annual = numeric()))
  }
  out <- out[order(out$annual, out$name), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Caption note for the growth heatmap of `n` countries
growth_note <- function(n, extremes = ranked_extremes) {
  ranked <- "Ranked by average yearly growth, compounded over each country's years with data"
  if (n > 2 * extremes) {
    sprintf("%s; showing the highest and lowest %d of %d.", ranked, extremes, n)
  } else {
    paste0(ranked, ".")
  }
}

# Yearly values from `from_year` for each country in the region (capped at
# `max_countries`, the most populous): name, year, value. Countries are in
# alphabetical order.
yearly_grid <- function(countries, history, from_year = 2005, max_countries = social_max_countries) {
  region <- region_countries(countries, max_countries)[, c("name", "iso3")]
  out <- merge(region, history[history$year >= from_year, , drop = FALSE], by = "iso3")
  out <- out[order(out$name, out$year), c("name", "year", "value"), drop = FALSE]
  rownames(out) <- NULL
  out
}
