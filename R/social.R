# Data for the Social indicators page. Each function takes the region's
# countries (from fetch_data(): name, iso3, population) and an indicator's
# history from fetch_wb_history() (iso3, year, value), and returns what one
# chart needs. Countries without data (e.g. Taiwan isn't in World Bank data)
# are left out.

# Most countries a per-country chart shows. Every region fits; for the whole
# world, those charts show the most populous countries.
social_max_countries <- 60

# Countries of `countries` that have a code, keeping the `max_countries`
# most populous
region_countries <- function(countries, max_countries = social_max_countries) {
  out <- countries[!is.na(countries$iso3), c("name", "iso3", "population"), drop = FALSE]
  if (nrow(out) > max_countries) {
    out <- out[order(-out$population, out$name), , drop = FALSE][seq_len(max_countries), , drop = FALSE]
  }
  out
}

# Each country's most recent value: iso3, year, value
latest_values <- function(history) {
  history <- history[order(history$iso3, -history$year), , drop = FALSE]
  history[!duplicated(history$iso3), , drop = FALSE]
}

# Latest value per country in the region, smallest first (a flipped
# category axis draws the first value at the bottom, so the largest is on
# top): name, iso3, year, value
ranked_latest <- function(countries, history, max_countries = social_max_countries) {
  out <- merge(region_countries(countries, max_countries), latest_values(history), by = "iso3")
  out <- out[order(out$value, out$name), c("name", "iso3", "year", "value"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Change per country between `from_year` (or the first year with a value up
# to `from_year + window`, as many countries miss a year here and there) and
# its latest value, if that's from `min_end_year` or later (North Korea's
# latest internet figure is from 2004, which isn't "now"). Sorted by the
# latest value, smallest first: name, iso3, start_year, start, end_year, end
change_since <- function(countries, history, from_year = 2000, window = 5,
                         min_end_year = 2015, max_countries = social_max_countries) {
  early <- history[history$year >= from_year & history$year <= from_year + window, , drop = FALSE]
  early <- early[order(early$iso3, early$year), , drop = FALSE]
  early <- early[!duplicated(early$iso3), , drop = FALSE]
  names(early) <- c("iso3", "start_year", "start")

  late <- latest_values(history)
  names(late) <- c("iso3", "end_year", "end")

  out <- merge(region_countries(countries, max_countries), early, by = "iso3")
  out <- merge(out, late, by = "iso3")
  out <- out[out$end_year > out$start_year & out$end_year >= min_end_year, , drop = FALSE]
  out <- out[order(out$end, out$name), c("name", "iso3", "start_year", "start", "end_year", "end"), drop = FALSE]
  rownames(out) <- NULL
  out
}

# Countries' latest values grouped into bins of `width` (e.g. 0.5 births per
# woman): one row per bin from the lowest to the highest occupied bin, with
# empty bins kept so the shape of the distribution shows. Columns: from, to,
# label, count, countries (names, comma-separated).
binned_latest <- function(countries, history, width = 0.5) {
  # Every country: a histogram shows the whole distribution
  latest <- ranked_latest(countries, history, max_countries = Inf)
  if (nrow(latest) == 0) {
    return(data.frame(from = numeric(), to = numeric(), label = character(),
                      count = integer(), countries = character()))
  }

  lower <- floor(latest$value / width) * width
  breaks <- seq(min(lower), max(lower), by = width)
  data.frame(
    from = breaks,
    to = breaks + width,
    label = sprintf("%s–%s", format(breaks), format(breaks + width)),
    count = vapply(breaks, \(b) sum(abs(lower - b) < width / 1e6), integer(1)),
    countries = vapply(breaks, \(b) paste(sort(latest$name[abs(lower - b) < width / 1e6]), collapse = ", "), "")
  )
}

# Yearly values for each country in the region (name, year, value), and the
# median across those countries each year (year, value), for years where at
# least `min_countries` have a value
yearly_trends <- function(countries, history, min_countries = 3) {
  region <- countries[!is.na(countries$iso3), c("name", "iso3"), drop = FALSE]
  series <- merge(region, history, by = "iso3")
  series <- series[order(series$name, series$year), c("name", "year", "value"), drop = FALSE]
  rownames(series) <- NULL

  years <- sort(unique(series$year))
  counts <- vapply(years, \(y) sum(series$year == y), integer(1))
  years <- years[counts >= min_countries]
  median_by_year <- data.frame(
    year = years,
    value = vapply(years, \(y) stats::median(series$value[series$year == y]), numeric(1))
  )

  list(series = series, median = median_by_year)
}

# "2000–2024", or "2024" when all values are from one year
year_span <- function(years) {
  years <- range(years, na.rm = TRUE)
  if (years[1] == years[2]) as.character(years[1]) else paste0(years[1], "–", years[2])
}

# Most countries a trend chart draws one line each for. With more, the
# lines become an unreadable mass, so the chart shows the spread instead.
trend_max_lines <- 10

# Yearly values for the urban population chart, in whichever form reads
# best: one line per country for a region with few countries ("lines",
# with `trends` from yearly_trends()), or the median and middle half of
# countries each year ("band", with `spread` from yearly_spread()).
trend_or_spread <- function(countries, history, max_lines = trend_max_lines) {
  trends <- yearly_trends(countries, history)
  if (length(unique(trends$series$name)) <= max_lines) {
    list(kind = "lines", trends = trends)
  } else {
    list(kind = "band", spread = yearly_spread(countries, history))
  }
}

# How many countries a ranked lollipop chart shows at each end (highest and
# lowest) when a region has more than twice as many
lollipop_extremes <- 10
