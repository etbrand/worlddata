# Charts for the Economic indicators page, using the data functions in
# R/economic.R. Like R/plots_social.R, they set echarts options directly.

# Tooltip formatting for US$ amounts: "$1.23 trillion", "$456.7 million"
money_js <- "v => {
  v = Number(v);
  for (const [size, unit] of [[1e12, ' trillion'], [1e9, ' billion'], [1e6, ' million']]) {
    if (Math.abs(v) >= size) {
      const x = v / size;
      return '$' + x.toLocaleString(undefined, { maximumFractionDigits: x < 10 ? 2 : 1 }) + unit;
    }
  }
  return '$' + v.toLocaleString(undefined, { maximumFractionDigits: 0 });
}"

# Treemap -------------------------------------------------------------------------

# Carbon blues, darkest for the smallest economies. On the dark theme, the
# biggest economies stand out most.
treemap_blues <- c("#001d6c", "#002d9c", "#0043ce", "#0f62fe", "#4589ff")

# Each country's color, by its GDP on a log scale (GDPs span six orders of
# magnitude, so a linear scale would make all but a few the same color)
treemap_colors <- function(values) {
  logs <- log10(values)
  span <- diff(range(logs))
  t <- if (span > 0) (logs - min(logs)) / span else rep(1, length(logs))
  treemap_blues[1 + round(t * (length(treemap_blues) - 1))]
}

# A rectangle per country, sized by its GDP. `data` comes from gdp_shares();
# `share_of` names what the shares are of, for the tooltip.
gdp_treemap <- function(data, share_of = "the region") {
  colors <- treemap_colors(data$value)
  items <- lapply(seq_len(nrow(data)), function(i) {
    list(
      name = data$name[i],
      value = data$value[i],
      share = data$share[i],
      year = data$year[i],
      itemStyle = list(color = colors[i])
    )
  })

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(sprintf(
        "p => { const money = %s;
          return '<strong>' + p.name + '</strong><br/>' + money(p.value) + ' (' + p.data.year + ')<br/>' +
            (p.data.share * 100).toLocaleString(undefined, { maximumFractionDigits: 1 }) + '%% of ' + %s; }",
        money_js, js_string(share_of)
      ))
    )
  # echarts4r adds a default y axis, which stops a treemap from drawing
  e$x$opts$xAxis <- NULL
  e$x$opts$yAxis <- NULL
  e$x$opts$series <- list(list(
    type = "treemap",
    data = items,
    left = 0, right = 0, top = 0, bottom = 0,
    roam = FALSE,
    nodeClick = FALSE,
    breadcrumb = list(show = FALSE),
    # Largest first, left to right and top to bottom
    sort = "desc",
    squareRatio = 1.2,
    # Gaps between rectangles in the tile's background color
    itemStyle = list(borderColor = "var(--cds-layer-01)", borderWidth = 1, gapWidth = 1),
    emphasis = list(itemStyle = list(borderColor = highlight_color)),
    label = list(
      show = TRUE,
      color = "#f4f4f4",
      fontSize = 12,
      overflow = "truncate",
      ellipsis = "…",
      # Only economies with 1% or more of the region: smaller rectangles are
      # too small for a readable label (hover shows the name)
      formatter = htmlwidgets::JS(
        "p => p.data.share < 0.01 ? '' : p.name + '\\n' + (p.data.share * 100).toLocaleString(undefined, { maximumFractionDigits: 1 }) + '%'"
      )
    )
  ))
  e
}

# Band chart --------------------------------------------------------------------

# The regional median each year, with a band from the 25th to the 75th
# percentile. `spread` is yearly_spread()$spread.
spread_band_chart <- function(spread, label, digits = 1) {
  years <- as.character(spread$year)
  # Hex with transparency (no commas, which light_dark() pairs can't contain):
  # blue 60 at 18% on light, blue 50 at 25% on dark
  band_color <- light_dark("#0f62fe2e", "#4589ff40")
  hidden_line <- list(width = 0, opacity = 0)
  median_points <- lapply(seq_len(nrow(spread)), function(i) {
    list(value = spread$median[i], low = spread$low[i], high = spread$high[i], n = spread$n[i])
  })

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "axis",
      axisPointer = list(type = "line", lineStyle = list(color = social_muted)),
      formatter = htmlwidgets::JS(sprintf(
        "ps => { const fmt = %s; const p = ps.find(x => x.seriesName === 'Median'); if (!p) return '';
          const d = p.data;
          return '<strong>' + p.name + '</strong><br/>Median: ' + fmt(d.value) + '%%<br/>' +
            'Middle half of countries: ' + fmt(d.low) + '%% to ' + fmt(d.high) + '%%<br/>' +
            d.n + ' countries'; }",
        tooltip_number_js(digits)
      ))
    )
  e$x$opts$grid <- list(left = 48, right = 24, top = 32, bottom = 32)
  e$x$opts$xAxis <- list(list(
    type = "category",
    data = as.list(years),
    boundaryGap = FALSE,
    axisLabel = list(color = plot_colors$text),
    axisLine = list(lineStyle = list(color = plot_colors$grid)),
    axisTick = list(show = FALSE)
  ))
  e$x$opts$yAxis <- list(value_axis(label, nameLocation = "end", nameGap = 12))
  e$x$opts$legend <- list(
    show = TRUE, top = 0, right = 0,
    data = list("Median", "Middle half of countries"),
    textStyle = list(color = plot_colors$text)
  )
  e$x$opts$series <- list(
    # The band: an invisible line at the 25th percentile, then the distance
    # to the 75th stacked on it and filled. "all" stacks onto negative
    # values too (deflation), rather than onto zero.
    list(type = "line", name = "low", stack = "band", stackStrategy = "all",
         data = as.list(spread$low), symbol = "none", silent = TRUE, lineStyle = hidden_line),
    list(type = "line", name = "Middle half of countries", stack = "band", stackStrategy = "all",
         data = as.list(spread$high - spread$low), symbol = "none", silent = TRUE,
         lineStyle = hidden_line, areaStyle = list(color = band_color),
         itemStyle = list(color = band_color)),
    list(type = "line", name = "Median", data = median_points, symbol = "none", z = 3,
         lineStyle = list(width = 2.5, color = plot_colors$bar),
         itemStyle = list(color = plot_colors$bar))
  )
  e
}

# Heatmap -----------------------------------------------------------------------

# Ranges of yearly growth (%) and their colors: reds for contraction, blues
# for growth, strongest at the extremes. On a dark background the strongest
# colors are the brightest; on a light one, the darkest.
growth_pieces <- list(
  list(lt = -3, color = light_dark("#a2191f", "#fa4d56"), label = "Below −3%"),
  list(gte = -3, lt = 0, color = light_dark("#ff8389", "#a2191f"), label = "−3% to 0%"),
  list(gte = 0, lt = 3, color = light_dark("#a6c8ff", "#002d9c"), label = "0% to 3%"),
  list(gte = 3, lt = 6, color = light_dark("#4589ff", "#0f62fe"), label = "3% to 6%"),
  list(gte = 6, color = light_dark("#0043ce", "#78a9ff"), label = "6% or more")
)

# A cell per country and year, colored by its value. `grid` comes from
# yearly_grid(); countries are rows, ranked by their compounded average
# yearly growth (growth_ranking()), fastest at the top. `extremes`: as for
# lollipop_chart(), show only the fastest and slowest few.
growth_heatmap <- function(grid, label, extremes = NULL) {
  ranking <- growth_ranking(grid)
  rows <- split_rows(ranking, extremes)
  names <- rows$name
  # Cells for the countries shown only
  grid <- grid[grid$name %in% names[!is.na(rows$annual)], , drop = FALSE]
  years <- seq(min(grid$year), max(grid$year))
  label_width <- country_label_width(names)
  cells <- lapply(seq_len(nrow(grid)), function(i) {
    r <- match(grid$name[i], names)
    list(
      value = list(match(grid$year[i], years) - 1, r - 1, round(grid$value[i], 1)),
      country = grid$name[i],
      year = grid$year[i],
      span = if (rows$first[r] == rows$last[r]) as.character(rows$first[r]) else paste0(rows$first[r], "–", rows$last[r]),
      total = round(rows$total[r], 1),
      annual = round(rows$annual[r], 1)
    )
  })
  legend_height <- 32

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(sprintf(
        "p => { const d = p.data;
          const pct = v => (v > 0 ? '+' : '') +
            Number(v).toLocaleString(undefined, { minimumFractionDigits: 1, maximumFractionDigits: 1 }) + '%%';
          return '<strong>' + d.country + '</strong><br/>' + %s + '<br/>' + d.year + ': ' + pct(p.value[2]) +
            '<br/>' + d.span + ': ' + pct(d.annual) + ' a year, ' + pct(d.total) + ' in total'; }",
        js_string(label)
      ))
    )
  e$x$opts$grid <- list(left = label_width + 24, right = 16, top = legend_height + 8, bottom = 40)
  e$x$opts$xAxis <- list(list(
    type = "category",
    data = as.list(as.character(years)),
    axisLabel = list(color = plot_colors$text),
    axisLine = list(lineStyle = list(color = plot_colors$grid)),
    axisTick = list(show = FALSE),
    splitArea = list(show = FALSE)
  ))
  # The first row (slowest growth) at the bottom, the fastest at the top
  e$x$opts$yAxis <- list(split_axis(rows, label_width))
  e$x$opts$visualMap <- list(list(
    type = "piecewise",
    dimension = 2,
    pieces = growth_pieces,
    orient = "horizontal",
    top = 0,
    right = 0,
    itemWidth = 12,
    itemHeight = 12,
    itemGap = 12,
    textStyle = list(color = plot_colors$text, fontSize = 12)
  ))
  e$x$opts$series <- list(list(
    type = "heatmap",
    data = cells,
    itemStyle = list(borderColor = "var(--cds-layer-01)", borderWidth = 1),
    emphasis = list(itemStyle = list(borderColor = highlight_color, borderWidth = 1))
  ))
  e <- htmlwidgets::onRender(e, resize_to_rows_js(length(names), extra = legend_height))
  add_tear(e, rows)
}

# Ranked bars -------------------------------------------------------------------

# A bar per country at its latest value, largest at the top. `data` comes
# from ranked_latest(). Rows named in `highlight` get a different color.
# `extremes` and `median` as for lollipop_chart().
ranked_bar_chart <- function(data, label, digits = 1, highlight = NULL, extremes = NULL, median = NULL) {
  rows <- split_rows(data[, c("name", "year", "value")], extremes)
  label_width <- country_label_width(rows$name)
  bars <- lapply(seq_len(nrow(rows)), function(i) {
    # Heading and gap rows: no bar
    if (is.na(rows$value[i])) {
      return(NA)
    }
    bar <- list(value = rows$value[i], year = rows$year[i])
    if (rows$name[i] %in% highlight) bar$itemStyle <- list(color = highlight_color)
    bar
  })

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(sprintf(
        "p => { const fmt = %s; return '<strong>' + p.name + '</strong><br/>' + %s + ': ' + fmt(p.value) + ' (' + p.data.year + ')'; }",
        tooltip_number_js(digits), js_string(label)
      ))
    )
  # Room above the plot for the median line's label
  e$x$opts$grid <- list(left = label_width + 24, right = 24, top = if (is.null(median)) 16 else 32, bottom = 48)
  e$x$opts$yAxis <- list(split_axis(rows, label_width))
  e$x$opts$xAxis <- list(value_axis(label, scale = FALSE))
  e$x$opts$series <- list(list(
    type = "bar",
    data = bars,
    barCategoryGap = "30%",
    itemStyle = list(color = plot_colors$bar),
    emphasis = list(itemStyle = list(color = light_dark("#0043ce", "#78a9ff")))
  ))
  e$x$opts$series[[1]]$markLine <- median_line(median, digits)
  e <- htmlwidgets::onRender(e, resize_to_rows_js(nrow(rows)))
  add_tear(e, rows)
}
