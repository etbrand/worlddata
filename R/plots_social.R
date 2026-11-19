# Charts for the Social indicators page. Each takes the output of one of the
# data functions in R/social.R. The charts set echarts options directly
# (like indicator_scatter_chart()) for full control over layering and
# tooltips.

# A chart with the app's shared look: no legend, transparent background,
# dark tooltip. Callers add axes and series.
social_base_chart <- function() {
  echarts4r::e_charts() |>
    plot_tooltip(trigger = "item") |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_color(plot_colors$bar, background = "transparent") |>
    plot_theme()
}

# A value axis in the app's style; `...` overrides any setting
value_axis <- function(name = NULL, ...) {
  defaults <- list(
    type = "value",
    name = name,
    nameLocation = "middle",
    nameGap = 28,
    nameTextStyle = list(color = plot_colors$text),
    # Don't force the axis to start at zero
    scale = TRUE,
    axisLabel = list(color = plot_colors$text),
    axisLine = list(lineStyle = list(color = plot_colors$grid)),
    splitLine = list(lineStyle = list(color = plot_colors$grid))
  )
  utils::modifyList(defaults, list(...))
}

country_axis <- function(names, label_width) {
  list(
    type = "category",
    data = as.list(names),
    axisLabel = country_axis_label(label_width),
    axisLine = list(lineStyle = list(color = plot_colors$grid)),
    axisTick = list(show = FALSE)
  )
}

# Colors are theme references, resolved when a chart renders (see
# plot_colors in R/plots.R)

# Muted grey for stems, connectors and background lines
social_muted <- "var(--cds-border-strong-01)"

# For a row that stands apart from the rest, e.g. the world among regions:
# the primary text color, near-black on light and near-white on dark
highlight_color <- "var(--cds-text-primary)"

# Distinct colors for a handful of lines (the seven regions), from Carbon's
# categorical palettes, leaving out blues close to the app's accent and the
# near-black/near-white that marks the highlighted line
series_colors <- light_dark(
  c("#6929c4", "#1192e8", "#005d5d", "#9f1853", "#fa4d56", "#198038", "#b28600"),
  c("#8a3ffc", "#33b1ff", "#007d79", "#ff7eb6", "#fa4d56", "#6fdc8c", "#d2a106")
)

# Number formatting in tooltips, to `digits` decimal places
tooltip_number_js <- function(digits) {
  sprintf(
    "v => Number(v).toLocaleString(undefined, { minimumFractionDigits: %d, maximumFractionDigits: %d })",
    digits, digits
  )
}

# Lollipop chart ----------------------------------------------------------------

# One stem and dot per country at its latest value, ranked, largest at the
# top. `data` comes from ranked_latest(). Rows named in `highlight` get a
# different color.
#
# `extremes`: with more than twice this many rows, show only the highest and
# lowest `extremes`, under headings and with a gap between them, on one
# scale. `median`: draw a dashed line at this value (e.g. the median of all
# the rows, including those not shown).
lollipop_chart <- function(data, label, digits = 1, highlight = NULL, extremes = NULL, median = NULL) {
  rows <- split_rows(data[, c("name", "year", "value")], extremes)
  label_width <- country_label_width(rows$name)
  countries <- which(!is.na(rows$value))
  points <- lapply(countries, function(i) {
    point <- list(value = list(rows$value[i], rows$name[i]), name = rows$name[i], year = rows$year[i])
    if (rows$name[i] %in% highlight) point$itemStyle <- list(color = highlight_color)
    point
  })

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(sprintf(
        "p => { const fmt = %s; return '<strong>' + p.name + '</strong><br/>' + %s + ': ' + fmt(p.value[0]) + ' (' + p.data.year + ')'; }",
        tooltip_number_js(digits), js_string(label)
      ))
    )
  # Room above the plot for the median line's label
  e$x$opts$grid <- list(left = label_width + 24, right = 24, top = if (is.null(median)) 16 else 32, bottom = 48)
  e$x$opts$yAxis <- list(split_axis(rows, label_width))
  e$x$opts$xAxis <- list(value_axis(label))
  dots <- list(type = "scatter", data = points, symbolSize = 9, z = 3)
  dots$markLine <- median_line(median, digits)
  e$x$opts$series <- list(
    # Stems: thin bars, drawn from the left edge of the axis (none for the
    # heading and gap rows, whose value is NA)
    list(type = "bar", data = as.list(rows$value), barWidth = 2, silent = TRUE,
         itemStyle = list(color = social_muted)),
    dots
  )
  e <- htmlwidgets::onRender(e, resize_to_rows_js(nrow(rows)))
  add_tear(e, rows)
}

# Highest and lowest ------------------------------------------------------------
#
# A long ranking (the lollipop, dumbbell and ranked bar charts) can show
# only its highest and lowest few, on one scale, with a heading over each
# group and a torn-paper break between them.

# The rows of a ranked chart, from the bottom of the chart up: every row of
# `data` (smallest first), or with more than twice `extremes` rows, the
# lowest few, a "Lowest" heading, two blank gap rows (for the tear) and the
# highest few with a "Highest" heading on top. Keeps `data`'s columns (NA
# for headings and the gap, apart from name) and adds `heading` (TRUE for
# headings).
split_rows <- function(data, extremes = NULL) {
  out <- data
  out$heading <- rep(FALSE, nrow(out))
  if (is.null(extremes) || nrow(data) <= 2 * extremes) {
    return(out)
  }
  # A row of NAs of the right types, with this name. Category names must be
  # unique, so the two blank gap rows differ: one space and two.
  blank <- function(name, heading) {
    row <- out[1, , drop = FALSE]
    row[] <- lapply(row, \(col) col[NA_integer_])
    row$name <- name
    row$heading <- heading
    row
  }
  out <- rbind(
    utils::head(out, extremes), blank("Lowest", TRUE),
    blank(" ", FALSE), blank("  ", FALSE),
    utils::tail(out, extremes), blank("Highest", TRUE)
  )
  rownames(out) <- NULL
  out
}

# The gap rows of split_rows() output (1-based); none if nothing was left out
split_gap <- function(rows) {
  which(!rows$heading & !nzchar(trimws(rows$name)))
}

# A country axis for split_rows() output, with the headings in the label
# column in semibold, like a small heading
split_axis <- function(rows, label_width) {
  axis <- country_axis(rows$name, label_width)
  headings <- rows$name[rows$heading]
  if (length(headings)) {
    axis$axisLabel$formatter <- htmlwidgets::JS(sprintf(
      "v => [%s].includes(v) ? '{heading|' + v + '}' : v",
      paste(vapply(headings, js_string, ""), collapse = ", ")
    ))
    axis$axisLabel$rich <- list(heading = list(fontWeight = 600, fontSize = 12, color = highlight_color))
  }
  axis
}

# A tear across chart `e`, centred in the gap between the highest and
# lowest (two blank rows, so it stays clear of the rows either side); `e`
# unchanged if every row is shown
add_tear <- function(e, rows) {
  gap <- split_gap(rows)
  if (!length(gap)) {
    return(e)
  }
  htmlwidgets::onRender(e, tear_js(gap - 1, grid_left = e$x$opts$grid$left, grid_right = e$x$opts$grid$right))
}

# A series markLine: a dashed vertical line at `median`, labeled above the
# plot (charts leave 32px or more above the plot for it). NULL for no line.
median_line <- function(median, digits = 1) {
  if (is.null(median)) {
    return(NULL)
  }
  list(
    silent = TRUE,
    symbol = "none",
    data = list(list(xAxis = median)),
    lineStyle = list(color = social_muted, type = "dashed", width = 1),
    label = list(
      position = "end",
      color = plot_colors$text,
      fontSize = 12,
      formatter = paste("Median", format(round(median, digits), nsmall = digits))
    )
  )
}

# JS for htmlwidgets::onRender(): a torn-paper break across the plot,
# centred between category rows `rows` (0-based; the blank gap rows), from
# the axis line to the right edge of the plot, clear of the labels. Two
# zigzag edges with the tile's background between them hide the axis line,
# gridlines and any reference line there, showing that rows were left out.
# ECharts can't break a category axis itself, so this is drawn on top, and
# redrawn when the chart's size changes.
tear_js <- function(rows, grid_left, grid_right) {
  sprintf(
    "function(el) {
      const chart = echarts.getInstanceByDom(el);
      if (!chart) return;
      let drawn = '';
      const draw = () => {
        const ys = [%s].map(row => chart.convertToPixel({ yAxisIndex: 0 }, row));
        if (ys.some(y => y == null || isNaN(y))) return;
        const y = ys.reduce((a, b) => a + b, 0) / ys.length;
        const left = %d, right = el.clientWidth - %d;
        const key = [Math.round(y), left, right].join();
        if (key === drawn) return;
        drawn = key;
        // Zigzag edges: a point every 6px, alternating 2.5px up and down
        const top = [], bottom = [];
        for (let x = left, i = 0; x < right + 6; x += 6, i++) {
          const wobble = i %% 2 ? 2.5 : -2.5;
          top.push([Math.min(x, right), y - 4 + wobble]);
          bottom.push([Math.min(x, right), y + 4 + wobble]);
        }
        // Theme tokens, read now (this runs after plot_theme_js()): the
        // tile's background between the edges, muted edges
        const css = getComputedStyle(document.documentElement);
        const layer = css.getPropertyValue('--cds-layer-01').trim() || '#262626';
        const edge = css.getPropertyValue('--cds-border-strong-01').trim() || '#6f6f6f';
        chart.setOption({ graphic: [
          { id: 'tear-gap', type: 'polygon', silent: true, z: 100,
            shape: { points: top.concat(bottom.slice().reverse()) }, style: { fill: layer } },
          { id: 'tear-top', type: 'polyline', silent: true, z: 101,
            shape: { points: top }, style: { stroke: edge, lineWidth: 1, fill: null } },
          { id: 'tear-bottom', type: 'polyline', silent: true, z: 101,
            shape: { points: bottom }, style: { stroke: edge, lineWidth: 1, fill: null } }
        ] });
      };
      setTimeout(draw, 0);
      new ResizeObserver(() => setTimeout(draw, 50)).observe(el);
      // The theme can change while the app is open: redraw in its colors
      window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => { drawn = ''; setTimeout(draw, 50); });
    }",
    paste(rows, collapse = ", "), grid_left, grid_right
  )
}

# Dumbbell chart ----------------------------------------------------------------

# A grey dot for each country's early value and a blue dot for its latest,
# joined by a line. `data` comes from change_since(), sorted by the latest
# value. `extremes` and `median` as for lollipop_chart(); `median` is of
# the latest values.
dumbbell_chart <- function(data, label, digits = 0, extremes = NULL, median = NULL) {
  # "Around 2000", or the year itself when every start value is from it
  # (e.g. the regional figures, which start in 2005)
  start_name <- if (length(unique(data$start_year)) == 1) as.character(data$start_year[1]) else "Around 2000"
  rows <- split_rows(data[, c("name", "start_year", "start", "end_year", "end")], extremes)
  label_width <- country_label_width(rows$name)
  point <- function(i, which) {
    list(
      value = list(rows[[which]][i], rows$name[i]),
      name = rows$name[i],
      start = rows$start[i], start_year = rows$start_year[i],
      end = rows$end[i], end_year = rows$end_year[i]
    )
  }
  countries <- which(!is.na(rows$end))

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(sprintf(
        "p => { const fmt = %s; const d = p.data;
          return '<strong>' + p.name + '</strong><br/>' + %s + '<br/>' +
            d.start_year + ': ' + fmt(d.start) + '<br/>' + d.end_year + ': ' + fmt(d.end); }",
        tooltip_number_js(digits), js_string(label)
      ))
    )
  # Room above the plot for the legend, and below it for the median line's
  # label
  e$x$opts$grid <- list(left = label_width + 24, right = 24, top = if (is.null(median)) 32 else 52, bottom = 48)
  e$x$opts$yAxis <- list(split_axis(rows, label_width))
  e$x$opts$xAxis <- list(value_axis(label, min = 0))
  e$x$opts$legend <- list(
    show = TRUE, top = 0, right = 0,
    data = list(start_name, "Latest"),
    textStyle = list(color = plot_colors$text)
  )
  latest <- list(type = "scatter", name = "Latest", data = lapply(countries, point, "end"),
                 symbolSize = 9, z = 4, itemStyle = list(color = plot_colors$bar))
  latest$markLine <- median_line(median, digits)
  e$x$opts$series <- list(
    # Connectors: an invisible bar up to the smaller value, then a thin bar
    # spanning the change, stacked (none for the heading and gap rows)
    list(type = "bar", stack = "connector", silent = TRUE, barWidth = 2,
         data = as.list(pmin(rows$start, rows$end)), itemStyle = list(color = "transparent")),
    list(type = "bar", stack = "connector", silent = TRUE, barWidth = 2,
         data = as.list(abs(rows$end - rows$start)), itemStyle = list(color = social_muted)),
    list(type = "scatter", name = start_name, data = lapply(countries, point, "start"),
         symbolSize = 8, z = 3, itemStyle = list(color = light_dark("#8d8d8d", "#a8a8a8"))),
    latest
  )
  e <- htmlwidgets::onRender(e, resize_to_rows_js(nrow(rows), extra = if (is.null(median)) 0 else 20))
  add_tear(e, rows)
}

# Histogram ---------------------------------------------------------------------

# Number of countries in each bin of the latest values. `bins` comes from
# binned_latest(); the tooltip lists the countries in a bin.
histogram_chart <- function(bins, label) {
  bars <- lapply(seq_len(nrow(bins)), function(i) {
    names <- strsplit(bins$countries[i], ", ", fixed = TRUE)[[1]]
    shown <- if (length(names) > 10) {
      paste0(paste(names[1:10], collapse = ", "), sprintf(" and %d more", length(names) - 10))
    } else {
      paste(names, collapse = ", ")
    }
    list(value = bins$count[i], countries = shown)
  })

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        "p => '<strong>' + p.name + '</strong><br/>' + p.value + (p.value === 1 ? ' country' : ' countries') +
          (p.data.countries ? '<br/><span style=\"display:inline-block;max-width:16rem;white-space:normal\">' + p.data.countries + '</span>' : '')"
      )
    )
  e$x$opts$grid <- list(left = 48, right = 16, top = 16, bottom = 48)
  e$x$opts$xAxis <- list(list(
    type = "category",
    data = as.list(bins$label),
    name = label,
    nameLocation = "middle",
    nameGap = 30,
    nameTextStyle = list(color = plot_colors$text),
    axisLabel = list(color = plot_colors$text),
    axisLine = list(lineStyle = list(color = plot_colors$grid))
  ))
  e$x$opts$yAxis <- list(value_axis("Countries", minInterval = 1, scale = FALSE, nameGap = 32))
  e$x$opts$series <- list(list(type = "bar", data = bars, barCategoryGap = "8%"))
  e
}

# Trend lines -------------------------------------------------------------------

# A faint line per country and the regional median in blue. `trends` comes
# from yearly_trends(). Hover a line to see which country it is.
#
# With `colors` (one per line, e.g. for the seven regions from
# region_trends()), each line gets its own color and a legend entry, and
# the reference line, named `reference`, is drawn in highlight_color.
trend_chart <- function(trends, label, digits = 1, reference = "Regional median", colors = NULL) {
  colored <- !is.null(colors)
  reference_color <- if (colored) highlight_color else plot_colors$bar
  line_points <- function(d) lapply(seq_len(nrow(d)), \(i) list(d$year[i], d$value[i]))
  countries <- split(trends$series, trends$series$name)

  # Lines have a point per year that's invisible until hovered: hovering one
  # shows the tooltip and highlights that country's line
  hover_points <- list(
    symbol = "circle",
    symbolSize = 7,
    showSymbol = TRUE,
    itemStyle = list(opacity = 0)
  )

  country_series <- lapply(seq_along(countries), function(i) {
    name <- names(countries)[i]
    color <- if (colored) colors[(i - 1) %% length(colors) + 1] else social_muted
    hover_color <- if (colored) color else highlight_color
    c(
      list(
        type = "line",
        name = name,
        data = line_points(countries[[name]]),
        # The series color shows in the legend and tooltip marker
        itemStyle = list(color = color, opacity = 0),
        lineStyle = if (colored) list(width = 2, color = color) else list(width = 1, color = color, opacity = 0.5),
        emphasis = list(
          focus = "series",
          lineStyle = list(width = if (colored) 3 else 2, color = hover_color, opacity = 1),
          itemStyle = list(opacity = 1, color = hover_color)
        )
      ),
      hover_points[setdiff(names(hover_points), "itemStyle")]
    )
  })
  median_series <- c(
    list(
      type = "line",
      name = reference,
      data = line_points(trends$median),
      z = 3,
      itemStyle = list(color = reference_color, opacity = 0),
      lineStyle = list(width = 3, color = reference_color),
      emphasis = list(itemStyle = list(opacity = 1, color = reference_color))
    ),
    hover_points[setdiff(names(hover_points), "itemStyle")]
  )

  e <- social_base_chart() |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(sprintf(
        "p => { const fmt = %s;
          return '<strong>' + p.seriesName + '</strong><br/>' + p.value[0] + ': ' + fmt(p.value[1]); }",
        tooltip_number_js(digits)
      ))
    )
  years <- range(c(trends$series$year, trends$median$year))
  # A legend entry per line goes below the chart, where it can wrap
  e$x$opts$grid <- list(left = 48, right = 24, top = 32, bottom = if (colored) 88 else 32)
  e$x$opts$xAxis <- list(value_axis(
    min = years[1], max = years[2], scale = FALSE, minInterval = 1,
    # Years without a thousands separator
    axisLabel = list(color = plot_colors$text, formatter = htmlwidgets::JS("v => String(v)"))
  ))
  e$x$opts$yAxis <- list(value_axis(label, nameLocation = "end", nameGap = 12))
  e$x$opts$legend <- if (colored) {
    list(
      show = TRUE, bottom = 0, left = 0, right = 0,
      data = as.list(c(names(countries), reference)),
      textStyle = list(color = plot_colors$text)
    )
  } else {
    list(
      show = TRUE, top = 0, right = 0, data = list(reference),
      textStyle = list(color = plot_colors$text)
    )
  }
  e$x$opts$series <- c(country_series, list(median_series))
  e
}

# A JS string literal for `x`, for building JS code
js_string <- function(x) {
  paste0("'", gsub("'", "\\\\'", gsub("\\\\", "\\\\\\\\", x)), "'")
}
