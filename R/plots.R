# Chart colors follow the app's light or dark theme (DESIGN.md). ECharts
# draws on a canvas and can't use CSS, so colors are written as either
#   var(--cds-...)          a Carbon theme token, or
#   light-dark(#..., #...)  a pair, for colors Carbon has no token for
#                           (e.g. its data-visualisation palettes)
# and plot_theme_js() turns them into real colors when a chart renders.
light_dark <- function(light, dark) {
  # The pair is split at its comma when resolved, so neither color may
  # contain one: use hex (with alpha for transparency, e.g. "#4589ff40"),
  # not rgba()
  if (any(grepl(",", c(light, dark), fixed = TRUE))) {
    stop("light_dark() colors can't contain commas; use hex instead of rgba().", call. = FALSE)
  }
  sprintf("light-dark(%s, %s)", light, dark)
}

plot_colors <- list(
  # Carbon's data-visualisation blue: blue 60 on light, blue 50 on dark
  bar = light_dark("#0f62fe", "#4589ff"),
  text = "var(--cds-text-secondary)",
  grid = "var(--cds-border-subtle-01)",
  tooltip_bg = "var(--cds-layer-01)",
  tooltip_text = "var(--cds-text-primary)",
  # Carbon's categorical palettes, light then dark, in their recommended order
  categorical = light_dark(
    c("#6929c4", "#1192e8", "#005d5d", "#9f1853", "#fa4d56", "#570408", "#198038",
      "#002d9c", "#ee538b", "#b28600", "#009d9a", "#012749", "#8a3800", "#a56eff"),
    c("#8a3ffc", "#33b1ff", "#007d79", "#ff7eb6", "#fa4d56", "#fff1f1", "#6fdc8c",
      "#4589ff", "#d12771", "#d2a106", "#08bdba", "#bae6ff", "#ba4e00", "#d4bbff")
  )
)

# Carbon's typeface for chart text (see DESIGN.md). ECharts draws on a
# canvas and defaults to the system sans-serif, not the page's font.
plot_font_family <- "'IBM Plex Sans', system-ui, -apple-system, sans-serif"

# Every chart's shared setup: text in IBM Plex Sans (labels at label-01
# size, 12px; tooltips at body-01 size, 14px, see plot_tooltip()) and colors
# for the current theme (plot_theme_js())
plot_theme <- function(e) {
  e$x$opts$textStyle <- list(fontFamily = plot_font_family, fontSize = 12)
  htmlwidgets::onRender(e, plot_theme_js)
}

# JS for htmlwidgets::onRender(): replace the var() and light-dark() colors
# anywhere in the chart's options with real colors for the current theme,
# and again if the system's light/dark setting changes. Dark means a dark
# Carbon background (--cds-background).
plot_theme_js <- "function(el) {
  const chart = echarts.getInstanceByDom(el);
  if (!chart) return;
  const isDark = () => {
    const rgb = getComputedStyle(document.body).backgroundColor.match(/\\d+/g) || [255, 255, 255];
    return (0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2]) < 128;
  };
  const resolve = (value, dark) => {
    if (typeof value === 'string') {
      const token = value.match(/^var\\((--[\\w-]+)\\)$/);
      if (token) return getComputedStyle(document.documentElement).getPropertyValue(token[1]).trim() || value;
      const pair = value.match(/^light-dark\\((.+), (.+)\\)$/);
      if (pair) return dark ? pair[2] : pair[1];
      return value;
    }
    if (Array.isArray(value)) return value.map(v => resolve(v, dark));
    if (value && typeof value === 'object' && !(value instanceof Function)) {
      const out = {};
      for (const key in value) out[key] = resolve(value[key], dark);
      return out;
    }
    return value;
  };
  const original = chart.getOption();
  const apply = () => chart.setOption(resolve(original, isDark()));
  apply();
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', () => setTimeout(apply, 0));
}"

plot_tooltip <- function(e, ...) {
  echarts4r::e_tooltip(
    e,
    ...,
    backgroundColor = plot_colors$tooltip_bg,
    borderColor = plot_colors$grid,
    textStyle = list(color = plot_colors$tooltip_text, fontFamily = plot_font_family, fontSize = 14)
  )
}

# Charts with a row per country ------------------------------------------------
#
# Sizing shared by charts with one row per country (the Social page's
# lollipop and dumbbell charts): the height grows with the number of
# countries, and long names are truncated (the tooltip shows the full name).

row_layout <- list(
  row_height = 14,   # px per country; the largest region (~60) is ~890px
  margin = 48,       # top + bottom space, including x-axis labels
  min_height = 280,  # px, so charts with few rows aren't squashed
  font_size = 12,    # label-01, like every chart label (DESIGN.md)
  char_width = 7,    # approx px per character of IBM Plex Sans at `font_size`
  max_label_chars = 24
)

rows_chart_height <- function(n, extra = 0) {
  # A few rows (e.g. the seven regions) get more room each
  max(n * row_layout$row_height + row_layout$margin, row_layout$min_height) + extra
}

# Width for country names on a category axis. echarts' `containLabel` only
# measures a sample of labels once there are more than ~40, so long names
# can be clipped; size the margin from the longest (truncated) name instead.
country_label_width <- function(names) {
  label_chars <- min(max(nchar(names), 0), row_layout$max_label_chars)
  ceiling(label_chars * row_layout$char_width)
}

# Axis label settings for country names, truncated to `width`
country_axis_label <- function(width) {
  list(
    interval = 0,
    color = plot_colors$text,
    fontSize = row_layout$font_size,
    width = width,
    overflow = "truncate",
    ellipsis = "…"
  )
}

# JS for htmlwidgets::onRender(): size the chart for `n` country rows, plus
# `extra` px (e.g. for a legend).
# Shiny ignores a widget's own height, so resize the output after rendering.
resize_to_rows_js <- function(n, extra = 0) {
  sprintf(
    "function(el) {
      el.style.height = '%dpx';
      const chart = echarts.getInstanceByDom(el);
      if (chart) chart.resize();
    }",
    rows_chart_height(n, extra)
  )
}

# Language word cloud ---------------------------------------------------------

# Number of countries in which each language is spoken, most common first.
# All listed languages count: the API lists them roughly alphabetically, so
# there's no reliable "primary" language to pick.
language_cloud_data <- function(data) {
  langs <- strsplit(data$languages[!is.na(data$languages)], ", ", fixed = TRUE)
  # factor() so an empty vector gives an empty table rather than an error
  counts <- table(factor(unlist(langs)))

  out <- data.frame(
    language = as.character(names(counts)),
    countries = as.integer(counts)
  )
  out[order(-out$countries, out$language), , drop = FALSE]
}

language_cloud <- function(data) {
  # The color function runs inside ECharts, where plot_theme_js() can't
  # reach, so it picks the light or dark palette itself
  palette <- sprintf(
    "(() => { const all = ['%s']; const rgb = getComputedStyle(document.body).backgroundColor.match(/\\d+/g) || [255, 255, 255]; const dark = (0.299 * rgb[0] + 0.587 * rgb[1] + 0.114 * rgb[2]) < 128; return all.map(c => c.match(/^light-dark\\((.+), (.+)\\)$/)[dark ? 2 : 1]); })()",
    paste(plot_colors$categorical, collapse = "', '")
  )

  language_cloud_data(data) |>
    echarts4r::e_charts() |>
    echarts4r::e_cloud(
      language,
      countries,
      shape = "circle",
      sizeRange = c(22, 128),
      rotationRange = c(0, 0),
      textStyle = list(
        fontFamily = plot_font_family,
        # Color by position so colors stay the same between renders
        color = htmlwidgets::JS(sprintf(
          "function(p) { const c = %s; return c[p.dataIndex %% c.length]; }",
          palette
        ))
      )
    ) |>
    plot_tooltip(
      trigger = "item",
      formatter = htmlwidgets::JS(
        "p => p.name + ': ' + p.value + (p.value === 1 ? ' country' : ' countries')"
      )
    ) |>
    echarts4r::e_color(background = "transparent") |>
    plot_theme()
}

# Indicator scatter -----------------------------------------------------------
#
# Bubble chart of two World Bank indicators, one bubble per country sized by
# population. indicator_scatter_chart() draws it; when only the indicators
# change, indicator_scatter_update() gives the options to merge into the
# existing chart (echarts4r::e_merge()) so the bubbles animate to their new
# positions. Both use the same points and axes.

# Points for `data` from indicator_scatter_data(), in its (stable) row order.
# Each value is [x, y, population, bubble size]; x and y are NA (sent as
# null, so not drawn) where a value is missing or can't go on a log axis.
indicator_scatter_points <- function(data, x_key, y_key) {
  x <- data$x
  y <- data$y
  if (wb_indicators[[x_key]]$log) x[!is.na(x) & x <= 0] <- NA
  if (wb_indicators[[y_key]]$log) y[!is.na(y) & y <= 0] <- NA

  population <- data$population
  population[is.na(population)] <- 0
  # Area grows with population, so the radius grows with its square root
  size <- 6 + 34 * sqrt(population / max(population, 1))

  lapply(seq_len(nrow(data)), function(i) {
    list(
      id = data$iso3[i],
      name = data$name[i],
      value = list(x[i], y[i], population[i], size[i])
    )
  })
}

# The parts of the axes that depend on the indicators
indicator_scatter_axis <- function(key) {
  indicator <- wb_indicators[[key]]
  list(type = if (indicator$log) "log" else "value", name = indicator$label)
}

indicator_scatter_update <- function(data, x_key, y_key) {
  list(
    series = list(list(data = indicator_scatter_points(data, x_key, y_key))),
    xAxis = list(indicator_scatter_axis(x_key)),
    yAxis = list(indicator_scatter_axis(y_key))
  )
}

# `chart_id` is the output's element id: the tooltip reads the current axis
# names from the chart, since a proxy update can't send a new JS formatter.
# `labels`: name each bubble on the chart, for a handful of bubbles (the
# seven regions); with many countries the labels would bury the chart.
indicator_scatter_chart <- function(data, x_key, y_key, chart_id, labels = FALSE) {
  compact_number <- htmlwidgets::JS(
    "function(v) {
      if (v >= 1e9) return (v / 1e9) + 'B';
      if (v >= 1e6) return (v / 1e6) + 'M';
      if (v >= 1e3) return (v / 1e3) + 'K';
      return v;
    }"
  )

  axis <- function(key, ...) {
    c(
      indicator_scatter_axis(key),
      list(
        nameLocation = "middle",
        nameTextStyle = list(color = plot_colors$text),
        # Don't force the axis to start at zero
        scale = TRUE,
        axisLabel = list(color = plot_colors$text, formatter = compact_number),
        axisLine = list(lineStyle = list(color = plot_colors$grid)),
        splitLine = list(lineStyle = list(color = plot_colors$grid)),
        ...
      )
    )
  }

  tooltip <- htmlwidgets::JS(sprintf(
    "p => {
      const opts = echarts.getInstanceByDom(document.getElementById('%s')).getOption();
      const fmt = v => Number(v).toLocaleString(undefined, { maximumFractionDigits: 1 });
      return '<strong>' + p.name + '</strong><br/>' +
        opts.xAxis[0].name + ': ' + fmt(p.value[0]) + '<br/>' +
        opts.yAxis[0].name + ': ' + fmt(p.value[1]) + '<br/>' +
        'Population: ' + Number(p.value[2]).toLocaleString();
    }",
    chart_id
  ))

  e <- echarts4r::e_charts() |>
    # With labels, more room above and to the right, where labels can run
    # past the outermost bubbles
    echarts4r::e_grid(left = 72, right = if (labels) 96 else 32, top = if (labels) 40 else 24, bottom = 56) |>
    plot_tooltip(trigger = "item", formatter = tooltip) |>
    echarts4r::e_legend(show = FALSE) |>
    echarts4r::e_color(plot_colors$bar, background = "transparent") |>
    plot_theme()

  e$x$opts$xAxis <- list(axis(x_key, nameGap = 32))
  e$x$opts$yAxis <- list(axis(y_key, nameGap = 52))
  series <- list(
    type = "scatter",
    data = indicator_scatter_points(data, x_key, y_key),
    symbolSize = htmlwidgets::JS("function(value) { return value[3]; }"),
    itemStyle = list(opacity = 0.7, borderColor = "var(--cds-background)", borderWidth = 1)
  )
  if (labels) {
    # The bubble's name above it (label-01). The label settings stay in
    # place when a proxy update moves the bubbles.
    series$label <- list(
      show = TRUE,
      position = "top",
      distance = 6,
      formatter = "{b}",
      color = plot_colors$text,
      fontSize = 12
    )
    # Nudge labels apart vertically where neighbouring bubbles' names collide
    series$labelLayout <- list(moveOverlap = "shiftY")
  }
  e$x$opts$series <- list(series)
  # Slower than the default so the bubbles can be followed to their new place
  e$x$opts$animationDurationUpdate <- 800
  e
}
