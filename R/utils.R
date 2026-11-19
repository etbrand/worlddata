# Like `%||%`, which base R only has from 4.4
if_null <- function(x, default) {
  if (is.null(x)) default else x
}

# The app's mark: a globe (meridian and equator) with a rising bar chart in
# front of its lower right; the globe's lines stop just around the bars. On
# Carbon's 32px icon grid. www/favicon.svg, www/favicon.ico and
# www/apple-touch-icon.png are the same drawing.
app_logo_shapes <- paste0(
  '<defs><mask id="app-logo-cut" maskUnits="userSpaceOnUse" x="0" y="0" width="32" height="32">',
  '<rect width="32" height="32" fill="white"/><g fill="black">',
  '<rect x="16.5" y="22.5" width="6" height="9"/><rect x="21" y="19" width="6" height="12.5"/>',
  '<rect x="25.5" y="15.5" width="6" height="16"/></g></mask></defs>',
  '<g mask="url(#app-logo-cut)" fill="none" stroke="currentColor" stroke-width="2">',
  '<circle cx="13" cy="13" r="11"/><ellipse cx="13" cy="13" rx="4.5" ry="11"/>',
  '<line x1="2" y1="13" x2="24" y2="13"/></g>',
  '<rect x="18" y="24" width="3" height="6"/><rect x="22.5" y="20.5" width="3" height="9.5"/>',
  '<rect x="27" y="17" width="3" height="13"/>'
)

# The mark inline, in the surrounding text's color (e.g. before the app name
# in the header). Decorative: the text next to it names the app.
app_logo <- function(size = 20) {
  htmltools::HTML(sprintf(
    '<svg class="app-logo" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 32" width="%d" height="%d" fill="currentColor" aria-hidden="true" focusable="false">%s</svg>',
    size, size, app_logo_shapes
  ))
}

# Tile heading with a leading icon, in Carbon's heading-03 type style
# (DESIGN.md)
tile_title <- function(text, icon, level = "h2") {
  htmltools::tag(level, list(
    class = "tile-title with-icon cds--type-heading-03",
    carbonite::carbon_icon(icon, size = 20),
    htmltools::span(text)
  ))
}

# Tab label with a leading icon
tab_label <- function(text, icon) {
  htmltools::span(
    class = "with-icon",
    carbonite::carbon_icon(icon),
    htmltools::span(text)
  )
}

print2 <- function(...) {
  cat(file = stderr(), paste0(..., "\n"))
}

# A tile explaining why something can't be shown, e.g. an API error or a
# chart with no data
empty_state_tile <- function(title, message, icon = "information") {
  carbonite::carbon_tile(
    class = "empty-state-tile",
    tile_title(title, icon, level = "h3"),
    htmltools::p(class = "cds--type-body-01", message)
  )
}

# A section of a page: carbon_columns (`...`) in their own grid, spaced
# from what's above by spacing-10 (DESIGN.md). Start it with
# section_heading().
page_section <- function(...) {
  htmltools::div(class = "page-section", carbonite::carbon_grid(...))
}

# A section's h2, in Carbon's heading-05 type style
section_heading <- function(text) {
  htmltools::h2(text, class = "section-heading cds--type-heading-05")
}

# Page title and lead (intro paragraph), styled per DESIGN.md. `lead`: the
# paragraph's text or tags; `...`: anything else that belongs with the
# heading, below the lead (e.g. tags or a switcher).
page_heading <- function(title, lead, ..., tags = NULL) {
  htmltools::div(
    class = "page-heading",
    htmltools::h1(title, class = "page-title cds--type-heading-06"),
    # `tags` (e.g. carbon_tag()s): right under the title, before the lead
    if (!is.null(tags)) htmltools::div(class = "page-heading__tags", tags),
    htmltools::p(class = "page-lead cds--type-body-02", lead),
    ...
  )
}
