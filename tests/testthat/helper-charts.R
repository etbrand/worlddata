# The code of the first htmlwidgets::onRender() hook of chart `e` that
# contains `text`, or NULL if none does. Charts have several hooks (the
# shared theme colors first, then their own), so tests find them by content.
render_hook <- function(e, text) {
  codes <- vapply(e$jsHooks$render, `[[`, "", "code")
  match <- codes[grepl(text, codes, fixed = TRUE)]
  if (length(match)) match[[1]]
}
