# A notice at the top of a page when the country list behind every page
# couldn't be loaded (e.g. the World Bank API is down and there's no copy
# on disk yet), with a button to try again. Empty otherwise. Each page has
# its own instance, as an output can only appear once.

mod_data_status_ui <- function(id) {
  ns <- shiny::NS(id)
  carbonite::carbon_grid(
    carbonite::carbon_column(width = 16, shiny::uiOutput(ns("status")))
  )
}

# `result`: reactive country list from fetch_data(), or an api_error.
# `retry`: function called when "Try again" is clicked.
mod_data_status_server <- function(id, result, retry) {
  shiny::moduleServer(id, function(input, output, session) {
    output$status <- shiny::renderUI({
      error <- result()
      if (!inherits(error, "api_error")) {
        return(NULL)
      }
      data_status_notice(conditionMessage(error), session$ns("retry"))
    })

    shiny::observeEvent(input$retry, retry())
  })
}

# The notice itself: an error notification and a "Try again" button below
# it, spaced from the page heading under them (see .data-status in
# www/styles.css)
data_status_notice <- function(message, retry_id) {
  htmltools::div(
    class = "data-status",
    carbonite::carbon_inline_notification(
      title = "Couldn't load the country data",
      subtitle = paste(message, "The pages will fill in once it loads."),
      kind = "error",
      close_button = FALSE
    ),
    carbonite::carbon_button(retry_id, "Try again", icon = "renew", kind = "tertiary", size = "sm")
  )
}
