# Start a test with empty API caches, and empty them again afterwards, so
# mocked responses are always used and nothing leaks between tests
local_empty_cache <- function(env = parent.frame()) {
  reset <- function() {
    api_cache$reset()
    wb_cache$reset()
    disk_cache()$reset()
  }
  reset()
  withr::defer(reset(), envir = env)
}
