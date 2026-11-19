# Keep the disk cache (see disk_cache()) in a temporary folder for the test run,
# so tests never read (or overwrite) the real cached data
withr::local_envvar(
  WORLDDATA_CACHE_DIR = tempfile("worlddata-cache-"),
  .local_envir = testthat::teardown_env()
)
