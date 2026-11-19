# CLAUDE.md

## Project Overview
A demo Shiny application showcasing IBM Carbon Design System components (via our custom Carbon Shiny bindings package) by building an interactive explorer of the world's countries.
- **R Stack:** R, `shiny`, `carbonite`, `httr2`, `jsonlite`, `testthat`, `shinytest2`
- **External APIs (no keys needed):** World Bank Indicators API v2 (country list, regions and all indicators), Wikidata SPARQL (languages, flags, Wikipedia links), Wikipedia Action API (country summaries)

## Common Commands
- **Run the Shiny app locally:** `Rscript -e "shiny::runApp()"`
- **Install/update local Carbon package dependency:** `Rscript -e "devtools::install('../path-to-carbon-pkg')"` (or `renv::restore()`)
- **Run all tests:** `Rscript -e "devtools::test()"` (or `Rscript -e "testthat::test_dir('tests/testthat')"`)
- **Run end-to-end UI tests:** `Rscript -e "shinytest2::test_app()"`
- **Format R code:** `Rscript -e "styler::style_dir()"`

## Architecture & Directory Structure
- `app.R` — Main application entry point assembling the Carbon UI shell (`cds-header`, side nav, grid layout) and top-level server logic
- `R/api_countries.R` — the country list (World Bank economies joined with Wikidata details) and region filtering
- `R/api_worldbank.R`, `R/api_wikidata.R`, `R/api_wikipedia.R` — `httr2` clients for each API; `R/api_utils.R` — shared caching and error handling
- `R/mod_*.R` — Modular Shiny UI and server pairs (e.g., `mod_search_filters.R`, `mod_country_table.R`, `mod_country_detail.R`)
- `www/` — Custom app-specific static assets, favicons, or minor layout overrides. Link to them as `www/<file>` (the resource path `ui()` registers), never `<file>` at the root: Shiny only serves `www/` at the root when started on the app folder, so root links break when the app runs from an IDE or an app object.
- `tests/testthat/` — Unit tests for API response parsing, reactive module logic (`shiny::testServer()`), and `shinytest2` browser tests

## Code Style & Conventions
### Carbon UI & Shiny Modules
- **Package-First UI:** Always use components from `carbonite` (e.g., Carbon inputs, dropdowns, data tables, tiles, notifications, and modals) rather than raw `shiny::*` inputs or Bootstrap elements.
- **Modularize Features:** Structure UI and server sections as Shiny modules (`mod_<name>_ui()` and `mod_<name>_server()`) using `shiny::NS()`.
- **Carbon Grid Layout:** Use Carbon's 16-column grid classes/wrappers for responsive page layout.
- **Design rules:** Follow `DESIGN.md` for typography and spacing (Carbon type and spacing tokens) whenever changing UI code or `www/styles.css`.

### API (`httr2`) Conventions
- **Join by ISO3 code:** Countries are identified by ISO 3166-1 alpha-3 code (`iso3`) across the World Bank, Wikidata and the app. Regions are World Bank region codes (`EAS`, `SSF`, ...), which are also the codes of the World Bank's regional aggregates.
- **World Bank errors arrive as HTTP 200:** The World Bank API returns `[{message: ...}]` with status 200 on errors; check bodies with `check_wb_body()`. Fetch indicators for `country/all` (one request covers every region) and filter locally.
- **Go through `perform_json()`:** It catches HTTP errors (`httr2::req_error(is_error = \(resp) FALSE)`) and turns failures into `api_error` conditions with user-facing messages. Wikimedia APIs need `wikimedia_user_agent`.
- **Surface Errors in Carbon UI:** Display API timeouts, network failures, or empty search states using Carbon inline notifications or empty-state tiles instead of raw R error text.
- **Cache API Responses:** The country list is kept on disk for a day with `cached_daily()` (which keeps serving the old copy if a refresh fails); other responses use the in-memory `cachem` caches, so filter changes do not trigger redundant network requests.

## Workflow & Verification Rules
1. Never hardcode absolute local file paths; use `here::here()` or relative paths from the app root.
2. Keep API data-fetching and JSON-parsing logic in the `R/api_*.R` files as pure functions so they can be unit-tested without starting a Shiny server.
3. When adding a new filter or view, add a corresponding `testthat` test mocking the `httr2` response (or testing `shiny::testServer()`) to verify reactive updates.
4. Do not commit `.Renviron`, `rsconnect/` deployment secrets, or local cache files.
