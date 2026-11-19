# DESIGN.md

Design rules for World Data Dashboard. They exist so the app looks like a Carbon product: the spacing and typography of the Carbon Design System documentation (reference screenshots in `.claude/design-refs/`), applied to our dark (g100) theme. Read this before changing UI code or `www/styles.css`.

The reference screenshots are light theme and captured at 1.5× scale (Carbon's 48px header is 72px in them); measurements below are converted to CSS pixels. Only their spacing and typography apply here: colors come from our theme, and we don't need every component they show.

## Principles

1. **Tokens, not numbers.** Every font size, weight, line height, letter spacing, margin, padding and gap comes from a Carbon token (`--cds-*` custom properties, which carbonite's CSS defines). A literal `px`/`rem` value in `styles.css` needs a comment saying why no token fits (e.g. an image's height).
2. **Let carbonite components style themselves.** Don't restyle the inside of `cds-*` elements. Space them from their neighbors; don't change their padding, fonts or heights.
3. **Room between things, density inside them.** Components (tables, inputs, accordions) stay compact, as Carbon designs them. The space *around* components and between groups is generous: when unsure, use the next step up the spacing scale.
4. **One typeface.** IBM Plex Sans for everything, charts included; IBM Plex Mono only for code. No Plex Serif (the Carbon website uses it for expressive marketing titles; this is a product UI).

## Typography

carbonite loads IBM Plex Sans (300, 400, 600 and italics), Plex Serif and Plex Mono, and sets Plex Sans on `body`. Use these roles, and only these:

| Role | Token | Size / line height | Weight | Used for |
|---|---|---|---|---|
| Page title | `heading-06` | 42 / 50px | 300 (Light) | One `h1` per page (`.page-title`) |
| Page lead | `body-02` | 16 / 24px | 400 | The intro paragraph under a page title (`.page-lead`), in `--cds-text-secondary`. Chosen over 20px (`heading-03`), which looked too large |
| Section heading | `heading-05` | 32 / 40px | 400 | `h2` sections, e.g. "Using the app" |
| Subsection / tile title | `heading-03` | 20 / 28px | 400 | `h3`, tile titles (`.tile-title`) |
| Small heading | `heading-02` | 16 / 24px | 600 | Headings inside dense content (profile modal sections) |
| Prose | `body-02` | 16 / 24px | 400 | Paragraphs in tiles and pages |
| UI text | `body-01` | 14 / 20px, 0.16px | 400 | Tables, accordion content, lists in tiles, chart questions |
| Caption | `helper-text-01` | 12 / 16px, 0.32px | 400 | Chart captions, sources, notes; `--cds-text-secondary` |
| Label | `label-01` | 12 / 16px, 0.32px | 400 | Form labels (carbonite handles these) |

Rules:

- **Apply a role with Carbon's type class**, `cds--type-<token>` (carbonite includes them): it sets the size, weight, line height and letter spacing together, e.g. `htmltools::h3("Inflation", class = "cds--type-heading-03")`. Never set a font size on its own: carbonite's reset styles `h3` as `heading-04` (28px, line height 1.29), so `font-size: 1.25rem` alone gives a 20px title with a 25.7px line instead of `heading-03`'s 28px.
- **In CSS** (when a class can't be added, e.g. text the app doesn't render itself), use all four token properties:

  ```css
  .tile-title {
    font-size: var(--cds-heading-03-font-size);
    font-weight: var(--cds-heading-03-font-weight);
    line-height: var(--cds-heading-03-line-height);
    letter-spacing: var(--cds-heading-03-letter-spacing);
  }
  ```

- **Weights are 300, 400 and 600 only.** Never `bold`/700: browsers fake it because carbonite doesn't load Plex Sans Bold. carbonite's reset makes `strong` 600; use `strong`, not `b`.
- **Sentence case** for every title, heading, label, tab and button ("Economic indicators", not "Economic Indicators").
- **Headings step down one role at a time**: a page title is followed by sections, sections by subsections. Don't use heading tags for size; use the role's class.
- **Line length**: prose and leads are at most `40rem` wide (about 70–80 characters), as in the references. Tables and charts may be wider.
- **Secondary text** (leads, chart questions, captions) uses `--cds-text-secondary`; helper notes may use `--cds-text-helper`. Body text uses the default `--cds-text-primary`.
- **Charts use the same type.** ECharts defaults to the system sans-serif (e.g. Microsoft YaHei on Windows), so every chart sets `textStyle.fontFamily` to `"IBM Plex Sans", system-ui, sans-serif` through the shared base chart. Axis and legend labels are 12px (`label-01`); tooltips are 14px (`body-01`).

## Spacing

Carbon's spacing scale (`--cds-spacing-*`). Use these steps only:

| Token | Value | Token | Value |
|---|---|---|---|
| `spacing-01` | 2px | `spacing-07` | 32px |
| `spacing-02` | 4px | `spacing-08` | 40px |
| `spacing-03` | 8px | `spacing-09` | 48px |
| `spacing-04` | 12px | `spacing-10` | 64px |
| `spacing-05` | 16px | `spacing-11` | 80px |
| `spacing-06` | 24px | `spacing-12` | 96px |

### How much space, by relationship

The closer two things are related, the less space between them. These are minimums:

| Between | Space |
|---|---|
| Parts of one element (icon and its label, title and its subtitle) | `spacing-02`–`spacing-03` (4–8px) |
| Elements in one component or tile (title → question → chart → caption) | `spacing-03`–`spacing-05` (8–16px) |
| Components in one group (an input above the table it controls; tiles in a grid) | `spacing-06`–`spacing-07` (24–32px) |
| Groups within a section | `spacing-07`–`spacing-08` (32–40px) |
| A page's intro (title, lead) → its content | `spacing-09` (48px) |
| Sections of a page (`h2` sections) | `spacing-10` (64px) above the heading |

Never put two unrelated blocks closer than `spacing-05` (16px), and never let text touch the edge of its container.

### Page layout

| Measurement | Value | Reference |
|---|---|---|
| Header → page title | `spacing-07` (32px), set by `carbon_page()` | Title band in screenshots 2 and 3 |
| Page title → lead | `spacing-06` (24px) | Chosen by eye: 16px looked cramped, 32px more than needed |
| Lead → first content | `spacing-09` (48px) | Chosen by eye: 32px looked cramped, 64px more than needed; close to screenshot 2's ~40px |
| Title → tags under it (`page_heading(tags = )`) | `spacing-03` (8px) | The tags belong to the title |
| Tags → lead | `spacing-06` (24px) | Same as title → lead |
| Lead → switcher under it | `spacing-05` (16px) | |
| Tabs → tab content | `spacing-06` (24px) | |
| Callout row (At a glance) → tabs | `spacing-08` (40px) | A group within the page |
| Section heading: space above / below | `spacing-10` (64px) / `spacing-06` (24px) | "Overview" and "Color" in screenshots 2 and 3 |
| Subsection heading: space above / below | `spacing-07` (32px) / `spacing-05` (16px) | |
| Paragraph → paragraph | `spacing-05` (16px) | |
| Bottom of the page | `spacing-10` (64px) after the last content | `carbon_page()` gives 32px; add the rest |

Use Carbon's 16-column grid (`carbon_grid()`/`carbon_column()`) for layout, and align page titles, leads and section headings with the left edge of the grid's content, i.e. the same edge as the tiles and tables below them.

### Tiles and grids

- **Tiles in a grid have the same gap in both directions**: the vertical gap between rows matches the grid's 32px gutter. Use `carbon_grid(row_gap = 7)` (`spacing-07`), not margins on the tiles.
- **Tiles in one row are the same height** (`height: 100%` of the column).
- **Inside a tile**: Carbon's tile padding (`spacing-05`, 16px) is the minimum. Title → question/description `spacing-03` (8px); description → content (chart, list, links) `spacing-06` (24px); content → caption `spacing-05` (16px).
- **Tags, buttons and links in a row** are `spacing-03` (8px) apart, with `spacing-05` (16px) between the row and the blocks above and below.

### Tables and lists

- Carbon tables and structured lists set their own row heights and cell padding; don't override them.
- A control that affects a table (e.g. "Table columns") sits `spacing-06` (24px) above it.
- Bulleted lists in prose: `spacing-03` (8px) between items, indented by `spacing-05` (16px).

## Color

- **Use theme tokens for UI color**: `--cds-text-primary`, `--cds-text-secondary`, `--cds-text-helper`, `--cds-layer-01`, `--cds-border-subtle-01` and so on. No hex values in `styles.css` except as the token's fallback, e.g. `var(--cds-text-secondary, #c6c6c6)`.
- **Chart colors** live in `plot_colors`, `series_colors` and the treemap and heatmap palettes in `R/plots*.R`. They are Carbon g100 values, as echarts can't read CSS variables. Keep them there; don't add new ones elsewhere.

## Building blocks

Use these helpers rather than styling pages by hand; they apply the rules above.

| Helper | Gives |
|---|---|
| `page_heading(title, lead, ..., tags = )` (`R/utils.R`) | The page title, optional tags right under it, the lead, anything else below the lead (e.g. a switcher), and the 48px before the page's content |
| `page_section(...)` and `section_heading(text)` (`R/utils.R`) | A section in its own grid, 64px below what's above, starting with an `h2` |
| `indicator_page(heading, ...)` (`R/indicator_page.R`) | A heading, then a grid of tiles with 32px between rows and columns |
| `tile_title(text, icon)` (`R/utils.R`) | A tile title with its icon, in `heading-03` |
| `callout_tile(callout)` (`R/mod_glance.R`) | A headline figure: `label-01` label, `heading-06` number with a `body-02` unit, `helper-text-01` note and year |
| `div(class = "tab-content", ...)` | A tab's content, 24px below the tabs |
| `plot_font()`, `plot_tooltip()` (`R/plots.R`) | IBM Plex Sans in a chart (12px labels, 14px tooltips); every chart builder calls both |

## Checking a change

1. Measure, don't eyeball: in the browser (or a headless Chrome check), read the computed `font-size`, `line-height`, `font-weight` and the gaps between elements' bounding boxes, and compare them with the tables above.
2. Check every page: At a Glance (both tabs), Social indicators, Economic indicators (Countries and Regions views), Comparisons and About, at desktop width and at phone width (~400px). Phone emulation works: `carbon_page()` sets the viewport meta tag. Wide tables scroll sideways inside their own container; the page itself shouldn't.
3. Check fonts: the computed `font-family` of chart text and of any `strong` text should resolve to IBM Plex Sans at 300, 400 or 600.
4. Some spacing belongs to carbonite components and is left alone, e.g. a multiselect sits 2px above its container's content edge.
