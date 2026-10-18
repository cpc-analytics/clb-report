# Design language

The report follows the WHO Data Design Language. Colors, font and text sizes are defined once and reused:

| Where | What |
|---|---|
| `R/plot-theme.R` | color tokens (`collab$...`), palettes, region colors, text sizes, chart themes |
| `assets/styles.css` | the same colors as CSS variables, page typography, KPI cards |
| `R/table-theme.R` | table styles (`kable_collab()`, gt theme) |

To change a color, edit it in `R/plot-theme.R` and in the `:root` block of `assets/styles.css`.

## Font

Noto Sans: regular 400, semibold 600, bold 700.

## Colors

| Token | Value | Use |
|---|---|---|
| `brand` | `#008dc9` | main data color, table headers, cards, links |
| `brand_stronger` | `#0f2d5b` | headings, chart titles, second series on a brand background |
| `brand_weaker` | `#90d3fb` | light brand accents |
| `fg` | `#000000` | body text |
| `fg_weaker` | `#595959` | axes, subtitles, secondary text |
| `fg_weakest` | `#cccccc` | gridlines, ticks, borders |
| `bg` / `bg_weaker` | `#ffffff` / `#f7f7f7` | background, striped rows and card backdrops |
| `cat0` to `cat5` | `#f4a81d` `#f26829` `#bd53bd` `#6363c0` `#008dc9` `#40bf73` | nominal categories |
| `cat_other` | `#cccccc` | the "other" category |
| `seq1` to `seq4` | `#d7e6f3` `#75b6d6` `#0582bb` `#0f2d5b` | ordered quantities, light to dark |
| `improving`, `worsening`, `stagnating` | `#27a3b4` `#cf7e50` `#746985` | direction of change; `_text` variants for text and arrows |

## Rules

- One series: `brand`. Several series: `collab_palette`, which is `brand` first, then `cat0`, `cat1`, `cat2`, `cat3`, `cat5`.
- WHO regions have fixed colors (AFR `#6363c0`, AMR `#f26829`, EMR `#bd53bd`, EUR `#008dc9`, SEAR `#40bf73`, WPR `#f4a81d`). Use `region_fill_values()`; never assign region colors by order or size.
- Ordered quantities, such as the member-volume bins on the map, use the sequential scale.
- Change over time uses the trend colors, with the `_text` variant for arrows and text.
- Text on a colored fill uses `label_color()`: white when the contrast is at least 3:1, otherwise black.
- Chart titles: bold, `brand_stronger`. Subtitles and axis text: `fg_weaker`. Gridlines: dotted `fg_weakest`, horizontal only on line charts.
- Tables: one column grid. The first column takes the remaining width; every other column is 124 px wide, so stacked tables line up. Framed (`fg_weakest` border, `brand_stronger` top edge), small uppercase headers in `brand_stronger` on `bg_weaker`, light row separators, 13 px text, bold total row on `bg_weaker`, a short title above each table where several follow one another. Text is left-aligned, numbers right-aligned, short codes and badges centered, header aligned with its column. Changes are shown as chips (`bg_weaker` fill, `fg_weakest` outline, `brand_stronger` text) whose box ends where the header text ends. Open/Private is plain colored text (green and orange text colors), no box. Neutral fills only: no pastel tints next to the brand blue of the chart lines. Styled by `kable_collab()` and `collab_gt_theme()` in `R/table-theme.R` and the `.collab-table` rules in `assets/styles.css`. No bars or colored cells.
- Corners: 4 px everywhere (`--radius` in `assets/styles.css`): table frames, chips, badges, cards.
- Cards: `brand` background with white text.

## Text sizes

WHO chart sizes in pixels, by chart size:

| Size | Small | Medium | Large |
|---|---|---|---|
| s | 12 | 14 | 14 |
| m | 14 | 16 | 16 |
| l | 16 | 18 | 18 |
| xl | 18 | 20 | 20 |
| xxl | 20 | 22 | 22 |
| xxxl | 28 | 38 | 54 |

Charts are drawn 10 inches wide and scaled to the page column, so one point is about 1.17 px. In `collab_size`: axis and legend text 10.5 pt (about 12 px), subtitle 12 pt (about 14 px), title 14 pt (about 16 px).

## Numbers and dates

Numbers are shown to a warranted precision: counts as whole numbers, shares to one decimal, KPI cards to whole percent. Dates follow WHO style, day first: "6 October 2026" in titles, subtitles and chart labels, "6 Oct 2026" in table headers. In running text, a date is written "October 6, 2026". All of this is `fmt_date()` in `R/metrics.R`.

## Not yet aligned

- The "target met" value on KPI cards uses `#39FF14`, which is not a WHO color. It was kept for contrast on the brand blue; a card layout that does not need it is the alternative.
- Gender colors (`#f4a81d` male, `#6363c0` female, `#40bf73` non-binary) are not used yet; gender data is unreported.
