# Workflow

Detail behind the README: inputs, each cycle, definitions and checks.

## Data folder

| Folder | Content |
|---|---|
| `raw/` | exports: `users-YYYY-MM-DD.csv`, country files, `survey/pulse-survey-YYYY-MM.csv` |
| `clean/` | written by the ingest script |
| `reference/` | `community-tags.csv`, `country-codes.xlsx` |
| `manual/` | hand-maintained counts: `repositories.csv`, `cop-activity.csv` |
| `forum-stats/` | daily forum totals: `likes-`, `posts-`, `signups-`, `topics-YYYY-MM-DD.csv`; the latest file of each type is used |

Raw exports can also live outside the repository: set `CLB_RAW_DIR` to that folder.

## Each report cycle

1. Export the users list from the forum and save it as `data/raw/users-YYYY-MM-DD.csv` (the export date).
2. Update the other inputs when they change: country file, forum statistics, `data/manual/` files.
3. Build the clean files:

   ```bash
   Rscript scripts/ingest-data.R
   ```

   Options: `--force` rebuilds every clean file, `--delete-raw` removes raw files once ingested. The script prints file names and row counts only.
4. Render the site:

   ```bash
   quarto render
   ```

5. Publish to the `gh-pages` branch:

   ```bash
   quarto publish gh-pages
   ```

The date under the title of the main page is the date of the latest users export.

## Manual inputs

| File | Columns | Used for |
|---|---|---|
| `data/manual/repositories.csv` | `group`, `metric`, `value` | repository figures, Engagement page |
| `data/manual/cop-activity.csv` | `Tags` or `Community`, `Topics`, `Posts`, `Likes` | five largest communities table, Communities page |

A missing file or community is shown as a note or "n/a" on the page.

## Definitions

All values are set in `R/config.R`; the calculations are in `R/metrics.R`.

| Measure | Definition |
|---|---|
| Member | Account in the users export that is not a system account or a placeholder-e-mail account |
| Active | Last seen within 90 days of the snapshot date. Dormant: 91 to 180 days. Uninterested: over 180 days |
| Posting activity | Posted within 30 days of the snapshot date |
| Engagement level | Reading minutes per month of account age: high 15 or more, moderate 2 or more, otherwise barely |
| Long-term retention | No gap above 90 days between logins across the previous 12 months. Needs about 12 months of snapshots, otherwise not reported |
| New members | Accounts created after the previous snapshot date, up to the latest |
| Cross-community connection | Members in two or more communities of practice |
| Institutions | Distinct e-mail domains |
| WHO-affiliated | E-mail domain `who.int` or a subdomain of it |
| Geographic reach | Distinct countries in the country file that map to a WHO region |
| KPI targets | Set in `R/config.R` |

The monthly comparison uses the latest snapshot and the one closest to a month earlier. A warning is shown when the gap is under 21 or over 40 days.

## Git guard

`scripts/git-hooks/` holds a pre-commit and a pre-push check that refuse files which must stay local (data, docs, spreadsheets, local notes). Turn it on once per clone:

```bash
git config core.hooksPath scripts/git-hooks
```

## Checks

```bash
Rscript tests/test-metrics.R
Rscript tests/test-ingest.R
```

Both run on made-up data and read no files from `data/`.
