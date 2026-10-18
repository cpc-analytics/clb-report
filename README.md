# Collaboratory Report

Web report on the Collaboratory community platform (WHO Hub for Pandemic and Epidemic Intelligence): membership, activity, representation, communities and survey results. Built with R and Quarto, published as a static site: https://cpc-analytics.github.io/clb-report/

## Quick start

```bash
Rscript scripts/install-packages.R      # once
Rscript scripts/ingest-data.R           # after saving a new export in data/raw/
quarto render
quarto publish gh-pages
```

The export is saved as `data/raw/users-YYYY-MM-DD.csv`. Details for every input are in [guides/workflow.md](guides/workflow.md).

## Folders

```
index.qmd, pages/    the site
R/                   code the pages use: settings, calculations, loaders, styling
scripts/             commands run by hand
assets/              logo and stylesheet
guides/              workflow and design language
tests/               checks on made-up data
data/                local, not tracked
docs/                build output, not tracked
```

## More

- [guides/workflow.md](guides/workflow.md): inputs, each cycle, definitions, checks
- [guides/design.md](guides/design.md): colors, font, sizes, table and chart rules (WHO Data Design Language)
