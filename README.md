# BioExplorer

An interactive, browser-based guide to Melbourne's urban biodiversity.

**Live demo:** [Open the Shiny app](https://qinghao.shinyapps.io/gardenapp/)

The GitHub Pages experience is a static website in `docs/`. It includes:

- an interactive biodiversity map with search, kingdom and month filters;
- garden and plant diversity comparisons;
- butterfly survey comparisons and species cards;
- bird sighting and seasonality charts.

The website runs entirely in the browser and does not require an R or Shiny server.

## Refreshing the site data

After changing the source CSV files, run:

```bash
node scripts/build-pages-data.mjs
scripts/build-pages-assets.sh
```

GitHub Pages should publish from the `docs/` folder on the `main` branch.

## Legacy Shiny app

The original Shiny implementation remains in the repository for reference. You can try the [Shiny live app](https://qinghao.shinyapps.io/gardenapp/) or launch it locally from `app.R`.
