# GardenApp (React + R Shiny)

This project combines a React header (built with Vite) and an R Shiny app hosting four Leaflet maps:

- Biodiversity
- Butterfly
- Flower
- Garden

## Quick start

1) **Install front-end deps & build assets**

```bash
cd frontend
npm install
npm run build
```

This produces `frontend/dist/assets/index.js` and `frontend/dist/assets/index.css` with stable file names.

2) **Run the Shiny app**

In R (e.g., RStudio or terminal):

```r
shiny::runApp("app.R")
# or simply: source("app.R")
```

The React header will appear at the top of the app (mounted on `#react-root`). Use the **Refresh Data** button to trigger a global input (`react_refresh`) so each map reloads its CSV.

## CSV schema

Place your CSVs under `dataset/`:

- `biodiversity.csv`
- `butterfly.csv`
- `flower.csv`
- `garden.csv`

Expected columns (flexible names supported):
- `lat` or `latitude`
- `lon` / `lng` / `longitude`
- `name` (or `title`, `site`, `label`)
- `category` (or `type`, `class`, `group`)
- optional `value` (or `score`, `count`, `size`)

Any missing columns are filled with defaults where possible. Rows without `lat`/`lon` are dropped.

## Development mode for React

If you want to develop React independently:

```bash
cd frontend
npm run dev
```

This spins up Vite on localhost for the pure React page (it mounts to the `#root` in `frontend/index.html`). The Shiny integration only uses the built bundle in `frontend/dist`.
