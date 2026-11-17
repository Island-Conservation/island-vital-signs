### **Island Vital Signs**

Current version: v1.1.0

Try the app here: 🔗 [Island Vital Signs](https://islandconservation.shinyapps.io/island-vital-signs/)

## Overview

Island Vital Signs is a decision support tool for island managers to visualize long-term trends, generate standardized reports of vegetation change, assess and monitor vegetative trends, and identify areas to prioritize for future interventions. This tool helps identify and prioritize areas for future conservation where active restoration may be required, guide intra-island reintroduction site selection on predator-free islands, and optimize in situ monitoring to measure change in places most likely to experience impact after invasive mammal removal.

The app uses NASA Landsat data to track three vegetation metrics through time — tree canopy cover (TCC), canopy moisture (NDMI), and productivity (NDVI, EVI, or MSAVI2) — modeled per pixel per year for each island in the dataset. The modeling framework is described by [Honzák et al. (2024)](https://doi.org/10.1088/1748-9326/ad77b7) and [Wails et al. (2026, pre-print)](https://doi.org/10.21203/rs.3.rs-10554051/v1).

## Modules

Modeling results are organized into tabs within the app:

0.  **Overview** — An orientation to the app's modules, key terms, and how to read the maps.
1.  **Annual Values** — Model-predicted annual values. Users select an island, a vegetation metric, and two years to compare. The two years are returned as a side-by-side leaflet map with a draggable slider. Pixels are selectable, and pixel-level time series plots (smoothed trend and annual variation) render below.
2.  **Change Over Time** — Model-predicted annualized and accumulated change. Users select an island, metric, change type, time period, and probability level. The left side of the map shows all modeled pixels, the right side only those significant at the selected probability level. On islands with a modeled invasive species eradication, pre-intervention, post-intervention, and dynamic baseline periods are available.
3.  **Classified Change** — Six trend and condition questions, such as where a metric is high or low over the full time series, where it has been high or low in the last five years, and where it is increasing or decreasing. Selecting a question maps only the pixels that answer it.
4.  **Management Questions** — Five management-framed questions answered from the modeled results, such as where productivity is high and increasing, or where tree canopy cover has changed the most. Each question uses a predetermined metric, so no metric selection is needed.

## Demo

![Quick demo of the per-pixel annual predictions module](images/m2-demo.gif)

## File Structure

```
├── global.R           # Global Shiny script: libraries, palettes, data load
├── ui.R               # UI Shiny script
├── server.R           # Server Shiny script
├── R/                 # Helper functions, sourced by global.R
├── text/              # In-app documentation (Markdown)
├── www/               # Static assets: CSS, JS, fonts, images
├── images/            # README images
├── renv.lock          # Pinned R package versions
├── renv/              # renv bootstrap
├── LICENSE            # CC0 1.0 Universal
├── README.md          # This file
```

## Data

The modeled rasters and island metadata the app reads from `data/` are **not
included in this repository**. They are large, are refreshed island by island on
their own schedule, and are published separately as a versioned Zenodo deposit.
The deployed app receives them at deploy time, not through git.

Cloning this repository therefore gives you the application code, not a runnable
copy of the app.

## Running Locally

1.  Clone the repository and open it in RStudio.
2.  Restore the package environment: `renv::restore()`.
3.  Place the data directory at `data/` in the project root.
4.  Optional: create a file named `carto.env` in the project root containing
    `CARTO_API_KEY=your-key-here`. CARTO watermarks anonymous basemap requests;
    without a key the app falls back to Esri gray canvas basemaps and still
    works.
5.  Run the app: `shiny::runApp()`.

## How to Cite

> **Windschitl, E., M. N. Call, C. N. Wails, B. Cosentino, J. Gibbs, M. Honzak, J. Sexton, P. Wang, and D. Will.** 2026. Island Vital Signs \[Computer software\]. Zenodo. doi: [10.5281/zenodo.21500194](https://doi.org/10.5281/zenodo.21500194)

That DOI always resolves to the most recent release. To cite an exact release,
use that release's own DOI, listed under "Versions" on the
[Zenodo record](https://doi.org/10.5281/zenodo.21500194).

## License

Released under the [PolyForm Noncommercial License 1.0.0](LICENSE). Any
noncommercial purpose is permitted, and the license explicitly covers use by
charitable organizations, educational institutions, public research
organizations, environmental protection organizations, and government
institutions — regardless of how they are funded.

Note that the v1.0.0 release was published under CC0 1.0 and remains so; that
dedication is irrevocable. This license applies to v1.1.0 onward.

## Acknowledgments

Supported by the Salesforce Nature Accelerator and NASA Biological Diversity and
Ecological Conservation, Award 80NSSC23K1539.
