# 🗺️ DistApp - Interactive visualization of DistAnt data

This repository holds the source code for a Shiny application that visualizes
ecological model outputs (such as species distribution models) for the Southern
Ocean and Antarctic region. The application is a product of the DistAnt
project, a collaborative effort involving the SCAR EG-ABI (Expert Group on
Biodiversity Informatics), ADVANCE (Royal Belgian Institute of Natural
Sciences), and the Integrated Digital East Antarctica program at the Australian
Antarctic Division.

## 🔎 Overview

DistApp searches the
[SCAR DistAnt Ecological Model Output Repository](https://doi.org/10.5281/zenodo.10910075)
and previews any of its 435 published layers on demand.

The app keeps **no copy of the data**. The collection is 1.9 GB of
cloud-optimized GeoTIFFs, and every layer is read over HTTP range requests
straight out of the internal overviews that cloud-optimized GeoTIFFs carry. A
layer is never downloaded, cropped to a working file, or held in memory: the
viewer is built from a 550 × 550 decimated read and then discarded. That is what
makes it cheap enough to run on a small institutional server.

Three data sources are used, and all three are optional at runtime:

| Source | What it is used for |
| --- | --- |
| `metadata.csv` in [SCAR/distant](https://github.com/SCAR/distant) | The layer catalogue: 484 rows, deduplicated to 435 unique layers |
| [Source Cooperative](https://data.source.coop/scar/distant/) (`s3://scar/distant/`) | The individual layer GeoTIFFs — this is what the viewer reads |
| Zenodo `10.5281/zenodo.10910075` | Publication-level ZIP archives, and the citable collection record |

## 🗂️ Repository Structure

```
DistApp/
├── app.R                   # The Shiny application: UI, server, download bodies
├── R/
│   ├── config.R            # Endpoints, rendering budget, field labels
│   ├── distant_api.R       # metadata.csv, Source Cooperative index, Zenodo archives
│   ├── classes.R           # Thematic class names and colours
│   ├── render_layer.R      # Decimated read, polar warp, plotting, render cache
│   ├── basemap.R           # Cached base map
│   └── downloads.R         # Delivery routes, CSV export, download bodies
├── inst/
│   ├── class_labels.yml    # Class names for layers that ship without any
│   └── zenodo_record.json  # Offline snapshot of the collection record
├── data/metadata.csv       # Fallback copy of the catalogue, used if GitHub is down
├── dev/
│   ├── smoke_test.R        # Headless checks of the data and render paths
│   └── test_app.R          # Headless exercise of the Shiny server
└── README.md
```

## 📦 Prerequisites

* **R (>= 4.1.0)** from [R Project](https://www.r-project.org/)
* **The following R packages**, which `app.R` checks for on startup and names if
  any are missing:

  | Package | Used for |
  | --- | --- |
  | `shiny` | the app itself |
  | `bslib` (>= 0.3.0) | the sidebar layout and theme |
  | `DT` | the searchable results table |
  | `ggplot2`, `scales` | drawing the layers |
  | `terra` | reading and reprojecting the GeoTIFFs (bundles GDAL) |
  | `SOmap` | the base map |
  | `yaml` | the class-label table |
  | `jsonlite` | parsing the Zenodo record |

  ```R
  install.packages(c("shiny", "bslib", "DT", "ggplot2", "scales",
                     "terra", "SOmap", "yaml", "jsonlite"))
  ```

Network access to `raw.githubusercontent.com`, `data.source.coop` and
`zenodo.org` is needed on first run. Afterwards the app starts from `.cache/`
without contacting any of them.

## Installation and execution

```bash
git clone https://github.com/biodiversity-aq/DistApp.git
cd DistApp
```

Launch the app with no data-preparation step:

```R
shiny::runApp("app.R")
```

or from a shell:

```bash
Rscript -e 'shiny::runApp("app.R", port = 8080, host = "0.0.0.0")'
```

In RStudio, opening `app.R` and clicking **Run App** works too.

## Usage

### Searching

The sidebar holds four filters, which combine:

* **Taxon** — type-ahead over all 135 taxa in the collection
* **Modelling method** — 14 methods, from Maxent to boosted regression trees
* **Future projections** — whether the layer includes projection scenarios
* **Reference** — the publication a layer comes from, listed by a short label
  (`Freer et al. (2019)`) while filtering on the full citation

The results table is paged, sortable and single-select; picking a row loads the
layer. **Refresh catalogue** re-reads all three sources without restarting.

### The layer pane

Selecting a row opens two tabs:

* **Viewer** — the map, plus a legend with real class names for categorical
  layers, a band picker for multi-band files, the source grid size, the CRS and
  the value range actually drawn
* **Metadata** — the layer's full record as a two-column table, its citation, and
  exactly where the file lives (Source Cooperative URL and size, Zenodo archive
  and size, source-code repository)

### Downloading

Users never have to go to Zenodo themselves. The primary button gives them the
**selected layer's own GeoTIFF**:

* **Under 25 MB** it is streamed through the app, so the browser saves it under
  a sensible name.
* **Over 25 MB** the button becomes a direct link to Source Cooperative, so a
  single 450 MB request cannot tie up a worker.
* **19 of the 435 layers** are described in the metadata but not published as
  standalone files. For those the button points at the Zenodo ZIP of the
  publication that contains them.

Three smaller downloads are always available: the figure as a PNG, the layer's
metadata as a single-row CSV, and a BibTeX entry for the collection.

## Notes

* **Everything is cached in `.cache/`** and nothing is tracked in git. Rendered
  previews are keyed on layer, band, ETag, rendering configuration and month, so
  a repeated view of the same layer is instant and a republished file is picked
  up automatically. The catalogue, base map and Zenodo record have the same
  treatment. Deleting `.cache/` is always safe.
* **Reads are serialised** by a process-wide lock. A render takes a few seconds
  and the canvas is around 300,000 cells, so queueing is a better trade than
  letting every concurrent user build one.
* **Large rasters are read through a decimation VRT.** The largest layer in the
  collection is 2.5 billion cells; without this, GDALWarp reads the whole thing
  to downsample it and the request takes nearly three minutes. A few hundred
  bytes of VRT pointing at the file's own overviews bring that to about ten
  seconds. See `write_source_vrt()` in `R/render_layer.R`.
* **Categorical class names** come from the GeoTIFF's GDAL raster attribute
  table where the publisher shipped one (Tóth et al.), otherwise from
  `inst/class_labels.yml`. Layers that ship with neither are shown as
  `Class 1`, `Class 2`, … and the UI says so. Adding a name is a few lines of
  YAML.
* **Cite the concept DOI** `10.5281/zenodo.10910075`, never a version DOI: the
  concept always resolves to whatever is current. The app's BibTeX download does
  this for you.
* **`preprocess_data.R` is legacy.** It predates this app and downloads the whole
  collection locally with `raster()` and the AWS CLI to build four pre-rendered
  maps. It still runs, but nothing in `app.R` depends on its output, and it no
  longer needs to be run to use the app.

## Testing

Both suites run headless and exit non-zero on failure.

```bash
Rscript dev/smoke_test.R   # metadata, catalogue, class labels, base map,
                          # renders, delivery routes, cache
Rscript dev/test_app.R    # the Shiny server: filters, selection, band picker,
                          # tabs, and each download body
```

`smoke_test.R` needs network access to read the GeoTIFFs themselves, which is the
thing under test. `test_app.R` additionally needs `shiny` to be able to build a
mock session.

## Deployment

The app is a single R process with no database, no message queue and no local
data directory, so a Docker Compose service is just R plus `shiny`:

```yaml
services:
  distapp:
    image: shiny:latest
    command: [ "R", "-e", "shiny::runApp('/app/app.R', port=8080, host='0.0.0.0')" ]
    volumes:
      - .:/app
      # Keep .cache/ on a volume so a restart does not re-read the collection.
      - distapp-cache:/app/.cache
    ports: [ "8080:8080" ]

volumes:
  distapp-cache:
```

`app.R` resolves its own directory and sets `DISTANT_APP_DIR` itself, so the only
thing to persist is `.cache/`. Letting it live on the container filesystem throws
away every rendered preview on each redeploy.

## Contributions

Contributions to the project are welcome. If you would like to improve the
application, fix bugs, or add new features, feel free to create a pull request on
this repository.

## License

This project is open source under the [MIT License](LICENSE).
