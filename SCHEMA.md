# DistApp schema

What every file in this repository is for, which of them the app genuinely
cannot start without, and the shape of the data that moves between the parts.

Nothing here is loaded at runtime except `app.R` and `R/*.R`; everything else is
either configuration the app reads, a fallback for when the network is down, or
a development tool.

---

## 1. Every file

### Runnable code — all of it required

| File | Role | Loaded by |
| --- | --- | --- |
| `app.R` | The Shiny application. Resolves the app directory, checks packages, sources `R/*.R`, builds the process-wide base map and class table, then defines the UI, the server and the four download handlers. | `shiny::runApp()` |
| `R/config.R` | Every tunable in one `config` list: remote endpoints, the render budget, nodata sentinels, colour palettes, display labels, and the `app_paths` map. Also `init_gdal()` and `ensure_dirs()`. | `app.R:43` |
| `R/distant_api.R` | Builds the layer catalogue. Fetches `metadata.csv` from GitHub, resolves each row to a real COG URL and size via the Source Cooperative S3 listing, resolves the Zenodo concept DOI to a concrete record, and caches all of it. | `app.R:43` |
| `R/classes.R` | Class naming for categorical layers. Parses GDAL `.aux.xml` raster attribute tables, reads the manual label table, decides whether a layer is categorical at all, and assigns class colours. | `app.R:43` |
| `R/render_layer.R` | Turning a COG into a plot. Decimated read through a GDAL VRT, warp onto the shared polar canvas, colour scale, base-map underlay, and the render cache with its lock and eviction. | `app.R:43` |
| `R/basemap.R` | The SOmap base map. Composes SOmap's plot plotters into one ggplot, extracts its panel as a grob, caches both, and falls back to a plain graticule. | `app.R:43` |
| `R/downloads.R` | The delivery routes and the four download bodies: the COG stream, the figure PNG, the metadata CSV, the BibTeX entry. | `app.R:43` |

`R/*.R` is sourced **alphabetically**, so `R/config.R` is always first and is the
only module whose load-time order matters — it defines `config`, `app_paths` and
`%||%`, which the others reference inside function bodies.

### Data — one required, two optional

| File | Role | Required? |
| --- | --- | --- |
| `data/metadata.csv` | Bundled copy of the collection's metadata table (485 rows, 23 columns). Used only when `raw.githubusercontent.com` is unreachable at startup. | **Yes, for offline start.** Without it *and* without network the app cannot build a catalogue and stops. |
| `inst/class_labels.yml` | Human-readable class names and optional colours for categorical layers whose publisher shipped none. | **No.** `read_class_labels()` returns an empty list; names then fall back to the `.aux.xml` sidecar, then to `Class 1`, `Class 2`, … The Metadata tab always says which source was used. |
| `inst/zenodo_record.json` | Offline snapshot of the Zenodo concept record, used only when the Zenodo API cannot be reached. | **No.** `zenodo_snapshot()` returns `NULL`; the app then simply cannot say which publication archive holds an unpublished layer. |

### Project files — not needed to run

| File | Role |
| --- | --- |
| `README.md` | Prose documentation. Note that its "Repository layout" tree and its Deployment section reference `preprocess_data.R`, a `Dockerfile` and `R/viewer.R`, none of which exist in this repository. |
| `LICENSE` | MIT licence text. |
| `.gitignore` | Excludes R session files, `Rplots.pdf`, `processed_data/`, `.cache/`. Its comment references a `dev/refresh_metadata.R` that does not exist. |
| `dev/smoke_test.R` | Headless checks of the data and render paths: metadata, catalogue, class labels, base map, four real renders, the panel underlay, delivery routes, cache keys. |
| `dev/test_app.R` | Headless exercise of the real Shiny server through `shiny::testServer`: filters, row selection, band picker, tabs, all four download bodies. |
| `dev/check_downloads.R` | Each download body end to end over HTTP, including the Zenodo archive check. |
| `dev/_dbg_head.R` | **Dead code.** A leftover debug scratch from the repository author: it sources `R/` and probes a layer's `zip_url` with `curl`. Nothing references it; it can be deleted. |

### Generated at runtime — not in git, safe to delete

| Path | Contents |
| --- | --- |
| `.cache/basemap.rds` | The base map: `list(format = "panel-2", plot = <ggplot>, panel = <grob>)`. ~30 s to build, then cached for the life of the cache. |
| `.cache/renders/*.rds` | One cached render per layer and band, pruned to `config$cache_max_entries` (25) oldest-first. Each is a few MB. |
| `.cache/vrt/*.vrt` | Decimation VRTs, a few hundred bytes each, pruned with the renders. |
| `.cache/zenodo_record.rds` | The resolved Zenodo record, 24 h TTL. |
| `Rplots.pdf` | Stray R default-device output. Gitignored; delete freely. |
| `rsconnect/` | shinyapps.io deployment manifests, created by `rsconnect::deployApp()`. |

---

## 2. What the app truly needs

**Hard requirement — the app will not start without these seven files:**

```
app.R
R/config.R
R/distant_api.R
R/classes.R
R/render_layer.R
R/basemap.R
R/downloads.R
```

Plus, on first run, network access to `raw.githubusercontent.com`,
`data.source.coop` and `zenodo.org`.

**For a cold start with no network,** add `data/metadata.csv`. Without a
catalogue there is nothing to show and `load_layers()` stops with the fetch error.

**Safe to delete with no functional loss, only degraded output:**
`inst/class_labels.yml` (legend text falls back to `Class N`),
`inst/zenodo_record.json` (no archive fallback for unpublished layers),
`README.md`, `LICENSE`, `.gitignore`, all of `dev/`.

**Minimum for a network-connected deployment** is therefore the seven code
files plus the three directories `R/`, and a writable `.cache/`. The app creates
`.cache/`, `.cache/renders/` and `.cache/vrt/` itself via `ensure_dirs()`; it
does **not** create `inst/` or `data/`.

---

## 3. Schema

### 3.1 Startup sequence

```
app.R
 ├─ resolve DISTANT_APP_DIR, setwd, check packages
 ├─ source R/*.R            (config.R first, alphabetically)
 ├─ init_gdal()             GDAL/PROJ config for /vsicurl range requests
 ├─ ensure_dirs()           creates .cache/, .cache/renders/, .cache/vrt/
 ├─ get_base_map()          → .cache/basemap.rds, or plain_canvas() on failure
 └─ read_class_labels()     → list() if inst/class_labels.yml is absent
```

Both of the last two are process-wide, built once per app process, not per
session. The catalogue is *not* built here — it is built lazily by
`build_catalogue()` the first time `filtered()` is read.

### 3.2 Per-session sequence

```
user filters ──► filtered()          435 rows, filtered client-side
   row click ──► layer()             one row
                    │
                    ├─ nlyr > 1 ──► band_names() ──► effective_band()  [clamped]
                    │                    │
                    │              output$band_ui  →  selectInput("band")
                    ▼
                rendered()  ──► cached_render()  ──► render_layer_plot()
                                                        │
                                          load_band()  →  decimated_source()
                                                        →  warp_to_polar()
                                                        →  as_canvas_frame()
                                                        →  scale + underlay_base()
                                                        ▼
                                              output$plot   (620 px ggplot)
                                              dl_plot        (1500 px PNG)
```

`dl_layer` deliberately does **not** go through `rendered()`: it streams the
publisher's own COG, so the GeoTIFF is byte-for-byte theirs with no base map and
no styling. The figure PNG is the opposite — it is `rendered()` with the SOmap
panel painted underneath, at the base-map and opacity settings in force.

### 3.3 Catalogue row — 31 columns, one per layer

23 come from `metadata.csv`; 8 are derived at build time by `build_catalogue()`.

| Column | Type | Meaning |
| --- | --- | --- |
| `id` | chr | **Primary key.** e.g. `Fa2020-bioregions`. The 484 published rows contain 7 files repeated 8× each; `load_layers()` drops exact duplicates so this is unique across 435 rows. |
| `file` | chr | Original filename, e.g. `Fa2020-bioregions_cog.tif` |
| `taxon` | chr | Target species or group |
| `input_data` | chr | Predictor data the model used |
| `modelling_method` | chr | e.g. Maxent, boosted regression tree |
| `output_type` | chr | Drives the continuous colour ramp via `config$continuous_palettes` |
| `future_projections` | lgl | Coerced from the text column; `TRUE`/`FALSE` |
| `uncertainty_type` | chr | How model uncertainty is expressed |
| `model_performance` | num | The score |
| `model_performance_measure` | chr | The metric the score is measured by (AUC, TSS, …) |
| `xmin` `xmax` `ymin` `ymax` | num | Native extent |
| `x_resolution` `y_resolution` | num | Native cell size |
| `spatial_units` | chr | Units of the above |
| `crs` | chr | Native CRS |
| `repo` | chr | Model source-code repository |
| `data_usage_notes` | chr | Free text |
| `licence` | chr | Layer licence |
| `reference` | chr | Short author-year label, used for the Reference filter |
| `citation` | chr | Full citation |
| `cog_url` | chr | **Derived.** Resolved `/vsicurl/` URL. `NA` when the layer has no standalone file. |
| `cog_size` | num | **Derived.** Bytes, from the S3 listing. |
| `etag` | chr | **Derived.** Source object's content hash; part of the render cache key, so a re-upload invalidates cached renders automatically. |
| `pub_dir` | chr | **Derived.** Zenodo publication directory holding this layer. |
| `zip_url` | chr | **Derived.** Zenodo archive URL. `NA` for most layers. |
| `zip_size` | num | **Derived.** |
| `zip_name` | chr | **Derived.** |
| `aux_xml_url` | chr | **Derived.** GDAL `.aux.xml` sidecar, when the publisher shipped one. |

### 3.4 Delivery route — the return of `layer_delivery(layer)`

One of four values. `app.R:download_button()` switches on `route`:

| `route` | When | What the user gets |
| --- | --- | --- |
| `"cog"` | Has a COG, and `cog_size <= config$proxy_max_mb` (25 MB) | A `downloadButton` that streams the COG **through** the server. |
| `"cog-direct"` | Has a COG larger than 25 MB | An `<a href>` to Source Cooperative, so one 450 MB request cannot tie up a worker. |
| `"zenodo"` | No COG, but `zip_url` is known | An `<a>` to the Zenodo archive of the containing publication. |
| `"none"` | Neither | A `span` reading "No downloadable file", plus a Metadata-tab explanation. |

Every route also carries `url`, `filename`, `size` and a human `note`.

### 3.5 `inst/class_labels.yml`

```yaml
<layer id>:                 # optionally "<layer id>|<band name>" when one
  labels:                   # publication contributes several thematic layers
    1: "Antarctic inner shelf"
    2: "Antarctic outer shelf"
  colours:                  # optional; hex, keyed the same way as labels
    1: "#2A5178"
```

Resolution order in `R/classes.R:resolve_classes()`:

1. this file, via `manual_labels()`
2. the COG's own `.aux.xml` GDAL raster attribute table, via `read_rat()`
3. `Class 1`, `Class 2`, … for the classes actually present

All three return the same record, which is what `thematic_scale()` consumes:

```r
list(values = <chr, the class keys on screen>,
     labels = <chr, one per value>,
     colours = <chr, one hex per value>,
     source = <chr, which of the three above was used>)
```

`source` is surfaced in the layer pane, so the legend is never silently wrong.

### 3.6 `inst/zenodo_record.json`

A trimmed Zenodo record, not the full API response:

```json
{ "_comment": "…",
  "id": 14792295,
  "doi": "10.5281/zenodo.10910075",
  "landing": "https://doi.org/…",
  "published": "2025-…",
  "files": [ { "key": "cuzin-roudy_et_al-2014.zip", "size": 674000 } ] }
```

`as_zenodo_record()` normalises it to the same shape as a live API response, so
the rest of the code cannot tell which source it came from. Resolution order is
live API → `.cache/zenodo_record.rds` (24 h) → this snapshot → nothing.

### 3.7 Cached render — the return of `render_layer_plot()`

```r
list(
  plot     = <ggplot>,   # the layer with base_panel_grob() painted underneath
  classes  = <list|nil>,  # the record from §3.5, NULL for continuous layers
  thematic = <lgl>,       # decides whether a legend is drawn
  info     = list(band_name, nlyr, band_names, crs, range, src_cells, src_read)
)
```

`src_cells` vs `src_read` is the decimation report: the layer pane shows both, so
a viewer can see that a 2.5-billion-cell source was never read whole.

`cache_key(layer, band)` is `band` + COG basename + first 12 chars of the ETag
+ `base_format` + a squash of `config$preview_px`, `polar_px_res`,
`polar_extent`, `source_target_cells` + the current month. Everything that can
change a pixel is in the key.

### 3.8 Remote interfaces

| Endpoint | Used for | Failure behaviour |
| --- | --- | --- |
| `raw.githubusercontent.com/SCAR/distant/master/metadata.csv` | The catalogue | Falls back to `data/metadata.csv`; if that is also missing, `stop()`. |
| `data.source.coop` S3 listing (`scar` bucket, `distant/` prefix) | COG URL, size and ETag per layer | Rows lose `cog_url`/`cog_size`, so a layer that would have been a `"cog"` route is offered only as `"zenodo"`, or as `"none"` if it has no archive either. |
| `zenodo.org/api` | Concept DOI → record → per-publication archives | Falls back to the cache, then the snapshot, then no archive offers. |
| `<cog_url>` via `/vsicurl/` | Range requests against the COG itself | The layer pane shows the read error. |
| `<cog_url>.aux.xml` | GDAL raster attribute table | Class names fall back to `inst/class_labels.yml`, then to numbering. |

### 3.9 Configuration groups

`R/config.R` is one flat `config` list, grouped by comment:

| Group | Keys |
| --- | --- |
| Data sources | `metadata_csv`, `metadata_csv_local`, `s3_endpoint`, `s3_bucket`, `s3_prefix`, `source_coop_url`, `zenodo_concept_doi`, `zenodo_concept_url`, `zenodo_api`, `zenodo_cache`, `zenodo_cache_ttl_hours`, `zenodo_snapshot` |
| HTTP behaviour | `http_retries`, `http_timeout_s` |
| Rendering budget | `preview_px` (700), `source_target_cells` (2100), `polar_px_res` (20000), `polar_extent` (5.5e6 m), `polar_label` |
| Data hygiene | `nodata_sentinels`, `max_categorical_classes` |
| Classification | `thematic_output_types`, `continuous_output_types`, `continuous_palettes` |
| Delivery | `proxy_max_mb` (25) |
| Caching | `cache_dir`, `cache_ttl_hours`, `cache_max_entries` (25) |
| Display | `filter_facets`, `results_columns`, `results_labels`, `metadata_fields` |
| Citation | `collection_citation` |

The polar CRS is **not** a config key: `polar_template()` hard-codes
`crs = "EPSG:3031"`, which is the only CRS the whole app speaks. The filter
list, the result columns and the display labels are all driven from `config`, so
the UI can be reshaped without touching `app.R`.

`app_paths` has exactly seven entries — `dir`, `cache`, `renders`, `vrt`,
`basemap`, `labels`, `local_metadata` — derived from these plus
`DISTANT_APP_DIR`, and is the only place the app touches the filesystem by path.
