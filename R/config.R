# R/config.R -- Central configuration for the DistAnt Shiny app.
#
# Everything that points at a remote service, tunes the rendering budget, or
# translates raw metadata column names into human readable labels lives here.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

config <- list(

  # ---- Data sources -------------------------------------------------------

  # The minimal metadata table that describes every layer in the collection.
  metadata_csv = "https://raw.githubusercontent.com/SCAR/distant/master/metadata.csv",

  # Bundled fallback, used when GitHub is unreachable at startup. Refreshed by
  # dev/refresh_metadata.R.
  metadata_csv_local = "data/metadata.csv",

  # Source Cooperative mirrors the collection as individual COGs. The S3 listing
  # API is what lets us resolve a metadata `file` to a real, downloadable URL.
  s3_endpoint = "https://data.source.coop",
  s3_bucket = "scar",
  s3_prefix = "distant/",

  # Source Cooperative browsable landing page.
  source_coop_url = "https://source.coop/scar/distant",

  # Zenodo. The concept DOI always resolves to the latest version; we resolve it
  # to a concrete record id at startup so we can address individual files.
  zenodo_concept_doi = "10.5281/zenodo.10910075",
  zenodo_concept_url = "https://doi.org/10.5281/zenodo.10910075",
  zenodo_api = "https://zenodo.org/api",

  # Zenodo sits behind a CDN that intermittently refuses automated clients, so
  # the record lookup degrades: live API, then this on-disk cache, then the
  # snapshot shipped in inst/. All three describe the same record until a new
  # version is published, and the first source that answers wins.
  zenodo_cache = "zenodo_record.rds",
  zenodo_cache_ttl_hours = 24,
  zenodo_snapshot = file.path("inst", "zenodo_record.json"),

  # The citation DistAnt asks users to use for any layer they reuse.
  collection_citation = paste(
    "Kovacs J (2020) A model of my favourite Southern Ocean species.",
    "Journal of Southern Ocean Stuff 123:1-10. Data obtained from the",
    "SCAR DistAnt Ecological Model Output Repository, 10.5281/zenodo.10910075"
  ),

  # ---- Rendering budget ---------------------------------------------------

  # Longest edge, in pixels, of the viewer itself. Everything the user sees is
  # at most this wide, so no part of the pipeline needs more resolution than
  # this before the final panel size.
  preview_px = 700,

  # Linear size, in cells, that a source band is decimated to before it is
  # warped onto the canvas. This is the single most important number for cost:
  # asking GDAL to resample the source into a grid this size makes its
  # RasterIO call pick the matching internal overview of the COG, so a
  # 2.5-billion-cell 100 m layer costs a 40 MB read instead of a 2.5 GB one.
  # Three times the display width is enough for the warp to still be doing a
  # modest 3:1 reduction rather than an extreme one.
  source_target_cells = 2100,

  # Pixel size of the polar stereographic (EPSG:3031) canvas the preview is
  # warped onto. 5.5e6 / 2e4 gives a 550 x 550 canvas: about 20 km per cell,
  # which is finer than the model's own grid for most layers and coarse enough
  # that a rendered layer costs single-digit MB rather than tens. Users who
  # need the real numbers download the COG.
  polar_px_res = 20000,

  # Canvas half-extent in EPSG:3031 metres. Covers roughly 60S to the pole,
  # with a little margin around the SOmap base.
  polar_extent = 5.5e6,

  # Above this many bytes a download is offered as a direct link to the archive
  # rather than proxied through the Shiny worker, so one big request cannot
  # saturate the institutional server.
  proxy_max_mb = 25,

  # Rasters are cached as rendered plots so that re-viewing a layer is instant
  # and the (relatively slow) first render happens at most once per layer per
  # week, no matter how many users ask for it.
  cache_dir = ".cache",
  cache_ttl_hours = 24 * 7,
  cache_max_entries = 25,

  # Network politeness.
  http_timeout_s = 30,
  http_retries = 3,

  # ---- Presentation --------------------------------------------------------

  polar_label = "Antarctica / Southern Ocean (EPSG:3031)",

  # output_type values that are known to be thematic. This is only a hint: the
  # renderer auto-detects categorical rasters from the data as well, because a
  # handful of regionalisation layers are reported under generic output types.
  thematic_output_types = c(
    "bioregions",
    "bioregional ecosystem types",
    "habitat complexes",
    "major environmental units",
    "species turnover",
    "inviolate wilderness",
    "negligibly impacted wilderness"
  ),

  # Types that are always continuous. Anything in neither list is decided from
  # the data, which costs one extra read but keeps regionalisation layers
  # published under a generic output type from being drawn as a class map.
  continuous_output_types = c(
    "habitat suitability",
    "habitat importance",
    "probability of presence",
    "nighttime abundance",
    "daytime abundance",
    "sea ice primary productivity",
    "ice free habitat",
    "relative risk",
    "spawning habitat duration (weeks)",
    "spawning habitat index"
  ),

  # A decimated sample with at most this many unique, integer valued cells is
  # treated as categorical unless the output type says otherwise.
  max_categorical_classes = 50,

  # Nodata sentinels to strip from the drawn values.
  #
  # The collection is not consistent here, and the reader does not surface what
  # a file declares: terra reports NAflag() == NaN for every band, so the
  # sentinel has to be named. Measured across the collection, these three cover
  # it -- the byte-valued regionalisation layers carry 99 and the IBCSO
  # bathymetry carries -9999. This is not cosmetic. 99 covers 99.1% of the
  # cells of To2025-tier_1_major_environmental_units, so leaving it in turns that
  # layer into a single flat class and puts -9999 at the bottom of every
  # bathymetry ramp. Add to this list if a new sentinel appears upstream.
  nodata_sentinels = c(-999, -9999, 99),

  # Continuous ramps, keyed by output type. Anything unlisted uses viridis.
  continuous_palettes = list(
    "habitat suitability"          = list(palette = "viridis", rev = FALSE),
    "habitat importance"           = list(palette = "viridis", rev = FALSE),
    "probability of presence"      = list(palette = "viridis", rev = FALSE),
    "daytime abundance"            = list(palette = "viridis", rev = FALSE),
    "nighttime abundance"          = list(palette = "viridis", rev = FALSE),
    "sea ice primary productivity" = list(palette = "Greens",  rev = TRUE),
    "ice free habitat"             = list(palette = "Greys",   rev = FALSE),
    "supporting"                   = list(palette = "cividis", rev = TRUE)
  ),

  # Legend title defaults to the COG's own band description, which is why we
  # do not have to guess one here.

  # Human readable metadata table. Order is preserved in the UI.
  metadata_fields = c(
    id                      = "Layer ID",
    file                    = "File name",
    taxon                   = "Taxon",
    input_data              = "Input data",
    modelling_method        = "Modelling method",
    output_type             = "Output type",
    future_projections      = "Future projections",
    uncertainty_type        = "Uncertainty type",
    model_performance       = "Model performance",
    model_performance_measure = "Performance measure",
    xmin                    = "Extent min x",
    xmax                    = "Extent max x",
    ymin                    = "Extent min y",
    ymax                    = "Extent max y",
    spatial_units           = "Spatial units",
    x_resolution            = "Resolution x",
    y_resolution            = "Resolution y",
    crs                     = "Coordinate reference system",
    repo                    = "Source code repository",
    data_usage_notes        = "Data usage notes",
    licence                 = "Licence",
    reference               = "Reference",
    citation                = "Citation"
  ),

  # Columns shown in the search results table, with their headers.
  results_columns = c("taxon", "output_type", "modelling_method",
                      "future_projections", "id"),

  results_labels = c(taxon              = "Taxon",
                     output_type         = "Output type",
                     modelling_method    = "Method",
                     future_projections  = "Future",
                     id                 = "Layer ID",
                     file               = "File"),

  # The four search facets requested for the sidebar.
  filter_facets = c("taxon", "modelling_method", "future_projections", "reference")
)

# Derived storage locations. `DISTANT_APP_DIR` is set by the entry point before
# this file is sourced, so paths resolve the same way however the app is
# launched (RStudio, `runApp("app.R")`, or a shiny-server scgi_app).
app_dir <- Sys.getenv("DISTANT_APP_DIR", unset = normalizePath("."))

app_paths <- list(
  dir     = app_dir,
  cache   = file.path(app_dir, config$cache_dir),
  renders = file.path(app_dir, config$cache_dir, "renders"),
  vrt     = file.path(app_dir, config$cache_dir, "vrt"),
  basemap = file.path(app_dir, config$cache_dir, "basemap.rds"),
  labels  = file.path(app_dir, "inst", "class_labels.yml"),
  local_metadata = file.path(app_dir, config$metadata_csv_local)
)

# GDAL needs to be told to use HTTP range requests and to skip directory
# listings, otherwise opening a COG over https:// triggers a full bucket crawl.
init_gdal <- function() {
  Sys.setenv(
    GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR",
    CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif,.xml",
    GDAL_HTTP_MULTIRANGE = "SINGLE_GET",
    GDAL_HTTP_VERSION = "2",
    GDAL_HTTP_MERGE_CONSECUTIVE_RANGES = "YES",
    VSI_CACHE = "FALSE"
  )
  invisible(TRUE)
}

ensure_dirs <- function() {
  dir.create(app_paths$cache, recursive = TRUE, showWarnings = FALSE)
  dir.create(app_paths$renders, recursive = TRUE, showWarnings = FALSE)
  dir.create(app_paths$vrt, recursive = TRUE, showWarnings = FALSE)
  invisible(TRUE)
}
