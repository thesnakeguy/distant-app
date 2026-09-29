#!/usr/bin/env Rscript
# dev/smoke_test.R -- Headless verification of the DistAnt app's data and render
# paths. Run from the app root:
#
#   Rscript dev/smoke_test.R
#
# Everything here is offline-safe: the metadata falls back to data/metadata.csv,
# the Zenodo archives to inst/zenodo_record.json, and the base map to a plain
# graticule if SOmap will not build. The only unavoidable network use is reading
# the COGs themselves, which is the thing under test.

Sys.setenv(DISTANT_APP_DIR = normalizePath("."))
suppressPackageStartupMessages({
  library(shiny)
})
for (f in list.files("R", full.names = TRUE, pattern = "[.]R$")) source(f)
init_gdal()
ensure_dirs()

failures <- 0L
ok <- function(label, cond, detail = "") {
  passed <- isTRUE(cond)
  if (!passed) failures <<- failures + 1L
  cat(sprintf("  [%s] %s%s\n", if (passed) "pass" else "FAIL", label,
              if (nzchar(detail)) paste0(" -- ", detail) else ""))
}
section <- function(x) cat(sprintf("\n== %s ==\n", x))

timed <- function(label, expr) {
  t0 <- Sys.time()
  value <- force(expr)
  cat(sprintf("  (%s took %.1fs)\n", label,
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  value
}

# ---------------------------------------------------------------------------
section("metadata")

layers <- timed("load_layers", load_layers())
ok("435 unique layers", nrow(layers) == 435L, sprintf("got %d", nrow(layers)))
ok("no duplicate ids", !any(duplicated(layers$id)))
ok("23 columns", ncol(layers) == 23L, sprintf("got %d", ncol(layers)))
ok("future_projections is logical",
   is.logical(layers$future_projections))
ok("spatial units resolved", all(!is.na(layers$spatial_units)))
ok("short_reference shortens author lists",
   identical(short_reference(paste0("Chaabani S, L\u00f3pez-Gonz\u00e1lez PJ, Casado-Amezua P, ",
                                    "Pehlke H (2019) Ecological niche modelling.")),
             "Chaabani S et al. (2019)"),
   short_reference(paste0("Chaabani S, L\u00f3pez-Gonz\u00e1lez PJ, Casado-Amezua P, ",
                          "Pehlke H (2019) Ecological niche modelling.")))
ok("short_reference reduces bare URLs to a hostname",
   identical(short_reference("https://ibcso.org/"), "ibcso.org"))

# ---------------------------------------------------------------------------
section("catalogue")

cat_data <- timed("build_catalogue", build_catalogue())
ok("catalogue covers every layer", nrow(cat_data) == nrow(layers))
ok("416 layers resolve to a COG",
   sum(!is.na(cat_data$cog_url)) == 416L,
   sprintf("got %d", sum(!is.na(cat_data$cog_url))))
ok("no COG URL is malformed", all(grepl("^https://data.source.coop/scar/distant/",
                                       cat_data$cog_url[!is.na(cat_data$cog_url)])))
ok("420 layers have a publication archive",
   sum(!is.na(cat_data$zip_url)) == 420L,
   sprintf("got %d", sum(!is.na(cat_data$zip_url))))
ok("every archive URL points at the latest record",
   all(grepl("^https://zenodo[.]org/records/14792295/files/.+[.]zip/content$",
             cat_data$zip_url[!is.na(cat_data$zip_url)])),
   head(setdiff(sub("[.]zip/content$", "", cat_data$zip_url), ""), 1L))

nocog <- cat_data[is.na(cat_data$cog_url), ]
ok("19 layers have no COG", nrow(nocog) == 19L, sprintf("got %d", nrow(nocog)))
ok("the 7 inferable publications still get an archive",
   sum(!is.na(nocog$zip_url)) == 7L,
   paste(sort(substr(nocog$id[!is.na(nocog$zip_url)], 1, 3)), collapse = " "))
ok("the other 12 are correctly left with none",
   sum(is.na(nocog$zip_url)) == 12L,
   sprintf("got %d", sum(is.na(nocog$zip_url))))

# ---------------------------------------------------------------------------
section("class labels")

labels <- read_class_labels()
ok("manual label table loads", length(labels) >= 3L, sprintf("%d keys", length(labels)))
fa <- cat_data[cat_data$id == "Fa2020-bioregions", ]
ok("Fabri-Ruiz bioregions resolve to the manual table", nrow(fa) == 1L)
if (nrow(fa) == 1L) {
  cls <- resolve_classes(fa, "bioregion", 1:12, labels)
  ok("...with source class_labels.yml",
     identical(cls$source, "class_labels.yml"))
  ok("...covering all 12 classes", length(cls$values) == 12L)
  ok("...named from the YAML",
     identical(unname(cls$labels[[1]]), "Antarctic inner shelf"),
     unname(cls$labels[[1]]))
  ok("...carrying the curated hex colours",
     identical(cls$colours[[1]], "#2A5178"), cls$colours[[1]])
}
probe <- fa
probe$id <- "Zz9999-not-a-real-layer"
probe$aux_xml_url <- NA_character_
c2 <- resolve_classes(probe, "bioregion", c(3, 5, 7), labels)
ok("an unknown layer falls back to numbering the observed classes",
   identical(c2$source, "unlabelled (values shown)") &&
     identical(unname(c2$labels), c("Class 3", "Class 5", "Class 7")),
   paste(c2$labels, collapse = ", "))

# A missing sidecar is the normal case -- only the Toth layers ship one -- so a
# 404 must not surface as a warning on every other thematic layer.
probe$aux_xml_url <- paste0(config$s3_endpoint, "/", config$s3_bucket, "/",
                            config$s3_prefix, "definitely/absent.aux.xml")
warned <- character()
c3 <- withCallingHandlers(
  resolve_classes(probe, "bioregion", 1:3, labels),
  warning = function(w) warned <<- c(warned, conditionMessage(w))
)
ok("a missing sidecar is silent, not a warning", !length(warned),
   paste(warned, collapse = "; "))
ok("...and still numbers the classes", identical(unname(c3$labels),
   c("Class 1", "Class 2", "Class 3")))

# ---------------------------------------------------------------------------
section("base map")

base <- timed("get_base_map", get_base_map())
ok("base map is a ggplot", inherits(base, "ggplot"))
ok("base map carries layers", length(base$layers) > 0L)
# SOmap, not the graticule fallback. SOgg() is the one API that composes, and it
# only does so under a ggplot2 it was written for, so a silent fall-through to
# plain_canvas() is the failure mode worth pinning down here.
ok("base map is the real SOmap, not the fallback",
   identical(base_map_source(base), "SOmap"), base_map_source(base))
ok("base map can be rendered",
   is.list(tryCatch(ggplot2::ggplotGrob(base), error = function(e) e)) &&
   !inherits(tryCatch(ggplot2::ggplotGrob(base), error = function(e) e), "error"))
# The layer is painted on top of the base's panel, so a missing panel is a
# silently bare map rather than an error.
ok("base map yields a panel to draw the layer over",
   inherits(base_panel_grob(base), "grob"),
   class(base_panel_grob(base))[1])

# ---------------------------------------------------------------------------
section("renders")

layer_by_id <- function(id) {
  hit <- cat_data[cat_data$id == id, , drop = FALSE]
  if (nrow(hit) != 1L) stop("expected exactly one layer called ", id)
  hit
}

render_one <- function(label, id, band = 1L, expect_thematic = NA) {
  layer <- layer_by_id(id)
  res <- timed(label, render_layer_plot(layer, band, base, labels))
  ok(paste0(label, ": builds"), inherits(res$plot, "ggplot"))
  ok(paste0(label, ": renders to a grob"),
     !inherits(tryCatch(ggplot2::ggplotGrob(res$plot), error = function(e) e), "error"))
  if (!is.na(expect_thematic)) {
    ok(paste0(label, ": thematic == ", expect_thematic),
       identical(res$thematic, expect_thematic))
  }
  res
}

continuous <- render_one("Fr2019 habitat suitability",
                         "Fr2019-Electrona_antarctica", expect_thematic = FALSE)
ok("continuous range is sane and non-degenerate",
   continuous$info$range[1] < continuous$info$range[2],
   paste(round(continuous$info$range, 4), collapse = ".."))

thematic <- render_one("Fa2020 bioregions", "Fa2020-bioregions",
                       expect_thematic = TRUE)
ok("thematic legend is labelled", !is.null(thematic$classes) &&
     !any(grepl("^Class ", thematic$classes$labels)))

big <- render_one("To2025 100 m regionalisation",
                  "To2025-tier_1_major_environmental_units",
                  expect_thematic = TRUE)
ok("the 2.5-billion-cell layer reads an overview, not the full grid",
   big$info$src_read < big$info$src_cells / 10,
   sprintf("read %s of %s cells",
           format(big$info$src_read, big.mark = ","),
           format(big$info$src_cells, big.mark = ",")))
ok("the 100 m layer's nodata sentinel never becomes a class",
   !any(big$classes$values == 99) && !any(grepl("99", big$classes$labels)),
   paste(big$classes$labels, collapse = " | "))
ok("the 100 m layer draws only the cells that are really classified",
   sum(big$plot$data$value %in% big$classes$values) < 5000,
   sprintf("%d classified cells", sum(big$plot$data$value %in% big$classes$values)))

bed <- render_one("IBCSO bathymetry", "IBCSO_bed")
ok("the bathymetry sentinel is kept out of the colour ramp",
   bed$info$range[1] > -9999, paste(round(bed$info$range, 1), collapse = ".."))

missing <- tryCatch(render_layer_plot(layer_by_id("Ch2019-Scleractinia"), 1L, base, labels),
                    error = function(e) conditionMessage(e))
ok("a layer with no COG fails with an explanation, not a stack trace",
   is.character(missing) && grepl("not published as an individual layer", missing),
   substr(missing, 1L, 70L))

# ---------------------------------------------------------------------------
section("downloads")

cog_layer <- layer_by_id("Fr2019-Electrona_antarctica")
d1 <- layer_delivery(cog_layer)
ok("a COG layer is delivered as the COG itself", identical(d1$route, "cog"))
zip_layer <- cat_data[is.na(cat_data$cog_url) & !is.na(cat_data$zip_url), ][1, ]
if (nrow(zip_layer)) {
  d2 <- layer_delivery(zip_layer)
  ok("a COG-less layer falls back to its publication archive",
     identical(d2$route, "zenodo"), d2$route)
}
none_layer <- cat_data[is.na(cat_data$cog_url) & is.na(cat_data$zip_url), ][1, ]
if (nrow(none_layer)) {
  d3 <- layer_delivery(none_layer)
  ok("a layer with neither source is reported as unavailable",
     identical(d3$route, "none"), d3$route)
}

csv <- metadata_csv(cat_data[1, ])
ok("metadata CSV export is produced", nzchar(csv) && grepl("id,file,taxon", csv))
ok("metadata CSV quotes commas inside values",
   grepl('"[^"]*,[^"]*"', csv))
ok("metadata CSV has a header and exactly one record",
   length(strsplit(csv, "\n", fixed = TRUE)[[1]]) == 2L)
ok("metadata CSV refuses multi-row input",
   inherits(try(metadata_csv(cat_data[1:3, ]), silent = TRUE), "try-error"))
ok("collection bibtex names the concept DOI",
   grepl("10.5281/zenodo.10910075", collection_bibtex(), fixed = TRUE))

# ---------------------------------------------------------------------------
section("cache")

key <- cache_key(cog_layer, 1L)
ok("cache key is filesystem safe", grepl("^[A-Za-z0-9]+$", key))
ok("cache key changes with the band", !identical(key, cache_key(cog_layer, 2L)))
ok("cache key changes with the etag",
   !identical(key, cache_key(transform(cog_layer, etag = "changed"), 1L)))
ok("cached render round-trips",
   {
     cached_render(cog_layer, 1L, base, labels)
     file.exists(file.path(app_paths$renders, paste0(key, ".rds")))
   })

# ---------------------------------------------------------------------------
cat(sprintf("\n%s\n", if (failures == 0L) "all checks passed" else
  sprintf("%d check(s) FAILED", failures)))
quit(status = if (failures == 0L) 0L else 1L)
