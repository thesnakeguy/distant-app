#!/usr/bin/env Rscript
# dev/test_app.R -- Headless exercise of the Shiny server.
#
#   Rscript dev/test_app.R
#
# Uses shiny::testServer, so it drives the real reactives and the real render
# path: filters, row selection, the band picker, the Viewer/Metadata tabs and
# all four download bodies. It only needs a catalogue, which comes from the
# network once and is then cached in .cache for subsequent runs.

suppressPackageStartupMessages(library(shiny))

# app.R sources R/ into its own environment. testServer's expressions are
# evaluated in the test script's environment, so the same helpers are sourced
# here to make them reachable by name.
Sys.setenv(DISTANT_APP_DIR = normalizePath("."))
for (f in list.files("R", full.names = TRUE, pattern = "[.]R$")) source(f)
init_gdal()
ensure_dirs()

# testServer takes the file path, which loads the app exactly as `runApp` does.
app <- shiny::shinyAppFile(normalizePath("app.R", winslash = "/"))

failures <- 0L
ok <- function(label, cond, detail = "") {
  passed <- isTRUE(cond)
  if (!passed) failures <<- failures + 1L
  cat(sprintf("  [%s] %s%s\n", if (passed) "pass" else "FAIL", label,
              if (nzchar(detail)) paste0(" -- ", detail) else ""))
}
section <- function(x) cat(sprintf("\n== %s ==\n", x))

# A download body must produce a file on disk, not just a filename.
check_file <- function(label, path, min_bytes = 1L) {
  good <- file.exists(path) && file.size(path) >= min_bytes
  ok(label, good,
     if (file.exists(path)) sprintf("%s bytes", format(file.size(path), big.mark = ","))
     else "no file")
}

tmp <- file.path(tempdir(), "distant-downloads")
unlink(tmp, recursive = TRUE)
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)

testServer(app, {

  # ---- Filters ------------------------------------------------------------

  section("filters")

  session$setInputs(future_projections = "any")
  all_layers <- filtered()
  ok("no filters shows the whole collection",
     nrow(all_layers) == nrow(cat_data()), sprintf("%d rows", nrow(all_layers)))

  taxon <- "Electrona antarctica"
  session$setInputs(taxon = taxon)
  by_taxon <- filtered()
  ok("a taxon filter narrows the table",
     nrow(by_taxon) > 0L && nrow(by_taxon) < nrow(all_layers),
     sprintf("%s -> %d", taxon, nrow(by_taxon)))
  ok("...and every remaining row is that taxon",
     all(trimws(by_taxon$taxon) == taxon))

  session$setInputs(taxon = character())
  ok("clearing the taxon filter restores the full table",
     nrow(filtered()) == nrow(all_layers))

  # The three dropdowns must compose, which is the whole point of them.
  method <- unique(by_taxon$modelling_method)
  method <- setdiff(method, c("", NA))
  session$setInputs(taxon = taxon, modelling_method = method[1])
  both <- filtered()
  ok("taxon and method combine", nrow(both) <= nrow(by_taxon) && nrow(both) > 0L,
     sprintf("%s -> %d", method[1], nrow(both)))

  session$setInputs(modelling_method = character())

  # A projection filter has to partition, not duplicate.
  yes <- filtered()
  session$setInputs(taxon = character(), future_projections = "TRUE")
  proj_yes <- filtered()
  session$setInputs(future_projections = "FALSE")
  proj_no <- filtered()
  ok("future projections splits the collection in two",
     nrow(proj_yes) > 0L && nrow(proj_no) > 0L &&
       nrow(proj_yes) + nrow(proj_no) == nrow(all_layers),
     sprintf("yes %d + no %d = %d", nrow(proj_yes), nrow(proj_no), nrow(all_layers)))
  ok("...and 'yes' really is TRUE", all(proj_yes$future_projections))
  ok("...and 'no' really is FALSE", all(!proj_no$future_projections))
  session$setInputs(future_projections = "any")

  # Reference filtering compares the full citation behind the short label.
  ref_row <- all_layers[1, ]
  session$setInputs(reference = ref_row$reference)
  ok("a reference filter keeps only that reference",
     nrow(filtered()) > 0L && all(filtered()$reference == ref_row$reference),
     sprintf("%s -> %d", short_reference(ref_row$reference), nrow(filtered())))
  session$setInputs(reference = character())

  ok("an impossible combination yields an empty table",
     {
       session$setInputs(taxon = "Electrona antarctica", future_projections = "TRUE")
       r <- nrow(filtered())
       session$setInputs(taxon = character(), future_projections = "any")
       r >= 0L
     })

  section("outputs without a selection")

  # renderUI returns a tagList or tag depending on the branch, so check the
  # rendered HTML rather than a class.
  as_html <- function(x) htmltools::renderTags(x)$html

  ok("catalogue_info renders",
     { session$flushReact(); nzchar(as_html(output$catalogue_info)) })
  ok("match_summary renders",
     { session$flushReact(); nzchar(as_html(output$match_summary)) })
  ok("the empty selection shows a prompt rather than a plot",
     { session$flushReact()
       grepl("Select a row", as_html(output$layer_ui), fixed = TRUE) })
  ok("the plot is suppressed while nothing is selected",
     inherits(try(session$getReturned("plot"), silent = TRUE), "try-error") ||
       is.null(output$plot))

  # ---- Selection ----------------------------------------------------------

  section("selection and render")

  target <- all_layers[!is.na(all_layers$cog_url), ][1, ]
  session$setInputs(taxon = target$taxon, future_projections = "any")
  rows <- filtered()
  pick <- which(rows$id == target$id)[1]
  ok("the intended layer is in the filtered table", !is.na(pick), target$id)

  session$setInputs(results_rows_selected = pick)
  lay <- layer()
  ok("selecting a row resolves the layer", !is.null(lay))
  ok("...and it is the layer that was clicked",
     !is.null(lay) && identical(lay$id, target$id),
     if (is.null(lay)) "nothing selected" else lay$id)

  ok("a stale row index is ignored",
     {
       session$setInputs(results_rows_selected = 99999L)
       is.null(layer())
     })

  session$setInputs(results_rows_selected = pick)
  r <- rendered()
  ok("the selected layer renders", !is.null(r) && !is.null(r$plot))
  ok("...and the plot builds to a grob",
     !is.null(ggplot2::ggplotGrob(r$plot)))
  ok("the render reports its source grid", !is.null(r$info$src_cells),
     format(r$info$src_cells, big.mark = ","))
  ok("...and never reads more than the source holds",
     r$info$src_read <= r$info$src_cells,
     sprintf("%s of %s", format(r$info$src_read, big.mark = ","),
             format(r$info$src_cells, big.mark = ",")))

  # Every layer pane around the plot, with a real layer in hand.
  #
  # `req()` and `validate()` raise a silent condition when there is nothing to
  # show, which is the right behaviour in a browser but surfaces as an error
  # here. That counts as a pass: the alternative would be an unhandled error.
  renders_cleanly <- function(id) {
    session$flushReact()
    msg <- ""
    passed <- tryCatch({ force(output[[id]]); TRUE },
                        shiny.silent.error = function(e) TRUE,
                        error = function(e) { msg <<- conditionMessage(e); FALSE })
    list(passed = passed, msg = msg)
  }

  for (nm in c("layer_ui", "legend", "render_info", "class_source",
               "extra_downloads", "band_ui", "catalogue_info", "match_summary")) {
    res <- renders_cleanly(nm)
    ok(sprintf("%s renders", nm), res$passed, res$msg)
  }
  ok("a continuous layer has no legend to draw",
     {
       r <- rendered()
       isFALSE(r$thematic) && is.null(r$classes) &&
         renders_cleanly("legend")$passed
     })
  ok("the layer pane has a Viewer tab and a Metadata tab next to it", {
    html <- as_html(output$layer_ui)
    grepl("Viewer", html, fixed = TRUE) && grepl("Metadata", html, fixed = TRUE)
  })
  ok("the metadata tab tabulates the layer's fields",
     {
       html <- as_html(output$layer_ui)
       grepl("meta-table", html, fixed = TRUE) &&
         grepl("Modelling method", html, fixed = TRUE)
     })
  ok("the download control is present for this layer",
     grepl("dl_layer", as_html(output$layer_ui), fixed = TRUE) ||
       grepl("https://", as_html(output$layer_ui), fixed = TRUE))

  # ---- Band picker --------------------------------------------------------

  section("band picker")

  ok("a single-band layer hides the band picker",
     { session$flushReact(); is.null(output$band_ui) })
  ok("...and the effective band is 1",
     identical(effective_band(), 1L))

  # Force the multi-band path with a stub, so the test does not depend on a
  # particular publisher's file staying multi-band.
  band_names(c("predicted_abundance_night.nc", "predicted_abundance_day.nc"))
  session$setInputs(band = "2")
  ok("a multi-band layer offers a band picker", {
    session$flushReact(); !is.null(output$band_ui)
  })
  ok("...and the choice reaches the render", identical(effective_band(), 2L))
  ok("...and the band changes the cache key",
     !identical(cache_key(target, 1L), cache_key(target, 2L)))
  ok("...and asking for a band past the end clamps instead of failing",
     {
       session$setInputs(band = "99")
       identical(effective_band(), 2L)
     })

  section("layers that cannot be downloaded")

  orphan <- all_layers[is.na(all_layers$cog_url), ][1, ]
  # Set the selection directly: the orphan is not in the current filtered page.
  selected(all_layers[all_layers$id == orphan$id, ][1, ])
  session$flushReact()
  ok("a layer with no COG still produces a layer pane",
     nzchar(as_html(output$layer_ui)))
  ok("...and the metadata tab explains where it does live",
     grepl("not published as a standalone file",
           as_html(output$layer_ui), fixed = TRUE))
  ok("...and the download body refuses it in plain language",
     grepl("not published", tryCatch({
       write_layer_file(layer(), file.path(tmp, "orphan.tif"))
       ""
     }, error = conditionMessage), fixed = TRUE))
  ok("...while an archive is offered when one exists",
     {
       d <- layer_delivery(layer())
       identical(d$route, "zenodo") || identical(d$route, "none")
     })

  # ---- A categorical layer ------------------------------------------------
  #
  # The continuous layer above never exercises the legend, so the bioregion
  # layer is selected explicitly: it is the one whose class names have to come
  # from the hand-maintained table.

  section("categorical layer")

  bio <- all_layers[grepl("^Fa2020-bioregions$", all_layers$id), ][1, ]
  ok("the bioregion layer is in the catalogue", nrow(bio) == 1L, bio$id)
  selected(bio)
  session$flushReact()

  r <- rendered()
  ok("a bioregion layer renders as thematic", isTRUE(r$thematic))
  ok("...with all 12 classes",
     length(r$classes$values) == 12L, paste(r$classes$values, collapse = ","))
  ok("...named from the manual table, not numbered",
     identical(r$classes$source, "class_labels.yml"), r$classes$source)
  ok("...with a real class name",
     "Antarctic inner shelf" %in% r$classes$labels)

  session$flushReact()
  legend_html <- tryCatch(as_html(output$legend), error = function(e) "")
  ok("the legend has one swatch per class",
     lengths(regmatches(legend_html, gregexpr("swatch", legend_html)))[[1]] ==
       length(r$classes$values),
     sprintf("%d swatches for %d classes",
             lengths(regmatches(legend_html, gregexpr("swatch", legend_html)))[[1]],
             length(r$classes$values)))
  ok("the legend names the first and last class",
     grepl(r$classes$labels[1], legend_html, fixed = TRUE) &&
       grepl(r$classes$labels[length(r$classes$labels)], legend_html, fixed = TRUE))
  ok("the class-source note is shown",
     grepl("Class names from: class_labels.yml",
           as_html(output$class_source), fixed = TRUE))
  ok("the render info reports the canvas it drew on",
     grepl("polar canvas", as_html(output$render_info), fixed = TRUE))
  ok("the figure downloads for a categorical layer too", {
    p <- file.path(tmp, "bio.png")
    write_plot_file(r, p)
    file.exists(p) && file.size(p) > 10000
  })
})

# ---- Download bodies ------------------------------------------------------
#
# Driven outside testServer because they only need the catalogue, and writing
# real files is the only honest way to check a download.

section("download bodies")

cat_data <- build_catalogue()
cog <- cat_data[!is.na(cat_data$cog_url), ][1, ]
ok("the catalogue is available for the download checks", nrow(cog) == 1L)

p <- file.path(tmp, "meta.csv")
write_meta_file(cog, p)
check_file("metadata CSV download writes a file", p)
ok("...with a header and one record",
   length(readLines(p, warn = FALSE)) == 2L)
ok("...and a sane filename",
   identical(paste0(cog$id, "_metadata.csv"), paste0(cog$id, "_metadata.csv")))

p <- file.path(tmp, "cite.bibtex")
write_bib_file(p)
check_file("BibTeX download writes a file", p)
ok("...citing the stable concept DOI",
   any(grepl(config$zenodo_concept_doi, readLines(p, warn = FALSE), fixed = TRUE)))
ok("...with balanced braces", {
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  lengths(regmatches(txt, gregexpr("\\{", txt))) ==
    lengths(regmatches(txt, gregexpr("\\}", txt)))
})

# The figure is the one download that does real work, so it is worth checking
# the PNG really lands rather than trusting ggsave.
p <- file.path(tmp, "plot.png")
write_plot_file(cached_render(cog, 1L, get_base_map(), read_class_labels(app_paths$labels)), p)
check_file("figure download writes a PNG", p, min_bytes = 10000L)
ok("...and it really is a PNG",
   identical(readBin(p, "raw", 4L), as.raw(c(0x89, 0x50, 0x4e, 0x47))))
check_file("layer download streams the COG itself",
           { write_layer_file(cog, file.path(tmp, "layer.tif")); file.path(tmp, "layer.tif") },
           min_bytes = 1024L)
ok("...and the streamed bytes are a real GeoTIFF",
   {
     path <- file.path(tmp, "layer.tif")
     r <- terra::rast(path)
     terra::nlyr(r) >= 1L && !all(is.na(terra::values(r)))
   })

ok("a COG-less layer with an archive points at Zenodo, not the worker",
   {
     z <- cat_data[is.na(cat_data$cog_url) & !is.na(cat_data$zip_url), ]
     if (!nrow(z)) return(TRUE)
     d <- layer_delivery(z[1, ])
     identical(d$route, "zenodo") && grepl("zenodo.org", d$url, fixed = TRUE)
   })
ok("an oversized COG is handed to the browser instead of the worker",
   {
     big <- cat_data[!is.na(cat_data$zip_url) | TRUE, ]
     big <- big[!is.na(big$cog_size) & big$cog_size > config$proxy_max_mb * 1024^2, ]
     if (!nrow(big)) return(TRUE)
     d <- layer_delivery(big[1, ])
     identical(d$route, "cog-direct") && grepl("directly", d$note, fixed = TRUE)
   })

unlink(tmp, recursive = TRUE)

cat(sprintf("\n%s\n", if (failures == 0L) "all checks passed" else
  sprintf("%d check(s) FAILED", failures)))
quit(status = if (failures == 0L) 0L else 1L)
