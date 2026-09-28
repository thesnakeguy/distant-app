# R/downloads.R -- Getting data and figures out of the app.
#
# Two routes exist, and the UI picks per layer:
#
#   * Source Cooperative serves every published layer as an individual COG.
#     Small files are streamed through the R worker so the browser saves them
#     under a sensible name; above config$proxy_max_mb the user gets the direct
#     link instead, so one 450 MB request cannot occupy a Shiny worker on an
#     institutional server.
#   * Zenodo archives whole publications as a single zip, which is the only
#     route for the handful of layers that are described in the metadata but
#     not published as standalone COGs.

#' Decide how a layer should be delivered.
#'
#' @return list with `route` ("cog", "cog-direct", "zenodo" or "none"), the
#'   target `url`, the suggested `filename`, `size` and a `note` for the UI.
layer_delivery <- function(layer) {
  has_cog <- !is.na(layer$cog_url)
  has_zip <- !is.na(layer$zip_url)
  if (!has_cog && !has_zip) {
    return(list(route = "none", url = NA_character_, filename = NA_character_,
                size = NA_real_,
                note = "This layer has no downloadable file yet."))
  }

  if (has_cog) {
    proxied <- layer$cog_size <= config$proxy_max_mb * 1024^2
    list(
      route    = if (proxied) "cog" else "cog-direct",
      url      = layer$cog_url,
      filename = layer$file,
      size     = layer$cog_size,
      note     = if (proxied) {
        sprintf("Just this layer (%s), streamed from Source Cooperative.",
                human_size(layer$cog_size))
      } else {
        sprintf(paste("Just this layer (%s) from Source Cooperative. It is too",
                      "large to send through this server, so the browser",
                      "downloads it directly."), human_size(layer$cog_size))
      }
    )
  } else {
    list(
      route    = "zenodo",
      url      = layer$zip_url,
      filename = layer$zip_name,
      size     = layer$zip_size,
      note     = sprintf(paste("Not published as a standalone layer. The",
                               "Zenodo archive for %s (%s) contains it."),
                         layer$pub_dir, human_size(layer$zip_size))
    )
  }
}

#' Stream a remote file into the temporary path Shiny handed us.
#'
#' Memory use is bounded by the chunk size, so the size limit is a politeness
#' measure rather than a correctness one.
stream_to_file <- function(url, path, chunk = 1024L^2L) {
  src <- url(url, open = "rb")
  on.exit(close(src), add = TRUE)
  sink <- file(path, open = "wb")
  on.exit(close(sink), add = TRUE)
  repeat {
    block <- readBin(src, "raw", n = chunk)
    if (!length(block)) break
    writeBin(block, sink)
  }
  invisible(path)
}

#' Escape a value for a CSV cell.
csv_cell <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  ifelse(grepl('[",\n]', x),
         paste0('"', gsub('"', '""', x, fixed = TRUE), '"'),
         x)
}

#' The full metadata record for a layer as a single-row CSV.
metadata_csv <- function(layer) {
  if (nrow(layer) != 1L) {
    stop("metadata_csv() exports one layer at a time; got ", nrow(layer),
         " rows.", call. = FALSE)
  }
  fields <- intersect(names(config$metadata_fields), names(layer))
  values <- vapply(fields, function(f) {
    v <- layer[[f]]
    if (is.logical(v)) {
      ifelse(is.na(v), "", ifelse(v, "yes", "no"))
    } else if (is.numeric(v)) {
      format(v, trim = TRUE, scientific = FALSE)
    } else {
      as.character(v)
    }
  }, character(1))
  # Raw field names, not the friendly labels, so the export can be appended to
  # DistAnt's own metadata.csv without renaming anything.
  names(values) <- fields

  header <- paste(csv_cell(names(values)), collapse = ",")
  row    <- paste(csv_cell(unname(values)), collapse = ",")
  paste(c(header, row), collapse = "\n")
}

# ---------------------------------------------------------------------------
# Download bodies
# ---------------------------------------------------------------------------
#
# These are the `content` functions of the app's four download handlers. They
# live here, rather than inline in the handlers, so dev/test_app.R can write a
# real file for each one instead of only checking that the button renders.

#' Stream the layer's own COG to a download.
write_layer_file <- function(lay, file) {
  if (is.null(lay) || is.na(lay$cog_url)) {
    stop("This layer is not published as an individual COG; use the ",
         "publication archive instead.", call. = FALSE)
  }
  stream_to_file(lay$cog_url, file)
  invisible(file)
}

#' Render the current viewer plot to a PNG.
write_plot_file <- function(r, file) {
  if (is.null(r) || is.null(r$plot)) {
    stop("No layer has been rendered yet.", call. = FALSE)
  }
  ggplot2::ggsave(file, r$plot, width = 10, height = 10, dpi = 150, bg = "#ffffff")
  invisible(file)
}

#' Write the layer's metadata record as a single-row CSV.
write_meta_file <- function(lay, file) {
  writeLines(metadata_csv(lay), file)
  invisible(file)
}

#' Write a BibTeX entry for the collection.
write_bib_file <- function(file) {
  writeLines(collection_bibtex(), file)
  invisible(file)
}
