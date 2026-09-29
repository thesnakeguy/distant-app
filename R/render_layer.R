# R/render_layer.R -- Turning a cloud-optimized geotiff into a plot.
#
# The collection is 1.9 GB in total and includes rasters with 2.5 billion cells
# and files of 450 MB, so nothing here ever reads at full resolution. GDAL
# serves the decimated read straight from the COG's internal overviews, which
# is what keeps the app light enough for an institutional server.

# ---------------------------------------------------------------------------
# Access
# ---------------------------------------------------------------------------

#' Open a remote COG without downloading it.
#'
#' Everything downstream is a range request against this object. The band
#' inventory is reported so the UI can offer a picker for the multi-variable
#' outputs the collection README describes.
open_cog <- function(url) {
  r <- terra::rast(paste0("/vsicurl/", url))
  list(rast = r, nlyr = terra::nlyr(r), band_names = names(r))
}

# ---------------------------------------------------------------------------
# Reading a band onto the shared polar canvas
# ---------------------------------------------------------------------------

# terra reports GDAL's data types under its own short names; a VRT has to spell
# them the way the GDAL vocabulary does.
gdal_data_type <- function(type) {
  switch(as.character(type)[1],
         INT1U = "Byte",   INT1S = "Int8",
         INT2U = "UInt16", INT2S = "Int16",
         INT4U = "UInt32", INT4S = "Int32",
         INT8U = "UInt64", INT8S = "Int64",
         FLT4S = "Float32", FLT8S = "Float64",
         FLT4L = "CFloat32", FLT8L = "CFloat64",
         as.character(type)[1])
}

#' How much a source band has to be decimated before it is warped.
#'
#' Integer, and never below one, so the grid it is resampled into keeps the
#' source's own pixel alignment. A band already smaller than the target is read
#' as-is: there is nothing to gain by inventing a smaller grid for it.
decimation_factor <- function(r, target_cells = config$source_target_cells) {
  ncol <- terra::ncol(r)
  if (!is.finite(ncol) || ncol <= target_cells) return(1L)
  max(1L, as.integer(floor(ncol / target_cells)))
}

#' A one band VRT that resamples `vsi` into a coarser grid.
#'
#' This is the trick that makes the app light enough to run anywhere. Reprojecting
#' a source straight onto the canvas is what every other GDAL reader does, and
#' GDALWarp answers it by reading the source window at full resolution -- for the
#' collection's 100 m regionalisation layers that is 2.5 billion cells, about
#' 160 seconds and 2.5 GB, for a map that is 550 pixels wide.
#'
#' A `SimpleSource` whose destination rectangle is much smaller than its source
#' rectangle is a different request. It comes back through `RasterIO`, which is
#' documented to select the closest internal overview of a TIFF when it is asked
#' for fewer cells than the band holds. The 100 m layer then reads its 8x
#' overview: 40 MB, under ten seconds. Nothing outside the GDAL that terra
#' already links against is involved, and the VRT is a few hundred bytes of
#' text that can be written once and reused.
#'
#' @return Path to the VRT.
write_source_vrt <- function(path, r, vsi, band = 1L, fact = 1L) {
  nc <- terra::ncol(r)
  nr <- terra::nrow(r)
  ext <- terra::ext(r)
  res <- terra::res(r)
  # Round the target grid up so the last row and column still cover the source;
  # the sliver that hangs over the edge reads as nodata, which is correct.
  dncol <- as.integer(ceiling(nc / fact))
  dnrow <- as.integer(ceiling(nr / fact))
  gt <- c(ext[1], res[1] * fact, 0, ext[4], 0, -res[2] * fact)

  nodata <- tryCatch(terra::NAflag(r)[band], error = function(e) NA_real_)
  # GDAL already treats NaN as nodata for floating point bands, and writing
  # <NoDataValue>nan</NoDataValue> is not portable across versions.
  nodata_tag <- if (length(nodata) == 1L && !is.na(nodata) && !is.nan(nodata)) {
    sprintf("    <NoDataValue>%s</NoDataValue>\n", format(nodata, scientific = FALSE))
  } else {
    ""
  }

  xml <- sprintf(
    paste0(
      '<VRTDataset rasterXSize="%d" rasterYSize="%d">\n',
      '  <SRS>%s</SRS>\n',
      '  <GeoTransform>%s</GeoTransform>\n',
      '  <VRTRasterBand dataType="%s" band="1">\n',
      '%s',
      '    <SimpleSource>\n',
      '      <SourceFilename relativeToVRT="0">%s</SourceFilename>\n',
      '      <SourceBand>%d</SourceBand>\n',
      '      <SrcRect xOff="0" yOff="0" xSize="%d" ySize="%d"/>\n',
      '      <DstRect xOff="0" yOff="0" xSize="%d" ySize="%d"/>\n',
      '    </SimpleSource>\n',
      '  </VRTRasterBand>\n',
      '</VRTDataset>\n'
    ),
    dncol, dnrow,
    xml_escape(terra::crs(r, proj = TRUE)),
    paste(sprintf("%.15g", gt), collapse = ","),
    gdal_data_type(terra::datatype(r)[band]),
    nodata_tag,
    xml_escape(vsi), as.integer(band), nc, nr, dncol, dnrow
  )

  tmp <- paste0(path, ".tmp")
  writeLines(xml, tmp, useBytes = TRUE)
  # Renaming keeps two concurrent renders from ever seeing a half written VRT.
  if (!file.rename(tmp, path)) stop("Could not write ", path, call. = FALSE)
  path
}

#' Open a source band decimated to roughly `target_cells` across.
#'
#' Returns the raster itself when no decimation is needed, so the common case --
#' the many small, already-tiled habitat suitability layers -- costs nothing but
#' the read it was always going to do.
decimated_source <- function(r, vsi, band = 1L, fact = 1L,
                             target_cells = config$source_target_cells) {
  if (fact <= 1L) return(r)
  if (is.null(vsi) || is.na(vsi) || !nzchar(vsi)) return(r)

  # The name carries the source, the band and the factor, so a changed factor
  # or a new band writes a new file instead of reading a stale grid.
  key <- paste0(basename(vsi), "_", as.integer(band), "_", as.integer(fact))
  path <- file.path(app_paths$vrt,
                    paste0(substr(gsub("[^A-Za-z0-9._-]", "_", key), 1L, 100L), ".vrt"))
  if (!file.exists(path)) {
    write_source_vrt(path, r, vsi, band, fact)
  }
  terra::rast(path)
}

#' The one canvas every layer is projected onto.
#'
#' Empty, so building it costs nothing, but it pins the output grid exactly:
#' CRS, extent and resolution all come from the template rather than from the
#' source layer. That is what makes a 0.1 degree product and a 100 m product
#' produce the same map, and what lets them line up with the cached base.
polar_template <- function(res = config$polar_px_res, ext = config$polar_extent) {
  terra::rast(
    xmin = -ext, xmax = ext, ymin = -ext, ymax = ext,
    resolution = res, crs = "EPSG:3031"
  )
}

#' Warp one band onto the polar canvas.
#'
#' `src` should already be decimated (see `decimated_source`); the warp itself is
#' a few hundred thousand output cells, which is cheap whichever resampling
#' method it is given.
warp_to_polar <- function(r, method = "near", template = polar_template()) {
  terra::project(r, template, method = method)
}

#' The band to read, plus the layer's own band inventory.
select_band <- function(r, band) {
  nlyr <- max(1L, terra::nlyr(r))
  band <- max(1L, min(as.integer(band), nlyr))
  list(
    rast       = if (band > 1L) terra::subset(r, band) else r,
    band       = band,
    nlyr       = nlyr,
    band_names = names(r)
  )
}

#' Value range for the colour ramp.
#'
#' Taken from the warped canvas rather than the source so that the ramp always
#' spans exactly the colours on screen. Some products ship
#' `STATISTICS_MINIMUM`/`MAXIMUM` tags, but their MEAN and STDDEV fold in the
#' nodata sentinel and their extremes can disagree with what survives warping to
#' this canvas, so the only statistics trusted here are the ones computed from
#' the data actually being drawn.
value_range <- function(values) {
  rng <- suppressWarnings(range(values[is.finite(values)], na.rm = TRUE))
  if (!length(rng) || any(!is.finite(rng)) || rng[1] >= rng[2]) c(0, 1) else
    as.numeric(rng)
}


#' Read one band of a layer onto the polar canvas.
#'
#' `thematic` decides the resampling method, and it has to be settled before the
#' warp because averaging a class map blends classes into values that exist in
#' no class. When the caller has not decided, the output type names are enough
#' to make the call without a second read; only genuinely ambiguous layers pay
#' for a near-neighbour probe on top of the real read.
#'
#' The source is decimated once, before the warp, so the expensive read is
#' satisfied from the COG's overviews. The probe reuses that same decimated
#' raster, so an ambiguous layer still only ever reads once at full width.
#'
#' @return list with `data` (data frame of x, y, value), `range`, `sampled`,
#'   band metadata, and `themed`.
load_band <- function(url, band = 1L, layer = NULL, thematic = NULL) {
  cog <- open_cog(url)
  sel <- select_band(cog$rast, band)
  r <- sel$rast

  if (is.null(thematic)) thematic <- is_thematic(layer, NULL)
  if (is.null(thematic)) thematic <- FALSE
  if (!is.logical(thematic) || length(thematic) != 1L || is.na(thematic)) {
    thematic <- FALSE
  }
  themed <- thematic

  # Built from the header we have already read, so this costs no extra request.
  src <- decimated_source(r, paste0("/vsicurl/", url), band,
                          decimation_factor(r))

  # The collection does not agree on how to flag nodata, and the reader does not
  # report what a file declares (terra returns NAflag() == NaN for every band
  # here), so the sentinels are named in config. They are stripped after the
  # warp rather than before it: the warp is the only step that reads the source
  # at all, and one read is worth more than a second pass over the header.
  nodata <- tryCatch(terra::NAflag(r)[band], error = function(e) NA_real_)

  warped <- warp_to_polar(src, method = if (themed) "near" else "average")
  df <- as_canvas_frame(warped, nodata)
  if (is.null(df)) {
    stop("This layer does not cover the polar canvas; it cannot be drawn on the ",
         "Antarctic base map.", call. = FALSE)
  }

  # The probe is only needed when the name-based guess came back undecided.
  if (!themed) {
    looks_thematic <- is_thematic(layer, df$value)
    if (isTRUE(looks_thematic)) {
      themed <- TRUE
      df <- as_canvas_frame(warp_to_polar(src, method = "near"), nodata)
      if (is.null(df)) {
        stop("This layer does not cover the polar canvas; it cannot be drawn on the ",
             "Antarctic base map.", call. = FALSE)
      }
    }
  }

  list(
    data       = df,
    range      = value_range(df$value),
    sampled    = df$value,
    band_name  = if (length(sel$band_names) >= sel$band) sel$band_names[sel$band]
                  else names(r)[1],
    nlyr       = sel$nlyr,
    band_names = sel$band_names,
    crs        = as.character(terra::crs(r, describe = FALSE)),
    res        = terra::res(r)[1],
    src_cells  = terra::ncell(r),
    src_read   = if (identical(src, r)) terra::ncell(r) else terra::ncell(src),
    nodata     = nodata,
    themed     = themed
  )
}

#' Canvas cells as a data frame, ready to plot.
#'
#' The full canvas extent is kept, NA cells included, for two reasons. It keeps
#' every layer on the same grid whatever its coverage, which is what makes the
#' panels comparable side by side; and it keeps the raster's cell edges aligned
#' with the panel's, which is what stops ggplot2 warning that the pixels will be
#' shifted. Missing cells are drawn transparent by the scale's `na.value`.
#'
#' Only the nodata sentinels are dropped. They are finite, so they would
#' otherwise be drawn as data.
#'
#' @param nodata the band nodata, or NA when it has none.
#' @return NULL when nothing survives, which is how a layer that does not reach
#'   the canvas is told apart from one that does.
as_canvas_frame <- function(warped, nodata = NA_real_) {
  df <- terra::as.data.frame(warped, xy = TRUE, na.rm = FALSE)
  if (!nrow(df)) return(NULL)
  names(df)[3] <- "value"
  v <- df$value
  keep <- is.na(v) | (is.finite(v) &
    !v %in% config$nodata_sentinels &
    !(length(nodata) == 1L && !is.na(nodata) && !is.nan(nodata) & v == nodata))
  df <- df[keep, , drop = FALSE]
  if (!any(is.finite(df$value))) return(NULL)
  df
}

# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------

continuous_scale <- function(output_type, limits) {
  spec <- config$continuous_palettes[[output_type]]
  pal <- if (is.null(spec)) "viridis" else spec$palette
  rev <- isTRUE(spec$rev)
  # `na.rm` is deliberately absent: the canvas frame has already dropped
  # non-finite cells, and ggplot2 4 rejects the argument on continuous scales.
  ggplot2::scale_fill_gradientn(
    colours  = grDevices::hcl.colors(51, palette = pal, rev = rev),
    limits   = limits,
    na.value = "#FFFFFF00",
    oob      = scales::squish
  )
}

thematic_scale <- function(cls) {
  ggplot2::scale_fill_manual(
    values   = stats::setNames(cls$colours, as.character(cls$values)),
    limits   = as.character(cls$values),
    breaks   = as.character(cls$values),
    labels   = cls$labels,
    na.value = "#FFFFFF00",
    drop     = FALSE
  )
}

#' Paint the base map underneath a layer.
#'
#' The base map is already a finished picture, so it is placed as one: a
#' full-canvas annotation under the raster, which is what the layer's own
#' transparent nodata is there to let through. Merging the base's layers into the
#' layer's plot instead cannot work -- ggplot2 allows one scale per aesthetic,
#' and the bathymetry and the layer both want `fill`, so one of the two would be
#' coloured with the other's ramp. Drawing the base first also keeps the layer's
#' own coordinate system, labels and theme authoritative.
underlay_base <- function(plot, panel, extent = config$polar_extent) {
  if (is.null(panel)) return(plot)
  # Prepended, not appended: a layer added with `+` is drawn last, and would
  # cover the map it is meant to sit on.
  plot$layers <- c(
    list(ggplot2::annotation_custom(panel, -extent, extent, -extent, extent)),
    plot$layers
  )
  plot
}

#' Compose the preview and the cached base map.
#'
#' @param base the SOmap base plot from basemap.R, or NULL to draw a bare map.
render_layer_plot <- function(layer, band = 1L, base = NULL,
                              class_labels = read_class_labels(app_paths$labels)) {
  if (is.na(layer$cog_url)) {
    stop(sprintf(
      paste("%s is described in the DistAnt metadata but is not published as",
            "an individual layer. The Metadata tab names the publication it",
            "belongs to; the full archive can be downloaded there."),
      layer$file), call. = FALSE)
  }

  d <- load_band(layer$cog_url, band, layer = layer)
  cls <- if (d$themed) resolve_classes(layer, d$band_name, d$sampled, class_labels) else NULL
  # A table that does not cover the observed classes falls back to numbering
  # rather than legend classes that were never drawn.
  if (is.null(cls)) d$themed <- FALSE
  if (d$themed) {
    # A class map is drawn on a discrete scale, and ggplot2 refuses to put
    # numeric data on one. The values are the keys of the label table, so the
    # same textual form is used on both sides.
    d$data$value <- as.character(d$data$value)
    # Anything the label table does not name is not one of its classes. This is
    # a second line of defence against an undeclared nodata sentinel, and it
    # keeps the legend to the classes that are actually on screen.
    known <- as.character(cls$values)
    d$data <- d$data[is.na(d$data$value) | d$data$value %in% known, , drop = FALSE]
  }

  # The frame is pinned rather than derived from the data, so a layer that only
  # covers part of the Southern Ocean still lines up with the base map, and so
  # the legend does not resize between layers.
  ext <- config$polar_extent

  p <- ggplot2::ggplot(d$data, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$value), interpolate = FALSE)

  p <- if (d$themed) p + thematic_scale(cls)
       else p + continuous_scale(layer$output_type, d$range)

  p <- p +
    ggplot2::coord_fixed(ratio = 1, xlim = c(-ext, ext), ylim = c(-ext, ext),
                         expand = FALSE) +
    ggplot2::labs(
      fill    = if (!is.na(d$band_name) && nzchar(d$band_name)) d$band_name
                else layer$output_type,
      caption = paste(layer$id, "\u00b7", config$polar_label)
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid   = ggplot2::element_blank(),
      axis.text    = ggplot2::element_blank(),
      axis.title   = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(size = 8, colour = "#666666"),
      plot.margin  = ggplot2::margin(0, 0, 2, 0)
    )

  p <- underlay_base(p, base_panel_grob(base))

  list(
    plot     = p,
    classes  = cls,
    thematic = d$themed,
    info     = d[c("band_name", "nlyr", "band_names", "crs", "range",
                   "src_cells", "src_read")]
  )
}

# ---------------------------------------------------------------------------
# Render cache
# ---------------------------------------------------------------------------

#' Stable cache key for a rendered layer.
#'
#' The etag is the source object's content hash, so an upstream re-upload
#' invalidates the cache automatically. The date keeps entries from a previous
#' release of the app from being mistaken for current ones.
cache_key <- function(layer, band) {
  # Everything that changes the pixels has to survive into the key, or a stale
  # render gets served. Only short, already-unique components are used (the COG
  # basename is unique across the whole bucket), so nothing is ever truncated
  # away and the key stays filesystem safe.
  ident <- basename(layer$cog_url %||% "")
  if (!nzchar(ident) || is.na(ident)) ident <- layer$id %||% "unknown"
  squash <- function(x) {
    gsub("[^A-Za-z0-9]", "", as.character(x), useBytes = TRUE)
  }
  paste0(
    as.integer(band),
    squash(tools::file_path_sans_ext(ident))[1],
    # The ETag changes whenever the publisher replaces the file, which is the
    # one thing that can invalidate a render without changing the URL.
    squash(substr(layer$etag %||% "-", 1L, 12L))[1],
    # How the base map is put under the layer, so a render composited the old way
    # is never served as though it had been.
    squash(base_format),
    paste0(squash(c(config$preview_px, config$polar_px_res, config$polar_extent,
                    config$source_target_cells)), collapse = ""),
    squash(format(Sys.Date(), "%Y-%m"))
  )
}

#' Serialise renders across every session.
#'
#' A rendered canvas is around 1.2 million cells, so letting N users each
#' trigger one simultaneously is the one way this app could be made to
#' misbehave on a small server. A render takes a couple of seconds, so queueing
#' is a better trade than unbounded memory. The lock is process-wide, always
#' released, and gives up rather than blocking a request forever.
with_render_lock <- function(expr, timeout_s = 180) {
  lock <- render_lock
  waited <- 0
  while (!is.null(lock$owner) && waited < timeout_s) {
    Sys.sleep(0.2)
    waited <- waited + 0.2
  }
  if (!is.null(lock$owner)) {
    stop("The render queue did not clear within two minutes. Try again.", call. = FALSE)
  }
  lock$owner <- TRUE
  on.exit(lock$owner <- NULL, add = TRUE)
  force(expr)
}

render_lock <- new.env(parent = emptyenv())
render_lock$owner <- NULL

#' Render a layer, reusing a cached result when it is still fresh.
#'
#' The cache is keyed on the source object's etag, so an upstream re-upload
#' invalidates it without anyone having to clear the directory by hand.
cached_render <- function(layer, band, base, class_labels) {
  path <- file.path(app_paths$renders, paste0(cache_key(layer, band), ".rds"))
  age_s <- config$cache_ttl_hours * 3600
  if (file.exists(path) &&
      difftime(Sys.time(), file.mtime(path), units = "secs") < age_s) {
    hit <- tryCatch(readRDS(path), error = function(e) NULL)
    if (!is.null(hit)) return(hit)
  }

  res <- with_render_lock(render_layer_plot(layer, band, base, class_labels))
  tryCatch({
    saveRDS(res, path)
    prune_render_cache()
  }, error = function(e) NULL)
  res
}

#' Keep the render cache from growing without bound.
#'
#' Each entry is a few MB, so on a small server the count matters more than
#' the age. Eviction is oldest-first by file time. The VRTs go at the same time:
#' they are pure derived data, a few hundred bytes each, and cheap to rewrite,
#' so they follow the same age limit rather than getting a policy of their own.
prune_render_cache <- function(max_entries = config$cache_max_entries) {
  files <- list.files(app_paths$renders, pattern = "\\.rds$", full.names = TRUE)
  if (length(files) > max_entries) {
    info <- file.info(files)
    unlink(files[order(info$mtime, decreasing = TRUE)[-seq_len(max_entries)]])
  }
  prune_source_vrts()
  invisible(TRUE)
}

#' Delete decimation VRTs older than the render cache lifetime.
prune_source_vrts <- function(ttl_hours = config$cache_ttl_hours) {
  files <- list.files(app_paths$vrt, pattern = "\\.vrt$", full.names = TRUE)
  if (!length(files)) return(invisible(TRUE))
  age <- as.numeric(difftime(Sys.time(), file.mtime(files), units = "hours"))
  unlink(files[!is.na(age) & age > ttl_hours])
  invisible(TRUE)
}

#' Empty the render cache, e.g. after changing the palette or class labels.
clear_render_cache <- function() {
  unlink(list.files(app_paths$renders, full.names = TRUE))
  unlink(list.files(app_paths$vrt, full.names = TRUE))
  invisible(TRUE)
}
