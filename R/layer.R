layer_source <- function(url) {
  terra::rast(paste0("/vsicurl/", utils::URLencode(url, reserved = FALSE)))
}

layer_bands <- function(url) {
  r <- layer_source(url)
  nms <- names(r)
  bad <- is.na(nms) | !nzchar(nms)
  nms[bad] <- paste0("band ", which(bad))
  nms
}

prepare_layer <- function(url, ext, band = 1) {
  src <- layer_source(url)[[band]]
  src_crop <- terra::crop(
    src,
    ext
  )
  raster::raster(src_crop)
}


# Discrete layers (whole-number values: species richness, classes) get their
# own colour scheme, continuous layers keep the viridis ramp
layer_colours <- function(layer) {
  v <- raster::sampleRegular(layer, 20000)
  v <- v[!is.na(v)]
  discrete <- length(v) > 0 && all(v == round(v))
  hcl.colors(100, if (discrete) "YlGnBu" else "viridis")
}

build_map <- function(layer, base_gg, col = layer_colours(layer),
                      file = NULL, width = 8, height = 8, dpi = 150) {
  if (!is.null(file)) {
    grDevices::png(file, width = width, height = height, units = "in",
                   res = dpi, bg = "white")
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  plot(base_gg)
  SOmap::SOplot(layer, add = TRUE, col = col)
  invisible(file)
}
