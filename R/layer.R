prepare_layer <- function(url, ext) {
  src <- terra::rast(paste0("/vsicurl/", utils::URLencode(url, reserved = FALSE)))[[1]]
  proj <- terra::project(src, "EPSG:4326")
  proj_crop <- terra::crop(x = proj, y = ext)
  proj_ant <- terra::project(proj_crop, "EPSG:3031", method = "bilinear") 
  projected <- raster::raster(proj_ant)
}

build_map <- function(layer, base_gg, col = hcl.colors(100, "viridis")) {
  plot(base_gg)
  SOmap::SOplot(layer, add = TRUE, col = col)
}
