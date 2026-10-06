# index <- source_coop_index()
# url <- layer_url(index, 1)
# url


# ext <- terra::ext(-180, 180, -90, -45) %>%
#   terra::vect(crs = "EPSG:4326")
# layer <- prepare_layer(url, ext)

# prepare_layer <- function(url, ext, band = 1) {
#   src <- layer_source(url)[[band]]
#   ext_src <- terra::project(ext, terra::crs(src))
#   src_crop <- terra::crop(src, ext_src)
#   proj_ant <- terra::project(
#   src_crop,
#   "EPSG:3031",
#   method = "bilinear",
#   res = 10000
# )
#   raster::raster(proj_ant)
# }

# ext <- terra::ext(
#   -3300000,
#    2200000,
#    150000,
#    3400000
# )

# prepare_layer <- function(url, ext, band = 1) {
#   src <- layer_source(url)[[band]]
#   src_crop <- terra::crop(
#     src,
#     ext
#   )
#   raster::raster(src_crop)
# }

# build_map(layer, base_gg)

