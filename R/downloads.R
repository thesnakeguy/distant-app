download_png <- function(layer, base_map, col, file,
                         width = 8, height = 8, dpi = 150) {
  suppressWarnings(
    ggplot2::ggsave(file, build_map(layer, base_map, col),
                    width = width, height = height, dpi = dpi, bg = "white")
  )
}

download_tif <- function(url, file) {
  old <- options(timeout = 3600)
  on.exit(options(old), add = TRUE)
  utils::download.file(utils::URLencode(url, reserved = FALSE), file,
                       mode = "wb", quiet = TRUE)
}
