download_png <- function(layer, base_map, file,
                         width = 8, height = 8, dpi = 150) {
  build_map(layer, base_map, file = file,
            width = width, height = height, dpi = dpi)
}

download_tif <- function(url, file) {
  old <- options(timeout = 3600)
  on.exit(options(old), add = TRUE)
  utils::download.file(utils::URLencode(url, reserved = FALSE), file,
                       mode = "wb", quiet = TRUE)
}

# fetch(name, dest) must save one file; files are the archive contents
download_zip <- function(files, file, fetch) {
  dir <- tempfile("distant")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  paths <- vapply(files, function(name) {
    fetch(name, file.path(dir, name))
    file.path(dir, name)
  }, character(1), USE.NAMES = FALSE)
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zipr(file, basename(paths), root = dir)
  } else {
    utils::zip(file, paths, flags = "-j")
  }
}
