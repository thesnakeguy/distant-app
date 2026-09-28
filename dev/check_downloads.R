#!/usr/bin/env Rscript
# dev/check_downloads.R -- Prove that every download format really delivers a
# usable file, for one specific layer.
#
#   Rscript dev/check_downloads.R                       # default layer
#   Rscript dev/check_downloads.R CR2014-Euphausia_superba_cog.tif
#
# The layer is chosen by its `file` value from metadata.csv, which is what the
# results table shows and what the app's own download button is named after.
#
# Unlike dev/test_app.R, this is a diagnostic: it prints what it found and only
# exits non-zero if a format is actually broken.

Sys.setenv(DISTANT_APP_DIR = normalizePath("."))
suppressPackageStartupMessages(library(terra))
for (f in list.files("R", full.names = TRUE, pattern = "[.]R$")) source(f)
init_gdal()
ensure_dirs()

want <- commandArgs(trailingOnly = TRUE)
if (!length(want)) want <- "CR2014-Euphausia_superba_cog.tif"

failures <- 0L
ok <- function(label, cond, detail = "") {
  passed <- isTRUE(cond)
  if (!passed) failures <<- failures + 1L
  cat(sprintf("  [%s] %s%s\n", if (passed) "pass" else "FAIL", label,
              if (nzchar(detail)) paste0(" -- ", detail) else ""))
}
section <- function(x) cat(sprintf("\n== %s ==\n", x))
hdr <- function(x) cat(sprintf("\n-- %s\n", x))

cat_data <- build_catalogue()
row <- cat_data[cat_data$file == want, ]
if (nrow(row) != 1L) {
  stop("No unique layer named ", want, " (", nrow(row), " matches).",
       call. = FALSE)
}
lay <- row[1, ]

hdr("catalogue row")
cat(sprintf("  id          %s\n", lay$id))
cat(sprintf("  file        %s\n", lay$file))
cat(sprintf("  taxon       %s\n", lay$taxon))
cat(sprintf("  output_type %s\n", lay$output_type))
cat(sprintf("  cog_url     %s\n", lay$cog_url))
cat(sprintf("  cog_size    %s bytes\n", format(lay$cog_size, big.mark = ",")))
cat(sprintf("  etag        %s\n", lay$etag))
cat(sprintf("  zip_url     %s\n", lay$zip_url))

dir <- file.path(tempdir(), "dl-check")
unlink(dir, recursive = TRUE)
dir.create(dir, recursive = TRUE, showWarnings = FALSE)

# ---------------------------------------------------------------------------
section("1. the COG")
# ---------------------------------------------------------------------------

d <- layer_delivery(lay)
cat(sprintf("  route  %s\n  note   %s\n", d$route, d$note))
ok("the catalogue says the file exists somewhere", d$route != "none")

path <- file.path(dir, lay$file)
write_layer_file(lay, path)

ok("a file was written", file.exists(path))
if (file.exists(path)) {
  n <- file.size(path)
  cat(sprintf("  size on disk  %s bytes\n", format(n, big.mark = ",")))
  # A truncated stream is the failure that would produce a .tif that GDAL
  # cannot open, so the byte count is checked against the catalogue.
  ok("the whole file arrived, not a truncated stream",
     n == lay$cog_size,
     sprintf("expected %s, got %s", format(lay$cog_size, big.mark = ","),
             format(n, big.mark = ",")))
}

r <- tryCatch(rast(path), error = function(e) e)
ok("terra can open the downloaded file", !inherits(r, "error"),
   if (inherits(r, "error")) conditionMessage(r) else "")

if (!inherits(r, "error")) {
  cat(sprintf("  dimensions   %d x %d x %d band(s)\n",
              nrow(r), ncol(r), nlyr(r)))
  cat(sprintf("  crs          %s\n", crs(r, proj = TRUE)))
  cat(sprintf("  band name    %s\n", names(r)))
  v <- values(r, mat = FALSE)
  cat(sprintf("  values       min %s  max %s  non-NA %s of %s\n",
              format(min(v, na.rm = TRUE), digits = 5),
              format(max(v, na.rm = TRUE), digits = 5),
              format(sum(!is.na(v)), big.mark = ","),
              format(length(v), big.mark = ",")))
  ok("it has at least one band", nlyr(r) >= 1L)
  ok("it has real data, not an all-nodata grid", any(!is.na(v)))
  ok("it carries a coordinate reference system", !is.na(crs(r)))
  ok("the band names survived the download",
     all(nzchar(names(r))), paste(names(r), collapse = ", "))

  # This layer is small, so overviews are irrelevant to it -- but they are what
  # let the app read a 2.5-billion-cell layer in ten seconds, so they are
  # checked on the largest one the collection has. dev/smoke_test.R covers the
  # same ground.
  ok("a multi-band file is a multi-band file all the way through",
     nlyr(r) == 2L, sprintf("%d band(s): %s", nlyr(r),
                            paste(names(r), collapse = ", ")))
  for (b in seq_len(nlyr(r))) {
    vb <- values(r[[b]], mat = FALSE)
    ok(sprintf("band %d (%s) holds data", b, names(r)[b]),
       any(!is.na(vb)),
       sprintf("min %s max %s", format(min(vb, na.rm = TRUE), digits = 4),
               format(max(vb, na.rm = TRUE), digits = 4)))
  }
}

hdr("the same file straight from the URL, with no app involved")
cat(sprintf("  %s\n", lay$cog_url))
r2 <- tryCatch(rast(paste0("/vsicurl/", lay$cog_url)), error = function(e) e)
ok("terra can open the COG over /vsicurl/ too", !inherits(r2, "error"),
   if (inherits(r2, "error")) conditionMessage(r2) else "")

hdr("what the app does when it opens this layer")
info <- tryCatch(open_cog(lay$cog_url), error = function(e) e)
ok("the band inventory can be read from the URL header alone",
   !inherits(info, "error"))
if (!inherits(info, "error")) {
  cat(sprintf("  nlyr  %d\n  bands %s\n", info$nlyr,
              paste(info$band_names, collapse = ", ")))
  ok("the app would show a band picker for this layer", info$nlyr > 1L,
     if (info$nlyr > 1L) "multi-band, so the picker appears" else "single band")
  for (b in seq_len(info$nlyr)) {
    rb <- tryCatch(render_layer_plot(lay, b, base = get_base_map(),
                                     class_labels = read_class_labels(app_paths$labels)),
                   error = function(e) e)
    ok(sprintf("band %d (%s) renders", b, info$band_names[b]),
       !inherits(rb, "error") && !inherits(ggplot2::ggplotGrob(rb$plot), "error"),
       if (inherits(rb, "error")) conditionMessage(rb) else
         if (isTRUE(rb$thematic))
           sprintf("categorical, %d classes", length(rb$classes$values))
         else sprintf("continuous, %s to %s",
                      format(rb$info$range[1], digits = 4),
                      format(rb$info$range[2], digits = 4)))
  }
}

# ---------------------------------------------------------------------------
section("2. the figure (PNG)")
# ---------------------------------------------------------------------------

p <- file.path(dir, paste0(lay$id, ".png"))
rend <- cached_render(lay, 1L, get_base_map(), read_class_labels(app_paths$labels))
ok("the layer renders", !is.null(rend) && !is.null(rend$plot))
write_plot_file(rend, p)
ok("a file was written", file.exists(p))
if (file.exists(p)) {
  n <- file.size(p)
  cat(sprintf("  size on disk  %s bytes\n", format(n, big.mark = ",")))
  ok("it is not empty", n > 5000L, format(n, big.mark = ","))
  magic <- readBin(p, "raw", 8L)
  ok("it is a real PNG",
     identical(magic[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47))))
  # The IHDR chunk carries width and height as big-endian uint32 at bytes
  # 17-24 of the file: 8-byte signature, 4-byte length, 4-byte "IHDR".
  con <- file(p, "rb")
  ihdr <- readBin(con, "raw", n = 24L)
  close(con)
  w <- sum(as.integer(ihdr[17:20]) * c(2^24, 2^16, 2^8, 1))
  h <- sum(as.integer(ihdr[21:24]) * c(2^24, 2^16, 2^8, 1))
  cat(sprintf("  dimensions   %d x %d px (expected %d x %d at 150 dpi)\n",
              w, h, 1500L, 1500L))
  ok("it has the dimensions ggsave was asked for", w == 1500L && h == 1500L)
}

# ---------------------------------------------------------------------------
section("3. the metadata (CSV)")
# ---------------------------------------------------------------------------

p <- file.path(dir, paste0(lay$id, "_metadata.csv"))
write_meta_file(lay, p)
ok("a file was written", file.exists(p))
if (file.exists(p)) {
  lines <- readLines(p, warn = FALSE)
  cat(sprintf("  size on disk  %s bytes, %d line(s)\n",
              format(file.size(p), big.mark = ","), length(lines)))
  back <- utils::read.csv(p, colClasses = "character", check.names = FALSE)
  ok("it parses back as a table", nrow(back) == 1L && ncol(back) > 0L,
     sprintf("%d x %d", nrow(back), ncol(back)))
  ok("it carries the layer's id", identical(back$id, lay$id), back$id)
  ok("the taxon survived quoting and escaping",
     identical(back$taxon, lay$taxon), back$taxon)
  ok("it uses metadata.csv's own column names, not display labels",
     all(c("id", "file", "taxon", "reference", "licence") %in% names(back)))
  cat("\n  --- first 400 characters ---\n")
  cat(substr(paste(lines, collapse = "\n"), 1, 400), "\n")
}

# ---------------------------------------------------------------------------
section("4. the citation (BibTeX)")
# ---------------------------------------------------------------------------

p <- file.path(dir, "SCAR_DistAnt.bibtex")
write_bib_file(p)
ok("a file was written", file.exists(p))
if (file.exists(p)) {
  txt <- paste(readLines(p, warn = FALSE), collapse = "\n")
  cat(sprintf("\n%s\n", txt))
  ok("it is a BibTeX entry", grepl("^@", txt))
  ok("it cites the concept DOI, not a version DOI",
     grepl(config$zenodo_concept_doi, txt, fixed = TRUE) &&
       !grepl(sub("^10\\.5281/zenodo\\.", "", config$zenodo_concept_doi), txt,
              fixed = TRUE))
  ok("its braces balance",
     lengths(regmatches(txt, gregexpr("\\{", txt))) ==
       lengths(regmatches(txt, gregexpr("\\}", txt))))
}

# ---------------------------------------------------------------------------
section("5. the Zenodo archive for this publication")
# ---------------------------------------------------------------------------

ok("the layer's publication has an archive", !is.na(lay$zip_url),
   if (is.na(lay$zip_url)) "none" else lay$zip_name)
ok("the archive really is on Zenodo",
   !is.na(lay$zip_url) && grepl("zenodo.org", lay$zip_url, fixed = TRUE))
if (!is.na(lay$zip_url)) {
  cat(sprintf("  url   %s\n  size  %s\n", lay$zip_url,
              human_size(lay$zip_size)))
  # HEAD only: the archive can be hundreds of megabytes and this is a check on
  # the link, not on the bytes.
  code <- tryCatch({
    res <- curl::curlFetchMemory(lay$zip_url,
                                  handle = curl::new_handle(nobody = TRUE))
    as.integer(attr(res, "status_code"))
  }, error = function(e) NA_integer_)
  ok("the archive URL responds and is a file, not an error page",
     !is.na(code) && code == 200L, sprintf("HTTP %s", code))
}

unlink(dir, recursive = TRUE)
cat(sprintf("\n%s\n", if (failures == 0L) "all checks passed" else
  sprintf("%d check(s) FAILED", failures)))
quit(status = if (failures == 0L) 0L else 1L)
