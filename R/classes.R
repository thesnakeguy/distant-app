# R/classes.R -- Thematic class labels and colours.
#
# DistAnt layers are mostly continuous, but a subset are categorical
# (bioregions, regionalisations, ecosystem types). Their class names live in
# three different places, in decreasing order of reliability:
#
#   1. a GDAL raster attribute table in a `.aux.xml` sidecar (Toth et al.),
#   2. a hand maintained table in inst/class_labels.yml (Fabri-Ruiz et al. ...),
#   3. nothing at all, in which case classes are simply numbered.

# ---------------------------------------------------------------------------
# XML sidecar parsing
# ---------------------------------------------------------------------------

#' Escape a string for inclusion in an XML document.
#'
#' The inverse of `xml_unescape`. Needed because the WKT that GDAL hands back
#' for a coordinate reference system is full of `<`, `>` and quotes, and because
#' the `/vsicurl/` prefix in a VRT source name can contain an ampersand.
xml_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&apos;", x, fixed = TRUE)
}

xml_unescape <- function(x) {
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&apos;", "'", x, fixed = TRUE)
  # Numeric references, decimal and hex.
  dec <- regmatches(x, gregexpr("&#x?[0-9A-Fa-f]+;", x))
  if (length(dec) && length(dec[[1]])) {
    for (ref in unique(dec[[1]])) {
      v <- if (substr(ref, 3, 3) == "x") {
        strtoi(substring(ref, 4, nchar(ref) - 1), 16L)
      } else {
        strtoi(substring(ref, 3, nchar(ref) - 1), 10L)
      }
      if (!is.na(v)) x <- gsub(ref, intToUtf8(v), x, fixed = TRUE)
    }
  }
  gsub("&amp;", "&", x, fixed = TRUE)
}

#' Pull every element at a given depth out of a well formed XML fragment.
#'
#' GDAL emits its sidecars in a completely predictable shape, so a small
#' block-level parser is enough and avoids pulling in an XML dependency.
xml_blocks <- function(x, tag) {
  m <- gregexpr(sprintf("<%s(\\s[^>]*)?>.*?</%s>", tag, tag), x, perl = TRUE)[[1]]
  if (length(m) == 1L && m[1] == -1L) return(character())
  regmatches(x, gregexpr(sprintf("<%s(\\s[^>]*)?>.*?</%s>", tag, tag), x,
                         perl = TRUE))[[1]]
}

xml_inner <- function(x, tag) {
  xml_text(xml_blocks(x, tag))
}

#' Text content of each matched block, tags stripped.
xml_text <- function(blocks, tag = NULL) {
  if (!length(blocks)) return(character())
  # Blocks are the whole element, so the opening tag may carry attributes.
  sub("^<[^>]*>([^<]*)<[^>]*>$", "\\1", blocks, perl = TRUE)
}

# All <F> cells of one <Row>, in document order.
xml_cells <- function(row) {
  xml_unescape(xml_inner(row, "F"))
}

#' Read a GDAL raster attribute table from a `.aux.xml` sidecar.
#'
#' A missing sidecar is the normal case, not an error: only the Toth layers ship
#' one. The request is therefore made quietly, because a 404 is the expected
#' answer rather than something to warn about.
#'
#' @return Named character vector, class value -> label, or NULL when the
#'   sidecar is absent or carries no table.
read_rat <- function(url) {
  if (is.na(url) || !nzchar(url)) return(NULL)
  txt <- tryCatch(suppressWarnings(http_get(url)), error = function(e) NULL)
  if (is.null(txt) || is.na(txt) || !nzchar(txt) ||
      !grepl("GDALRasterAttributeTable", txt, fixed = TRUE)) {
    return(NULL)
  }

  tab  <- xml_blocks(txt, "GDALRasterAttributeTable")
  rows <- xml_blocks(tab, "Row")
  if (!length(rows)) return(NULL)

  fields <- vapply(xml_blocks(tab, "FieldDefn"), xml_inner, character(1),
                   tag = "Name")
  # GDAL numbers FieldDefn from 1; <F> values are positional, so a field list of
  # value, category means <F>1 is the key and <F>2 the label.
  key_idx <- match("value", tolower(fields))
  lab_idx <- match("category", tolower(fields))
  if (is.na(lab_idx)) lab_idx <- if (length(fields) >= 2L) 2L else NA_integer_
  if (is.na(key_idx) || is.na(lab_idx) || key_idx == lab_idx) return(NULL)

  cells <- lapply(rows, xml_cells)
  pick <- function(i) vapply(cells, function(cc) {
    if (length(cc) >= i) cc[i] else NA_character_
  }, character(1))

  keys <- pick(key_idx)
  labs <- pick(lab_idx)
  ok <- !is.na(keys) & !is.na(labs) & nzchar(labs)
  if (!any(ok)) return(NULL)

  stats::setNames(labs[ok], keys[ok])
}

# ---------------------------------------------------------------------------
# Manual label table
# ---------------------------------------------------------------------------

read_class_labels <- function(path = app_paths$labels) {
  if (!file.exists(path)) return(list())
  tryCatch(yaml::read_yaml(path), error = function(e) {
    warning("Could not read ", path, ": ", conditionMessage(e))
    list()
  })
}

#' Longest matching manual label table for a layer.
#'
#' Keys are layer ids, optionally suffixed with the COG's own band name so that
#' a publication contributing several thematic layers can label each of them.
manual_labels <- function(labels, layer, band = NULL) {
  for (key in c(paste0(layer, "|", band), layer)) {
    if (!is.null(key) && key %in% names(labels)) return(labels[[key]])
  }
  NULL
}

#' Normalise a manual label entry.
#'
#' Accepts either a bare mapping (`1: "name"`) or a mapping with explicit
#' colours (`list(labels = ..., colours = ...)`). YAML reads a mapping as a
#' list whose element names are the class values, so those names are the keys.
split_labels <- function(entry) {
  if (is.null(entry)) return(NULL)

  keyed <- function(x) {
    if (is.null(x) || !length(x)) return(NULL)
    n <- names(x)
    if (is.null(n) || any(!nzchar(n))) n <- as.character(seq_along(x))
    list(key = n, value = as.character(unlist(x, use.names = FALSE)))
  }

  if (is.list(entry) && !is.data.frame(entry)) {
    labs <- keyed(entry$labels)
    cols <- keyed(entry$colours)
    if (is.null(labs)) return(NULL)
    out <- list(labels = stats::setNames(labs$value, labs$key))
    if (!is.null(cols)) out$colours <- stats::setNames(cols$value, cols$key)
    return(out)
  }

  labs <- keyed(entry)
  if (is.null(labs)) return(NULL)
  list(labels = stats::setNames(labs$value, labs$key))
}

# ---------------------------------------------------------------------------
# Palette generation
# ---------------------------------------------------------------------------

#' A distinct, printable colour per class.
#'
#' 8 base hues from Dark 3 give clean separation for small legends; beyond that
#' the sequence is interpolated, which is good enough for the 20-odd class
#' regionalisation layers and far cheaper than maintaining palettes by hand.
class_colours <- function(values) {
  n <- length(values)
  if (n <= 8L) {
    hcl.colors(n, palette = "Dark 3", rev = FALSE)
  } else {
    grDevices::hcl.colors(n, palette = "Zissou 1", rev = FALSE)
  }
}

# ---------------------------------------------------------------------------
# Assembly
# ---------------------------------------------------------------------------

#' Resolve the class table for a layer.
#'
#' @param layer one row of the catalogue.
#' @param band  the COG band currently being displayed, or NULL.
#' @param sampled integer values observed in the decimated preview.
#' @return `NULL` when the layer should be drawn as a continuous raster,
#'   otherwise a list with `values`, `labels`, `colours` and `source`.
resolve_classes <- function(layer, band = NULL, sampled = NULL,
                            labels = read_class_labels(app_paths$labels)) {
  manual <- split_labels(manual_labels(labels, layer$id, band))

  if (!is.null(manual)) {
    vals <- suppressWarnings(as.numeric(names(manual$labels)))
    if (all(is.na(vals))) vals <- seq_along(manual$labels)
    keep <- !is.na(vals) & !duplicated(vals)
    return(list(
      values  = vals[keep],
      labels  = manual$labels[keep],
      colours = manual$colours %||% class_colours(vals[keep]),
      source  = "class_labels.yml"
    ))
  }

  rat <- tryCatch(read_rat(layer$aux_xml_url), error = function(e) NULL)
  if (!is.null(rat)) {
    vals <- suppressWarnings(as.numeric(names(rat)))
    if (all(is.na(vals))) vals <- seq_along(rat)
    return(list(
      values  = vals,
      labels  = unname(rat),
      colours = class_colours(vals),
      source  = "GDAL raster attribute table"
    ))
  }

  # Nothing on record: number the classes that are actually present so the
  # legend stays honest about what was drawn.
  if (is.null(sampled) || !length(sampled)) return(NULL)
  vals <- sort(unique(sampled))
  if (length(vals) > config$max_categorical_classes) return(NULL)
  if (!all(vals == as.integer(vals))) return(NULL)
  list(
    values  = vals,
    labels  = paste("Class", vals),
    colours = class_colours(vals),
    source  = "unlabelled (values shown)"
  )
}

#' Decide continuous vs thematic for a layer.
#'
#' Called twice. First with only the metadata, where the output type is the
#' only signal available: TRUE or FALSE if the type settles it, NA if it does
#' not. Then with the values that were actually read, where the data gets the
#' final say -- several regionalisation layers are published under generic
#' output types, and a small number of distinct integer cells means thematic
#' whatever the type says.
is_thematic <- function(layer, sampled = NULL) {
  ot <- if (is.null(layer)) NULL else layer$output_type

  if (is.null(sampled)) {
    if (is.null(ot)) return(NA)
    if (ot %in% config$thematic_output_types) return(TRUE)
    return(if (ot %in% config$continuous_output_types) FALSE else NA)
  }

  vals <- sort(unique(stats::na.omit(as.numeric(sampled))))
  if (!length(vals)) return(FALSE)
  if (length(vals) > config$max_categorical_classes) return(FALSE)
  if (!all(vals == as.integer(vals))) return(FALSE)
  if (!is.null(ot) && ot %in% config$thematic_output_types) return(TRUE)
  # Spatially contiguous, integral and sparse: a classification.
  length(vals) <= 30L
}
