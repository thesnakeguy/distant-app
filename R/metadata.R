metadata_source <- "https://raw.githubusercontent.com/SCAR/distant/master/metadata.csv"
not_specified <- "(not specified)"

load_metadata <- function(url = metadata_source) {
  meta <- utils::read.csv(url, stringsAsFactors = FALSE, na.strings = "",
                          check.names = FALSE)
  meta <- unique(meta)
  meta$taxon[is.na(meta$taxon)] <- not_specified
  meta$modelling_method[is.na(meta$modelling_method)] <- not_specified
  meta
}

filter_layers <- function(meta, taxon = "", method = "", future = "", reference = "") {
  out <- meta
  if (nzchar(taxon)) out <- out[out$taxon == taxon, , drop = FALSE]
  if (nzchar(method)) out <- out[out$modelling_method == method, , drop = FALSE]
  if (nzchar(future)) {
    out <- out[as.character(out$future_projections) == future, , drop = FALSE]
  }
  if (nzchar(reference)) {
    out <- out[out$reference == reference, , drop = FALSE]
  }
  out
}

shorten <- function(x, n = 90) {
  ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

citation_example <- function(row) {
  cite <- trimws(as.character(row$citation))
  if (!is.na(cite) && nzchar(cite) && cite != "See reference") {
    # El-Gabbas entries miss the space after "Southern Ocean."
    return(gsub("Ocean.PANGAEA", "Ocean. PANGAEA", cite, fixed = TRUE))
  }
  ref <- sub("[.[:space:]]+$", "", trimws(as.character(row$reference)))
  paste0(ref,
         ". Data obtained from the SCAR DistAnt Ecological Model Output Repository, ",
         "10.5281/zenodo.10910075 (licence: ", row$licence, ").")
}

extent_wgs84 <- function(row) {
  raw <- paste(row$xmin, row$xmax, row$ymin, row$ymax, sep = ", ")
  crs <- trimws(as.character(row$crs))
  tryCatch({
    if (length(crs) != 1 || is.na(crs) || !nzchar(crs)) stop("no crs")
    box <- sf::st_bbox(
      c(xmin = as.numeric(row$xmin), xmax = as.numeric(row$xmax),
        ymin = as.numeric(row$ymin), ymax = as.numeric(row$ymax)),
      crs = crs
    )
    out <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(box), 4326))
    # Antarctic data never crosses the equator: refuse implausible results
    if (isTRUE(unname(out["ymax"]) > 0)) stop("north of the equator")
    paste(round(out["xmin"], 2), round(out["xmax"], 2),
          round(out["ymin"], 2), round(out["ymax"], 2), sep = ", ")
  }, error = function(e) paste0(raw, " (", crs, ")"))
}

metadata_table <- function(row, url = NULL) {
  value <- function(x) {
    x <- trimws(as.character(x))
    if (length(x) != 1 || is.na(x)) NA_character_ else x
  }
  extent <- extent_wgs84(row)
  resolution <- paste(row$x_resolution, row$y_resolution, sep = ", ")
  fields <- data.frame(
    Field = c("File", "Taxon", "Modelling method", "Output type",
              "Future projections", "Input data", "Uncertainty type",
              "Model performance", "Model performance measure",
              "Extent (xmin, xmax, ymin, ymax in EPSG:4326)", "Resolution (x, y)",
              "Spatial units", "CRS", "Licence", "Data usage notes",
              "Reference"),
    Value = c(value(row$file), value(row$taxon), value(row$modelling_method),
              value(row$output_type),
              if (isTRUE(row$future_projections)) "yes" else "no",
              value(row$input_data), value(row$uncertainty_type),
              value(row$model_performance),
              value(row$model_performance_measure),
              value(extent), value(resolution), value(row$spatial_units),
              value(row$crs), value(row$licence), value(row$data_usage_notes),
              value(row$reference)),
    stringsAsFactors = FALSE
  )
  if (!is.null(url)) {
    fields <- rbind(fields, data.frame(Field = "Source", Value = value(url)))
  }
  fields[!is.na(fields$Value) & nzchar(fields$Value), , drop = FALSE]
}
