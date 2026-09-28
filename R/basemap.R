# R/basemap.R -- The base map every layer is composited over, built once.
#
# Layers are read in the source projection and warped to the same polar
# stereographic canvas, so the base has to match that canvas exactly. The base
# is built a single time, serialised to .cache/basemap.rds, and restored on later
# starts, which keeps the SOmap download and the layer composition off the
# startup path entirely.
#
# SOmap is optional. Its plot helpers have moved between major versions and
# older releases are not compatible with ggplot2 4.x, so each known calling
# convention is tried in turn and a plain annotated canvas is used if none of
# them work. Losing the bathymetry is a cosmetic downgrade; it must never stop
# the app from serving a layer.

#' Compose an SOmap object into a ggplot, trying each known API.
#'
#' SOmap has shipped three incompatible ways of turning a `SOmap` object into a
#' plot. Rather than pinning one, try the current helper first and fall back to
#' the older dispatcher, composing the stored layer lists by hand if needed.
so_plot <- function(somap) {
  attempts <- list(
    function() SOmap::SOgg(somap, bbox = TRUE),
    function() SOmap::SO_plotter(somap, bbox = TRUE),
    function() SOmap::SO_plotter(somap),
    function() {
      # Pre-0.8 releases keep the individual ggplot layers in a list on the
      # object, named by the order they should be drawn in.
      p <- ggplot2::ggplot()
      for (nm in somap$plot_sequence) {
        if (nzchar(nm) && !is.null(somap[[nm]])) p <- p + somap[[nm]]
      }
      p
    }
  )

  errors <- character()
  for (attempt in attempts) {
    res <- tryCatch(attempt(), error = function(e) e)
    if (inherits(res, "error")) {
      errors <- c(errors, conditionMessage(res))
      next
    }
    # A plot that cannot be built is no better than one that cannot be made.
    ok <- tryCatch({
      ggplot2::ggplot_build(res)
      TRUE
    }, error = function(e) e)
    if (inherits(ok, "error")) {
      errors <- c(errors, conditionMessage(ok))
      next
    }
    return(list(plot = res, via = "SOmap"))
  }
  stop(paste0("no usable SOmap API: ", paste(unique(errors), collapse = "; ")),
       call. = FALSE)
}

#' The fallback canvas: a graticule, so the projection is still readable.
#'
#' Without this a missing or incompatible SOmap would leave the viewer on a
#' blank white square, which reads as "the layer failed to load" rather than
#' "there is no basemap here".
plain_canvas <- function() {
  ext <- config$polar_extent
  lat <- seq(-80, -40, by = 10)
  lon <- seq(-180, 180, by = 30)

  # EPSG:3031: x = r sin(lon), y = -r cos(lon) on the sphere.
  graticule <- do.call(rbind, lapply(lat, function(la) {
    r <- 6371000 * cos(la * pi / 180)
    data.frame(x = r * sin(lon * pi / 180), y = -r * cos(lon * pi / 180),
               lat = la, lon = lon)
  }))

  ggplot2::ggplot(graticule, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_path(colour = "#3d4c5c", linewidth = 0.3) +
    ggplot2::annotate(
      "text", x = ext * 0.92, y = 0, label = "0\u00b0", colour = "#5c6b7a",
      size = 2.4, angle = 0
    ) +
    ggplot2::annotate(
      "text", x = 0, y = -ext * 0.93, label = "180\u00b0W \u2013 0\u00b0 \u2013 180\u00b0E",
      colour = "#5c6b7a", size = 2.2
    ) +
    ggplot2::coord_quickmap(xlim = c(-ext, ext), ylim = c(-ext, ext)) +
    ggplot2::labs(caption = "South polar stereographic (EPSG:3031) \u00b7 base map unavailable") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
      plot.caption     = ggplot2::element_text(size = 8, colour = "#7a8794"),
      plot.margin      = ggplot2::margin(0, 0, 2, 0)
    )
}

#' Build the base map, or return the cached one.
#'
#' `trim = -45` matches the framing used across the DistAnt figures: the whole
#' of Antarctica plus the surrounding Southern Ocean.
get_base_map <- function(force = FALSE,
                         trim = -45,
                         path = app_paths$basemap) {
  if (!force && file.exists(path)) {
    hit <- tryCatch(readRDS(path), error = function(e) NULL)
    if (inherits(hit, "theme") || (is.list(hit) && !is.null(hit$plot))) return(hit$plot)
    if (inherits(hit, "ggplot") || inherits(hit, "theme")) return(hit)
  }

  ext <- config$polar_extent
  dark <- ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
    panel.grid       = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    axis.text        = ggplot2::element_blank(),
    axis.title       = ggplot2::element_blank(),
    plot.margin      = ggplot2::margin(0, 0, 0, 0)
  )

  p <- tryCatch({
    res <- so_plot(SOmap::SOmap(trim = trim, border_width = 0.25))
    res$plot +
      ggplot2::coord_quickmap(xlim = c(-ext, ext), ylim = c(-ext, ext)) +
      dark
  }, error = function(e) {
    warning("Could not build the SOmap base map (", conditionMessage(e),
            "); using a plain canvas.", call. = FALSE)
    plain_canvas()
  })

  tryCatch(saveRDS(p, path), error = function(e) NULL)
  p
}

#' Report what the base map actually is, for the viewer caption.
base_map_source <- function(base = get_base_map()) {
  if (inherits(base, "ggplot") &&
      length(base$layers) &&
      any(grepl("Path|path", vapply(base$layers,
                                   function(l) class(l$geom)[1], "")))) {
    "graticule"
  } else if (inherits(base, "ggplot") && length(base$layers)) "SOmap" else "canvas"
}

#' Drop the cached base map so it is rebuilt on the next request.
invalidate_base_map <- function() {
  unlink(app_paths$basemap)
  invisible(TRUE)
}
