# R/basemap.R -- The base map every layer is composited over, built once.
#
# Layers are read in the source projection and warped to the same polar
# stereographic canvas, so the base has to match that canvas exactly. The base
# is built a single time, serialised to .cache/basemap.rds, and restored on later
# starts, which keeps the SOmap download and the layer composition off the
# startup path entirely.
#
# SOmap is asked for its bathymetry, coastline and ice edge, and its answer is
# turned into a ggplot layer by layer (see so_plot()). Should that fail on an
# installation the build falls back to a plain graticule rather than to no map at
# all: losing the bathymetry is a cosmetic downgrade, but it must never stop the
# app from serving a layer.
#
# What a layer is composited over is the base map's *panel*: the base is built
# and drawn once here, and only that one grob travels into render_layer.R. The
# base's own layers are never spliced into the layer's plot -- ggplot2 allows one
# scale per aesthetic, and the bathymetry and the layer both want `fill`, so a
# merged plot can only ever give one of them the right colours.

# Bumped whenever the shape of .cache/basemap.rds changes, so a file written by a
# different version of the app is rebuilt rather than half-understood.
base_format <- "panel-2"

# The base map's panel, drawn once and then reused for every layer.
#
# Rasterising SOmap costs seconds and the result is the same for every layer in
# the session, so it is held here rather than redrawn per render. get_base_map()
# fills it in, from the cache on a warm start and from a fresh build otherwise.
base_panel_cache <- new.env(parent = emptyenv())
base_panel_cache$grob <- NULL

#' Look up one of SOmap's recorded plot calls by its `"package::function"` name.
so_plotter_fn <- function(name) {
  parts <- strsplit(name, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2L) utils::getFromNamespace(parts[2], parts[1]) else
    utils::getFromNamespace(name, "SOmap")
}

#' Compose an SOmap object into a ggplot.
#'
#' `SOmap::SOgg()` does not return a plot; it returns an `SOmap_gg` whose
#' `plot_sequence` names the pieces of one, each piece a list of `SO_plotter`
#' objects that record a call as a `plotfun` string and its `plotargs`. Replaying
#' those calls in order builds the real ggplot. Each `SO_plotter` keeps its own
#' data, so the bathymetry, the coastline and the ice edge all arrive on the plot
#' they were written for -- unlike a hand-spliced layer list, which loses the
#' bathymetry (its fill mapping resolves against data the layer does not have).
so_plot <- function(somap) {
  gg <- SOmap::SOgg(somap)
  pieces <- unlist(lapply(gg$plot_sequence, function(nm) gg[[nm]]),
                   recursive = FALSE)

  p <- NULL
  for (piece in pieces) {
    fn <- so_plotter_fn(piece$plotfun)
    p <- if (is.null(p)) do.call(fn, piece$plotargs) else
      p + do.call(fn, piece$plotargs)
  }
  if (is.null(p)) stop("SOmap produced no plot layers", call. = FALSE)

  # Fail here, with a readable message, rather than inside the viewer.
  ggplot2::ggplot_build(p)
  p
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
    ggplot2::coord_quickmap(xlim = c(-ext, ext), ylim = c(-ext, ext),
                            expand = FALSE) +
    ggplot2::labs(caption = "South polar stereographic (EPSG:3031) \u00b7 base map unavailable") +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::theme(
      plot.background  = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
      panel.background = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
      plot.caption     = ggplot2::element_text(size = 8, colour = "#7a8794"),
      plot.margin      = ggplot2::margin(0, 0, 2, 0)
    )
}

#' The base map's own chrome: the dark canvas the coastline is read against.
#'
#' Drawn as an underlay, so the panel is opaque on purpose. That dark fill *is*
#' the background of the map; the layer is painted on top of it and lets it show
#' through wherever the layer's own nodata is transparent. Nothing else is drawn
#' -- no grid, no axes, no margin -- because the layer brings its own.
base_theme <- function() {
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
    plot.background  = ggplot2::element_rect(fill = "#0e1a24", colour = NA),
    panel.grid       = ggplot2::element_blank(),
    panel.border     = ggplot2::element_blank(),
    axis.text        = ggplot2::element_blank(),
    axis.title       = ggplot2::element_blank(),
    plot.margin      = ggplot2::margin(0, 0, 0, 0)
  )
}

#' The base map's panel on its own, as a grob a layer can be drawn over.
#'
#' Everything a layer does not need -- the base's caption, its scales, its
#' coordinates -- is left behind, because the layer brings its own. What is left
#' is the part that has to be shared: the map, already drawn, in the same
#' coordinate system and to the same extent the layer is warped onto.
base_panel <- function(p) {
  if (is.null(p) || !inherits(p, "ggplot")) return(NULL)
  g <- tryCatch(ggplot2::ggplotGrob(p), error = function(e) NULL)
  if (is.null(g)) return(NULL)
  i <- which(g$layout$name == "panel")
  if (!length(i)) return(NULL)
  g$grobs[[i[1]]]
}

#' The base map's panel, drawn once per session and reused for every layer.
base_panel_grob <- function(base = get_base_map()) {
  if (is.null(base_panel_cache$grob)) {
    base_panel_cache$grob <- base_panel(base)
  }
  base_panel_cache$grob
}

#' Build the base map, or return the cached one.
#'
#' `trim = -45` matches the framing used across the DistAnt figures: the whole
#' of Antarctica plus the surrounding Southern Ocean. The cache holds the plot
#' and its drawn panel together, and is only read when it carries this release's
#' own format marker, so a file left behind by an earlier version of the app is
#' rebuilt rather than half-understood.
get_base_map <- function(force = FALSE,
                         trim = -45,
                         path = app_paths$basemap) {
  if (!force && file.exists(path)) {
    hit <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(hit) && identical(hit$format, base_format) &&
        inherits(hit$plot, "ggplot")) {
      base_panel_cache$grob <- hit$panel
      return(hit$plot)
    }
  }

  ext <- config$polar_extent

  p <- tryCatch({
    # `expand = FALSE` is what keeps the two in register: the layer's canvas is
    # the same square with no expansion, so the panel grob fills it exactly and
    # the coastline lands on the grid the layer was warped onto.
    #
    # The limits are given explicitly, which is also what makes SOmap's own
    # coord_sf safe to keep: with both the range and the aspect fixed, and the
    # coastline already in EPSG:3031 metres, it has nothing left to decide and
    # nothing to reproject. (Swapping in a coord_quickmap here does not work --
    # ggplot2 4.x's geom_sf insists on a CoordSf -- and the limits would not fit
    # the panel to the square anyway.)
    so_plot(SOmap::SOmap(trim = trim, border_width = 0.25)) +
      ggplot2::coord_sf(xlim = c(-ext, ext), ylim = c(-ext, ext),
                        expand = FALSE) +
      base_theme()
  }, error = function(e) {
    warning("Could not build the SOmap base map (", conditionMessage(e),
            "); using a plain canvas.", call. = FALSE)
    plain_canvas()
  })

  base_panel_cache$grob <- base_panel(p)
  tryCatch(saveRDS(list(format = base_format, plot = p,
                        panel = base_panel_cache$grob), path),
           error = function(e) NULL)
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
  base_panel_cache$grob <- NULL
  invisible(TRUE)
}
