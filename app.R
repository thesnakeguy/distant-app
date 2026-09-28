# app.R -- DistAnt layer browser.
#
# Searches the SCAR DistAnt Ecological Model Output Repository
# (doi:10.5281/zenodo.10910075) and previews any published layer without
# downloading it. The collection is 1.9 GB of cloud-optimized geotiffs, so the
# app never materialises a layer: GDAL reads the decimated preview straight out
# of the COG's internal overviews and the real data is only transferred if the
# user asks for it.
#
# Modules, all sourced from R/:
#   config.R        endpoints, rendering budget, field labels
#   distant_api.R   metadata.csv, Source Cooperative index, Zenodo archives
#   classes.R       thematic class names and colours
#   render_layer.R  decimated read, polar warp, plotting, render cache
#   basemap.R       cached SOmap base map
#   downloads.R     delivery routes and CSV helpers

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# Resolve the app root once so paths are the same however the app is launched
# (`runApp("app.R")`, a shiny-server scgi_app, or a double-clicked app.R).
DISTANT_APP_DIR <- local({
  args <- commandArgs(trailingOnly = FALSE)
  fa <- grep("^--file=", args, value = TRUE)
  if (length(fa)) {
    return(dirname(normalizePath(sub("^--file=", "", fa[1]), winslash = "/")))
  }
  normalizePath(".", winslash = "/")
})
Sys.setenv(DISTANT_APP_DIR = DISTANT_APP_DIR)
setwd(DISTANT_APP_DIR)

required <- c("shiny", "bslib", "DT", "ggplot2", "terra", "scales", "yaml",
              "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("DistApp needs these packages: ", paste(missing, collapse = ", "),
       ". Install them with install.packages().", call. = FALSE)
}
if (utils::packageVersion("bslib") < "0.3.0") {
  stop("DistApp needs bslib >= 0.3.0 for its sidebar layout.", call. = FALSE)
}

for (f in list.files("R", pattern = "[.]R$", full.names = TRUE)) source(f)

init_gdal()
ensure_dirs()

# The base map and the manual class table are process-wide, so they are built
# once at startup rather than per session. The base map is also written to
# .cache, which keeps later restarts off the network entirely.
base_map    <- get_base_map()
class_labels <- read_class_labels(app_paths$labels)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- bslib::page_sidebar(
  title = div(class = "d-inline-flex align-items-baseline gap-2",
              span("DistAnt", class = "fw-bold"),
              span("layer browser", class = "text-muted fw-normal fs-6")),
  fillable = TRUE,
  theme = bslib::bs_theme(bootswatch = "flatly"),

  sidebar = bslib::sidebar(
    width = 340,
    title = "Search the collection",
    p(class = "text-muted small mb-3",
      "Filters combine. Leave everything at its default to list all layers."),

    shiny::selectizeInput("taxon", "Taxon", choices = NULL, multiple = TRUE,
                          options = list(placeholder = "Start typing a species...",
                                         maxOptions = 200, maxItems = 10,
                                         plugins = list("remove_button"))),

    shiny::selectizeInput("modelling_method", "Modelling method", choices = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "All methods",
                                         maxOptions = 100, maxItems = 10,
                                         plugins = list("remove_button"))),

    shiny::radioButtons("future_projections", "Future projections",
                        choices = c("Any" = "any", "Yes" = "TRUE", "No" = "FALSE"),
                        inline = TRUE),

    shiny::selectizeInput("reference", "Reference", choices = NULL,
                          multiple = TRUE,
                          options = list(placeholder = "All references",
                                         maxOptions = 100, maxItems = 8,
                                         plugins = list("remove_button"))),

    shiny::tags$hr(),
    div(class = "d-flex flex-column gap-2",
        shiny::actionButton("reset", "Reset filters",
                            class = "btn-sm btn-outline-secondary w-100"),
        shiny::actionButton("refresh", "Refresh catalogue",
                            class = "btn-sm btn-outline-secondary w-100",
                            title = paste("Re-download metadata.csv, the Source",
                                           "Cooperative index and the Zenodo",
                                           "file list"))),

    shiny::tags$hr(),
    shiny::uiOutput("catalogue_info")
  ),

  shiny::tags$head(shiny::tags$style(shiny::HTML("
    #results { font-size: 0.85rem; }
    .swatch { display: inline-block; width: 15px; height: 15px; margin-right: 7px;
              border: 1px solid #9999; vertical-align: -3px; border-radius: 2px; }
    .citation-box { background: var(--bs-tertiary-bg); border-left: 3px solid var(--bs-primary);
      padding: .75rem 1rem; font-size: .85rem; line-height: 1.5; }
    .meta-table td { vertical-align: top; padding: .35rem .5rem; }
    .meta-table td.field { width: 32%; color: var(--bs-secondary-color);
      font-weight: 400; }
    .meta-table td.value { font-family: var(--bs-font-monospace); font-size: .82rem;
      word-break: break-word; }
    .dl-wrap { display: flex; flex-direction: column; gap: .4rem; }
    .dl-wrap .btn { width: 100%; }
  ")), shiny::tags$script(shiny::HTML("
    Shiny.addCustomMessageHandler('distant_clear_selection', function(id) {
      var el = document.getElementById(id);
      if (!el || !window.jQuery) return;
      var dt = window.jQuery(el).DataTable();
      if (dt && dt.rows) dt.rows().deselect();
    });
  "))),

  div(class = "container-fluid px-3",
      uiOutput("match_summary"),

      DT::DTOutput("results"),

      uiOutput("layer_ui"))
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  cat_data <- shiny::reactive(catalogue())

  # ---- Facet vocabulary ---------------------------------------------------

  facet_values <- function(c, col) {
    v <- as.character(c[[col]])
    v[is.na(v) | !nzchar(trimws(v))] <- "(unspecified)"
    sort(unique(trimws(v)))
  }

  # Long citations make unusable dropdown entries, so each reference gets a
  # short label. The option *values* stay the full citation, which is what the
  # filter compares against.
  reference_choices <- function(c) {
    v <- facet_values(c, "reference")
    stats::setNames(v, make.unique(short_reference(v), sep = " "))
  }

  # Re-populating the facet widgets is keyed off a counter rather than the
  # filter inputs themselves, so typing in a dropdown cannot re-trigger it.
  reindex <- shiny::reactiveVal(0)

  shiny::observeEvent(reindex(), {
    c <- cat_data()
    shiny::updateSelectizeInput(session, "taxon",
      choices = facet_values(c, "taxon"), selected = input$taxon)
    shiny::updateSelectizeInput(session, "modelling_method",
      choices = facet_values(c, "modelling_method"),
      selected = input$modelling_method)
    shiny::updateSelectizeInput(session, "reference",
      choices = reference_choices(c), selected = input$reference)
  })

  # ---- Filtering ----------------------------------------------------------

  filtered <- shiny::reactive({
    c <- cat_data()
    keep <- rep(TRUE, nrow(c))

    if (length(input$taxon))
      keep <- keep & trimws(c$taxon) %in% input$taxon
    if (length(input$modelling_method))
      keep <- keep & trimws(c$modelling_method) %in% input$modelling_method
    if (length(input$reference))
      keep <- keep & c$reference %in% input$reference
    fp <- input$future_projections
    if (!is.null(fp) && fp != "any")
      keep <- keep & c$future_projections == identical(fp, "TRUE")

    c[keep, , drop = FALSE]
  })

  output$catalogue_info <- shiny::renderUI({
    c <- cat_data()
    tagList(
      div(class = "small text-muted",
          sprintf("%s layers in the metadata table.",
                  format(nrow(c), big.mark = ","))),
      div(class = "small text-muted",
          sprintf("%s are published as individual layers on Source Cooperative; the rest sit inside a Zenodo archive.",
                  format(sum(!is.na(c$cog_url)), big.mark = ","))),
      div(class = "small text-muted mt-2",
          "Collection: ",
          shiny::tags$a(href = config$zenodo_concept_url, target = "_blank",
                        rel = "noopener", config$zenodo_concept_doi))
    )
  })

  shiny::observeEvent(input$reset, {
    shiny::updateSelectizeInput(session, "taxon", selected = character())
    shiny::updateSelectizeInput(session, "modelling_method", selected = character())
    shiny::updateSelectizeInput(session, "reference", selected = character())
    shiny::updateRadioButtons(session, "future_projections", selected = "any")
  })

  shiny::observeEvent(input$refresh, {
    shiny::withProgress(message = "Re-reading the collection index", value = 0.4, {
      tryCatch({
        reset_catalogue_cache()
        reindex(reindex() + 1L)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Could not refresh:", conditionMessage(e)),
                                type = "error", duration = 12)
        FALSE
      })
    })
  })

  # ---- Results table ------------------------------------------------------

  output$match_summary <- shiny::renderUI({
    n <- nrow(filtered())
    span(class = if (n == 0L) "text-danger fw-bold" else "text-muted small",
         sprintf("%s of %s layers match%s",
                 format(n, big.mark = ","),
                 format(nrow(cat_data()), big.mark = ","),
                 if (n == 0L) " \u2014 try widening the filters." else "."))
  })

  output$results <- DT::renderDataTable(
    {
      d <- filtered()
      out <- d[, config$results_columns, drop = FALSE]
      out$future_projections <- ifelse(out$future_projections, "yes", "no")
      out
    },
    rownames = FALSE,
    selection = list(mode = "single", target = "row"),
    colnames = unname(config$results_labels[config$results_columns]),
    options = list(
      pageLength = 12,
      lengthMenu = c(12, 25, 50, 100),
      order = list(list(1, "asc")),
      dom = "ltip",
      scrollX = TRUE
    )
  )

  selected <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$results_rows_selected, {
    d <- filtered()
    idx <- input$results_rows_selected
    selected(if (length(idx) == 1L && idx[1] >= 1L && idx[1] <= nrow(d))
      d[idx[1], , drop = FALSE] else NULL)
  })

  # Changing the filters invalidates the row selection: the index DT reports
  # back would otherwise point at a different layer. Clearing the highlight is
  # a client-side operation, and DT 0.33 exposes no R function for it
  # (DT::updateDataTable is not exported and DT::reloadData ends in
  # table.ajax.reload, which a client-side table does not have), so it goes
  # through the handler in the page head.
  shiny::observeEvent(filtered(), {
    selected(NULL)
    session$sendCustomMessage("distant_clear_selection", list(id = "results"))
  })

  layer <- shiny::reactive(selected())

  # ---- Band inventory -----------------------------------------------------

  band_no    <- shiny::reactiveVal(1L)
  band_names <- shiny::reactiveVal(NULL)

  # Reading the band inventory is a header request, not a data read, so it is
  # cheap enough to do on every selection change.
  shiny::observeEvent(layer(), {
    band_no(1L)
    band_names(NULL)
    lay <- layer()
    if (is.null(lay) || is.na(lay$cog_url)) return()
    info <- tryCatch(open_cog(lay$cog_url), error = function(e) NULL)
    if (!is.null(info) && info$nlyr > 1L) band_names(info$band_names)
  })

  output$band_ui <- shiny::renderUI({
    bn <- band_names()
    if (is.null(bn)) return(NULL)
    shiny::selectInput("band", "Band", choices = stats::setNames(seq_along(bn), bn),
                       selected = 1L, width = "100%")
  })

  shiny::observeEvent(input$band, {
    b <- suppressWarnings(as.integer(input$band))
    if (length(b) == 1L && !is.na(b)) band_no(b)
  })

  effective_band <- shiny::reactive({
    bn <- band_names()
    if (is.null(bn)) return(1L)
    max(1L, min(band_no(), length(bn)))
  })

  # ---- Render -------------------------------------------------------------

  rendered <- shiny::reactive({
    lay <- layer()
    if (is.null(lay)) return(NULL)
    shiny::withProgress(message = "Reading the layer's overviews", value = 0.4, {
      cached_render(lay, effective_band(), base_map, class_labels)
    })
  })

  output$plot <- shiny::renderPlot({
    r <- rendered()
    shiny::validate(shiny::need(!is.null(r), " "))
    r$plot
  }, res = 96)

  # ---- Layer pane ---------------------------------------------------------

  output$layer_ui <- shiny::renderUI({
    lay <- layer()
    if (is.null(lay)) {
      return(div(class = "text-center text-muted py-5",
                 "Select a row in the table above to view a layer."))
    }

    # bslib styles shiny's tabsetPanel for free; bslib does not re-export it.
    shiny::tabsetPanel(
      id = "layer_tabs", type = "tabs",

      shiny::tabPanel(
        "Viewer",
        div(class = "row g-3 mt-1",
            div(class = "col-12 col-xl-9",
                div(class = "card",
                    div(class = "card-body p-1",
                        shiny::plotOutput("plot", height = "620px")))),
            div(class = "col-12 col-xl-3",
                shiny::uiOutput("band_ui"),
                div(class = "mt-3", download_button(lay)),
                shiny::uiOutput("extra_downloads"),
                div(class = "mt-3 small text-muted", delivery_note(lay)),
                shiny::uiOutput("render_info"),
                shiny::uiOutput("class_source"))),
        shiny::uiOutput("legend")),

      shiny::tabPanel("Metadata", metadata_tab(lay))
    )
  })

  # Secondary downloads plus the delivery note are cheap, so they live outside
  # the main UI tree and are injected into the Viewer tab.
  output$extra_downloads <- shiny::renderUI({
    shiny::req(layer())
    div(class = "mt-2 dl-wrap",
        shiny::downloadButton("dl_plot", "Download figure (PNG)",
                              class = "btn-outline-secondary"),
        shiny::downloadButton("dl_meta", "Download metadata (CSV)",
                              class = "btn-outline-secondary"),
        shiny::downloadButton("dl_bib", "Download citation (BibTeX)",
                              class = "btn-outline-secondary"))
  })

  output$render_info <- shiny::renderUI({
    r <- rendered()
    shiny::req(r)
    info <- r$info
    n <- as.integer(2 * config$polar_extent / config$polar_px_res)
    rows <- list(
      "Source CRS"   = info$crs,
      "Source grid"  = sprintf("%s cells", format(info$src_cells, big.mark = ",")),
      "Preview"      = sprintf("resampled to %s km cells on a %d x %d polar canvas",
                               format(config$polar_px_res / 1000, trim = TRUE), n, n),
      "Band"         = if (is.na(info$band_name) || !nzchar(info$band_name))
                          "1" else info$band_name,
      "Values shown" = if (r$thematic) paste("categorical:", length(r$classes$values), "classes")
                       else sprintf("%s to %s", format(info$range[1], digits = 4),
                                    format(info$range[2], digits = 4))
    )
    tagList(
      shiny::tags$hr(),
      shiny::tags$dl(class = "small mb-0",
        lapply(names(rows), function(nm) shiny::tagList(
          shiny::tags$dt(class = "text-muted fw-normal", nm),
          shiny::tags$dd(class = "mb-1 meta-value", rows[[nm]]))))
    )
  })

  output$class_source <- shiny::renderUI({
    r <- rendered()
    shiny::req(r, r$classes)
    span(class = "small text-muted",
         sprintf("Class names from: %s.", r$classes$source))
  })

  output$legend <- shiny::renderUI({
    r <- rendered()
    shiny::req(r, r$classes)
    cls <- r$classes
    tagList(
      shiny::tags$hr(),
      shiny::tags$h6(class = "text-uppercase small text-secondary", "Legend"),
      tags$table(class = "table table-sm meta-table mb-0",
                 tags$tbody(lapply(seq_along(cls$values), function(i) tags$tr(
                   tags$td(style = sprintf("width:2.5rem"),
                            span(class = "swatch",
                                 style = sprintf("background:%s", cls$colours[i]))),
                   tags$td(class = "value", sprintf("%s  %s", cls$values[i],
                                                    cls$labels[i])))))))
  })

  # ---- Downloads ----------------------------------------------------------
  #
  # The bodies live in top-level helpers rather than inline in the handlers so
  # they can be exercised without a browser (see dev/test_app.R).

  output$dl_layer <- shiny::downloadHandler(
    filename = function() layer()$file,
    content  = function(file) write_layer_file(layer(), file),
    contentType = "image/tiff"
  )

  output$dl_plot <- shiny::downloadHandler(
    filename = function() paste0(layer()$id, ".png"),
    content  = function(file) write_plot_file(rendered(), file),
    contentType = "image/png"
  )

  output$dl_meta <- shiny::downloadHandler(
    filename = function() paste0(layer()$id, "_metadata.csv"),
    content  = function(file) write_meta_file(layer(), file),
    contentType = "text/csv"
  )

  output$dl_bib <- shiny::downloadHandler(
    filename = function() "SCAR_DistAnt.bibtex",
    content  = function(file) write_bib_file(file),
    contentType = "text/plain"
  )
}

# ---------------------------------------------------------------------------
# Small pure helpers used by the server above
# ---------------------------------------------------------------------------
# The four `write_*` download bodies live in R/downloads.R so that the test
# harness can share them with this file rather than reimplementing them.

#' The primary download control for a layer.
download_button <- function(lay) {
  d <- layer_delivery(lay)
  if (identical(d$route, "cog")) {
    shiny::downloadButton("dl_layer", "Download layer (COG)", class = "btn-primary w-100")
  } else if (identical(d$route, "none")) {
    shiny::span(class = "text-muted small", "No downloadable file")
  } else {
    shiny::tags$a(href = d$url, target = "_blank", rel = "noopener",
                  download = d$filename, class = "btn btn-primary w-100",
                  if (identical(d$route, "cog-direct"))
                    "Download layer (COG, direct link)"
                  else sprintf("Download %s (Zenodo)", d$filename))
  }
}

delivery_note <- function(lay) layer_delivery(lay)$note

#' The Metadata tab: a two-column table of the layer's record, plus the
#' citation and where the file itself lives.
metadata_tab <- function(lay) {
  fields <- intersect(names(config$metadata_fields), names(lay))
  values <- vapply(fields, function(f) {
    v <- lay[[f]]
    if (is.logical(v)) ifelse(is.na(v), "", ifelse(v, "yes", "no"))
    else if (is.numeric(v)) format(v, trim = TRUE, digits = 7)
    else { s <- as.character(v); s[is.na(s)] <- ""; trimws(s) }
  }, character(1))
  names(values) <- unname(config$metadata_fields[fields])

  shown <- nzchar(values)
  rows <- lapply(names(values)[shown], function(nm) tags$tr(
    tags$td(class = "field", nm),
    tags$td(class = "value", values[[nm]])))

  # Cite the concept DOI rather than the current version's DOI: a citation
  # copied out of the app has to keep resolving after the next release.
  citation <- if (identical(lay$citation, "See reference")) {
    config$collection_citation
  } else {
    paste0(lay$citation, "  Data obtained from the SCAR DistAnt Ecological",
           " Model Output Repository, ", config$zenodo_concept_doi, ".")
  }

  # A flat definition list of where the file actually lives, skipping any
  # route that is not available for this particular layer.
  pair <- function(label, value) shiny::tagList(
    shiny::tags$dt(class = "text-muted fw-normal", label), value)
  provenance <- c(
    if (!is.na(lay$cog_url)) list(pair(
      "Source Cooperative",
      shiny::tags$dd(class = "meta-value", lay$cog_url))) else NULL,
    if (!is.na(lay$cog_url)) list(pair(
      "Layer size", shiny::tags$dd(class = "meta-value", human_size(lay$cog_size)))) else NULL,
    if (!is.na(lay$zip_url)) list(pair(
      "Zenodo archive",
      shiny::tags$dd(class = "meta-value",
                     shiny::tags$a(href = lay$zip_url, target = "_blank",
                                   rel = "noopener", lay$zip_name)))) else NULL,
    if (!is.na(lay$zip_url)) list(pair(
      "Archive size", shiny::tags$dd(class = "meta-value",
                                      human_size(lay$zip_size)))) else NULL,
    if (!is.na(lay$repo)) list(pair(
      "Model source code",
      shiny::tags$dd(class = "meta-value",
                     shiny::tags$a(href = lay$repo, target = "_blank",
                                   rel = "noopener", lay$repo)))) else NULL
  )

  div(class = "row g-4 mt-1",
      div(class = "col-12 col-lg-7",
          tags$table(class = "table table-sm meta-table",
                     tags$tbody(rows))),
      div(class = "col-12 col-lg-5",
          shiny::h6(class = "text-uppercase small text-secondary", "Citation"),
          div(class = "citation-box mb-3", citation),
          shiny::h6(class = "text-uppercase small text-secondary",
                    "Where this layer lives"),
          if (is.na(lay$cog_url)) div(class = "small text-muted mb-2",
              "This layer is not published as a standalone file; use the archive below."),
          shiny::tags$dl(class = "small mb-0", provenance)))
}

shiny::shinyApp(ui, server)
