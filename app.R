### Globals scope -> runs once on startup ###

library(shiny)
library(DT)
library(SOmap)
library(raster)
library(terra)
library(raster) # dependency for SOplot, takes RasterLayer and not SpatRaster from 'terra'
library(sf)
library(shinycssloaders)
library(dplyr)

shinyOptions(cache = cachem::cache_mem(max_size = 300 * 1024^2))

# Globals
circle <- terra::vect(cbind(seq(-180, 180, length.out = 361), -45), crs = "EPSG:4326") %>%
  terra::project("EPSG:3031")
ext <- ext(circle)

# Set values, filters, index, metadata
meta <- load_metadata()
index <- source_coop_index()
base_gg <- SOmap::SOmap()
taxon_values <- sort(unique(meta$taxon))
taxon_choices <- c("All" = "", stats::setNames(taxon_values, taxon_values))
method_values <- sort(unique(meta$modelling_method))
method_choices <- c("All" = "", stats::setNames(method_values, method_values))
future_choices <- c("All" = "", "Yes" = "TRUE", "No" = "FALSE")
reference_values <- sort(unique(meta$reference))
reference_choices <- c("All" = "",
                       stats::setNames(reference_values,
                                       shorten(reference_values, 90)))


# Theming
app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#0f3b5c",
  secondary = "#4fb3df",
  base_font = '"Segoe UI", "Helvetica Neue", Arial, sans-serif'
)

# Html tags
app_css <- tags$head(tags$style(HTML("
  body {
    background-color: #eef6fb;
    background-image: linear-gradient(180deg, #d9ecf7 0%, #f3f9fd 35%,
                                      #ffffff 100%);
    color: #0b2f4e;
  }
  .app-header {
    background: linear-gradient(115deg, #071e33 0%, #0f3b5c 55%, #14618f 100%);
    border-bottom: 4px solid #4fb3df;
    color: #ffffff;
    margin: -15px -15px 22px -15px;
    padding: 20px 30px 18px 30px;
    box-shadow: 0 4px 14px rgba(7, 30, 51, 0.35);
  }
  .app-header h1 { font-size: 27px; font-weight: 700; margin: 0;
                   letter-spacing: 0.5px; }
  .app-header p { margin: 4px 0 0 0; font-size: 14px; opacity: 0.85; }
  .app-header .header-rule { display: inline-block; width: 46px; height: 4px;
                             background: #8fd3f0; border-radius: 2px;
                             margin-right: 10px; vertical-align: middle; }
  .sidebar-card {
    background: #ffffff;
    border: 1px solid #d7e8f2;
    border-radius: 12px;
    box-shadow: 0 6px 18px rgba(11, 47, 78, 0.12);
    padding: 18px 16px 6px 16px;
    margin-bottom: 15px;
  }
  .sidebar-card .form-group > label {
    color: #0f3b5c;
    font-size: 12px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.6px;
    margin-bottom: 4px;
  }
  .sidebar-card .help-block { color: #5b7d95; font-size: 12px; }
  .form-control, .form-select {
    border: 1px solid #bcd9ea;
    border-radius: 8px;
    color: #0b2f4e;
  }
  .form-control:focus, .form-select:focus {
    border-color: #4fb3df;
    box-shadow: 0 0 0 3px rgba(79, 179, 223, 0.28);
  }
  .panel-card {
    background: #ffffff;
    border: 1px solid #d7e8f2;
    border-radius: 12px;
    box-shadow: 0 6px 18px rgba(11, 47, 78, 0.10);
    padding: 16px 18px;
    margin-bottom: 15px;
  }
  .panel-card h4, .section-title {
    color: #0f3b5c;
    font-size: 15px;
    font-weight: 700;
    text-transform: uppercase;
    letter-spacing: 0.6px;
    border-bottom: 2px solid #cfe6f2;
    padding-bottom: 6px;
    margin-top: 0;
  }
  .panel-card .help-block { color: #5b7d95; font-size: 12.5px; margin-bottom: 0; }
  .citation-box {
    background: linear-gradient(135deg, #f0f9ff 0%, #e4f3fb 100%);
    border: 1px solid #bcd9ea;
    border-left: 6px solid #0f3b5c;
    border-radius: 10px;
    box-shadow: 0 4px 12px rgba(11, 47, 78, 0.10);
    color: #0b2f4e;
    font-family: Georgia, 'Times New Roman', serif;
    font-size: 15.5px;
    line-height: 1.55;
    padding: 16px 18px;
    white-space: pre-wrap;
    margin-bottom: 18px;
  }
  .citation-label { color: #14618f; font-size: 12px; font-weight: 700;
                    text-transform: uppercase; letter-spacing: 0.8px;
                    margin: 0 0 6px 0; }
  .nav-tabs > li > a { color: #0f3b5c; font-weight: 600; }
  .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus,
  .nav-tabs > li.active > a:hover {
    color: #0f3b5c;
    border-top: 3px solid #4fb3df;
    background: #ffffff;
  }
  table.table { color: #0b2f4e; font-size: 13.5px; }
  table.table-striped > tbody > tr:nth-of-type(odd) {
    background-color: #eef6fb;
  }
  table.table > tbody > tr > td { border-top: 1px solid #e2eef5; }
  .dataTables_wrapper { color: #0b2f4e; font-size: 13.5px; }
  .dataTables_wrapper .dataTables_filter input {
    border: 1px solid #bcd9ea;
    border-radius: 8px;
    padding: 4px 8px;
  }
  .map-panel img { border-radius: 8px; }
  .map-downloads { margin-top: 4px; }
  .map-downloads .btn { margin-right: 8px; }
  .app-footer {
    background: #ffffff;
    border-top: 1px solid #d7e8f2;
    margin: 25px -15px -15px -15px;
    padding: 16px 30px;
    text-align: center;
    color: #5b7d95;
    font-size: 13px;
  }
  .app-footer span { display: block; margin-bottom: 10px;
                     text-transform: uppercase; letter-spacing: 0.6px;
                     font-weight: 700; color: #14618f; }
  .app-footer img { height: 80px; width: auto; margin: 0 18px;
                    vertical-align: middle; }
")))


### UI ###

ui <- fluidPage(

  theme = app_theme,
  app_css,
  tags$div(
    class = "app-header",
    tags$h1("SCAR DistAnt"),
    tags$p(
      tags$span(class = "header-rule"),
      "Browse Antarctic ecological model outputs"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$div(
        class = "sidebar-card",
        selectInput(
          "taxon",
          "Taxon",
          taxon_choices
        ),
        selectInput(
          "method",
          "Modelling method",
          method_choices
        ),
        selectInput(
          "future",
          "Future projections",
          future_choices
        ),
        selectInput(
          "reference",
          "Reference",
          reference_choices
        ),
        hr(),
        helpText(
          sprintf(
            "%d layers, loaded from Source Cooperative.",
            nrow(meta)
          )
        )
      )
    ),
    mainPanel(
      width = 9,
      tags$div(
        class = "panel-card",
        h4("Matching layers"),
        DTOutput("overview")
      ),
      tags$div(
        class = "panel-card",
        h4("Your selection"),
        radioButtons(
          "shown_layer",
          "Layer to show",
          choices = character(0),
          selected = character(0)
        ),
        uiOutput("selection_help"),
        actionButton(
          "clear_selection",
          "Clear selection",
          class = "btn-sm"
        )
      ),
      fluidRow(
        column(
          width = 7,
          tags$div(
            class = "panel-card map-panel",
            h4("Map"),
            selectInput(
              "band",
              "Layer",
              choices = character(0),
              selected = character(0)
            ),
            plotOutput(
              "map",
              height = "800px"
            ) |> withSpinner(type = 6),
            helpText(
              "SOmap base layer (Southern Ocean, 45 S). Loading a ",
              "layer for the first time can take up to half a minute."
            ),
            tags$div(
              class = "map-downloads",
              conditionalPanel(
                condition = "input.shown_layer && input.shown_layer.length > 0",
                downloadButton(
                  "download_png",
                  "Download PNG"
                ),
                downloadButton(
                  "download_tif",
                  "Download TIF"
                ),
                downloadButton(
                  "download_zip",
                  "Download TIFs (.zip)"
                )
              ),
              helpText(
                "\n PNG and TIF follow the layer shown on the map; the ",
                "zip contains every selected layer."
              )
            )
          )
        ),
        column(
          width = 5,
          tabsetPanel(
            tabPanel(
              "Metadata",
              tags$div(
                class = "panel-card",
                tags$p(
                  class = "citation-label",
                  "Example citation"
                ),
                tags$div(
                  class = "citation-box",
                  textOutput("citation")
                ),
                h4("Layer metadata"),
                tableOutput("meta_table")
              )
            )
          )
        )
      )
    )
  ),

  # ---------------------------------------------------------------------------
  # FOOTER
  # ---------------------------------------------------------------------------

  tags$div(
    class = "app-footer",
    tags$span(
      "DistAnt project in partnership with:"
    ),
    tags$img(
      src = "logos/SCAR_logo.png",
      alt = "SCAR"
    ),
    tags$img(
      src = "logos/biodiversity.aq_logo.jpeg",
      alt = "biodiversity.aq"
    ),
    tags$img(
      src = "logos/AAP_logo.jpeg",
      alt = "Australian Antarctic Program"
    ),
    tags$img(
      src = "logos/naturalsciences_logo.png",
      alt = "Institute of Natural Sciences"
    )
  )
)


# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  # ===========================================================================
  # 1. FILTERED METADATA
  #
  # This reactive is only concerned with the table/filter state.
  # It does not touch the map.
  # ===========================================================================

  filtered <- reactive({
    filter_layers(
      meta,
      input$taxon,
      input$method,
      input$future,
      input$reference
    )
  })

  # ===========================================================================
  # 2. PERSISTENT USER SELECTION
  #
  # Store filenames rather than DT row numbers.
  #
  # This means a layer remains selected even when it disappears from the
  # currently filtered table.
  # ===========================================================================

  selected <- reactiveVal(character(0))
  proxy <- DT::dataTableProxy("overview")

  # ---------------------------------------------------------------------------
  # Table selection -> stored filenames
  # ---------------------------------------------------------------------------

  observeEvent(input$overview_rows_selected, {
    idx <- input$overview_rows_selected
    # An empty DT selection is deliberately ignored.
    # "Clear selection" is the explicit way to clear everything.
    if (!length(idx)) {
      return()
    }
    df <- filtered()
    new_selection <- c(
      # Preserve selected files hidden by the current filter
      setdiff(selected(), df$file),

      # Add the files currently selected in the visible table
      df$file[idx]
    )
    # Important:
    # Don't invalidate the entire downstream reactive graph if nothing
    # actually changed.
    if (!identical(selected(), new_selection)) {
      selected(new_selection)
    }
  }, ignoreNULL = TRUE)

  # ===========================================================================
  # 3. OVERVIEW TABLE
  #
  # `isolate(selected())` is intentional.
  #
  # Clicking a table row changes `selected()`, but should NOT cause DT to be
  # completely rebuilt.
  #
  # Changing filters DOES rebuild the table.
  # ===========================================================================

  output$overview <- DT::renderDT({
    df <- filtered()
    keep <- isolate(selected())
    table_df <- data.frame(
      Taxon = df$taxon,
      Method = df$modelling_method,
      Output = df$output_type,
      `Future projections` = ifelse(
        df$future_projections,
        "yes",
        "no"
      ),
      Reference = shorten(
        df$reference,
        70
      ),
      File = df$file,
      check.names = FALSE
    )

    DT::datatable(
      table_df,
      selection = list(
        mode = "multiple",
        target = "row",
        selected = which(df$file %in% keep)
      ),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })

  # ---------------------------------------------------------------------------
  # Re-apply stored selections after the filtered table changes.
  #
  # This does NOT load anything on the map.
  # ---------------------------------------------------------------------------

  observeEvent(filtered(), {
    df <- filtered()
    keep <- selected()
    rows <- which(
      df$file %in% keep
    )
    DT::selectRows(
      proxy,
      rows
    )
  }, ignoreInit = TRUE)

  # ===========================================================================
  # 4. CLEAR SELECTION
  # ===========================================================================

  observeEvent(input$clear_selection, {
    selected(character(0))
    DT::selectRows(
      proxy,
      NULL
    )
    # Remove all choices from the persistent radio button.
    updateRadioButtons(
      session,
      "shown_layer",
      choices = character(0),
      selected = character(0)
    )
    # Remove band choices too.
    updateSelectInput(
      session,
      "band",
      choices = character(0),
      selected = character(0)
    )
  })

  # ===========================================================================
  # 5. SELECTED FILES
  # ===========================================================================

  selected_files <- reactive({
    selected()
  })

  # ===========================================================================
  # 6. CURRENTLY DISPLAYED LAYER
  #
  # IMPORTANT:
  #
  # `selected()` can contain many files.
  #
  # Only `shown_layer` is allowed to drive expensive layer preparation.
  # ===========================================================================

  observe({
    files <- selected_files()
    if (!length(files)) {
      updateRadioButtons(
        session,
        "shown_layer",
        choices = character(0),
        selected = character(0)
      )
      return()
    }
    current <- isolate(input$shown_layer)
    # Preserve the currently displayed layer if it is still selected.
    #
    # Otherwise display the first selected layer.
    if (
      is.null(current) ||
      !nzchar(current) ||
      !current %in% files
    ) {
      current <- files[1]
    }
    updateRadioButtons(
      session,
      "shown_layer",
      choices = files,
      selected = current
    )

  })

  # Friendly message when no files have been selected.
  output$selection_help <- renderUI({

    if (!length(selected_files())) {
      return(
        helpText(
          "No layers selected yet."
        )
      )
    }
    NULL
  })

  # ===========================================================================
  # 7. METADATA FOR CURRENTLY DISPLAYED FILE
  # ===========================================================================

  selected_row <- reactive({
    files <- selected_files()
    req(
      length(files) > 0
    )
    file <- input$shown_layer

    # During UI updates there can briefly be no value.
    if (
      is.null(file) ||
      !nzchar(file) ||
      !file %in% files
    ) {
      file <- files[1]
    }
    i <- match(
      file,
      meta$file
    )
    req(
      !is.na(i)
    )
    meta[
      i,
      ,
      drop = FALSE
    ]
  })


  # ===========================================================================
  # 8. URL FOR CURRENTLY DISPLAYED FILE
  # ===========================================================================

  selected_url <- reactive({
    file <- selected_row()$file
    url <- layer_url(
      index,
      file
    )
    req(
      !is.null(url),
      length(url) == 1,
      !is.na(url),
      nzchar(url)
    )
    url
  })

  # ===========================================================================
  # 9. BAND INFORMATION
  #
  # This opens the COG header, rather than loading the complete raster.
  #
  # Cache it by filename so changing unrelated inputs doesn't repeat the
  # header request.
  # ===========================================================================

  bands <- reactive({
    layer_bands(
      selected_url()
    )
  }) |>
    bindCache(
      selected_row()$file
    )

  # ===========================================================================
  # 10. UPDATE BAND SELECTOR
  #
  # Persistent UI input: don't rebuild the UI with renderUI().
  # ===========================================================================

  observe({
    b <- bands()
    if (!length(b)) {
      updateSelectInput(
        session,
        "band",
        choices = character(0),
        selected = character(0)
      )
      return()
    }
    current <- isolate(input$band)
    if (
      is.null(current) ||
      !current %in% b
    ) {
      current <- b[1]
    }
    updateSelectInput(
      session,
      "band",
      choices = b,
      selected = current
    )
  })

  # ===========================================================================
  # 11. BAND INDEX
  # ===========================================================================

  band_index <- reactive({
    b <- bands()
    req(
      length(b) > 0
    )
    idx <- match(
      input$band,
      b
    )
    if (
      is.na(idx) ||
      is.null(idx)
    ) {
      1L
    } else {
      as.integer(idx)
    }
  })


  # ===========================================================================
  # 12. PREPARED MAP LAYER
  #
  # THIS IS THE EXPENSIVE PART.
  #
  # It depends only on:
  #
  #   file + band
  #
  # It does NOT depend on:
  #   - table filters
  #   - table selection
  #   - metadata output
  #   - citation
  #
  # Consequently, filtering the table should not reload the raster.
  # ===========================================================================

  layer <- reactive({
    prepare_layer(
      selected_url(),
      ext,
      band_index()
    )
  }) |>
    bindCache(
      selected_row()$file,
      band_index()
    )


  # ===========================================================================
  # 13. MAP
  # ===========================================================================

  output$map <- renderPlot({
    req(
      length(selected()) > 0
    )
    build_map(
      layer(),
      base_gg
    )
  }, res = 96)


  # ===========================================================================
  # 14. PNG DOWNLOAD
  # ===========================================================================

  output$download_png <- downloadHandler(
    filename = function() {
      paste0(
        tools::file_path_sans_ext(
          selected_row()$file
        ),
        ".png"
      )
    },
    content = function(file) {
      download_png(
        layer(),
        base_gg,
        file,
        width = 8,
        height = 7,
        dpi = 200
      )
    }
  )

  # ===========================================================================
  # 15. TIF DOWNLOAD
  #
  # Deliberately uses the source URL rather than `layer()`.
  # This avoids preparing the raster just to download it.
  # ===========================================================================

  output$download_tif <- downloadHandler(
    filename = function() {
      selected_row()$file
    },
    content = function(file) {
      download_tif(
        selected_url(),
        file
      )
    }
  )

  # ===========================================================================
  # 16. ZIP DOWNLOAD
  #
  # Deliberately downloads the selected source files directly.
  # It does NOT call layer(), so downloading a ZIP does not require rendering
  # the currently displayed raster first.
  # ===========================================================================

  output$download_zip <- downloadHandler(
    filename = function() {

      "distant-layers.zip"
    },
    content = function(file) {
      files <- selected_files()
      req(
        length(files) > 0
      )
      download_zip(
        files,
        file,
        function(name, dest) {
          download_tif(
            layer_url(index, name),
            dest
          )
        }
      )
    }
  )

  # ===========================================================================
  # 17. METADATA
  # ===========================================================================

  output$meta_table <- renderTable({
    metadata_table(
      selected_row(),
      selected_url()
    )
  },
  striped = TRUE,
  spacing = "s",
  width = "100%"
  )
  # ===========================================================================
  # 18. CITATION
  # ===========================================================================

  output$citation <- renderText({
    citation_example(
      selected_row()
    )
  })
}



shinyApp(ui, server)
