### TO DO ###
# - Discrete intervals should have other color scheme (eg Species richness layer)
# - Picker to select layer if its a multilayer tif
# - Add logos from SCAR / biodiversity.aq / AADC (https://data.aad.gov.au/) / Natural sciences (naturalsciences.be)
# - multilayer download should be made possible in a simple intuitive way
# - set extent in "Layer metadata" to degrees in EPSG:4326, not it's Extent (xmin, xmax, ymin, ymax)
# - fix citation for El-Gabbas


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
ext <- terra::ext(-180, 180, -90, -45) %>%
  terra::as.polygons(crs = "EPSG:4326") # projected to EPSG:3031 in prepare_layer()

# Set values, filters, index, metadata
meta <- load_metadata()
index <- source_coop_index()
base_gg <- SOmap::SOmap()
map_colours <- hcl.colors(100, "viridis") # feeds into build_map()
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
")))


### UI ###

ui <- fluidPage(
  theme = app_theme,
  app_css,
  tags$div(
    class = "app-header",
    tags$h1("SCAR DistAnt"),
    tags$p(tags$span(class = "header-rule"),
           "Antarctic ecological model output - viewer")
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$div(
        class = "sidebar-card",
        selectInput("taxon", "Taxon", taxon_choices),
        selectInput("method", "Modelling method", method_choices),
        selectInput("future", "Future projections", future_choices),
        selectInput("reference", "Reference", reference_choices),
        hr(),
        helpText(sprintf("%d layers, loaded from Source Cooperative.",
                         nrow(meta)))
      )
    ),
    mainPanel(
      width = 9,
      tags$div(
        class = "panel-card",
        h4("Matching layers"),
        DTOutput("overview")
      ),
      fluidRow(
        column(
          width = 7,
          tags$div(
            class = "panel-card map-panel",
            h4("Map"),
            plotOutput("map", height = "800px") |> withSpinner(type = 6),
            helpText("SOmap base layer (Southern Ocean, 45 S). Loading a ",
                     "layer for the first time can take up to half a minute."),
            tags$div(
              class = "map-downloads",
              conditionalPanel(
                "input.overview_rows_selected",
                downloadButton("download_png", "Download PNG"),
                downloadButton("download_tif", "Download TIF")
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
                tags$p(class = "citation-label", "Example citation"),
                tags$div(class = "citation-box", textOutput("citation")),
                h4("Layer metadata"),
                tableOutput("meta_table")
              )
            )
          )
        )
      )
    )
  )
)

### SERVER ###

server <- function(input, output, session) {

  filtered <- reactive({
    filter_layers(meta, input$taxon, input$method, input$future, input$reference)
  })

  # Clear the table selection whenever a filter changes
  proxy <- DT::dataTableProxy("overview")
  observeEvent(
    list(input$taxon, input$method, input$future, input$reference),
    DT::selectRows(proxy, NULL)
  )

  output$overview <- DT::renderDT({
    df <- filtered()
    DT::datatable(
      data.frame(
        Taxon = df$taxon,
        Method = df$modelling_method,
        Output = df$output_type,
        `Future projections` = ifelse(df$future_projections, "yes", "no"),
        Reference = shorten(df$reference, 70),
        File = df$file,
        check.names = FALSE
      ),
      selection = "single",
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })

  selected_row <- reactive({
    req(input$overview_rows_selected)
    df <- filtered()
    req(input$overview_rows_selected <= nrow(df))
    df[input$overview_rows_selected, , drop = FALSE]
  })

  selected_url <- reactive({
    url <- layer_url(index, selected_row()$file)
    req(url)
    url
  })

  # Errors are not cached by bindCache, so a transient failure can be retried
  layer <- reactive({
    prepare_layer(selected_url(), ext)
  }) |>
    bindCache(selected_row()$file)

  output$map <- renderPlot({
    validate(need(!is.null(input$overview_rows_selected),
                  "Select a layer from the list to show it here."))

    out <- tryCatch(
      layer(),
      error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)  # let req()/validate() through
        validate(need(FALSE, paste("Layer could not be loaded:", conditionMessage(e))))
      }
    )

    build_map(out, base_gg, map_colours)
  }, res = 96)

  output$download_png <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(selected_row()$file), ".png")
    },
    content = function(file) {
      p <- build_map(layer(), base_gg, map_colours)
      ggplot2::ggsave(file, p, width = 8, height = 7, dpi = 200, bg = "white")
    }
  )

  output$download_tif <- downloadHandler(
    filename = function() selected_row()$file,
    content = function(file) download_tif(selected_url(), file)
  )

  output$meta_table <- renderTable({
    metadata_table(selected_row(), selected_url())
  }, striped = TRUE, spacing = "s", width = "100%")

  output$citation <- renderText({
    citation_example(selected_row())
  })
}


shinyApp(ui, server)
