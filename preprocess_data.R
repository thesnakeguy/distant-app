# script_preprocess_shiny.R
# This script preprocesses Antarctic data for visualization in a Shiny application
# It downloads data from a remote source, processes it, and creates map visualizations

# --- Load required packages ---
library(SOmap)
library(ggplot2)
library(sf)
library(raster)
library(dplyr)

# --- Configuration ---
config <- list(
  local_data_dir = "data_shiny_source_coop",
  remote_repo = "s3://scar/distant/",
  endpoint_url = "https://data.source.coop",
  output_dir = "processed_data",
  map_config = list(
    trim = -45,
    border_width = 0.25,
    base_size = 16,
    font_family = "Roboto"
  )
)

# --- Utility Functions ---

#' Execute a system command and handle errors
#' @param command The command to execute
#' @return The command output
execute_system_command <- function(command) {
  cat("Executing command:", command, "\n")
  tryCatch({
    result <- system(command, intern = TRUE, ignore.stderr = TRUE)
    return(result)
  }, error = function(e) {
    stop("Command execution failed: ", e$message)
  })
}

#' Create a base map with standard configuration
#' @return A configured SOmap object
create_base_map <- function() {
  p_base <- SOmap(
    trim = config$map_config$trim,
    border_width = config$map_config$border_width
  )
  pgg0_base <- SOgg(p_base)
  pgg0_base$bathy_legend <- NULL
  pgg0_base$ice[[1]]$plotargs$fill <- "white"
  pgg0_base$coastline[[1]]$plotargs$fill <- "grey80"
  return(pgg0_base)
}

#' Modify plot style for Shiny application
#' @param plot The plot to modify
#' @return A styled ggplot object
modify_plot_shiny <- function(plot) {
  plot + theme_void(base_size = config$map_config$base_size) +
    theme(
      legend.position = "bottom",
      text = element_text(family = config$map_config$font_family)
    )
}

#' Process and save a map visualization
#' @param data_url URL of the data source
#' @param output_file Name of the output file
#' @param process_function Function to process the data
#' @param base_map Base map configuration
process_and_save_map <- function(data_url, output_file, process_function, base_map) {
  tryCatch({
    processed_plot <- process_function(data_url, base_map)
    shiny_plot <- modify_plot_shiny(processed_plot)
    saveRDS(shiny_plot, file.path(config$output_dir, output_file))
    cat("Map processed and saved:", output_file, "\n")
  }, error = function(e) {
    warning("Failed to process map:", output_file, "\nError:", e$message)
  })
}

# --- Data Processing Functions ---

process_fabri_ruiz <- function(data_url, base_map) {
  cmap <- c(
    `Antarctic inner shelf` = "#2A5178",
    `Antarctic outer shelf` = "#599D8E",
    `Antarctic deep slope` = "#94FA8D",
    `Antarctic deep shelf` = "#14115E",
    `Ice shelf frontal zone` = "#FFFD8C",
    `Transition area` = "#BFBFBF",
    `Subantarctic deep slope` = "#760107",
    `Subantarctic island and shelf` = "#FAB067",
    `Subantarctic deep shelf` = "#DD0721",
    `Campbell Plateau` = "#A24526",
    `Deep Magellanic shelf` = "#FB3244",
    `Magellanic Plateau` = "#8869AC"
  )
  
  x <- raster(data_url)
  values(x) <- as.factor(values(x))
  xdf <- as.data.frame(SOproj(x), xy = TRUE)
  names(xdf)[3] <- "bioregion"
  
  pgg <- base_map
  temp <- base_map$init[[1]]$plotargs$data
  r <- max(temp$x[!is.na(temp$Depth)])
  theta <- seq(0, 2*pi, length.out = 300)
  
  pgg$background <- SO_plotter(
    plotfun = "ggplot2::geom_polygon",
    plotargs = list(
      data = data.frame(x = r * cos(theta), y = r * sin(theta)),
      mapping = aes(.data$x, .data$y),
      fill = base_map$bathy[[1]]$plotargs$col[40]
    )
  )
  
  pgg$plot_sequence <- c(pgg$plot_sequence[1], "background", pgg$plot_sequence[-1])
  pgg$bathy <- SO_plotter(
    plotfun = "ggplot2::geom_raster",
    plotargs = list(data = xdf, mapping = aes(fill = .data$bioregion))
  )
  
  pgg$scale_fill <- SO_plotter(
    plotfun = "ggplot2::scale_fill_manual",
    plotargs = list(
      values = unname(cmap),
      breaks = seq_len(length(cmap)),
      labels = names(cmap),
      na.value = "#FFFFFF00",
      name = "Benthic\nbioregion"
    )
  )
  
  return(plot(pgg))
}

process_hindell <- function(data_url, base_map) {
  x <- crop(raster(data_url), c(-180, 180, -80, -45))
  xdf <- as.data.frame(SOproj(x), xy = TRUE)
  names(xdf)[3] <- "habitat_importance"
  
  pgg <- base_map
  pgg$bathy <- SO_plotter(
    plotfun = "ggplot2::geom_raster",
    plotargs = list(data = xdf, mapping = aes(fill = .data$habitat_importance))
  )
  
  pgg$scale_fill <- SO_plotter(
    plotfun = "ggplot2::scale_fill_gradientn",
    plotargs = list(
      colors = hcl.colors(palette = "viridis", n = 51),
      na.value = "#FFFFFF00",
      name = "Habitat\nimportance"
    )
  )
  
  return(plot(pgg))
}

process_pinkerton <- function(data_url, base_map) {
  x <- SOproj(readAll(raster(data_url)))
  xdf <- as.data.frame(x, xy = TRUE)
  temp <- st_as_sf(xdf, coords = c("x", "y"))
  st_crs(temp) <- "EPSG:3031"
  temp <- sf::st_transform(temp, crs = "+proj=longlat")
  temp <- st_coordinates(temp)
  xdf <- xdf[temp[, 2] < -45, ]
  
  pgg <- base_map
  pgg$bathy <- SO_plotter(
    plotfun = "ggplot2::geom_raster",
    plotargs = list(data = xdf, mapping = aes(fill = .data$sea_ice_primary_productivity))
  )
  
  pgg$scale_fill <- SO_plotter(
    plotfun = "ggplot2::scale_fill_gradientn",
    plotargs = list(
      colors = hcl.colors(palette = "Greens", n = 51, rev = TRUE),
      na.value = "#FFFFFF00",
      name = "Primary\nproductivity\nmgC/m^2/day"
    )
  )
  
  return(plot(pgg))
}

process_freer <- function(data_url, base_map) {
  x <- crop(raster(data_url), c(-180, 180, -75, -45))
  xdf <- as.data.frame(SOproj(x), xy = TRUE)
  names(xdf)[3] <- "habitat_suitability"
  
  pgg <- base_map
  pgg$bathy <- SO_plotter(
    plotfun = "ggplot2::geom_raster",
    plotargs = list(data = xdf, mapping = aes(fill = .data$habitat_suitability))
  )
  
  pgg$scale_fill <- SO_plotter(
    plotfun = "ggplot2::scale_fill_gradientn",
    plotargs = list(
      colors = hcl.colors(palette = "Spectral", n = 51, rev = TRUE),
      na.value = "#FFFFFF00",
      name = "Habitat\nimportance"
    )
  )
  
  return(plot(pgg))
}

# --- Main Execution ---

# Create necessary directories
dir.create(config$local_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(config$output_dir, recursive = TRUE, showWarnings = FALSE)

# Download data from remote repository
list_command <- paste0("aws s3 ls ", config$remote_repo, " --endpoint-url=", config$endpoint_url)
remote_files <- execute_system_command(list_command)
cat("Remote repository contents:\n")
print(remote_files)

sync_command <- paste0("aws s3 sync ", config$remote_repo, " ", config$local_data_dir, " --endpoint-url=", config$endpoint_url)
execute_system_command(sync_command)
cat("Data synchronization completed.\n")

# Create base map
base_map <- create_base_map()

# Process and save all maps
process_and_save_map(
  "https://data.source.coop/scar/distant/fabri-ruiz_et_al-2020/Fa2020-bioregions_cog.tif",
  "plot_fabri_ruiz.rds",
  process_fabri_ruiz,
  base_map
)

process_and_save_map(
  "https://data.source.coop/scar/distant/hindell_et_al-2020/Hi2023-aes_colony_weighted_cog.tif",
  "plot_hindell.rds",
  process_hindell,
  base_map
)

process_and_save_map(
  "https://data.source.coop/scar/distant/pinkerton_hayward-2021/Pi2021-annual_cog.tif",
  "plot_pinkerton.rds",
  process_pinkerton,
  base_map
)

process_and_save_map(
  "https://data.source.coop/scar/distant/freer_et_al-2019/Fr2019-Krefftichthys_anderssoni_cog.tif",
  "plot_freer.rds",
  process_freer,
  base_map
)

cat("All processing completed. Output files are in:", config$output_dir, "\n")