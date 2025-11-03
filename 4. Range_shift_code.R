# Load required libraries
library(terra)
library(dplyr)
library(ggplot2)

# Define paths
base_dir <- "G:/xxxxx/1. Datasets/Future_BioClim_2050/2. Consistency between models-True Skill Test"
current_dir <- file.path(base_dir, "current")
future_dir <- file.path(base_dir, "future")
output_dir <- file.path(base_dir, "range_shifts")
dir.create(output_dir, showWarnings = FALSE)

# Define species and models
species <- c("busseola", "chilo")
scenarios <- c("BAU", "PESS")
models <- c("HardGEM", "IPSL", "EarthVeg3", "ensemble")

# Load current rasters
current_rasters <- list(
  busseola = rast(file.path(current_dir, "busseola_current.tif")),
  chilo = rast(file.path(current_dir, "chilo_current.tif"))
)


# Initialize summary list
summary_list <- list()

# Loop through each species, scenario, and model
for (sp in species) {
  for (sc in scenarios) {
    for (mod in models) {
      
      # Define file name and read future raster
      future_file <- file.path(future_dir, paste0(mod, "_", sc, "_", sp, ".tif"))
      future_raster <- rast(future_file)
      current_raster <- current_rasters[[sp]]
      
      # Subtract: future - current
      shift_raster <- future_raster - current_raster
      
      # Save shift raster directly without classification
      out_name <- paste0(sp, "_", mod, "_", sc, "_shift_diff.tif")
      writeRaster(shift_raster, file.path(output_dir, out_name), overwrite = TRUE)
    }
  }
}

message("All raw range shift rasters saved in: ", output_dir)

