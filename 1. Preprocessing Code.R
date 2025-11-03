###########################################################################
# Title:       Geospatial processing workflow for stemborer mapping
# Description: Reproducible raster & vector pre-processing pipeline
#              Prepared for species: Busseola fusca & Chilo partellus
#              Study area: Kenya (country-level / subnational)
# Author:      Quinto Juma
# Date:        2025-11-03

###########################################################################

# ---------------------------
# Install packages (one-time)
# ---------------------------
# Uncomment and run if needed:
# install.packages(c('terra','sf','dplyr','stringr','janitor','readr','tictoc'))

# ---------------------------
# Load libraries
# ---------------------------
library(terra)     # replaces raster for most workflows
library(sf)        # vector handling
library(dplyr)
library(stringr)
library(janitor)
library(readr)
library(tictoc)    # optional timing

# ---------------------------
# PARAMETERS (edit these!)
# ---------------------------
# Working directories / file paths (edit before running)
base_dir        <- "E:/Geospatial_Project"      # project root (example)
shp_path        <- file.path(base_dir, "vector", "kenya_countries.shp")  # Kenya shapefile or countries
points_shp      <- file.path(base_dir, "vector", "traps_anopheles.shp")  # points (if applicable)
input_raster_dir<- file.path(base_dir, "rasters", "raw")   # input rasters root folder
output_dir      <- file.path(base_dir, "rasters", "processed")
out_gpkg        <- file.path(base_dir, "outputs", "processed_layers.gpkg")
extraction_csv  <- file.path(base_dir, "outputs", "samples_from_rasters.csv")

# Study metadata
species_focus   <- c("Busseola fusca", "Chilo partellus")
study_area_name <- "Kenya"

# Target CRS: default uses UTM zone 36N with WGS84 datum as in original script.
# If you want an EPSG code instead, replace this string with "EPSG:32636" or another.
target_crs_proj4 <- "+proj=utm +zone=36 +datum=WGS84 +units=m +north +no_defs"
# (Change target_crs_proj4 to appropriate UTM zone or EPSG for your study area if needed)

# Patterns for categorical rasters (so resampling uses nearest neighbour)
categorical_patterns <- c("lulc", "landcover", "class", "global_lulc")

# ---------------------------
# Create output folders (safe)
# ---------------------------
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(out_gpkg), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# Helper functions
# ---------------------------

# Function: detect categorical raster by filename or small unique values
is_categorical_raster <- function(r_path, patterns = categorical_patterns, unique_threshold = 50) {
  fname <- tolower(basename(r_path))
  if (any(sapply(patterns, function(p) grepl(p, fname)))) return(TRUE)
  # fallback: check number of unique values in a small sample (fast)
  r <- try(terra::rast(r_path), silent = TRUE)
  if (inherits(r, "try-error")) return(FALSE)
  vals <- try(terra::unique(r, n = unique_threshold), silent = TRUE)
  if (inherits(vals, "try-error")) return(FALSE)
  return(length(vals) <= unique_threshold)
}

# Function: project / align / crop / mask / write raster
process_raster <- function(inpath, mask_rast, outdir, method_cat = "near", method_cont = "bilinear", overwrite = TRUE) {
  r <- rast(inpath)
  # reproject to match mask (if different)
  if (!compareGeom(r, mask_rast, stopOnError = FALSE)) {
    r <- project(r, mask_rast, method = ifelse(is_categorical_raster(inpath), method_cat, method_cont))
  } else {
    # still ensure same resolution and extent by resampling to mask grid
    r <- resample(r, mask_rast, method = ifelse(is_categorical_raster(inpath), method_cat, method_cont))
  }
  rc <- crop(r, ext(mask_rast))
  rm(r)
  rm_g <- terra::mask(rc, mask_rast)
  # write out
  outname <- paste0(tools::file_path_sans_ext(basename(inpath)), ".tif")
  outfile <- file.path(outdir, outname)
  writeRaster(rm_g, filename = outfile, overwrite = overwrite)
  return(outfile)
}

# ---------------------------
# Read & prepare vector mask (study area)
# ---------------------------
cat("Reading study area shapefile...\n")
if (!file.exists(shp_path)) stop("Study area shapefile not found. Edit shp_path parameter.")

# read with sf and reproject
study_sf <- st_read(shp_path, quiet = TRUE)
# Clean common name fields
if ("country" %in% names(study_sf) == FALSE & "NAME" %in% names(study_sf)) {
  study_sf <- study_sf %>% rename(country_name = NAME)
}
# reproject to target CRS
study_sf <- st_transform(study_sf, crs = target_crs_proj4)

# Convert to terra vector (for masking)
study_vect <- vect(study_sf)

# Optional: if the shapefile contains multiple countries and you only want Kenya
# try to filter by country name (uncomment and adjust if needed)
# study_sf <- study_sf %>% filter(str_detect(tolower(NAME), "kenya"))
# study_vect <- vect(study_sf)

# Build a template raster mask using one reference raster or create an empty raster
# If you have a mask raster file, use it; otherwise create a blank template
mask_raster_path <- file.path(base_dir, "rasters", "maskFile.tif")
mask_rast <- NULL
if (file.exists(mask_raster_path)) {
  mask_rast <- rast(mask_raster_path)
  # reproject mask to target CRS if needed
  if (!crs(mask_rast) == crs(target_crs_proj4)) mask_rast <- project(mask_rast, target_crs_proj4)
  # crop mask to study_vect and ensure identical grid
  mask_rast <- crop(mask_rast, study_vect)
  mask_rast <- mask(mask_rast, study_vect)
} else {
  # create a default empty raster template with reasonable resolution
  cat("No mask raster found; creating template raster from study vector extent...\n")
  e <- ext(study_vect)
  # define resolution (meters) - adjust as needed
  res_m <- 1000    # default 1 km cell; change if you need finer/different
  ncol <- ceiling((e[2] - e[1]) / res_m)
  nrow <- ceiling((e[4] - e[3]) / res_m)
  mask_rast <- rast(ncols = ncol, nrows = nrow, ext = e, crs = target_crs_proj4)
  values(mask_rast) <- 1
  mask_rast <- mask(mask_rast, study_vect)
}

# ---------------------------
# Batch process rasters
# ---------------------------
cat("Listing input rasters...\n")
r_files <- list.files(input_raster_dir, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)
if (length(r_files) == 0) stop("No input rasters found; set input_raster_dir")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

cat("Processing rasters (project/resample/crop/mask) ...\n")
tic()
processed_files <- lapply(r_files, function(p) {
  cat(" -> ", basename(p), "\n")
  tryCatch({
    process_raster(p, mask_rast, outdir = output_dir)
  }, error = function(e) {
    message("ERROR processing ", p, ": ", e$message)
    return(NA)
  })
})
toc()

processed_files <- unlist(processed_files)
processed_files <- processed_files[file.exists(processed_files)]

# ---------------------------
# Optionally: stack processed rasters
# ---------------------------
if (length(processed_files) > 0) {
  cat("Creating raster stack from processed rasters...\n")
  rast_list <- rast(processed_files)
  # If memory is constrained, consider writing a multi-band file or working by subsets
  out_stack_file <- file.path(output_dir, "all_processed_stack.tif")
  writeRaster(rast_list, filename = out_stack_file, overwrite = TRUE)
  cat("Stack saved to:", out_stack_file, "\n")
} else {
  stop("No processed rasters available to stack.")
}

# ---------------------------
# Extract raster values to points (if points provided)
# ---------------------------
if (file.exists(points_shp)) {
  cat("Reading point shapefile and extracting raster values...\n")
  pts_sf <- st_read(points_shp, quiet = TRUE) %>% st_transform(crs = target_crs_proj4)
  pts_vect <- vect(pts_sf)
  # extract (terra::extract handles stacks)
  vals <- terra::extract(rast_list, pts_vect, df = TRUE)
  # combine with point attributes
  pts_df <- cbind(as.data.frame(pts_sf), vals[,-1])  # remove ID column from vals
  write_csv(pts_df, extraction_csv)
  cat("Extraction saved to:", extraction_csv, "\n")
} else {
  cat("No point shapefile found at `points_shp`. Skipping extraction.\n")
}

# ---------------------------
# Save processed rasters and vector mask into a GeoPackage for QGIS
# ---------------------------
cat("Writing outputs to GeoPackage for easy QGIS import...\n")
# write the study vector (as polygon) to gpkg
sf::st_write(study_sf, out_gpkg, layer = "study_area", delete_layer = TRUE, quiet = TRUE)
# write raster layers as separate layers (terra supports writing to GPKG via writeRaster with gdal)
# but many QGIS users prefer GeoTIFFs; we saved GeoTIFF stack earlier. We will add single-band rasters to gpkg if needed:
for (p in processed_files) {
  layername <- make.names(tools::file_path_sans_ext(basename(p)))
  # convert raster to a temporary VRT and then write as raster in gpkg (if desired)
  try({
    writeRaster(rast(p), filename = out_gpkg, overwrite = TRUE, gdal = "GPKG", subds = NULL, option = c("COMPRESS=DEFLATE"))
  }, silent = TRUE)
}

cat("All done. Processed rasters are in:", output_dir, "\n")
cat("GeoPackage (vector) saved to:", out_gpkg, "\n")
cat("Species focus:", paste(species_focus, collapse = "; "), "\n")
cat("Study area:", study_area_name, "\n")
