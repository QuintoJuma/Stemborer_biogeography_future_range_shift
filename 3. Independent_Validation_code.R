# ============================================================
# 🔍 Raster Validation Script
# Species: Busseola fusca & Chilo partellus
# Author: Quinto_Juma
# Date: 2025-11-03
# Description: Compares modeled presence–absence rasters against
# independent validation points to compute accuracy metrics.
# ============================================================

# ---- Load required packages ----
library(raster)
library(readr)
library(dplyr)

# ---- Input file paths ----
# Replace these with your local or relative paths
raster1_path <- "data/rasters/busseola_fusca_presence_absence.tif"
raster2_path <- "data/rasters/chilo_partellus_presence_absence.tif"
csv_path     <- "data/validation/new_val_data.csv"

# ---- Validate paths ----
stopifnot(file.exists(raster1_path),
          file.exists(raster2_path),
          file.exists(csv_path))

# ---- Load rasters and validation data ----
r_busseola <- raster(raster1_path)
r_chilo    <- raster(raster2_path)
points_df  <- read_csv(csv_path)

# Check expected columns
required_cols <- c("Longitude", "Latitude", "overlap")
if (!all(required_cols %in% names(points_df))) {
  stop("Validation CSV must contain: Longitude, Latitude, overlap")
}

# ---- Extract raster predictions at validation coordinates ----
coords <- points_df[, c("Longitude", "Latitude")]
vals_busseola <- raster::extract(r_busseola, coords)
vals_chilo    <- raster::extract(r_chilo, coords)

# ---- Combine extracted predictions with observed overlap ----
df <- points_df %>%
  mutate(
    pred_busseola = vals_busseola,
    pred_chilo    = vals_chilo,
    observed      = overlap
  ) %>%
  filter(!is.na(pred_busseola) & !is.na(pred_chilo) & !is.na(observed))

# ---- Function to compute classification metrics ----
compute_metrics <- function(predicted, observed) {
  TP <- sum(predicted == 1 & observed == 1)
  FP <- sum(predicted == 1 & observed == 0)
  FN <- sum(predicted == 0 & observed == 1)
  TN <- sum(predicted == 0 & observed == 0)
  
  Accuracy  <- (TP + TN) / (TP + FP + FN + TN)
  Recall    <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  Precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  F1        <- ifelse(!is.na(Recall) && !is.na(Precision) && (Recall + Precision) > 0,
                      2 * (Precision * Recall) / (Precision + Recall),
                      NA)
  
  tibble(
    True_Positives = TP,
    False_Positives = FP,
    False_Negatives = FN,
    True_Negatives = TN,
    Accuracy = round(Accuracy, 3),
    Recall = round(Recall, 3),
    Precision = round(Precision, 3),
    F1_Score = round(F1, 3)
  )
}

# ---- Compute metrics per species ----
metrics_busseola <- compute_metrics(df$pred_busseola, df$observed) %>%
  mutate(Species = "Busseola fusca")

metrics_chilo <- compute_metrics(df$pred_chilo, df$observed) %>%
  mutate(Species = "Chilo partellus")

# ---- Combine and export results ----
all_metrics <- bind_rows(metrics_busseola, metrics_chilo) %>%
  select(Species, everything())

# Save results
output_file <- "outputs/validation_metrics_by_species.csv"
dir.create(dirname(output_file), showWarnings = FALSE)
write_csv(all_metrics, output_file)

# ---- Print summary ----
cat("\n✅ Raster Validation Metrics:\n")
print(all_metrics)
cat("\nResults saved to:", output_file, "\n")
