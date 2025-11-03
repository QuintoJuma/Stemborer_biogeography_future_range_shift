# Load libraries
library(terra)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(tidyr) 
# Set paths
input_dir <- "G:\\xxxxxxx\\1. Datasets\\Future_BioClim_2050\\2. Consistency between models-True Skill Test\\future"   
output_dir <- "G:\\xxxxxxx\\1. Datasets\\Future_BioClim_2050\\2. Consistency between models-True Skill Test\\Output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# List all .tif files
all_files <- list.files(input_dir, pattern = "\\.tif$", full.names = TRUE)

# Define species
species_list <- c("busseola", "chilo")

# Create empty lists for summaries
summary_list <- list()
pairwise_agreement_list <- list()
model_consistency_list <- list()  # To hold consistency rankings

# Separate model types (BAU and PESS) for comparison
model_scenarios <- c("BAU", "PESS")

# Loop through each species
for (species in species_list) {
  
  cat("Processing:", species, "\n")
  
  # Filter files for this species and each scenario (BAU and PESS)
  species_files_bau <- all_files[grepl(paste0("_BAU_", species), all_files)]
  species_files_pess <- all_files[grepl(paste0("_PESS_", species), all_files)]
  
  # Check if files are being found
  cat("BAU Files for", species, ":", species_files_bau, "\n")
  cat("PESS Files for", species, ":", species_files_pess, "\n")
  
  # If files are empty, print an error and skip
  if (length(species_files_bau) == 0 | length(species_files_pess) == 0) {
    cat("Error: No files found for", species, "\n")
    next  # Skip to next species
  }
  
  # Stack BAU rasters
  raster_stack_bau <- rast(species_files_bau)
  model_names_bau <- gsub(".tif$", "", basename(species_files_bau))
  names(raster_stack_bau) <- model_names_bau
  
  # Stack PESS rasters
  raster_stack_pess <- rast(species_files_pess)
  model_names_pess <- gsub(".tif$", "", basename(species_files_pess))
  names(raster_stack_pess) <- model_names_pess
  
  # --- 1. Calculate Variance and Robust Classification ---
  
  # For BAU scenario
  pixel_variance_bau <- app(raster_stack_bau, var)
  variance_output_bau <- file.path(output_dir, paste0("pixel_variance_2050_", species, "_BAU.tif"))
  writeRaster(pixel_variance_bau, variance_output_bau, overwrite = TRUE)
  
  robust_sensitive_bau <- classify(pixel_variance_bau, 
                                   rcl = matrix(c(-Inf, 0.05, 1, 
                                                  0.05, Inf, 0), 
                                                ncol = 3, byrow = TRUE))
  classified_output_bau <- file.path(output_dir, paste0("robust_vs_sensitive_2050_", species, "_BAU.tif"))
  writeRaster(robust_sensitive_bau, classified_output_bau, overwrite = TRUE)
  
  # For PESS scenario
  pixel_variance_pess <- app(raster_stack_pess, var)
  variance_output_pess <- file.path(output_dir, paste0("pixel_variance_2050_", species, "_PESS.tif"))
  writeRaster(pixel_variance_pess, variance_output_pess, overwrite = TRUE)
  
  robust_sensitive_pess <- classify(pixel_variance_pess, 
                                    rcl = matrix(c(-Inf, 0.05, 1, 
                                                   0.05, Inf, 0), 
                                                 ncol = 3, byrow = TRUE))
  classified_output_pess <- file.path(output_dir, paste0("robust_vs_sensitive_2050_", species, "_PESS.tif"))
  writeRaster(robust_sensitive_pess, classified_output_pess, overwrite = TRUE)
  
  # --- 2. Full Pairwise Model Comparisons ---
  
  pairwise_results_bau <- data.frame(
    Species = character(),
    Scenario = character(),
    Model_1 = character(),
    Model_2 = character(),
    Agreement_Percent = numeric()
  )
  
  pairwise_results_pess <- data.frame(
    Species = character(),
    Scenario = character(),
    Model_1 = character(),
    Model_2 = character(),
    Agreement_Percent = numeric()
  )
  
  consistency_scores_bau <- setNames(rep(0, length(model_names_bau)), model_names_bau)
  consistency_scores_pess <- setNames(rep(0, length(model_names_pess)), model_names_pess)
  
  # For BAU scenario
  model_combinations_bau <- combn(model_names_bau, 2, simplify = FALSE)
  for (combo in model_combinations_bau) {
    model1 <- combo[1]
    model2 <- combo[2]
    
    r1 <- raster_stack_bau[[model1]]
    r2 <- raster_stack_bau[[model2]]
    
    agreement_bau <- mean(values(r1) == values(r2), na.rm = TRUE) * 100
    
    pairwise_results_bau <- rbind(pairwise_results_bau, data.frame(
      Species = species,
      Scenario = "BAU",
      Model_1 = model1,
      Model_2 = model2,
      Agreement_Percent = round(agreement_bau, 2)
    ))
    
    consistency_scores_bau[model1] <- consistency_scores_bau[model1] + agreement_bau
    consistency_scores_bau[model2] <- consistency_scores_bau[model2] + agreement_bau
  }
  
  # For PESS scenario
  model_combinations_pess <- combn(model_names_pess, 2, simplify = FALSE)
  for (combo in model_combinations_pess) {
    model1 <- combo[1]
    model2 <- combo[2]
    
    r1 <- raster_stack_pess[[model1]]
    r2 <- raster_stack_pess[[model2]]
    
    agreement_pess <- mean(values(r1) == values(r2), na.rm = TRUE) * 100
    
    pairwise_results_pess <- rbind(pairwise_results_pess, data.frame(
      Species = species,
      Scenario = "PESS",
      Model_1 = model1,
      Model_2 = model2,
      Agreement_Percent = round(agreement_pess, 2)
    ))
    
    consistency_scores_pess[model1] <- consistency_scores_pess[model1] + agreement_pess
    consistency_scores_pess[model2] <- consistency_scores_pess[model2] + agreement_pess
  }
  
  # --- 3. PLOT Consistency Heatmaps for Each Scenario ---
  
  # For BAU Scenario (Heatmap)
  pairwise_results_bau_summary <- pairwise_results_bau %>%
    group_by(Model_1) %>%
    summarize(Avg_Agreement = mean(Agreement_Percent)) %>%
    pivot_wider(names_from = Model_1, values_from = Avg_Agreement)  # Updated from spread() to pivot_wider()
  
  heatmap_bau <- ggplot(pairwise_results_bau, aes(x = Model_1, y = Model_2, fill = Agreement_Percent)) +
    geom_tile() +
    scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), limits = c(85, 100)) +
    labs(title = paste("Pairwise Agreement Heatmap -", toupper(species), "BAU"),
         fill = "Agreement %") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the heatmap for BAU
  ggsave(file.path(output_dir, paste0("heatmap_agreement_", species, "_BAU.png")), plot = heatmap_bau)
  
  # For PESS Scenario (Heatmap)
  pairwise_results_pess_summary <- pairwise_results_pess %>%
    group_by(Model_1) %>%
    summarize(Avg_Agreement = mean(Agreement_Percent)) %>%
    pivot_wider(names_from = Model_1, values_from = Avg_Agreement)  # Updated from spread() to pivot_wider()
  
  heatmap_pess <- ggplot(pairwise_results_pess, aes(x = Model_1, y = Model_2, fill = Agreement_Percent)) +
    geom_tile() +
    scale_fill_gradientn(colors = brewer.pal(9, "YlGnBu"), limits = c(85, 100)) +
    labs(title = paste("Pairwise Agreement Heatmap -", toupper(species), "PESS"),
         fill = "Agreement %") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the heatmap for PESS
  ggsave(file.path(output_dir, paste0("heatmap_agreement_", species, "_PESS.png")), plot = heatmap_pess)
  
  # --- SAVE CSV Summaries ---
  
  # Save pairwise results to CSV
  if (nrow(pairwise_results_bau) > 0) {
    write.csv(pairwise_results_bau, file = file.path(output_dir, paste0("pairwise_model_agreement_", species, "_BAU.csv")), row.names = FALSE)
  } else {
    cat("No pairwise results for BAU\n")
  }
  
  if (nrow(pairwise_results_pess) > 0) {
    write.csv(pairwise_results_pess, file = file.path(output_dir, paste0("pairwise_model_agreement_", species, "_PESS.csv")), row.names = FALSE)
  } else {
    cat("No pairwise results for PESS\n")
  }
  
  cat("Finished:", species, "\n\n")
}

# --- Save final CSV summaries ---
summary_all <- bind_rows(summary_list)
if (nrow(summary_all) > 0) {
  write.csv(summary_all, file = file.path(output_dir, "robustness_summary_2050.csv"), row.names = FALSE)
} else {
  cat("No summary results\n")
}

consistency_all <- bind_rows(model_consistency_list)
if (nrow(consistency_all) > 0) {
  write.csv(consistency_all, file = file.path(output_dir, "model_consistency_ranking_2050.csv"), row.names = FALSE)
} else {
  cat("No consistency results\n")
}

cat("✅ All processing completed! All rasters, plots, and summaries saved.\n")

