# ---- Load Required Libraries ----
library(raster)
library(sf)
library(dplyr)
library(tidyr)
library(exactextractr)

# ---- INPUT PATHS ----
busseola_BAU_path  <- "G:/5. xxxxx/5. Maps_and_datasets_for_mapping/Data_for_mapping/rasters_for_mapping_paper2/3. Shift analysis/0. Reclass_shift/BAU_reclass_busseola_shift.tif"
busseola_PESS_path <- "G:/5. xxxxx/5. Maps_and_datasets_for_mapping/Data_for_mapping/rasters_for_mapping_paper2/3. Shift analysis/0. Reclass_shift/PESS_reclass_busseola_shift.tif"
chilo_BAU_path     <- "G:/5. xxxxx/5. Maps_and_datasets_for_mapping/Data_for_mapping/rasters_for_mapping_paper2/3. Shift analysis/0. Reclass_shift/BAU_reclass_chilo_shift.tif"
chilo_PESS_path    <- "G:/5. xxxxx/5. Maps_and_datasets_for_mapping/Data_for_mapping/rasters_for_mapping_paper2/3. Shift analysis/0. Reclass_shift/PESS_reclass_chilo_shift.tif"
kenya_shp_path     <- "G:/1. Gis Disk Backup/GIS DATASETS/Datasets in Kenya/KEN_adm_shp/KEN_adm_shp/KEN_adm1.shp"

# ---- LOAD DATA ----
busseola_BAU  <- raster(busseola_BAU_path)
busseola_PESS <- raster(busseola_PESS_path)
chilo_BAU     <- raster(chilo_BAU_path)
chilo_PESS    <- raster(chilo_PESS_path)

kenya_counties <- st_read(kenya_shp_path)

zonal_pixel_area <- function(r, r_name, counties, pixel_area_km2) {
  results <- lapply(1:nrow(counties), function(i) {
    vals <- exact_extract(r, counties[i, ], include_cell = FALSE)[[1]]
    
    # Handle empty or invalid extraction
    if (is.null(vals) || nrow(vals) == 0 || !"value" %in% colnames(vals)) {
      return(data.frame(Loss_km2 = 0, NoChange_km2 = 0, Gain_km2 = 0))
    }
    
    # Remove NA values
    df <- vals[!is.na(vals$value), , drop = FALSE]
    if (nrow(df) == 0) {
      return(data.frame(Loss_km2 = 0, NoChange_km2 = 0, Gain_km2 = 0))
    }
    
    # Summarize pixel counts to area
    df_summary <- df %>%
      count(value) %>%
      complete(value = c(-1, 0, 1), fill = list(n = 0)) %>%
      mutate(area_km2 = n * pixel_area_km2) %>%
      select(value, area_km2) %>%
      pivot_wider(names_from = value, values_from = area_km2, names_prefix = "class_")
    
    # Fill missing classes with 0 explicitly using backticks
    df_summary <- df_summary %>%
      mutate(
        `class_-1` = ifelse(is.na(`class_-1`), 0, `class_-1`),
        `class_0`  = ifelse(is.na(`class_0`),  0, `class_0`),
        `class_1`  = ifelse(is.na(`class_1`),  0, `class_1`)
      ) %>%
      select(`class_-1`, `class_0`, `class_1`) %>%
      rename(
        Loss_km2     = `class_-1`,
        NoChange_km2 = `class_0`,
        Gain_km2     = `class_1`
      )
    
    return(df_summary)
  })
  
  results_df <- bind_rows(results)
  results_df$County <- counties$NAME_1
  results_df$Raster <- r_name
  
  # Ensure no NAs anywhere
  results_df[is.na(results_df)] <- 0
  
  return(results_df)
}



# ---- APPLY TO ALL RASTERS ----
county_areas <- bind_rows(
  zonal_pixel_area(busseola_BAU,  "busseola_BAU",  kenya_counties, pixel_area_km2),
  zonal_pixel_area(busseola_PESS, "busseola_PESS", kenya_counties, pixel_area_km2),
  zonal_pixel_area(chilo_BAU,     "chilo_BAU",     kenya_counties, pixel_area_km2),
  zonal_pixel_area(chilo_PESS,    "chilo_PESS",    kenya_counties, pixel_area_km2)
)

# ---- SAVE SEPARATE CSV FILES PER RASTER ----
write.csv(
  filter(county_areas, Raster == "busseola_BAU"),
  "busseola_BAU_shift_per_county.csv",
  row.names = FALSE
)

write.csv(
  filter(county_areas, Raster == "busseola_PESS"),
  "busseola_PESS_shift_per_county.csv",
  row.names = FALSE
)

write.csv(
  filter(county_areas, Raster == "chilo_BAU"),
  "chilo_BAU_shift_per_county.csv",
  row.names = FALSE
)

write.csv(
  filter(county_areas, Raster == "chilo_PESS"),
  "chilo_PESS_shift_per_county.csv",
  row.names = FALSE
)

# ---- National Summary Function ----
national_shift_summary <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = c(Loss_km2, NoChange_km2, Gain_km2),
                 names_to = "Class", values_to = "Area_km2") %>%
    mutate(Class = case_when(
      Class == "Gain_km2" ~ "Gain",
      Class == "Loss_km2" ~ "Loss",
      Class == "NoChange_km2" ~ "NoChange"
    ))
  
  summary_df <- df_long %>%
    group_by(Raster, Class) %>%
    summarise(Total_km2 = sum(Area_km2, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = Class, values_from = Total_km2, values_fill = 0) %>%
    mutate(
      Total_Area_km2 = Gain + Loss + NoChange,
      Gain_pct     = round(100 * Gain / Total_Area_km2, 1),
      Loss_pct     = round(100 * Loss / Total_Area_km2, 1),
      NoChange_pct = round(100 * NoChange / Total_Area_km2, 1)
    ) %>%
    select(Raster, Gain, Loss, NoChange, Total_Area_km2, Gain_pct, Loss_pct, NoChange_pct)
  
  return(summary_df)
}

# ---- Apply to your county_areas data ----
national_summary <- national_shift_summary(county_areas)

# ---- Save to CSV ----
write.csv(national_summary, "national_range_shift_summary.csv", row.names = FALSE)

# ---- View result ----
print(national_summary)


# ---- Load libraries for plotting (if not already loaded) ----
library(ggplot2)
library(patchwork)
library(rlang)

# ---- Define consistent color mapping and plot titles ----
class_colors <- c(
  "Gain" = "#a00404",       # Red
  "Loss" = "#62b02e",       # Green
  "No change" = "#9a9995"   # Gray
)

title_map <- c(
  "busseola_BAU"  = "italic('Busseola fusca')~'\u2014 Business as usual (2-45)'",
  "chilo_BAU"     = "italic('Chilo partellus')~'\u2014 Business as usual (2-45)'",
  "busseola_PESS" = "italic('Busseola fusca')~'\u2014 Pessimistic (5-85)'",
  "chilo_PESS"    = "italic('Chilo partellus')~'\u2014 Pessimistic (5-85)'"
)

# ---- Desired plot order ----
ordered_rasters <- c("busseola_BAU", "chilo_BAU", "busseola_PESS", "chilo_PESS")

# ---- Generate and store plots ----
plot_list <- list()

for (r_name in ordered_rasters) {
  df <- filter(county_areas, Raster == r_name)
  
  df_long <- df %>%
    pivot_longer(cols = c(Loss_km2, NoChange_km2, Gain_km2),
                 names_to = "Class", values_to = "Area_km2") %>%
    mutate(Class = case_when(
      Class == "Gain_km2" ~ "Gain",
      Class == "Loss_km2" ~ "Loss",
      Class == "NoChange_km2" ~ "No change"
    ))
  
  p <- ggplot(df_long, aes(x = reorder(County, -Area_km2), y = Area_km2,
                           fill = Class, color = Class)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.7, linewidth = 0.6) +
    coord_flip() +
    labs(
      title = parse_expr(title_map[[r_name]]),
      y = "Area (km²)", x = "County",
      fill = "Range Shift", color = "Range Shift"
    ) +
    scale_fill_manual(values = class_colors) +
    scale_color_manual(values = class_colors) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.text.y = element_text(size = 8)
    )
  
  plot_list[[r_name]] <- p
  
  # Optional: Save individual plots
  ggsave(
    filename = paste0("range_shift_", r_name, ".png"),
    plot = p, width = 10, height = 8, dpi = 600
  )
}

# ---- Combine all four plots in a 2x2 layout ----
combined_plot <- (plot_list[["busseola_BAU"]] | plot_list[["chilo_BAU"]]) /
  (plot_list[["busseola_PESS"]] | plot_list[["chilo_PESS"]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

# ---- Save the combined figure ----
ggsave("combined_range_shift_formatted_ordered.png", combined_plot,
       width = 16, height = 12, dpi = 600)
