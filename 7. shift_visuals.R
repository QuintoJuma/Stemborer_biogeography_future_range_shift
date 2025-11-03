# ---- Load libraries ----
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(patchwork)
library(rlang)

# ---- Set CSV folder path ----
csv_folder <- "G:/xxxxxxx/5. Maps_and_datasets_for_mapping/Data_for_mapping/rasters_for_mapping_paper2/3. Shift analysis/shift_analysis_per_county"

# ---- Read and combine all CSV files ----
csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)

county_areas <- bind_rows(lapply(csv_files, function(file) {
  df <- read_csv(file)
  raster_name <- str_remove(basename(file), "\\.csv$")
  raster_name <- str_remove(raster_name, "_shift_per_county")  # <- this line fixes it
  df$Raster <- raster_name
  if ("NoChang" %in% names(df)) {
    df <- df %>% rename(NoChange_km2 = NoChang)
  }
  return(df)
}))


# ---- Color mapping (Gain = red, Loss = green, No change = gray) ----
class_colors <- c(
  "Gain" = "#a00404",
  "Loss" = "#62b02e",
  "No change" = "#9a9995"
)

# ---- Title map with italic species + scenario ----
title_map <- c(
  "busseola_BAU"  = "italic('Busseola fusca')~'\u2014 Business as usual (2-45)'",
  "chilo_BAU"     = "italic('Chilo partellus')~'\u2014 Business as usual (2-45)'",
  "busseola_PESS" = "italic('Busseola fusca')~'\u2014 Pessimistic (5-85)'",
  "chilo_PESS"    = "italic('Chilo partellus')~'\u2014 Pessimistic (5-85)'"
)

# ---- Define raster order ----
ordered_rasters <- c("busseola_BAU", "chilo_BAU", "busseola_PESS", "chilo_PESS")
plot_list <- list()

# ---- Generate individual plots and store them ----
for (r_name in ordered_rasters) {
  df <- filter(county_areas, Raster == r_name)
  
  df_long <- df %>%
    pivot_longer(cols = c(Loss_km2, NoChange_km2, Gain_km2),
                 names_to = "Class", values_to = "Area_km2") %>%
    mutate(Class = str_replace(Class, "_km2", ""),
           Class = ifelse(Class == "NoChange", "No change", Class))  # Match to class_colors
  
  p <- ggplot(df_long, aes(x = reorder(County, -Area_km2), y = Area_km2,
                           fill = Class, color = Class)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.7, linewidth = 0.7) +
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
  
  # Optionally save each individual plot
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

}
