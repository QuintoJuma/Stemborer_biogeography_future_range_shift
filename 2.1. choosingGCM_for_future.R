# Install missing packages
if (!require("devtools")) install.packages("devtools")
if (!require("chooseGCM")) devtools::install_github("luizesser/chooseGCM")
if (!require("tictoc")) install.packages("tictoc")
if (!require("terra")) install.packages("terra")

# Load necessary libraries
library(chooseGCM)
library(terra)
library(tictoc)

# Start timing
tic()

# Set seed for reproducibility
set.seed(1)

# Define parameters
path <- "G:\\xxxxxx\\1. Datasets\\Future_BioClim_2050\\ChoosingGCM\\input_data\\WorldClim_data_gcms_all"
year <- "2050"
ssp <- "585"
resolution <- 30
var_names <- c("bio1",'bio12')
k_clusters <- 3

# Load study area (Kenya shapefile)
study_area_kenya <- terra::vect("G:\\xxxxxx\\1. Datasets\\Future_BioClim_2050\\ChoosingGCM\\ken_adm_iebc_20191031_shp\\ken_admbnda_adm0_iebc_20191031.shp")

plot(study_area_kenya)
# Download WorldClim data (if not already downloaded)
worldclim_data(
  path = path,
  period = "future",
  variable = "bioc",
  year = year,
  gcm = "all",
  ssp = ssp,
  resolution = resolution
)

# Import GCM data
s <- import_gcms(path)
names(s) <- gsub("_ssp585_30_2050", "", names(s))

# Compare GCMs
res <- compare_gcms(s, var_names, study_area_kenya, k = k_clusters)
print("GCM Comparison Summary:")
print(res$statistics_gcms)

# Summarize GCMs
s_sum <- summary_gcms(s, var_names, study_area_kenya)
print("Summary of GCMs:")
print(s_sum)

# Correlation Matrix
s_cor <- cor_gcms(s, var_names, study_area_kenya, scale = TRUE, method = "pearson")
print("Pearson Correlation:")
print(s_cor)

# Distance Matrix
s_dist <- dist_gcms(s, var_names, study_area_kenya, method = "euclidean")
print("Euclidean Distance:")
print(s_dist)

# K-means Clustering
kmeans_result <- kmeans_gcms(s, var_names, study_area_kenya, k = k_clusters)
print("Suggested GCMs from K-means:")
print(kmeans_result$suggested_gcms)

# Hierarchical Clustering
hclust_gcms(s, var_names, study_area_kenya, k = k_clusters)

# Optimal K selection methods
optk_gcms(s, var_names, study_area_kenya, cluster = "kmeans", method = "wss", n = 1000)
optk_gcms(s, var_names, study_area_kenya, cluster = "kmeans", method = "silhouette", n = 1000)
optk_gcms(s, var_names, study_area_kenya, cluster = "kmeans", method = "gap_stat", n = 1000)

# Monte Carlo Simulation
montecarlo_gcms(
  s, var_names, study_area_kenya,
  perm = 10000,
  dist_method = "euclidean",
  clustering_method = "kmeans"
)

# Environmental space plot
env_gcms(s, var_names, study_area_kenya, highlight = res$suggested_gcms$k3)
env_gcms(s, var_names, study_area_kenya, highlight = "sum")

# Closest distance analysis
closestdist_gcms(s, var_names, study_area_kenya)
closestdist_gcms(s, var_names, study_area_kenya, k = k_clusters)

# End timing
toc()
