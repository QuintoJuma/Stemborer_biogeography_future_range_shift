###############################################################################
# Title: Species Distribution Modeling for Busseola fusca and Chilo partellus
# Study Area: Kenya
# Author: Quinto Juma
# Date: 2025-11-03
# Description: This script builds ensemble SDMs for two stemborer species using 
#              current environmental predictors, and evaluates model accuracy 
#              and niche similarity between species.
###############################################################################

# ===============================
# 0. Setup Environment
# ===============================

# Define Java path (adjust if needed)
java_home <- "C:\\Program Files\\Java\\jdk-23"
Sys.setenv(JAVA_HOME = java_home)
Sys.setenv(PATH = paste(java_home, "bin", Sys.getenv("PATH"), sep = ";"))

# Install required packages if missing
required_packages <- c(
  "terra", "sp", "corrplot", "ggpubr", "PresenceAbsence", "RColorBrewer",
  "quickPlot", "rgdal", "vip", "pdp", "boot", "raster", "foreign", 
  "sdm", "gbm", "dplyr", "dismo", "rJava"
)

installed <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed)) install.packages(pkg, dependencies = TRUE)
}

# Load packages
lapply(required_packages, library, character.only = TRUE)

# Verify Java connection
.jinit()
cat("Java version:", .jcall("java/lang/System", "S", "getProperty", "java.version"), "\n")

# Set MaxEnt path
maxent_path <- "C:\\Users\\jmeltus\\Desktop\\maxent\\maxent.jar"
if (file.exists(maxent_path)) {
  message("✅ MaxEnt found and ready to use.")
} else {
  stop("❌ MaxEnt not found. Please set the correct 'maxent_path'.")
}

# ===============================
# 1. Load Predictors
# ===============================
predictor_dir <- "D:\\0. EcoPM_work\\Modelling rasters\\low_correlation_rasters for modelling"
predictors <- rast(list.files(path = predictor_dir, pattern = ".tif$", full.names = TRUE))
plot(predictors[[1]], main = "Predictor Example")

# ===============================
# 2. Load Species Occurrence Data
# ===============================
setwd("D:\\0. EcoPM_work\\Napier grass_occ")

busseola_fusca <- shapefile("busseola_fusca.shp")
chilo_partellus <- shapefile("chilo_partellus.shp")

# Reproject to WGS84
crs_target <- CRS("+proj=longlat +datum=WGS84 +no_defs")
busseola_fusca <- spTransform(busseola_fusca, crs_target)
chilo_partellus <- spTransform(chilo_partellus, crs_target)

# Plot occurrences
plot(predictors[[1]], main = "Occurrences for Busseola fusca and Chilo partellus")
plot(busseola_fusca, col = "red", pch = 19, add = TRUE)
plot(chilo_partellus, col = "blue", pch = 19, add = TRUE)
legend("bottomleft", legend = c("B. fusca", "C. partellus"), col = c("red", "blue"), pch = 19)

# ===============================
# 3. Prepare SDM Data
# ===============================
sdm_data_bf <- sdmData(
  formula = Occ ~ . + coords(long + lat),
  train = busseola_fusca,
  predictors = predictors,
  bg = list(n = 1000, method = "gRandom", remove = TRUE)
)

sdm_data_cp <- sdmData(
  formula = Occ ~ . + coords(long + lat),
  train = chilo_partellus,
  predictors = predictors,
  bg = list(n = 1000, method = "gRandom", remove = TRUE)
)

# ===============================
# 4. Train Models
# ===============================
model_dir <- file.path(predictor_dir, "Model_Output")
dir.create(model_dir, showWarnings = FALSE)

methods <- c("rf", "glm", "brt")

cat("\n--- Training models for Busseola fusca ---\n")
start <- Sys.time()
model_bf <- sdm(Occ ~ ., data = sdm_data_bf, methods = methods,
                replication = "cv", test.percent = 30, cv.folds = 10)
cat("Busseola fusca model runtime:", Sys.time() - start, "\n")

cat("\n--- Training models for Chilo partellus ---\n")
start <- Sys.time()
model_cp <- sdm(Occ ~ ., data = sdm_data_cp, methods = methods,
                replication = "cv", test.percent = 30, cv.folds = 10)
cat("Chilo partellus model runtime:", Sys.time() - start, "\n")

# ===============================
# 5. Ensemble Predictions
# ===============================
ensemble_bf <- ensemble(model_bf, newdata = predictors, 
                        filename = file.path(model_dir, "busseola_fusca_ensemble.tif"),
                        setting = list(method = "weighted", stat = "AUC"))

ensemble_cp <- ensemble(model_cp, newdata = predictors, 
                        filename = file.path(model_dir, "chilo_partellus_ensemble.tif"),
                        setting = list(method = "weighted", stat = "AUC"))

# Plot ensemble outputs
par(mfrow = c(1, 2))
plot(ensemble_bf, main = "Busseola fusca Ensemble")
plot(ensemble_cp, main = "Chilo partellus Ensemble")

# ===============================
# 6. Evaluate Model Performance
# ===============================
eval_bf <- getEvaluation(model_bf)
eval_cp <- getEvaluation(model_cp)
print(eval_bf)
print(eval_cp)

# ===============================
# 7. Variable Importance
# ===============================
plot(getVarImp(model_bf), main = "Variable Importance - B. fusca")
plot(getVarImp(model_cp), main = "Variable Importance - C. partellus")

# ===============================
# 8. Convert to Presence/Absence
# ===============================
pa_bf <- pa(ensemble_bf, model_bf, id = "ensemble", opt = 2)
pa_cp <- pa(ensemble_cp, model_cp, id = "ensemble", opt = 2)

writeRaster(pa_bf, file.path(model_dir, "presence_absence_busseola_fusca.tif"), overwrite = TRUE)
writeRaster(pa_cp, file.path(model_dir, "presence_absence_chilo_partellus.tif"), overwrite = TRUE)

# ===============================
# 9. Niche Similarity
# ===============================
niche_similarity <- nicheSimilarity(ensemble_bf, ensemble_cp, stat = c("Imod", "Icor", "R"))
print("Niche Similarity Metrics:")
print(niche_similarity)

# ===============================
# 10. Principal Component Niche Comparison
# ===============================
pc <- pca(predictors)
niche_bf <- niche(pc@data, ensemble_bf, c("Comp.1", "Comp.2"), out = TRUE)
niche_cp <- niche(pc@data, ensemble_cp, c("Comp.1", "Comp.2"), out = TRUE)
nicheSimilarity(niche_bf, niche_cp)

# ===============================
# 11. Export All Outputs
# ===============================
writeRaster(ensemble_bf, file.path(model_dir, "busseola_fusca_suitability.tif"), overwrite = TRUE)
writeRaster(ensemble_cp, file.path(model_dir, "chilo_partellus_suitability.tif"), overwrite = TRUE)

saveRDS(model_bf, file.path(model_dir, "busseola_fusca_model.rds"))
saveRDS(model_cp, file.path(model_dir, "chilo_partellus_model.rds"))

cat("\n✅ Modeling complete for Busseola fusca and Chilo partellus (Kenya).\n")
