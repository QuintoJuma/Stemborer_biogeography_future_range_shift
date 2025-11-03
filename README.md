# 🌍 Species Distribution Modeling and Range Shift Analysis of Cereal Stemborers  
### *Busseola fusca* and *Chilo partellus*

This repository contains a complete R-based workflow for modeling, validating, and visualizing current and future distributions of two major cereal stemborer pests, *Busseola fusca* and *Chilo partellus*, under multiple climate change scenarios.  
The project integrates species distribution modeling (SDM), model validation, climate projection comparison, and spatial analysis of potential range shifts.

---

## 🔬 Project Overview

The workflow is designed to:
- Model the current climatic suitability of *B. fusca* and *C. partellus*  
- Project future distributions under different GCMs and SSP scenarios  
- Evaluate model performance using independent validation datasets  
- Quantify and visualize potential range shifts across time horizons  
- Compare and summarize inter-model consistency and prediction skill  

---

## 📂 Repository Structure

| File | Description |
|------|--------------|
| **1. Preprocessing_Code.R** | Data preparation: cleaning occurrence records, formatting predictors, and preparing raster layers for modeling. |
| **2. Complete_modeling_workflow.R** | Core SDM workflow using MaxEnt and ensemble modeling for current and future projections. |
| **2.1. choosingGCM_for_future.R** | Selects the most representative Global Climate Models (GCMs) for future scenarios. |
| **3. Independent_Validation_code.R** | Validates model predictions using independent occurrence data and computes accuracy metrics. |
| **4. Range_shift_code.R** | Calculates presence or absence changes between baseline and future projections to identify range expansion or contraction. |
| **5. Comparative_skill_test.R** | Compares model performance across scenarios using accuracy, sensitivity, and True Skill Statistics (TSS). |
| **6. Shift_pixel_area_calculation.R** | Converts raster-based shifts into area estimates (km²) for spatial quantification. |
| **7. shift_visuals.R** | Generates maps and visualizations summarizing spatial range changes across GCMs and scenarios. |

---

## 🧩 Dependencies

The workflow primarily uses the following R packages:  
`raster`, `terra`, `dplyr`, `ggplot2`, `sf`, `readr`, `maxnet`, and `ENMeval`.

---

## 🚀 How to Use

1. Clone or download this repository.  
2. Open the R project and ensure all dependencies are installed.  
3. Run the scripts in numerical order for a full analysis pipeline.  
4. Update file paths and input data as required.  
5. Outputs include model rasters, validation metrics, and range shift maps.

---

## 🧠 Applications

- Pest risk mapping under climate change  
- Ecological niche and invasion potential analysis  
- Climate adaptation research for crop protection and pest management  

---

## 📜 Citation

If you use this repository, please cite it as:  

> Juma, Q. (2025). *Species Distribution Modeling and Range Shift Analysis of Cereal Stemborers (Busseola fusca and Chilo partellus)*. GitHub repository.  

---

## 🗺️ License

This project is released under the MIT License. You are free to use, modify, and distribute the code with proper attribution.

---

## 💬 Contact

For questions or collaboration opportunities, please contact:  
**Quinto Juma**  
Email: [jmeltus@icipe.org]  
Affiliation: International Centre of Insect Physiology and Ecology (icipe)
