# 📊 AHP Pairwise Comparison Tool

This Shiny app allows students to build a multi-criteria decision-making hierarchy and input pairwise comparisons using **Saaty’s scale** (Analytic Hierarchy Process). The app computes weights, checks consistency, and lays the foundation for sensitivity analysis.

------------------------------------------------------------------------

## 📁 Project Structure

ahp-app/ ├── app.R \# Main Shiny app ├── setup.R \# Installs required R packages ├── R/ │ ├── ahp_calculations.R \# AHP weighting logic │ ├── consistency_check.R \# Consistency ratio computation │ └── sensitivity.R \# Sensitivity analysis (to be added) ├── www/ │ └── custom.css \# Optional styling ├── data/ │ └── sample_inputs.RDS \# Optional sample data ├── outputs/ │ └── results/ \# Exported results (if used) └── README.md
