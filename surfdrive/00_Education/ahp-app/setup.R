# setup.R

# Required packages
packages <- c("shiny", "dplyr", "ggplot2", "plotly", "data.tree", "collapsibleTree")

# Optionally install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}
lapply(packages, install_if_missing)
