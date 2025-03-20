# This file installs (if not already done) and loads all required packages

# List of required CRAN packages
required_cran_packages <- c("bnlearn", "igraph", "ggplot2", "dplyr", "gridExtra")

# Function to check and install missing CRAN packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Install CRAN packages
invisible(lapply(required_cran_packages, install_if_missing))

# Ensure Bioconductor manager is installed (for BiDAG dependencies)
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}

# Install BiDAG (including Bioconductor dependencies)
if (!requireNamespace("BiDAG", quietly = TRUE)) {
  BiocManager::install("BiDAG", dependencies = TRUE)
}

# Load all packages
lapply(c(required_cran_packages, "BiDAG"), library, character.only = TRUE)
