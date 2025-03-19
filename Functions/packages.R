#This file installs (if not already done) and libraries all packages that were used

# List of required packages
required_packages <- c("bnlearn","igraph", "BiDAG", "ggplot2", "dplyr", "gridExtra")  

# Function to check and install missing packages
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

# Apply function to all required packages
invisible(lapply(required_packages, install_if_missing))

# Load all packages
lapply(required_packages, library, character.only = TRUE)
