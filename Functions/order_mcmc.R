# Install Bioconductor manager (if not already installed)
#if (!requireNamespace("BiocManager", quietly = TRUE)) {
#  install.packages("BiocManager")
#}

# Install Rgraphviz from Bioconductor
#BiocManager::install("Rgraphviz")





#stepsave and iterations by default
run_order_mcmc <- function(data, cpdag) {
  
  
  # Define scoring parameters for BGe score (Gaussian Bayesian Networks)
  score_params <- scoreparameters(scoretype = "bge", data = data)  
  
  # Run Order MCMC
  #cpdag= False means no PC algo in first step to reduce search space, just normal Order MCMC
  mcmc_res= sampleBN(score_params, cpdag= FALSE, algorithm = "order", chainout = TRUE )
  
  
  
  
  # Return results
  return(mcmc_res)
}







