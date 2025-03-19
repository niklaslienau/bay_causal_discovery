
### Generate Simulation Settings 

generate_settings_list <- function(num_samples_set, num_nodes_set, edge_prob_set, beta_range_set) {
  # Check that num_nodes_set and edge_prob_set have the same length
  if (length(num_nodes_set) != length(edge_prob_set)) {
    stop("num_nodes_set and edge_prob_set must have the same length.")
  }
  
  settings_list <- list()
  
  # Loop through each num_nodes entry and its corresponding edge_prob values
  for (i in seq_along(num_nodes_set)) {
    num_nodes <- num_nodes_set[i]
    edge_probs <- edge_prob_set[[i]]  # Extract vector of edge probabilities
    
    # Loop through each beta range correctly
    for (beta_index in seq_along(beta_range_set)) {
      beta_range <- beta_range_set[[beta_index]]  # Get correct beta range
      
      # Create all combinations of num_samples and edge_probs
      for (num_samples in num_samples_set) {
        for (edge_prob in edge_probs) {
          setting_name <- paste0(
            "nodes_", num_nodes, 
            "_samples_", num_samples,
            "_edge_", edge_prob,
            "_beta_", beta_index  # Store the index, not the range itself
          )
          
          settings_list[[setting_name]] <- list(
            num_samples = num_samples,
            num_nodes = num_nodes,  
            edge_prob = edge_prob,
            beta_range = beta_range  # Correctly mapped beta range
          )
        }
      }
    }
  }
  
  return(settings_list)
}





######## SIMULATION ACROSS ALL SETTINGS ################

run_monte_carlo_across_settings <- function(num_iterations = 100, settings_list) {
  
  # Initialize results list
  results <- list()
  
  for (setting_name in names(settings_list)) {
    # Extract parameter values from settings list
    setting <- settings_list[[setting_name]]
    num_nodes <- setting$num_nodes
    num_samples <- setting$num_samples
    edge_prob <- setting$edge_prob
    beta_range <- setting$beta_range  
    
    cat("Running Monte Carlo for", setting_name, "\n")
    
    # Run Monte Carlo for this setting
    mc_results <- run_monte_carlo(
      num_iterations = num_iterations,
      num_nodes = num_nodes,
      num_samples = num_samples,
      edge_prob = edge_prob
    )
    
    # Store results under the appropriate name
    results[[setting_name]] <- mc_results
  }
  
  return(results)
}

## Compute averages over all monte carlo runs

compute_summary_results <- function(sim_results, quantiles = c(0.25, 0.75)) {
  summary_results <- list()
  
  for (setting_name in names(sim_results)) {
    setting <- sim_results[[setting_name]]
    
    # Function to compute summary stats (mean + requested quantiles)
    compute_stats <- function(metric_values) {
      c(mean = mean(metric_values), quantile(metric_values, quantiles))
    }
    
    # Compute mean and requested quantiles for each metric
    summary_results[[setting_name]] <- list(
      PC = list(
        TPR = compute_stats(setting$PC$TPR),
        FPR = compute_stats(setting$PC$FPR),
        SHD = compute_stats(setting$PC$SHD)
      ),
      MCMC = list(
        TPR = compute_stats(setting$MCMC$TPR),
        FPR = compute_stats(setting$MCMC$FPR),
        SHD = compute_stats(setting$MCMC$SHD),
        runtime = compute_stats(setting$MCMC$runtime)
      )
    )
  }
  
  return(summary_results)
}





