

# Function to run multiple independent MCMC chains and return their log-likelihood traces
run_multiple_chains <- function(num_nodes, num_samples, num_chains , iterations = 100000, stepsave = 5000) {
  
  traces <- list()  # Store trace results
  
  # Simulate ONE dataset for all chains (ensuring same posterior)
  sim_result <- simulate_GBN(num_nodes = num_nodes, num_samples = num_samples, edge_prob = 0.1, beta_range = c(1,3))
  
  for (i in 1:num_chains) {
    cat("Running Chain", i, "for num_nodes =", num_nodes, "and num_samples =", num_samples, "\n")
    mcmc_result <- run_order_mcmc(sim_result$data)
    
    # Store trace results
    trace_df <- data.frame(Iteration = seq(1, length(mcmc_result$trace)) * stepsave,
                           LogLikelihood = mcmc_result$trace,
                           Chain = paste0("Chain_", i))
    
    # Apply burn-in **within each chain** (remove first 20% of its own iterations)
    burn_in <- floor(0.2 * nrow(trace_df))
    traces[[i]] <- trace_df[(burn_in + 1):nrow(trace_df), ]  # Remove first 20% of rows for this chain
  }
  
  # Combine all chains into a single dataframe after burn-in
  trace_df_final <- do.call(rbind, traces)
  trace_df_final$Chain <- as.factor(trace_df_final$Chain)  # Ensure Chain is a factor
  
  return(trace_df_final)
}

set.seed(123)
# Run MCMC chains for all conditions
trace_5_100   <- run_multiple_chains(num_nodes = 5, num_samples = 100, num_chains = 5)
trace_5_1000  <- run_multiple_chains(num_nodes = 5, num_samples = 1000, num_chains = 5)
trace_15_100  <- run_multiple_chains(num_nodes = 15, num_samples = 100, num_chains = 5)
trace_15_1000 <- run_multiple_chains(num_nodes = 15, num_samples = 1000, num_chains = 5)
trace_25_100  <- run_multiple_chains(num_nodes = 25, num_samples = 100, num_chains = 5)
trace_25_1000 <- run_multiple_chains(num_nodes = 25, num_samples = 1000, num_chains = 5)


# Function to plot log-likelihood traces for one setting 
plot_trace <- function(trace_df, title) {   
  ggplot(trace_df, aes(x = Iteration, y = LogLikelihood, color= Chain)) + 
    geom_line(size = 0.5) +  # Thinner lines for better visibility     
    theme_minimal() +     
    labs(title = title, x = "Iteration", y = "Log BGe Score") +     
    theme(legend.position = "none")  # Remove legend for cleaner layout 
  } 

# Generate plots
p1 <- plot_trace(trace_5_100, "Nodes: 5, Samples: 100")
p2 <- plot_trace(trace_5_1000, "Nodes: 5, Samples: 1000")
p3 <- plot_trace(trace_15_100, "Nodes: 15, Samples: 100")
p4 <- plot_trace(trace_15_1000, "Nodes: 15, Samples: 1000")
p5 <- plot_trace(trace_25_100, "Nodes: 25, Samples: 100")
p6 <- plot_trace(trace_25_1000, "Nodes: 25, Samples: 1000")

# Detect OS and set appropriate graphics device
if (Sys.info()["sysname"] == "Darwin") {  # macOS
  quartz()
} else if (Sys.info()["sysname"] == "Windows") {  # Windows
  windows()
} else {  # Linux 
  X11()
}

# Arrange in a 3x2 grid
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)
