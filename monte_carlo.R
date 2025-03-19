#This script runs the monte carlo simulation that evaluates the skeleton learning performance of PC vs Bayesian algos

#Source Function

#Sample from Bayesian Network
source("data_sim.R")
#PC algorithm 
source("pc_algorithm.R")
#Order MCMC Algorithm
source("order_mcmc.R")
#evaluation metrics 
source("skeleton_eval.R")


run_monte_carlo <- function(num_iterations = 100, num_nodes = 10, num_samples = 500, edge_prob = 0.2, beta_range= c(1,3)) {
  
  # Store results in a list
  results <- list(
    PC = list(TPR = numeric(num_iterations), FPR = numeric(num_iterations), SHD = numeric(num_iterations)),
    MCMC = list(TPR = numeric(num_iterations), FPR = numeric(num_iterations), SHD = numeric(num_iterations), runtime = numeric(num_iterations))
  )
  
  for (i in 1:num_iterations) {
    cat("Running iteration", i, "of", num_iterations, "\n")
    
    # Generate random DAG & sample data
    sim_result <- simulate_GBN(num_nodes = num_nodes, num_samples = num_samples, edge_prob = edge_prob, beta_range = beta_range)
    
    # Run PC algorithm
    pc_result <- run_PC_bnlearn(sim_result$data)
    
    # Run Order MCMC algorithm (no need for manual timing)
    mcmc_result <- run_order_mcmc(sim_result$data)
    
    # Extract runtime from the MCMC output
    runtime_mcmc <-  as.numeric(mcmc_result$info$runtimes["MCMCchain"])
    
    
    # Evaluate performance (skeleton metrics)
    eval_results <- evaluate_skeleton_performance(
      true_dag = sim_result$dag,
      pc_cpdag = pc_result$learned_cpdag,
      mcmc_cpdag = mcmc_result$DAG
    )
    
    # Store metrics for PC
    results$PC$TPR[i] <- eval_results$PC$TPR
    results$PC$FPR[i] <- eval_results$PC$FPR
    results$PC$SHD[i] <- eval_results$PC$Skeleton_SHD
    
    # Store metrics for MCMC
    results$MCMC$TPR[i] <- eval_results$MCMC$TPR
    results$MCMC$FPR[i] <- eval_results$MCMC$FPR
    results$MCMC$SHD[i] <- eval_results$MCMC$Skeleton_SHD
    results$MCMC$runtime[i] <- runtime_mcmc  # Store MCMC runtime
  }
  
  return(results)
}




