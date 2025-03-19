
################ Source Functions ################

#Load (and if necessary install) Packages
source("Functions/packages.R")
#Sample from Bayesian Network
source("Functions/data_sim.R")
#PC algorithm 
source("Functions/pc_algorithm.R")
#Order MCMC Algorithm
source("Functions/order_mcmc.R")
#evaluation metrics 
source("Functions/eval_metrics.R")
#Monte Carlo Simulation 
source("Functions/montecarlo.R")
#Monte Carlo across all simulation designs
source("Functions/simulation_design.R")
#Result Plots
source("Functions/final_plots.R")


################ Set parameters for simulation  ################

#DAG PARAMETERS
#network size
num_nodes_set <- c(5, 15, 25) 
#sample size
num_samples_set <- c(10,50,100,1000) 
#network density
edge_prob_set <- list(c(0.2,0.4),c(0.05,0.2), c(0.025,0.1)) #each entry must be vector with len(num_nodes_set)
#edge strength
beta_range_set <- list(c(0.1, 1), c(1, 3)) 

#MONTE CARLO SIMULATION PARAMETERS
#number of monte carlo iterations
num_iterations= 500

################ Run the simulation  ################


#SET SEED
set.seed(123)

# Generate list of all combinations (designs) we run monte carlo on
settings_list <- generate_settings_list(num_samples_set, num_nodes_set, edge_prob_set, beta_range_set)

#Run the monte carlo across all simulation designs
mc_res= run_monte_carlo_across_settings(num_iterations = num_iterations, settings_list = settings_list)

# Compute the quantiles and the average over all monte carlo runs within one design setting
final_res <- compute_summary_results(mc_res, quantiles = c(0.1, 0.9))

#Transform list to data frame for easier data slicing
final_res_df = convert_results_to_df(final_res )



############### Analyze Results ##################
# Plot Structural Hammingway Distance Analysis
shd_plot(final_res_df)

# Plot the True/False Positive Ratio Analyses
tpr_fpr_scatterplot(final_res_df)


############## MCMC Diagnostics
source("Functions/mcmc_diagnostics.R")


