#This script defines a function to compare the performance of structure learning algorithms
#Because PC only spits out CPDAG we compare performance of learned skelatons vs true skelaton


#We compute three metrics
#1. True Positive Rate
#2. False Positive Rate
#3. Skeleton Structural Hamming Distance (ignoring directionality)


################
evaluate_skeleton_performance <- function(true_dag, pc_cpdag, mcmc_cpdag) {
  # Convert True DAG (bnlearn) to adjacency matrix
  if (inherits(true_dag, "bn")) {
    true_skeleton <- as.matrix(amat(true_dag))
  } else {
    true_skeleton <- as.matrix(true_dag)
  }
  
  # Convert PC CPDAG (bnlearn) to adjacency matrix
  if (inherits(pc_cpdag, "bn")) {
    pc_skeleton <- as.matrix(amat(pc_cpdag))
  } else {
    pc_skeleton <- as.matrix(pc_cpdag)
  }
  
  # Convert MCMC CPDAG (BiDAG) to adjacency matrix
  if (inherits(mcmc_cpdag, "matrix")) {
    mcmc_skeleton <- mcmc_cpdag
  } else if (inherits(mcmc_cpdag, "dgCMatrix") || inherits(mcmc_cpdag, "dtCMatrix")) {
    mcmc_skeleton <- as.matrix(mcmc_cpdag)
  } else if (inherits(mcmc_cpdag, "ddiMatrix")) {
    mcmc_skeleton <- as.matrix(mcmc_cpdag)  # Convert diagonal matrix to standard matrix
  } else {
    stop("MCMC CPDAG format not recognized. Expected a matrix, dgCMatrix, dtCMatrix, or ddiMatrix.")
  }
  
  # Convert all graphs to undirected skeletons
  true_skeleton[true_skeleton > 0] <- 1
  pc_skeleton[pc_skeleton > 0] <- 1
  mcmc_skeleton[mcmc_skeleton > 0] <- 1
  
  # Number of edges in true DAG
  num_true_edges <- sum(true_skeleton) / 2  # Divide by 2 since undirected edges are counted twice
  
  # Function to compute TPR, FPR, and SHD
  compute_metrics <- function(estimated_skeleton, true_skeleton) {
    TP <- sum(estimated_skeleton & true_skeleton) / 2  # True Positives
    FP <- sum(estimated_skeleton & !true_skeleton) / 2  # False Positives
    TPR <- TP / num_true_edges  # True Positive Rate
    FPR <- FP / num_true_edges  # False Positive Rate
    skeleton_shd <- sum((true_skeleton != estimated_skeleton) & (true_skeleton + estimated_skeleton > 0)) / 2
    return(list(TPR = TPR, FPR = FPR, Skeleton_SHD = skeleton_shd))
  }
  
  # Compute metrics for PC and MCMC
  pc_metrics <- compute_metrics(pc_skeleton, true_skeleton)
  mcmc_metrics <- compute_metrics(mcmc_skeleton, true_skeleton)
  
  # Return results
  return(list(PC = pc_metrics, MCMC = mcmc_metrics))
}





