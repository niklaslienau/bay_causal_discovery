

# Function to run a constraint-based causal discovery method (PC algo) using bnlearn
run_PC_bnlearn <- function(data) {
  
  #  Run the constraint-based learning algorithm (PC algo)
  dag_bnlearn <- pc.stable(data , alpha=0.05)  # PC-stable algorithm for better consistency
  
  # Convert to an igraph object for visualization
  dag_igraph <- as.igraph(dag_bnlearn)
  
  # Return results
  return(list(
    learned_cpdag = dag_bnlearn,  # CPDAG output (Partially directed // Markov equiv. class)
    graph = dag_igraph  # For plotting
  ))
}



