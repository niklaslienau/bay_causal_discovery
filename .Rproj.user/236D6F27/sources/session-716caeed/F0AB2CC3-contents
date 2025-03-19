#This scirpt defines the function to simulate data from Gausian bayesian networks
#Input arguments for simulating are the following
#1. num_nodes = Network Size 
#2. num_samples= Sample Size
#3. edge_prob = controls density/sparsity
#4. beta_range = controls parameter size in conditonal distributions
library(bnlearn)  # For DAG structure
library(igraph)   # For DAG visualization


simulate_GBN <- function(num_nodes, num_samples, edge_prob, beta_range = c(1, 3)) {
  
  # Repeat until we get a DAG with at least one edge
  repeat {
    # Generate a random DAG using "ordered" method (ensures acyclicity)
    dag <- random.graph(nodes = as.character(1:num_nodes), num = 1, method = "ordered", prob = edge_prob)
    adj_matrix <- amat(dag)  # Get adjacency matrix
    
    # Count number of edges (P)
    num_edges <- sum(adj_matrix) / 2  # Divide by 2 since adjacency matrices count both directions
    
    # If P > 0, break out of loop; otherwise, generate a new DAG
    if (num_edges > 0) break  
  }
  
  # Ensure acyclicity by computing a topological order
  top_order <- as.numeric(node.ordering(dag))
  
  # Set up parameters: Regression Coefficients & Variances
  beta_matrix <- matrix(0, num_nodes, num_nodes, dimnames = list(as.character(1:num_nodes), as.character(1:num_nodes)))
  
  # Assign random weights to existing edges in the DAG
  for (i in 1:num_nodes) {
    for (j in 1:num_nodes) {
      if (adj_matrix[j, i] == 1) {  # j → i (DAG structure)
        beta_matrix[j, i] <- runif(1, beta_range[1], beta_range[2])  # Random weight for parent j
      }
    }
  }
  
  # Generate Data
  data <- matrix(0, nrow = num_samples, ncol = num_nodes)
  colnames(data) <- paste0("x_", 1:num_nodes)
  
  for (i in 1:num_samples) {
    for (node_name in top_order) {  
      node <- as.numeric(node_name)  
      parents <- which(adj_matrix[, node] == 1)
      mean_value <- if (length(parents) > 0) sum(beta_matrix[parents, node] * data[i, parents]) else 0
      data[i, node] <- rnorm(1, mean = mean_value, sd = 1)  # Draw from conditional standard normal
    }
  }
  
  # Store the DAG as an igraph object
  dag_igraph <- graph_from_adjacency_matrix(adj_matrix, mode = "directed")
  
  # Return both data and DAG object
  return(list(
    data = as.data.frame(data),
    dag = dag,
    dag_graph = dag_igraph
  ))
}




