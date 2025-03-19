library(ggplot2)
library(dplyr)
library(gridExtra)


### List to dataframe

convert_results_to_df <- function(final_res) {
  results_list <- list()
  
  for (setting_name in names(final_res)) {
    # Extract hyperparameters from setting name
    setting_parts <- unlist(strsplit(setting_name, "_"))
    
    # Ensure correct indexing of extracted values
    num_nodes <- as.numeric(setting_parts[2])
    num_samples <- as.numeric(setting_parts[4])
    edge_prob <- as.numeric(setting_parts[6])
    
    # Safeguard against errors in beta extraction
    if (length(setting_parts) >= 8) {
      beta_index <- as.numeric(setting_parts[8])
    } else {
      warning(paste("⚠ Warning: Beta index missing in setting name:", setting_name))
      beta_index <- NA  # Assign NA if not found
    }
    
    # Extract PC and MCMC metrics
    setting_data <- final_res[[setting_name]]
    
    results_list[[setting_name]] <- data.frame(
      num_nodes = num_nodes,
      num_samples = num_samples,
      edge_prob = edge_prob,
      beta_index = beta_index,  # Store as integer index
      method = c("PC", "MCMC"),
      TPR_mean = c(setting_data$PC$TPR["mean"], setting_data$MCMC$TPR["mean"]),
      TPR_Q1 = c(setting_data$PC$TPR[2], setting_data$MCMC$TPR[2]),
      TPR_Q2 = c(setting_data$PC$TPR[3], setting_data$MCMC$TPR[3]),
      FPR_mean = c(setting_data$PC$FPR["mean"], setting_data$MCMC$FPR["mean"]),
      FPR_Q1 = c(setting_data$PC$FPR[2], setting_data$MCMC$FPR[2]),
      FPR_Q2 = c(setting_data$PC$FPR[3], setting_data$MCMC$FPR[3]),
      SHD_mean = c(setting_data$PC$SHD["mean"], setting_data$MCMC$SHD["mean"]),
      SHD_Q1 = c(setting_data$PC$SHD[2], setting_data$MCMC$SHD[2]),
      SHD_Q2 = c(setting_data$PC$SHD[3], setting_data$MCMC$SHD[3]),
      runtime_mean = c(NA, setting_data$MCMC$runtime["mean"]),  # Only MCMC has runtime
      runtime_Q1 = c(NA, setting_data$MCMC$runtime[2]),
      runtime_Q3 = c(NA, setting_data$MCMC$runtime[3])
    )
  }
  
  # Combine all settings into one dataframe
  results_df <- do.call(rbind, results_list)
  rownames(results_df) <- NULL  # Remove rownames
  
  return(results_df)
}


##### Analysis PLots

#SHD 
shd_plot <- function(results_df) {
  library(ggplot2)
  library(gridExtra)
  library(dplyr)
  
  # Function to create SHD plots for different grouping variables
  create_shd_plot <- function(group_var, x_label, custom_labels = NULL, conditional_grouping = NULL) {
    plot_data <- results_df
    
    # If conditional grouping is needed (for edge_prob)
    if (!is.null(conditional_grouping)) {
      plot_data <- plot_data %>%
        group_by(!!sym(conditional_grouping), !!sym(group_var), method) %>%
        summarise(
          SHD_mean = mean(SHD_mean, na.rm = TRUE),
          SHD_Q1 = mean(SHD_Q1, na.rm = TRUE),
          SHD_Q2 = mean(SHD_Q2, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Identify weak and strong edge_prob values within each num_nodes group
      plot_data <- plot_data %>%
        group_by(!!sym(conditional_grouping)) %>%
        mutate(edge_category = ifelse(edge_prob == min(edge_prob), "Weak", "Strong")) %>%
        ungroup()
      
      # Now average over num_nodes to get final weak/strong categories
      plot_data <- plot_data %>%
        group_by(edge_category, method) %>%
        summarise(
          SHD_mean = mean(SHD_mean, na.rm = TRUE),
          SHD_Q1 = mean(SHD_Q1, na.rm = TRUE),
          SHD_Q2 = mean(SHD_Q2, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Set edge category as factor for correct x-axis ordering
      plot_data$edge_category <- factor(plot_data$edge_category, levels = c("Weak", "Strong"))
      
      ggplot(plot_data, aes(x = edge_category, y = SHD_mean, color = method)) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +  
        geom_errorbar(aes(ymin = SHD_Q1, ymax = SHD_Q2), position = position_dodge(width = 0.5), size = 0.7) +  
        theme_minimal() +
        labs(x = x_label, y = "SHD") +  
        theme(legend.position = "top", axis.text.x = element_text(size = 12))
      
    } else {
      # Standard processing for other variables
      plot_data <- plot_data %>%
        group_by(!!sym(group_var), method) %>%
        summarise(
          SHD_mean = mean(SHD_mean, na.rm = TRUE),
          SHD_Q1 = mean(SHD_Q1, na.rm = TRUE),
          SHD_Q2 = mean(SHD_Q2, na.rm = TRUE)
        ) %>%
        ungroup()
      
      # Apply custom labels if provided
      if (!is.null(custom_labels)) {
        plot_data[[group_var]] <- factor(plot_data[[group_var]], levels = names(custom_labels), labels = custom_labels)
      }
      
      ggplot(plot_data, aes(x = !!sym(group_var), y = SHD_mean, color = method)) +
        geom_point(size = 3, position = position_dodge(width = 0.5)) +  
        geom_errorbar(aes(ymin = SHD_Q1, ymax = SHD_Q2), position = position_dodge(width = 0.5), size = 0.7) +  
        theme_minimal() +
        labs(x = x_label, y = "SHD") +  
        theme(legend.position = "top", axis.text.x = element_text(size = 12))
    }
  }
  
  # Create four SHD plots
  p1 <- create_shd_plot("num_samples", "Sample Size")
  p2 <- create_shd_plot("num_nodes", "Number of Nodes")
  p3 <- create_shd_plot("edge_prob", "Edge Strength", conditional_grouping = "num_nodes")
  p4 <- create_shd_plot("beta_index", "Density/Sparsity", custom_labels = c("1" = "Sparse", "2" = "Dense"))
  
  # Arrange plots in a 2x2 grid
  quartz()
  grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
}


#TPR & FPR

library(ggplot2)
library(dplyr)
library(gridExtra)

library(ggplot2)
library(dplyr)
library(gridExtra)

# Function to create TPR vs FPR scatter plots across different (num_nodes, num_samples) conditions
tpr_fpr_scatterplot <- function(final_res_df) {
  
  # Filter relevant settings
  filtered_data <- final_res_df %>%
    filter(num_samples %in% c(10, 100),
           num_nodes %in% c(5, 25),
           beta_index == 1)  # Fix beta_index to 1 for consistency
  
  # Compute means for each combination of method, num_nodes, num_samples, edge_prob
  agg_data <- filtered_data %>%
    group_by(method, num_nodes, num_samples, edge_prob) %>%
    summarise(
      TPR_mean = mean(TPR_mean),
      FPR_mean = mean(FPR_mean),
      .groups = "drop"
    ) 
  
  # Label edge_prob values as "Sparse" (lower) and "Dense" (higher) **within each num_nodes group**
  agg_data <- agg_data %>%
    group_by(num_nodes, num_samples, method) %>%
    mutate(edge_label = ifelse(edge_prob == min(edge_prob), "Sparse", "Dense")) %>%
    ungroup()
  
  # Define marker shapes for sparsity levels
  shape_mapping <- c("Sparse" = 16, "Dense" = 17)  # Circle for sparse, triangle for dense
  
  # Generate individual plots for each (num_nodes, num_samples) combination
  plot_list <- list()
  conditions <- expand.grid(num_nodes = c(5, 25), num_samples = c(10, 100))  # Grid conditions
  
  for (i in 1:nrow(conditions)) {
    curr_nodes <- conditions$num_nodes[i]
    curr_samples <- conditions$num_samples[i]
    
    plot_data <- agg_data %>%
      filter(num_nodes == curr_nodes, num_samples == curr_samples)
    
    p <- ggplot(plot_data, aes(x = FPR_mean, y = TPR_mean, color = method, shape = edge_label)) +
      geom_point(size = 4) +  # Main points
      scale_shape_manual(values = shape_mapping) +  # Shape legend for sparsity
      theme_minimal() +
      labs(x = "False Positive Rate (FPR)", y = "True Positive Rate (TPR)", color = "Method", shape = "Density") +
      theme(
        legend.position = "none",  # Remove legends from individual plots
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.major = element_line(size = 0.2),
        panel.grid.minor = element_blank()
      ) +
      ggtitle(paste0("Nodes: ", curr_nodes, ", Samples: ", curr_samples))
    
    plot_list[[i]] <- p
  }
  
  # Extract legend from one of the plots
  get_legend <- function(a_plot) {
    tmp <- ggplot_gtable(ggplot_build(a_plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  # Create a separate legend plot
  legend_plot <- get_legend(
    ggplot(agg_data, aes(x = FPR_mean, y = TPR_mean, color = method, shape = edge_label)) +
      geom_point(size = 4) +
      scale_shape_manual(values = shape_mapping) +
      theme_minimal() +
      labs(color = "Method", shape = "Density") +
      theme(legend.position = "right")  # Legend only
  )
  
  # Arrange the 2x2 grid with a single legend on the right
  quartz()
  grid.arrange(
    grobs = c(plot_list, list(legend_plot)),
    layout_matrix = rbind(c(1, 2, 5), c(3, 4, 5)),  # Legend in the last column
    widths = c(1, 1, 0.3)  # Adjust legend width
  )
}












