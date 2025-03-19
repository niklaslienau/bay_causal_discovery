# Bayesian Causal Discovery 

This repository contains the replication files for my **final term paper** in the **2nd-year PhD Bayesian Modeling course** at Goethe University Frankfurt. This paper compares **Bayesian** and **constraint-based causal discovery methods** in recovering the underlying structure of Bayesian networks. I conduct a Monte Carlo simulation study evaluating the **order-MCMC** algorithm and the **PC algorithm** across varying sample sizes, network sizes, sparsity levels, and edge strengths. 

## Replication Instructions
To replicate the results, download the following files:
- The **`Functions/` folder** (contains all necessary functions)
- The **`run_sim.R`** file (main script)

### **Running the Simulation**
1. Open **`run_sim.R`** in RStudio.
2. The script will:
   - Source all required functions.
   - Allow users to set **simulation parameters**.
   - Run the **Monte Carlo simulation**.
   - Generate **final output plots** (as seen in the paper).
3. Run the script to reproduce the results.

---

##  File Structure
### **`Functions/`Folder**
This folder contains all the necessary functions:

1. **`packages.R`** → Loads and installs required R packages.
2. **`data_sim.R`** → Generates data from a Gaussian Bayesian network.
3. **`pc_algorithm.R`** → Implements the PC algorithm using the `bnlearn` package.
4. **`order_mcmc.R`** → Implements Order MCMC using the `BiDAG` package.
5. **`eval_metrics.R`** → Defines evaluation metrics for assessing causal discovery performance.
6. **`monte_carlo.R`** → Codes up the Monte Carlo algorithm.
7. **`simulation_design.R`** → Runs the Monte Carlo simulation over all design settings and stores results.
8. **`final_plots.R`** → Generates the final plots presented in the paper.
9. **`mcmc_diagnostics.R`** → Runs independent MCMC chains and provides trace plots.

### **`run_sim.R`**
- This is the **main replication script**.
- It first **sources** all functions from the `Functions/` folder.
- It allows users to set **simulation settings**.
- It executes the **Monte Carlo study** and produces the final plots.

---

##  Required R Packages
All required packages will be installed automatically when running `run_sim.R`. The list of packages used is:
```r
c("bnlearn","igraph", "BiDAG", "ggplot2", "dplyr", "gridExtra")
