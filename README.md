# Post-selection inference for causal effects after causal discovery
Chang T-H, Guo Z, Malinsky D (2024). Post-selection inference for causal effects after causal discovery. arXiv:2405.06763  

## Simulation
`code/fxn.R`: defines functions called in the simulation and data example code  
`code/sim.R`: code for running the simulation  
`code/plot.R`: code for creating the plots in the manuscript

Simulation results from different scenarios (varying the values of `n`, `M` and `c` in `code/sim.R`) are saved in `data/df_final.RDS`, which is read in `code/plot.R` to create the plots.

## Data example
`code/sachs.R`: code for replicating results in the single-cell protein network data example   

Data in `data/data.txt` is a clean version of the Sachs et al. (2005) data provided by Ramsey & Andrews (2018).  

