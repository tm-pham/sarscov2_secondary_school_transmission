rm(list=ls())
setwd("/home/julius_id/tpham/COVID19School/code/model_code/")
.libPaths("/hpc/local/CentOS7/julius_id/R_libs/4.0.1/")
packages <- c("data.table", "reshape2", "dplyr","truncnorm")
lapply(packages, require, character.only = TRUE)

### Scenario
suffix = c("_noInterv")
scenarios = c("No intervention")
if(length(suffix)!=length(scenarios)) print("Warning: Number of suffix and number of scenarios do not coincide!")

### Number of iterations
iter = 1
### Reproduction number
R0 = 2.0

### Packages and functions
source("covid19school_packages.R")  # Installs and loads all necessary packages
source("covid19school_functions.R") # Loads all relevant functions
source("covid19school_vars_baseline.R")      # Load all relevant variables
source("covid19school_init_vars_function.R") 
source("covid19school_epidemic.R")  # Load main function for simulation

### Isolation and quarantine flags
isolation_flags = rep(T, length(scenarios))
quarantine_flags = rep(F, length(scenarios))
quarantine_class_flags = rep(F, length(scenarios))
quarantine_fully_vacc_flags = rep(F, length(scenarios))
compliance_iso = 0.33 # Proportion of individuals that adhere to isolation

# Baseline: no boosting
booster_time = NULL

### Results path
resultsPath="../../results/"
folder='simT1'
if(!file.exists(paste0(resultsPath,folder))) dir.create(file.path(paste0(resultsPath,folder)))
print(paste0("File path = ", resultsPath,folder))
simulation_file='covid19school_start_simulation.R'

### Start simulation
source(paste0(resultsPath, folder,"/", simulation_file))