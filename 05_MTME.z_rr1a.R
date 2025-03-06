# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.
source('Functions_MTME.R')  # Load functions

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Fit rr1a model ----
k <- 1
## Run model ----
MTME.z_rr1a.asr <- asreml(
  Pheno_z ~ TraitEnv, # Fixed effect
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + # Common GTE effect
    diag(TraitEnv):vm(Gkeep, Ginv.sparse), # Specific GTE effect
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv), # Independent Ar1xAr1 for each TraitEnv
  sparse = ~ TraitEnv:Gdrop, # Genotypes without marker data
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13, # Don't use it, changes step size. Use update instead - DT suggestion
  workspace = '16gb'
)

# Print model info
print(paste('convergence =', MTME.z_rr1a.asr$converge))
MTME.z_rr1a.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Save
save.image('Data/MTME.z_rr1a.RData')

# Update model ----
MTME.z_rr1a.asr <- update_asreml(MTME.z_rr1a.asr, 
                                 max_updates = 10,
                                 save_path = 'Data/MTME.z_rr1a.RData')

# Save
save.image('Data/MTME.z_rr1a.RData')
