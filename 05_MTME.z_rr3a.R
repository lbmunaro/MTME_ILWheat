# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.
source("Functions_MTME.R")  # Load functions

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Fit rr3a model ----
k <- 3
## Run model ----
MTME.z_rr3a.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '72gb'
)

# Print model info
print(paste('convergence =', MTME.z_rr3a.asr$converge))
MTME.z_rr3a.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Save
save.image('Data/MTME.z_rr3a.RData')

# Update model ----
MTME.z_rr3a.asr <- update_asreml(MTME.z_rr3a.asr, 
                                 max_updates = 10,
                                 save_path = "Data/MTME.z_rr3a.RData")

# Save
save.image('Data/MTME.z_rr3a.RData')