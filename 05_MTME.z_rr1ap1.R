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

# Fit rr1ap1 model ----
k <- 1
## Run model ----
MTME.z_rr1ap1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    rr(TraitEnv,1):Gkeep + diag(TraitEnv):Gkeep +
    diag(TraitEnv):Block,
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)

# Print model info
print(paste('convergence =', MTME.z_rr1ap1.asr$converge))
MTME.z_rr1ap1.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Save
save.image('Data/MTME.z_rr1ap1.RData')

# Update model ----
MTME.z_rr1ap1.asr <- update_asreml(MTME.z_rr1ap1.asr, 
                                 max_updates = 10,
                                 save_path = "Data/MTME.z_rr1ap1.RData")

# Save
save.image('Data/MTME.z_rr1ap1.RData')