# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Fit rr2a model ----
## Run model ----
MTME.z_rr2a.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '88gb'
)

# Print model info
print('MTME.z-rr2a')
print(summary(MTME.z_rr2a.asr)$call)
print('AIC')
print(summary(MTME.z_rr2a.asr)$aic)
print(paste('convergence =', MTME.z_rr2a.asr$converge))

save.image('Data/MTME.z_rr2a.RData')

## Update1 model ----
MTME.z_rr2a.asr <- update(MTME.z_rr2a.asr)

# Print model info
print('MTME.z-rr2a - Update 1')
print(summary(MTME.z_rr2a.asr)$call)
print('AIC')
print(summary(MTME.z_rr2a.asr)$aic)
print(paste('convergence =', MTME.z_rr2a.asr$converge))

save.image('Data/MTME.z_rr2a.RData')