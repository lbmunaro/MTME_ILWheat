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

# Fit RR2a model ----
## Run model ----
MTME.std_RR2a.asr <- asreml(
  Pheno_std ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '120gb'
)

# Print model info
print('MTME.std-RR2a')
print(summary(MTME.std_RR2a.asr)$call)
print('AIC')
print(summary(MTME.std_RR2a.asr)$aic)
print(paste('convergence =', MTME.std_RR2a.asr$converge))

save.image('Data/MTME.std_rr2a.RData')

## Update1 model ----
MTME.std_RR2a.asr <- update(MTME.std_RR2a.asr)

# Print model info
print('MTME.std-RR2a - Update 1')
print(summary(MTME.std_RR2a.asr)$call)
print('AIC')
print(summary(MTME.std_RR2a.asr)$aic)
print(paste('convergence =', MTME.std_RR2a.asr$converge))

save.image('Data/MTME.std_rr2a.RData')