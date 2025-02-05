# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Objective ----
# - 

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Load data ----
## Pheno & Ginv
load('ILYT_Pheno-Gmatrix.RData')

# Fit FA2 model ----

## Run model ----
MTME_FA2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ fa(TraitEnv,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '128gb'
)

# Print model info
print('MTME-FA2')
print(summary(MTME_FA2.asr)$call)
print('AIC')
print(summary(MTME_FA2.asr)$aic)
print(paste('convergence =', MTME_FA2.asr$converge))

save.image('MTME_mod_fa2.RData')

#### Update model ----
# load('MTME_mod_fa2.RData')
# MTME_FA2.asr <- update(MTME_FA2.asr)
# # Print model info
# print('MTME-FA2 - Update 1')
# print(summary(MTME_FA2.asr)$call)
# print('AIC')
# print(summary(MTME_FA2.asr)$aic)
# print(paste('convergence =', MTME_FA2.asr$converge))
# save.image('MTME_mod_fa2.RData')

