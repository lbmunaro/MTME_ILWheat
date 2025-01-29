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

# Fit RR3 model ----

## Run model ----
MTME_RR3.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '192gb'
)

# Print model info
print('MTME-RR3')
print(summary(MTME_RR3.asr)$call)
print('AIC')
print(summary(MTME_RR3.asr)$aic)
print(paste('convergence =', MTME_RR3.asr$converge))

save.image('MTME_mod_rr3.RData')

#### Update model ----
# load('MTME_mod_rr3.RData')
# MTME_RR3.asr <- update(MTME_RR3.asr)
# # Print model info
# print('MTME-RR3 - Update 1')
# print(summary(MTME_RR3.asr)$call)
# print('AIC')
# print(summary(MTME_RR3.asr)$aic)
# print(paste('convergence =', MTME_RR3.asr$converge))
# save.image('MTME_mod_rr3.RData')

