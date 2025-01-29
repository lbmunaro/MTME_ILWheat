# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

# Objective ----
# - 

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Model 1 ----

STSE_1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 30,
  workspace = '12gb'
)
print('STSE_1')
print(summary(STSE_1.asr)$call)
STSE_1.asr$loglik
print('AIC')
print(summary(STSE_1.asr)$aic)
print(paste('convergence =',STSE_1.asr$converge))

## Save data ----
save.image('Data/STSE_mod1.RData')

# End ----