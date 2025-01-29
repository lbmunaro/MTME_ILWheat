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

# Model 0 ----

STSE_0.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ units | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 30,
  workspace = '12gb'
)
print('STSE_0')
print(summary(STSE_0.asr)$call)
STSE_0.asr$loglik
print('AIC')
print(summary(STSE_0.asr)$aic)
print(paste('convergence =',STSE_0.asr$converge))

## Save data
save.image('Data/STSE_mod0.RData')

# End ----