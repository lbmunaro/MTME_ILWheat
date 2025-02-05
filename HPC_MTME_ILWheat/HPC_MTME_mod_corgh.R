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

# Fit corgh model ----

## Run model ----
MTME_corgh.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ corgh(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '144gb'
)

# Print model info
print('MTME-corgh')
print(summary(MTME_corgh.asr)$call)
print('AIC')
print(summary(MTME_corgh.asr)$aic)
print(paste('convergence =', MTME_corgh.asr$converge))

save.image('MTME_mod_corgh.RData')

#### Update model ----
load('MTME_mod_corgh.RData')
MTME_corgh.asr <- update(MTME_corgh.asr)
# Print model info
print('MTME-corgh - Update 1')
print(summary(MTME_corgh.asr)$call)
print('AIC')
print(summary(MTME_corgh.asr)$aic)
print(paste('convergence =', MTME_corgh.asr$converge))
save.image('MTME_mod_corgh.RData')

# #### Update model ----
# load('MTME_mod_corgh.RData')
# MTME_corgh.asr <- update(MTME_corgh.asr)
# # Print model info
# print('MTME-corgh - Update 2')
# print(summary(MTME_corgh.asr)$call)
# print('AIC')
# print(summary(MTME_corgh.asr)$aic)
# print(paste('convergence =', MTME_corgh.asr$converge))
# save.image('MTME_mod_corgh.RData')

