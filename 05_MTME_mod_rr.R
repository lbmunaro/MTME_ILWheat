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
load('Data/ILYT_Pheno-Gmatrix.RData')

# Fit models ----
## RR2() ----

# Track time
MTME_RR2_start.time <- Sys.time()
print(MTME_RR2_start.time)

ILYT_Pheno |>
  glimpse()

### Run model ----
MTME_RR2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1v(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '88gb'
)

# Time to run
MTME_RR2_time <- Sys.time() - MTME_RR2_start.time
print(Sys.time())
print(MTME_RR2_time)

# Print model info
print('MTME-RR2')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(MTME_RR2.asr$noeff)
print(paste('convergence =', MTME_RR2.asr$converge))

save.image('Data/MTME_mod_rr.RData')