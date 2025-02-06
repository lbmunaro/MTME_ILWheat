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

# Fit RR2ap model ----
## Run model ----
MTME_RR2ap.asr <- asreml(
  Pheno ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    rr(TraitEnv,1):ide(Gkeep) + diag(TraitEnv):ide(Gkeep),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '120gb'
)

# Print model info
print('MTME-RR2ap')
print(summary(MTME_RR2ap.asr)$call)
print('AIC')
print(summary(MTME_RR2ap.asr)$aic)
print(paste('convergence =', MTME_RR2ap.asr$converge))

save.image('MTME_rr2ap.RData')

## Update1 model ----
MTME_RR2ap.asr <- update(MTME_RR2ap.asr)

# Print model info
print('MTME-RR2ap - Update 1')
print(summary(MTME_RR2ap.asr)$call)
print('AIC')
print(summary(MTME_RR2ap.asr)$aic)
print(paste('convergence =', MTME_RR2ap.asr$converge))

save.image('MTME_rr2ap.RData')

## Update2 model ----
MTME_RR2ap.asr <- update(MTME_RR2ap.asr)

# Print model info
print('MTME-RR2ap - Update 2')
print(summary(MTME_RR2ap.asr)$call)
print('AIC')
print(summary(MTME_RR2ap.asr)$aic)
print(paste('convergence =', MTME_RR2ap.asr$converge))

save.image('MTME_rr2ap.RData')

## Update3 model ----
MTME_RR2ap.asr <- update(MTME_RR2ap.asr)

# Print model info
print('MTME-RR2ap - Update 3')
print(summary(MTME_RR2ap.asr)$call)
print('AIC')
print(summary(MTME_RR2ap.asr)$aic)
print(paste('convergence =', MTME_RR2ap.asr$converge))

save.image('MTME_rr2ap.RData')
