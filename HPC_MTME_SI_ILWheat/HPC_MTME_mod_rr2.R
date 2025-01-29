# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Load data ----
## Pheno & Ginv
load('ILYT_Pheno-Gmatrix.RData')

# Fit RR2 model ----
## Run model ----
MTME_RR2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '128gb'
)

# Print model info
print('MTME-RR2')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(paste('convergence =', MTME_RR2.asr$converge))

save.image('MTME_rr2.RData')

## Update1 model ----
MTME_RR2.asr <- update(MTME_RR2.asr)

# Print model info
print('MTME-RR2 - Update 1')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(paste('convergence =', MTME_RR2.asr$converge))

save.image('MTME_rr2.RData')

## Update2 model ----
MTME_RR2.asr <- update(MTME_RR2.asr)

# Print model info
print('MTME-RR2 - Update 2')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(paste('convergence =', MTME_RR2.asr$converge))

save.image('MTME_rr2.RData')

## Update3 model ----
MTME_RR2.asr <- update(MTME_RR2.asr)

# Print model info
print('MTME-RR2 - Update 3')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(paste('convergence =', MTME_RR2.asr$converge))

save.image('MTME_rr2.RData')

# End
