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

# Fit rr3a model ----
## Run model ----
MTME.z_rr3a.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '48gb'
)

# Print model info
print('MTME.z-rr3a')
print('AIC')
print(summary(MTME.z_rr3a.asr)$aic)
print(paste('convergence =', MTME.z_rr3a.asr$converge))
MTME.z_rr3a.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr3a.RData')

## Update1 model ----
MTME.z_rr3a.asr <- update(MTME.z_rr3a.asr)

# Print model info
print('MTME.z-rr3a - Update 1')
print('AIC')
print(summary(MTME.z_rr3a.asr)$aic)
print(paste('convergence =', MTME.z_rr3a.asr$converge))
MTME.z_rr3a.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr3a.RData')

## Update2 model ----
MTME.z_rr3a.asr <- update(MTME.z_rr3a.asr)

# Print model info
print('MTME.z-rr3a - Update 2')
print('AIC')
print(summary(MTME.z_rr3a.asr)$aic)
print(paste('convergence =', MTME.z_rr3a.asr$converge))
MTME.z_rr3a.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr3a.RData')

## Update3 model ----
MTME.z_rr3a.asr <- update(MTME.z_rr3a.asr)

# Print model info
print('MTME.z-rr3a - Update 3')
print('AIC')
print(summary(MTME.z_rr3a.asr)$aic)
print(paste('convergence =', MTME.z_rr3a.asr$converge))
MTME.z_rr3a.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr3a.RData')