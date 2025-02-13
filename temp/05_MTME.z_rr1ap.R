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

# Fit rr1ap model ----
## Run model ----
MTME.z_rr1ap.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    rr(TraitEnv,1):ide(Gkeep) + diag(TraitEnv):ide(Gkeep),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 40,
  workspace = '16gb'
)

# Print model info
print('MTME.z-rr1ap')
print('AIC')
print(summary(MTME.z_rr1ap.asr)$aic)
print(paste('convergence =', MTME.z_rr1ap.asr$converge))
MTME.z_rr1ap.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr1ap.RData')

## Update1 model ----
MTME.z_rr1ap.asr <- update(MTME.z_rr1ap.asr)

# Print model info
print('MTME.z-rr1ap - Update 1')
print('AIC')
print(summary(MTME.z_rr1ap.asr)$aic)
print(paste('convergence =', MTME.z_rr1ap.asr$converge))
MTME.z_rr1ap.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr1ap.RData')

## Update2 model ----
MTME.z_rr1ap.asr <- update(MTME.z_rr1ap.asr)

# Print model info
print('MTME.z-rr1ap - Update 2')
print('AIC')
print(summary(MTME.z_rr1ap.asr)$aic)
print(paste('convergence =', MTME.z_rr1ap.asr$converge))
MTME.z_rr1ap.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr1ap.RData')

## Update3 model ----
MTME.z_rr1ap.asr <- update(MTME.z_rr1ap.asr)

# Print model info
print('MTME.z-rr1ap - Update 3')
print('AIC')
print(summary(MTME.z_rr1ap.asr)$aic)
print(paste('convergence =', MTME.z_rr1ap.asr$converge))
MTME.z_rr1ap.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_rr1ap.RData')