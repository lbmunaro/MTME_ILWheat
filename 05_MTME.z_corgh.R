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

# Fit corgh model ----
## Run model ----
MTME.z_corgh.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ corgh(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 80,
  workspace = '192gb'
)

# Print model info
print('MTME.z-corgh')
print('AIC')
print(summary(MTME.z_corgh.asr)$aic)
print(paste('convergence =', MTME.z_corgh.asr$converge))
MTME.z_corgh.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_corgh.RData')

## Update1 model ----
MTME.z_corgh.asr <- update(MTME.z_corgh.asr)

# Print model info
print('MTME.z-corgh - Update 1')
print('AIC')
print(summary(MTME.z_corgh.asr)$aic)
print(paste('convergence =', MTME.z_corgh.asr$converge))
MTME.z_corgh.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/MTME.z_corgh.RData')