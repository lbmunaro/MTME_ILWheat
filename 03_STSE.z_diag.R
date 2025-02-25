# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

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

# ILYT_Pheno |>
#   ggplot(aes(x=Col, y=Row, fill=Block)) +
#   geom_tile() +
#   facet_wrap(~Env)
# 
# Fit model 0----
STSE.z.asr0 <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb')
STSE.z.asr0 <- update(STSE.z.asr0)

print('STSE.z0')
print(paste('convergence =',STSE.z.asr0$converge))
STSE.z.asr0$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/STSE.z_diag.RData')

# Fit model 1----
STSE.z.asr1 <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    diag(TraitEnv):Gkeep,
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb')
STSE.z.asr1 <- update(STSE.z.asr1)

print('STSE.z1')
print(paste('convergence =',STSE.z.asr1$converge))
STSE.z.asr1$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/STSE.z_diag.RData')

# Fit model 2----
STSE.z.asr2 <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    diag(TraitEnv):Gkeep +
    diag(TraitEnv):Block,
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb')
STSE.z.asr2 <- update(STSE.z.asr2)

print('STSE.z2')
print(paste('convergence =',STSE.z.asr2$converge))
STSE.z.asr2$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

save.image('Data/STSE.z_diag.RData')

# End ----