# Single-Trait Multi-TraitEnvironment models ----
# This script fits Single-Trait Multi-TraitEnvironment models

# Clean workspace
rm(list = objects())  # Removes all objects from the TraitEnvironment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.
source("Functions_MTME.R")  # Load functions

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Fit models ----

## GY ----
### rr1----
# Run model
GY_STME.z_rr1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('GY_STME.z_rr1')
print(paste('convergence =',GY_STME.z_rr1.asr$converge))
GY_STME.z_rr1.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
GY_STME.z_rr1.asr <- update_asreml(GY_STME.z_rr1.asr, 
                                  max_updates = 10,
                                  save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr2----
# Run model
GY_STME.z_rr2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('GY_STME.z_rr2')
print(paste('convergence =',GY_STME.z_rr2.asr$converge))
GY_STME.z_rr2.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
GY_STME.z_rr2.asr <- update_asreml(GY_STME.z_rr2.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr3----
# Run model
GY_STME.z_rr3.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '20gb'
)
# Print model info
print('GY_STME.z_rr3')
print(paste('convergence =',GY_STME.z_rr3.asr$converge))
GY_STME.z_rr3.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
GY_STME.z_rr3.asr <- update_asreml(GY_STME.z_rr3.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

## TW ----
### rr1----
# Run model
TW_STME.z_rr1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('TW_STME.z_rr1')
print(paste('convergence =',TW_STME.z_rr1.asr$converge))
TW_STME.z_rr1.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
TW_STME.z_rr1.asr <- update_asreml(TW_STME.z_rr1.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr2----
# Run model
TW_STME.z_rr2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('TW_STME.z_rr2')
print(paste('convergence =',TW_STME.z_rr2.asr$converge))
TW_STME.z_rr2.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
TW_STME.z_rr2.asr <- update_asreml(TW_STME.z_rr2.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr3----
# Run model
TW_STME.z_rr3.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '20gb'
)
# Print model info
print('TW_STME.z_rr3')
print(paste('convergence =',TW_STME.z_rr3.asr$converge))
TW_STME.z_rr3.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
TW_STME.z_rr3.asr <- update_asreml(TW_STME.z_rr3.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

## HD ----
### rr1----
# Run model
HD_STME.z_rr1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('HD_STME.z_rr1')
print(paste('convergence =',HD_STME.z_rr1.asr$converge))
HD_STME.z_rr1.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
HD_STME.z_rr1.asr <- update_asreml(HD_STME.z_rr1.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr2----
# Run model
HD_STME.z_rr2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('HD_STME.z_rr2')
print(paste('convergence =',HD_STME.z_rr2.asr$converge))
HD_STME.z_rr2.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
HD_STME.z_rr2.asr <- update_asreml(HD_STME.z_rr2.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr3----
# Run model
HD_STME.z_rr3.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '20gb'
)
# Print model info
print('HD_STME.z_rr3')
print(paste('convergence =',HD_STME.z_rr3.asr$converge))
HD_STME.z_rr3.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
HD_STME.z_rr3.asr <- update_asreml(HD_STME.z_rr3.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

## HT ----
### rr1----
# Run model
HT_STME.z_rr1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('HT_STME.z_rr1')
print(paste('convergence =',HT_STME.z_rr1.asr$converge))
HT_STME.z_rr1.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
HT_STME.z_rr1.asr <- update_asreml(HT_STME.z_rr1.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr2----
# Run model
HT_STME.z_rr2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('HT_STME.z_rr2')
print(paste('convergence =',HT_STME.z_rr2.asr$converge))
HT_STME.z_rr2.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
HT_STME.z_rr2.asr <- update_asreml(HT_STME.z_rr2.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

### rr3----
# Run model
HT_STME.z_rr3.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '20gb'
)
# Print model info
print('HT_STME.z_rr3')
print(paste('convergence =',HT_STME.z_rr3.asr$converge))
HT_STME.z_rr3.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
HT_STME.z_rr3.asr <- update_asreml(HT_STME.z_rr3.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

## MAT ----
### rr1----
# Run model
MAT_STME.z_rr1.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,1):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'MAT') |> droplevels(),
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb'
)
# Print model info
print('MAT_STME.z_rr1')
print(paste('convergence =',MAT_STME.z_rr1.asr$converge))
MAT_STME.z_rr1.asr$trace |> as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Update model
MAT_STME.z_rr1.asr <- update_asreml(MAT_STME.z_rr1.asr, 
                                   max_updates = 10,
                                   save_path = "Data/STME.z_rr.a.RData")

save.image('Data/STME.z_rr.a.RData')

# End ----