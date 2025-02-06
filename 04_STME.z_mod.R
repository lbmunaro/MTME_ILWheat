# Single-Trait Multi-Environment models ----
# This script fits Single-Trait Multi-Environment models

# Objective ----
# - 

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

# Fit models ----

## GY ----
### fa1----
# Run model
GY_STME.z_fa1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,1):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('GY_STME.z_fa1')
print(summary(GY_STME.z_fa1.asr)$call)
print('AIC')
print(summary(GY_STME.z_fa1.asr)$aic)
print(paste('convergence =',GY_STME.z_fa1.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa2----
# Run model
GY_STME.z_fa2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('GY_STME.z_fa2')
print(summary(GY_STME.z_fa2.asr)$call)
print('AIC')
print(summary(GY_STME.z_fa2.asr)$aic)
print(paste('convergence =',GY_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
GY_STME.z_fa2.asr <- update(GY_STME.z_fa2.asr)

# Print model info
print('GY_STME.z_fa2-update1')
print(summary(GY_STME.z_fa2.asr)$call)
print('AIC')
print(summary(GY_STME.z_fa2.asr)$aic)
print(paste('convergence =',GY_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa3----
# Run model
GY_STME.z_fa3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,3):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('GY_STME.z_fa3')
print(summary(GY_STME.z_fa3.asr)$call)
print('AIC')
print(summary(GY_STME.z_fa3.asr)$aic)
print(paste('convergence =',GY_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
GY_STME.z_fa3.asr <- update(GY_STME.z_fa3.asr)

# Print model info
print('GY_STME.z_fa3-update1')
print(summary(GY_STME.z_fa3.asr)$call)
print('AIC')
print(summary(GY_STME.z_fa3.asr)$aic)
print(paste('convergence =',GY_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

## TW ----
### fa1----
# Run model
TW_STME.z_fa1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,1):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('TW_STME.z_fa1')
print(summary(TW_STME.z_fa1.asr)$call)
print('AIC')
print(summary(TW_STME.z_fa1.asr)$aic)
print(paste('convergence =',TW_STME.z_fa1.asr$converge))

save.image('Data/STME.z_mod.RData')

load('Data/STME.z_mod.RData')
### fa2----
# Run model
TW_STME.z_fa2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('TW_STME.z_fa2')
print(summary(TW_STME.z_fa2.asr)$call)
print('AIC')
print(summary(TW_STME.z_fa2.asr)$aic)
print(paste('convergence =',TW_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
TW_STME.z_fa2.asr <- update(TW_STME.z_fa2.asr)

# Print model info
print('TW_STME.z_fa2-update1')
print(summary(TW_STME.z_fa2.asr)$call)
print('AIC')
print(summary(TW_STME.z_fa2.asr)$aic)
print(paste('convergence =',TW_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa3----
# Run model
TW_STME.z_fa3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,3):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('TW_STME.z_fa3')
print(summary(TW_STME.z_fa3.asr)$call)
print('AIC')
print(summary(TW_STME.z_fa3.asr)$aic)
print(paste('convergence =',TW_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
TW_STME.z_fa3.asr <- update(TW_STME.z_fa3.asr)

# Print model info
print('TW_STME.z_fa3-update1')
print(summary(TW_STME.z_fa3.asr)$call)
print('AIC')
print(summary(TW_STME.z_fa3.asr)$aic)
print(paste('convergence =',TW_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
TW_STME.z_fa3.asr <- update(TW_STME.z_fa3.asr)

# Print model info
print('TW_STME.z_fa3-update2')
print(summary(TW_STME.z_fa3.asr)$call)
print('AIC')
print(summary(TW_STME.z_fa3.asr)$aic)
print(paste('convergence =',TW_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

## HD ----
### fa1----
# Run model
HD_STME.z_fa1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,1):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HD_STME.z_fa1')
print(summary(HD_STME.z_fa1.asr)$call)
print('AIC')
print(summary(HD_STME.z_fa1.asr)$aic)
print(paste('convergence =',HD_STME.z_fa1.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa2----
# Run model
HD_STME.z_fa2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HD_STME.z_fa2')
print(summary(HD_STME.z_fa2.asr)$call)
print('AIC')
print(summary(HD_STME.z_fa2.asr)$aic)
print(paste('convergence =',HD_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
HD_STME.z_fa2.asr <- update(HD_STME.z_fa2.asr)

# Print model info
print('HD_STME.z_fa2-update1')
print(summary(HD_STME.z_fa2.asr)$call)
print('AIC')
print(summary(HD_STME.z_fa2.asr)$aic)
print(paste('convergence =',HD_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa3----
# Run model
HD_STME.z_fa3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,3):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('HD_STME.z_fa3')
print(summary(HD_STME.z_fa3.asr)$call)
print('AIC')
print(summary(HD_STME.z_fa3.asr)$aic)
print(paste('convergence =',HD_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
HD_STME.z_fa3.asr <- update(HD_STME.z_fa3.asr)

# Print model info
print('HD_STME.z_fa3-update1')
print(summary(HD_STME.z_fa3.asr)$call)
print('AIC')
print(summary(HD_STME.z_fa3.asr)$aic)
print(paste('convergence =',HD_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

## HT ----
### fa1----
# Run model
HT_STME.z_fa1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,1):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HT_STME.z_fa1')
print(summary(HT_STME.z_fa1.asr)$call)
print('AIC')
print(summary(HT_STME.z_fa1.asr)$aic)
print(paste('convergence =',HT_STME.z_fa1.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa2----
# Run model
HT_STME.z_fa2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HT_STME.z_fa2')
print(summary(HT_STME.z_fa2.asr)$call)
print('AIC')
print(summary(HT_STME.z_fa2.asr)$aic)
print(paste('convergence =',HT_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
HT_STME.z_fa2.asr <- update(HT_STME.z_fa2.asr)

# Print model info
print('HT_STME.z_fa2-update1')
print(summary(HT_STME.z_fa2.asr)$call)
print('AIC')
print(summary(HT_STME.z_fa2.asr)$aic)
print(paste('convergence =',HT_STME.z_fa2.asr$converge))

save.image('Data/STME.z_mod.RData')

### fa3----
# Run model
HT_STME.z_fa3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,3):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('HT_STME.z_fa3')
print(summary(HT_STME.z_fa3.asr)$call)
print('AIC')
print(summary(HT_STME.z_fa3.asr)$aic)
print(paste('convergence =',HT_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

# Update model
HT_STME.z_fa3.asr <- update(HT_STME.z_fa3.asr)

# Print model info
print('HT_STME.z_fa3-update1')
print(summary(HT_STME.z_fa3.asr)$call)
print('AIC')
print(summary(HT_STME.z_fa3.asr)$aic)
print(paste('convergence =',HT_STME.z_fa3.asr$converge))

save.image('Data/STME.z_mod.RData')

## MAT ----
### fa1----
# Run model
MAT_STME.z_fa1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,1):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'MAT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('MAT_STME.z_fa1')
print(summary(MAT_STME.z_fa1.asr)$call)
print('AIC')
print(summary(MAT_STME.z_fa1.asr)$aic)
print(paste('convergence =',MAT_STME.z_fa1.asr$converge))

save.image('Data/STME.z_mod.RData')

# # End ----
# 
# rm(list = objects())
# 
# load('Data/STME.z_mod.RData')
# 
# Model selection ----
# # GY
# lrt.asreml(GY_STME.z_fa1.asr, GY_STME.z_fa2.asr, GY_STME.z_fa3.asr)
# summary(GY_STME.z_fa1.asr)$aic; summary(GY_STME.z_fa2.asr)$aic; summary(GY_STME.z_fa3.asr)$aic
# # fa3
# 
# # TW
# lrt.asreml(TW_STME.z_fa1.asr, TW_STME.z_fa2.asr, TW_STME.z_fa3.asr)
# summary(TW_STME.z_fa1.asr)$aic; summary(TW_STME.z_fa2.asr)$aic; summary(TW_STME.z_fa3.asr)$aic
# # fa3
# 
# # HD
# lrt.asreml(HD_STME.z_fa1.asr, HD_STME.z_fa2.asr, HD_STME.z_fa3.asr)
# summary(HD_STME.z_fa1.asr)$aic; summary(HD_STME.z_fa2.asr)$aic; summary(HD_STME.z_fa3.asr)$aic
# # fa2
# 
# lrt.asreml(HT_STME.z_fa1.asr, HT_STME.z_fa2.asr, HT_STME.z_fa3.asr)
# summary(HT_STME.z_fa1.asr)$aic; summary(HT_STME.z_fa2.asr)$aic; summary(HT_STME.z_fa3.asr)$aic
# # fa2
# 
# MAT
# Only FA1 fit
# lrt.asreml(MAT_STME.z_fa1.asr, MAT_STME.z_fa2.asr, MAT_STME.z_fa3.asr)
# summary(MAT_STME.z_fa1.asr)$aic; summary(MAT_STME.z_fa2.asr)$aic; summary(MAT_STME.z_fa3.asr)$aic

# # GEBVs ----
# 
# load('Data/STME.z_mod.RData')