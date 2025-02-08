# Single-Trait Multi-Environment models ----
# This script fits Single-Trait Multi-Environment models

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
### rr1----
# Run model
GY_STME.z_rr1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,1):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('GY_STME.z_rr1')
print(summary(GY_STME.z_rr1.asr)$call)
print('AIC')
print(summary(GY_STME.z_rr1.asr)$aic)
print(paste('convergence =',GY_STME.z_rr1.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr2----
# Run model
GY_STME.z_rr2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,2):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('GY_STME.z_rr2')
print(summary(GY_STME.z_rr2.asr)$call)
print('AIC')
print(summary(GY_STME.z_rr2.asr)$aic)
print(paste('convergence =',GY_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
GY_STME.z_rr2.asr <- update(GY_STME.z_rr2.asr)

# Print model info
print('GY_STME.z_rr2-update1')
print(summary(GY_STME.z_rr2.asr)$call)
print('AIC')
print(summary(GY_STME.z_rr2.asr)$aic)
print(paste('convergence =',GY_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr3----
# Run model
GY_STME.z_rr3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,3):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'GY') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('GY_STME.z_rr3')
print(summary(GY_STME.z_rr3.asr)$call)
print('AIC')
print(summary(GY_STME.z_rr3.asr)$aic)
print(paste('convergence =',GY_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
GY_STME.z_rr3.asr <- update(GY_STME.z_rr3.asr)

# Print model info
print('GY_STME.z_rr3-update1')
print(summary(GY_STME.z_rr3.asr)$call)
print('AIC')
print(summary(GY_STME.z_rr3.asr)$aic)
print(paste('convergence =',GY_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

## TW ----
### rr1----
# Run model
TW_STME.z_rr1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,1):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('TW_STME.z_rr1')
print(summary(TW_STME.z_rr1.asr)$call)
print('AIC')
print(summary(TW_STME.z_rr1.asr)$aic)
print(paste('convergence =',TW_STME.z_rr1.asr$converge))

save.image('Data/STME.z_rr.RData')

load('Data/STME.z_rr.RData')
### rr2----
# Run model
TW_STME.z_rr2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,2):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('TW_STME.z_rr2')
print(summary(TW_STME.z_rr2.asr)$call)
print('AIC')
print(summary(TW_STME.z_rr2.asr)$aic)
print(paste('convergence =',TW_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
TW_STME.z_rr2.asr <- update(TW_STME.z_rr2.asr)

# Print model info
print('TW_STME.z_rr2-update1')
print(summary(TW_STME.z_rr2.asr)$call)
print('AIC')
print(summary(TW_STME.z_rr2.asr)$aic)
print(paste('convergence =',TW_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr3----
# Run model
TW_STME.z_rr3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,3):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'TW') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('TW_STME.z_rr3')
print(summary(TW_STME.z_rr3.asr)$call)
print('AIC')
print(summary(TW_STME.z_rr3.asr)$aic)
print(paste('convergence =',TW_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
TW_STME.z_rr3.asr <- update(TW_STME.z_rr3.asr)

# Print model info
print('TW_STME.z_rr3-update1')
print(summary(TW_STME.z_rr3.asr)$call)
print('AIC')
print(summary(TW_STME.z_rr3.asr)$aic)
print(paste('convergence =',TW_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
TW_STME.z_rr3.asr <- update(TW_STME.z_rr3.asr)

# Print model info
print('TW_STME.z_rr3-update2')
print(summary(TW_STME.z_rr3.asr)$call)
print('AIC')
print(summary(TW_STME.z_rr3.asr)$aic)
print(paste('convergence =',TW_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

## HD ----
### rr1----
# Run model
HD_STME.z_rr1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,1):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HD_STME.z_rr1')
print(summary(HD_STME.z_rr1.asr)$call)
print('AIC')
print(summary(HD_STME.z_rr1.asr)$aic)
print(paste('convergence =',HD_STME.z_rr1.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr2----
# Run model
HD_STME.z_rr2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,2):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HD_STME.z_rr2')
print(summary(HD_STME.z_rr2.asr)$call)
print('AIC')
print(summary(HD_STME.z_rr2.asr)$aic)
print(paste('convergence =',HD_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
HD_STME.z_rr2.asr <- update(HD_STME.z_rr2.asr)

# Print model info
print('HD_STME.z_rr2-update1')
print(summary(HD_STME.z_rr2.asr)$call)
print('AIC')
print(summary(HD_STME.z_rr2.asr)$aic)
print(paste('convergence =',HD_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr3----
# Run model
HD_STME.z_rr3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,3):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HD') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('HD_STME.z_rr3')
print(summary(HD_STME.z_rr3.asr)$call)
print('AIC')
print(summary(HD_STME.z_rr3.asr)$aic)
print(paste('convergence =',HD_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
HD_STME.z_rr3.asr <- update(HD_STME.z_rr3.asr)

# Print model info
print('HD_STME.z_rr3-update1')
print(summary(HD_STME.z_rr3.asr)$call)
print('AIC')
print(summary(HD_STME.z_rr3.asr)$aic)
print(paste('convergence =',HD_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

## HT ----
### rr1----
# Run model
HT_STME.z_rr1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,1):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HT_STME.z_rr1')
print(summary(HT_STME.z_rr1.asr)$call)
print('AIC')
print(summary(HT_STME.z_rr1.asr)$aic)
print(paste('convergence =',HT_STME.z_rr1.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr2----
# Run model
HT_STME.z_rr2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,2):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('HT_STME.z_rr2')
print(summary(HT_STME.z_rr2.asr)$call)
print('AIC')
print(summary(HT_STME.z_rr2.asr)$aic)
print(paste('convergence =',HT_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
HT_STME.z_rr2.asr <- update(HT_STME.z_rr2.asr)

# Print model info
print('HT_STME.z_rr2-update1')
print(summary(HT_STME.z_rr2.asr)$call)
print('AIC')
print(summary(HT_STME.z_rr2.asr)$aic)
print(paste('convergence =',HT_STME.z_rr2.asr$converge))

save.image('Data/STME.z_rr.RData')

### rr3----
# Run model
HT_STME.z_rr3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,3):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'HT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '20gb'
)
# Print model info
print('HT_STME.z_rr3')
print(summary(HT_STME.z_rr3.asr)$call)
print('AIC')
print(summary(HT_STME.z_rr3.asr)$aic)
print(paste('convergence =',HT_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

# Update model
HT_STME.z_rr3.asr <- update(HT_STME.z_rr3.asr)

# Print model info
print('HT_STME.z_rr3-update1')
print(summary(HT_STME.z_rr3.asr)$call)
print('AIC')
print(summary(HT_STME.z_rr3.asr)$aic)
print(paste('convergence =',HT_STME.z_rr3.asr$converge))

save.image('Data/STME.z_rr.RData')

## MAT ----
### rr1----
# Run model
MAT_STME.z_rr1.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ rr(Env,1):vm(Gkeep, Ginv.sparse) + diag(Env):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'MAT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)
# Print model info
print('MAT_STME.z_rr1')
print(summary(MAT_STME.z_rr1.asr)$call)
print('AIC')
print(summary(MAT_STME.z_rr1.asr)$aic)
print(paste('convergence =',MAT_STME.z_rr1.asr$converge))

save.image('Data/STME.z_rr.RData')

# # End ----