# Single-Trait Multi-Environment models ----
# This script fits Single-Trait Multi-Environment models

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

## GY ----
### fa1----

# Track time
GY_STME_fa1_start.time <- Sys.time()
print(GY_STME_fa1_start.time)

# Run model
GY_STME_fa1.asr <- asreml(
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
print('GY_STME_fa1')
print(summary(GY_STME_fa1.asr)$call)
print('AIC')
print(summary(GY_STME_fa1.asr)$aic)
print(GY_STME_fa1.asr$noeff)
print(paste('convergence =',GY_STME_fa1.asr$converge))

# Time to run
GY_STME_fa1_time <- Sys.time() - GY_STME_fa1_start.time
print(Sys.time())
print(GY_STME_fa1_time)

save.image('Data/STME_mod.RData')

### fa2----

# Track time
GY_STME_fa2_start.time <- Sys.time()
print(GY_STME_fa2_start.time)

# Run model
GY_STME_fa2.asr <- asreml(
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
print('GY_STME_fa2')
print(summary(GY_STME_fa2.asr)$call)
print('AIC')
print(summary(GY_STME_fa2.asr)$aic)
print(GY_STME_fa2.asr$noeff)
print(paste('convergence =',GY_STME_fa2.asr$converge))

# Time to run
GY_STME_fa2_time <- Sys.time() - GY_STME_fa2_start.time
print(Sys.time())
print(GY_STME_fa2_time)

save.image('Data/STME_mod.RData')

# Update model
GY_STME_fa2.asr <- update(GY_STME_fa2.asr)

# Print model info
print('GY_STME_fa2-update1')
print(summary(GY_STME_fa2.asr)$call)
print('AIC')
print(summary(GY_STME_fa2.asr)$aic)
print(GY_STME_fa2.asr$noeff)
print(paste('convergence =',GY_STME_fa2.asr$converge))

save.image('Data/STME_mod.RData')

### fa3----

#load('Data/STME_mod.RData')
# Track time
GY_STME_fa3_start.time <- Sys.time()
print(GY_STME_fa3_start.time)

# Run model
GY_STME_fa3.asr <- asreml(
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
print('GY_STME_fa3')
print(summary(GY_STME_fa3.asr)$call)
print('AIC')
print(summary(GY_STME_fa3.asr)$aic)
print(GY_STME_fa3.asr$noeff)
print(paste('convergence =',GY_STME_fa3.asr$converge))

# Time to run
GY_STME_fa3_time <- Sys.time() - GY_STME_fa3_start.time
print(Sys.time())
print(GY_STME_fa3_time)

save.image('Data/STME_mod.RData')

# Update model
GY_STME_fa3.asr <- update(GY_STME_fa3.asr)

# Print model info
print('GY_STME_fa3-update1')
print(summary(GY_STME_fa3.asr)$call)
print('AIC')
print(summary(GY_STME_fa3.asr)$aic)
print(GY_STME_fa3.asr$noeff)
print(paste('convergence =',GY_STME_fa3.asr$converge))

save.image('Data/STME_mod.RData')

load('Data/STME_mod.RData')

## TW ----
### fa1----

load('Data/STME_mod.RData')

# Track time
TW_STME_fa1_start.time <- Sys.time()
print(TW_STME_fa1_start.time)

# Run model
TW_STME_fa1.asr <- asreml(
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
print('TW_STME_fa1')
print(summary(TW_STME_fa1.asr)$call)
print('AIC')
print(summary(TW_STME_fa1.asr)$aic)
print(TW_STME_fa1.asr$noeff)
print(paste('convergence =',TW_STME_fa1.asr$converge))

# Time to run
TW_STME_fa1_time <- Sys.time() - TW_STME_fa1_start.time
print(Sys.time())
print(TW_STME_fa1_time)

save.image('Data/STME_mod.RData')

### fa2----

# Track time
TW_STME_fa2_start.time <- Sys.time()
print(TW_STME_fa2_start.time)

# Run model
TW_STME_fa2.asr <- asreml(
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
print('TW_STME_fa2')
print(summary(TW_STME_fa2.asr)$call)
print('AIC')
print(summary(TW_STME_fa2.asr)$aic)
print(TW_STME_fa2.asr$noeff)
print(paste('convergence =',TW_STME_fa2.asr$converge))

# Time to run
TW_STME_fa2_time <- Sys.time() - TW_STME_fa2_start.time
print(Sys.time())
print(TW_STME_fa2_time)

save.image('Data/STME_mod.RData')

# Update model
TW_STME_fa2.asr <- update(TW_STME_fa2.asr)

# Print model info
print('TW_STME_fa2-update1')
print(summary(TW_STME_fa2.asr)$call)
print('AIC')
print(summary(TW_STME_fa2.asr)$aic)
print(TW_STME_fa2.asr$noeff)
print(paste('convergence =',TW_STME_fa2.asr$converge))

save.image('Data/STME_mod.RData')

### fa3----

# Track time
TW_STME_fa3_start.time <- Sys.time()
print(TW_STME_fa3_start.time)

# Run model
TW_STME_fa3.asr <- asreml(
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
print('TW_STME_fa3')
print(summary(TW_STME_fa3.asr)$call)
print('AIC')
print(summary(TW_STME_fa3.asr)$aic)
print(TW_STME_fa3.asr$noeff)
print(paste('convergence =',TW_STME_fa3.asr$converge))

# Time to run
TW_STME_fa3_time <- Sys.time() - TW_STME_fa3_start.time
print(Sys.time())
print(TW_STME_fa3_time)

save.image('Data/STME_mod.RData')

# Update model
TW_STME_fa3.asr <- update(TW_STME_fa3.asr)

# Print model info
print('TW_STME_fa3-update1')
print(summary(TW_STME_fa3.asr)$call)
print('AIC')
print(summary(TW_STME_fa3.asr)$aic)
print(TW_STME_fa3.asr$noeff)
print(paste('convergence =',TW_STME_fa3.asr$converge))

save.image('Data/STME_mod.RData')

# Update model
TW_STME_fa3.asr <- update(TW_STME_fa3.asr)

# Print model info
print('TW_STME_fa3-update2')
print(summary(TW_STME_fa3.asr)$call)
print('AIC')
print(summary(TW_STME_fa3.asr)$aic)
print(TW_STME_fa3.asr$noeff)
print(paste('convergence =',TW_STME_fa3.asr$converge))

save.image('Data/STME_mod.RData')

## HD ----
### fa1----

# Track time
HD_STME_fa1_start.time <- Sys.time()
print(HD_STME_fa1_start.time)

# Run model
HD_STME_fa1.asr <- asreml(
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
print('HD_STME_fa1')
print(summary(HD_STME_fa1.asr)$call)
print('AIC')
print(summary(HD_STME_fa1.asr)$aic)
print(HD_STME_fa1.asr$noeff)
print(paste('convergence =',HD_STME_fa1.asr$converge))

# Time to run
HD_STME_fa1_time <- Sys.time() - HD_STME_fa1_start.time
print(Sys.time())
print(HD_STME_fa1_time)

save.image('Data/STME_mod.RData')

### fa2----

# Track time
HD_STME_fa2_start.time <- Sys.time()
print(HD_STME_fa2_start.time)

# Run model
HD_STME_fa2.asr <- asreml(
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
print('HD_STME_fa2')
print(summary(HD_STME_fa2.asr)$call)
print('AIC')
print(summary(HD_STME_fa2.asr)$aic)
print(HD_STME_fa2.asr$noeff)
print(paste('convergence =',HD_STME_fa2.asr$converge))

# Time to run
HD_STME_fa2_time <- Sys.time() - HD_STME_fa2_start.time
print(Sys.time())
print(HD_STME_fa2_time)

save.image('Data/STME_mod.RData')

# Update model
HD_STME_fa2.asr <- update(HD_STME_fa2.asr)

# Print model info
print('HD_STME_fa2-update1')
print(summary(HD_STME_fa2.asr)$call)
print('AIC')
print(summary(HD_STME_fa2.asr)$aic)
print(HD_STME_fa2.asr$noeff)
print(paste('convergence =',HD_STME_fa2.asr$converge))

save.image('Data/STME_mod.RData')

### fa3----

# Track time
HD_STME_fa3_start.time <- Sys.time()
print(HD_STME_fa3_start.time)

# Run model
HD_STME_fa3.asr <- asreml(
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
print('HD_STME_fa3')
print(summary(HD_STME_fa3.asr)$call)
print('AIC')
print(summary(HD_STME_fa3.asr)$aic)
print(HD_STME_fa3.asr$noeff)
print(paste('convergence =',HD_STME_fa3.asr$converge))

# Time to run
HD_STME_fa3_time <- Sys.time() - HD_STME_fa3_start.time
print(Sys.time())
print(HD_STME_fa3_time)

save.image('Data/STME_mod.RData')

# Update model
HD_STME_fa3.asr <- update(HD_STME_fa3.asr)

# Print model info
print('HD_STME_fa3-update1')
print(summary(HD_STME_fa3.asr)$call)
print('AIC')
print(summary(HD_STME_fa3.asr)$aic)
print(HD_STME_fa3.asr$noeff)
print(paste('convergence =',HD_STME_fa3.asr$converge))

save.image('Data/STME_mod.RData')

## HT ----
### fa1----

# Track time
HT_STME_fa1_start.time <- Sys.time()
print(HT_STME_fa1_start.time)

# Run model
HT_STME_fa1.asr <- asreml(
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
print('HT_STME_fa1')
print(summary(HT_STME_fa1.asr)$call)
print('AIC')
print(summary(HT_STME_fa1.asr)$aic)
print(HT_STME_fa1.asr$noeff)
print(paste('convergence =',HT_STME_fa1.asr$converge))

# Time to run
HT_STME_fa1_time <- Sys.time() - HT_STME_fa1_start.time
print(Sys.time())
print(HT_STME_fa1_time)

save.image('Data/STME_mod.RData')

### fa2----

# Track time
HT_STME_fa2_start.time <- Sys.time()
print(HT_STME_fa2_start.time)

# Run model
HT_STME_fa2.asr <- asreml(
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
print('HT_STME_fa2')
print(summary(HT_STME_fa2.asr)$call)
print('AIC')
print(summary(HT_STME_fa2.asr)$aic)
print(HT_STME_fa2.asr$noeff)
print(paste('convergence =',HT_STME_fa2.asr$converge))

# Time to run
HT_STME_fa2_time <- Sys.time() - HT_STME_fa2_start.time
print(Sys.time())
print(HT_STME_fa2_time)

save.image('Data/STME_mod.RData')

# Update model
HT_STME_fa2.asr <- update(HT_STME_fa2.asr)

# Print model info
print('HT_STME_fa2-update1')
print(summary(HT_STME_fa2.asr)$call)
print('AIC')
print(summary(HT_STME_fa2.asr)$aic)
print(HT_STME_fa2.asr$noeff)
print(paste('convergence =',HT_STME_fa2.asr$converge))

save.image('Data/STME_mod.RData')

### fa3----

# Track time
HT_STME_fa3_start.time <- Sys.time()
print(HT_STME_fa3_start.time)

# Run model
HT_STME_fa3.asr <- asreml(
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
print('HT_STME_fa3')
print(summary(HT_STME_fa3.asr)$call)
print('AIC')
print(summary(HT_STME_fa3.asr)$aic)
print(HT_STME_fa3.asr$noeff)
print(paste('convergence =',HT_STME_fa3.asr$converge))

# Time to run
HT_STME_fa3_time <- Sys.time() - HT_STME_fa3_start.time
print(Sys.time())
print(HT_STME_fa3_time)

save.image('Data/STME_mod.RData')

# Update model
HT_STME_fa3.asr <- update(HT_STME_fa3.asr)

# Print model info
print('HT_STME_fa3-update1')
print(summary(HT_STME_fa3.asr)$call)
print('AIC')
print(summary(HT_STME_fa3.asr)$aic)
print(HT_STME_fa3.asr$noeff)
print(paste('convergence =',HT_STME_fa3.asr$converge))

save.image('Data/STME_mod.RData')

## MAT ----
### fa1----

# Track time
MAT_STME_fa1_start.time <- Sys.time()
print(MAT_STME_fa1_start.time)

# Run model
MAT_STME_fa1.asr <- asreml(
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
print('MAT_STME_fa1')
print(summary(MAT_STME_fa1.asr)$call)
print('AIC')
print(summary(MAT_STME_fa1.asr)$aic)
print(MAT_STME_fa1.asr$noeff)
print(paste('convergence =',MAT_STME_fa1.asr$converge))

# Time to run
MAT_STME_fa1_time <- Sys.time() - MAT_STME_fa1_start.time
print(Sys.time())
print(MAT_STME_fa1_time)

save.image('Data/STME_mod.RData')

### fa2----

# Track time
MAT_STME_fa2_start.time <- Sys.time()
print(MAT_STME_fa2_start.time)

# Run model
MAT_STME_fa2.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'MAT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '16gb'
)

# ASReml Version 4.2 21/11/2024 16:18:23
# Multi-section model using the sigma parameterization.
# LogLik        Sigma2     DF     wall
# 1     -394.2489           1.0   2411   16:20:44
# Error in asreml(Pheno_z ~ Env, random = ~fa(Env, 2):vm(Gkeep, Ginv.sparse),  :
# Error   : 3 singularities in the Average Information matrix.
# : To continue the analysis anyway, try running it again with the optional argument ai.sing = TRUE
# Calls: .rs.sourceWithProgress ... eval -> asreml -> vs_Call -> vsn.tryCatch.W.E.rethrow
# Execution halted

# Print model info
print('MAT_STME_fa2')
print(summary(MAT_STME_fa2.asr)$call)
print('AIC')
print(summary(MAT_STME_fa2.asr)$aic)
print(MAT_STME_fa2.asr$noeff)
print(paste('convergence =',MAT_STME_fa2.asr$converge))

# Time to run
MAT_STME_fa2_time <- Sys.time() - MAT_STME_fa2_start.time
print(Sys.time())
print(MAT_STME_fa2_time)

save.image('Data/STME_mod.RData')

# Update model
MAT_STME_fa2.asr <- update(MAT_STME_fa2.asr)

# Print model info
print('MAT_STME_fa2-update1')
print(summary(MAT_STME_fa2.asr)$call)
print('AIC')
print(summary(MAT_STME_fa2.asr)$aic)
print(MAT_STME_fa2.asr$noeff)
print(paste('convergence =',MAT_STME_fa2.asr$converge))

save.image('Data/STME_mod.RData')

### fa3----
load('Data/STME_mod.RData')
# Track time
MAT_STME_fa3_start.time <- Sys.time()
print(MAT_STME_fa3_start.time)

# Run model
MAT_STME_fa3.asr <- asreml(
  Pheno_z ~ Env,
  random = ~ fa(Env,3):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
  sparse = ~ Env:Gdrop,
  data = ILYT_Pheno |> filter(Trait == 'MAT') |> droplevels(),
  na.action = na.method(x = "include"),
  maxit = 20, ai.sing = TRUE,
  workspace = '20gb'
)
# Print model info
print('MAT_STME_fa3')
print(summary(MAT_STME_fa3.asr)$call)
print('AIC')
print(summary(MAT_STME_fa3.asr)$aic)
print(MAT_STME_fa3.asr$noeff)
print(paste('convergence =',MAT_STME_fa3.asr$converge))

# Time to run
MAT_STME_fa3_time <- Sys.time() - MAT_STME_fa3_start.time
print(Sys.time())
print(MAT_STME_fa3_time)

save.image('Data/STME_mod.RData')

# Update model
MAT_STME_fa3.asr <- update(MAT_STME_fa3.asr)

# Print model info
print('MAT_STME_fa3-update1')
print(summary(MAT_STME_fa3.asr)$call)
print('AIC')
print(summary(MAT_STME_fa3.asr)$aic)
print(MAT_STME_fa3.asr$noeff)
print(paste('convergence =',MAT_STME_fa3.asr$converge))

save.image('Data/STME_mod.RData')

# # End ----
# 
# rm(list = objects())
# 
# load('Data/STME_mod.RData')
# 
# Model selection ----
# # GY
# lrt.asreml(GY_STME_fa1.asr, GY_STME_fa2.asr, GY_STME_fa3.asr)
# summary(GY_STME_fa1.asr)$aic; summary(GY_STME_fa2.asr)$aic; summary(GY_STME_fa3.asr)$aic
# # fa3
# 
# # TW
# lrt.asreml(TW_STME_fa1.asr, TW_STME_fa2.asr, TW_STME_fa3.asr)
# summary(TW_STME_fa1.asr)$aic; summary(TW_STME_fa2.asr)$aic; summary(TW_STME_fa3.asr)$aic
# # fa3
# 
# # HD
# lrt.asreml(HD_STME_fa1.asr, HD_STME_fa2.asr, HD_STME_fa3.asr)
# summary(HD_STME_fa1.asr)$aic; summary(HD_STME_fa2.asr)$aic; summary(HD_STME_fa3.asr)$aic
# # fa2
# 
# lrt.asreml(HT_STME_fa1.asr, HT_STME_fa2.asr, HT_STME_fa3.asr)
# summary(HT_STME_fa1.asr)$aic; summary(HT_STME_fa2.asr)$aic; summary(HT_STME_fa3.asr)$aic
# # fa2
# 
# MAT
# Only FA1 fit
# lrt.asreml(MAT_STME_fa1.asr, MAT_STME_fa2.asr, MAT_STME_fa3.asr)
# summary(MAT_STME_fa1.asr)$aic; summary(MAT_STME_fa2.asr)$aic; summary(MAT_STME_fa3.asr)$aic

# # GEBVs ----
# 
# load('Data/STME_mod.RData')
# 
# head(summary(GY_STME_fa3.asr, coef=T)$coef.random)
# 
# GY_STME_fa2_time
