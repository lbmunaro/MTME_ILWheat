# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

# Objective ----
# - 

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# # Load data ----
# ## Pheno & Ginv
# load('Data/ILYT_Pheno-Gmatrix.RData')
# 
# # Model 2 ----
# 
# STSE_2.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
#     at(TraitEnv):ar1v(Col):ar1(Row),
#   residual = ~ dsum(~ units | TraitEnv),
#   sparse = ~ TraitEnv:Gdrop,
#   data = ILYT_Pheno,
#   na.action = na.method(x = "include"),
#   maxit = 30,
#   workspace = '12gb'
# )
# print('STSE_2')
# print(summary(STSE_2.asr)$call)
# STSE_2.asr$loglik
# print('AIC')
# print(summary(STSE_2.asr)$aic)
# print(paste('convergence =',STSE_2.asr$converge))
# 
# ## Save data ----
# save.image('Data/STSE_mod2.RData')

## Load
load('Data/STSE_mod2.RData')

STSE_2.asr <- update(STSE_2.asr)

print('STSE_2 - Update 1')
print(summary(STSE_2.asr)$call)
STSE_2.asr$loglik
print('AIC')
print(summary(STSE_2.asr)$aic)
print(paste('convergence =',STSE_2.asr$converge))
save.image('Data/STSE_mod2.RData')