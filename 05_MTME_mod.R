# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

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
## FA2() ----

# Track time
MTME_FA2_start.time <- Sys.time()
print(MTME_FA2_start.time)

ILYT_Pheno |>
  glimpse()

### Run model ----
MTME_FA2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ fa(TraitEnv,2):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '88gb'
)

# # ASReml Version 4.2 11/01/2025 21:36:52
# # Multi-section model using the sigma parameterization.
# # Move    119943 [87224] to    119940 [87221]   0.000        Infinity    Infinity
# # Move    119942 [87223] to    119939 [87220]   0.000       -Infinity    Infinity
# # Move    119928 [87209] to    119927 [87208]   0.000        Infinity    Infinity
# # Move    119921 [87202] to    119920 [87201]   0.000        Infinity  -80.08    
# # Move    119905 [87186] to    119903 [87184]   0.000        Infinity   -Infinity
# # Move    121389 [5680] to    121384 [5675]   0.000     -0.2361E+22 -0.2306E+19
# # Move    121384 [5675] to    121383 [5674]   0.000      0.3689E+20  0.1181E+22
# # Move    121247 [5538] to    121244 [5535]   0.000      0.4056E+32  0.3245E+33
# # Move    121232 [5523] to    121231 [5522]   0.000      0.4254E+38    Infinity
# # Move    120390 [4681] to    120387 [4678]   0.000        Infinity    Infinity
# # Move    120389 [4680] to    120387 [4678]   0.000       -Infinity    Infinity
# # Move    120370 [4661] to    120369 [4660]   0.000        Infinity   -Infinity
# # Move    120098 [4389] to    120097 [4388]   0.000        Infinity    Infinity
# # Move    120081 [4372] to    120080 [4371]   0.000       -Infinity   -Infinity
# # Error in asreml(Pheno_z ~ TraitEnv, random = ~fa(TraitEnv, 2):vm(Gkeep,  : 
# # Error   : Iteration failed; ifault: 2990
# # Calls: .rs.sourceWithProgress ... eval -> asreml -> vs_Call -> vsn.tryCatch.W.E.rethrow
# # In addition: Warning message:
# # In asreml(Pheno_z ~ TraitEnv, random = ~fa(TraitEnv, 2):vm(Gkeep,  :
# # Error   :  Lagrangian Equation order issue was not fixed.
# # Execution halted

# Time to run
MTME_FA2_time <- Sys.time() - MTME_FA2_start.time
print(Sys.time())
print(MTME_FA2_time)

# Print model info
print('MTME-FA2')
print(summary(MTME_FA2.asr)$call)
print('AIC')
print(summary(MTME_FA2.asr)$aic)
print(MTME_FA2.asr$noeff)
print(paste('convergence =', MTME_FA2.asr$converge))

save.image('Data/MTME_mod.RData')

# # Fit models ----
# ## FA3() ----
# 
# # Track time
# MTME_FA3_start.time <- Sys.time()
# print(MTME_FA3_start.time)
# 
# ILYT_Pheno |>
#   glimpse()
# 
# ### Run model ----
# MTME_FA3.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ fa(TraitEnv,3):vm(Gkeep, Ginv.sparse),
#   residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
#   sparse = ~ TraitEnv:Gdrop,
#   data = ILYT_Pheno,
#   na.action = na.method(x = "include"),
#   maxit = 20,
#   workspace = '88gb'
# )
# 
# ASReml Version 4.2 11/01/2025 22:33:25
# Multi-section model using the sigma parameterization.
# Move    122599 [5808] to    122596 [5805]   0.000        Infinity    Infinity
# Move    122594 [5803] to    122593 [5802]   0.000       -Infinity   -Infinity
# Move    122415 [4110] to    122413 [4108]   0.000        Infinity   -Infinity
# Move    122414 [4109] to    122413 [4108]   0.000       -Infinity   -Infinity
# Move    122411 [4105] to    122408 [4099]   0.000        Infinity   -Infinity
# Move    122410 [4101] to    122408 [4099]   0.000       -Infinity   -Infinity
# Move    122403 [4094] to    122402 [4093]   0.000       -Infinity    Infinity
# Move    122400 [4091] to    122395 [4086]   0.000       -Infinity   -Infinity
# Move    122386 [4076] to    122384 [4073]   0.000        Infinity    Infinity
# Move    122385 [4075] to    122384 [4073]   0.000       -Infinity    Infinity
# Move    122266 [3945] to    122230 [3907]   0.000        Infinity   -Infinity
# Move    122265 [3944] to      2825 [120193]   0.000        Infinity   -Infinity
# Move    122264 [3943] to    122262 [3941]   0.000       -Infinity   -Infinity
# Move    122263 [3942] to    122206 [3882]   0.000       -Infinity   -Infinity
# Move    122259 [3938] to    122258 [3937]   0.000        Infinity    Infinity
# Move    122163 [3998] to    122162 [3989]   0.000        Infinity   -Infinity
# Move    122102 [9759] to    122096 [9753]   0.000       -Infinity   -Infinity
# 2     -75439.51           1.0      0   23:27:00
# 
# # *** caught segfault ***
# #   address 0x10, cause 'memory not mapped'
# # 
# # Traceback:
# #   1: doTryCatch(return(expr), name, parentenv, handler)
# # 2: tryCatchOne(expr, names, parentenv, handlers[[1L]])
# # 3: tryCatchList(expr, classes, parentenv, handlers)
# # 4: tryCatch(expr, error = function(e) e)
# # 5: withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler)
# # 6: vsn.tryCatch.W.E(expr)
# # 7: vsn.tryCatch.W.E.rethrow(.Call(.NAME, ..., PACKAGE = this_info$dll_name),     call. = this_call)
# # 8: vs_Call("vs_main", data, list(info = ginverse, obj = asr_grm(ginverse,     data = data), mef = mkr(mef)), R.param, G.param, predict,     options, CORE_VERSION = ddd$core_version)
# # 9: asreml(Pheno_z ~ TraitEnv, random = ~fa(TraitEnv, 3):vm(Gkeep,     Ginv.sparse), residual = ~dsum(~ar1(Col):ar1(Row) | TraitEnv),     sparse = ~TraitEnv:Gdrop, data = ILYT_Pheno, na.action = na.method(x = "include"),     maxit = 20, workspace = "88gb")
# # 10: eval(statements[[idx]], envir = globalenv())
# # 11: eval(statements[[idx]], envir = globalenv())
# # 12: .rs.sourceWithProgress(script = "/home/lucasb4/Documents/MTME-SI-ILWheat/05_MTME_mod.R",     encoding = "UTF-8", con = stdout(), importRdata = NULL, exportRdata = NULL)
# # An irrecoverable exception occurred. R is aborting now ...
# 
# # Time to run
# MTME_FA3_time <- Sys.time() - MTME_FA3_start.time
# print(Sys.time())
# print(MTME_FA3_time)
# 
# # Print model info
# print('MTME-FA3')
# print(summary(MTME_FA3.asr)$call)
# print('AIC')
# print(summary(MTME_FA3.asr)$aic)
# print(MTME_FA3.asr$noeff)
# print(paste('convergence =', MTME_FA3.asr$converge))
# 
# save.image('Data/MTME_mod.RData')
load('Data/MTME_mod.RData')
