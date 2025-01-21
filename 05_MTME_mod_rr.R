# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

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
# # Fit models ----
# ## RR2() ----
# 
# # Track time
# MTME_RR2_start.time <- Sys.time()
# print(MTME_RR2_start.time)
# 
# ILYT_Pheno |>
#   glimpse()
# 
# ### Run model ----
# MTME_RR2.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
#   residual = ~ dsum(~ ar1v(Col):ar1(Row) | TraitEnv),
#   sparse = ~ TraitEnv:Gdrop,
#   data = ILYT_Pheno,
#   na.action = na.method(x = "include"),
#   maxit = 20,
#   workspace = '88gb'
# )
# 
# 
# SReml Version 4.2 12/01/2025 15:08:52
# Multi-section model using the sigma parameterization.
# LogLik        Sigma2     DF     wall
# 1      8880.623           1.0  38450   17:03:07  (  1 restrained)
# 2      11003.73           1.0  38450   17:16:48
# 3      12955.50           1.0  38450   17:30:08  (  1 restrained)
# 4      14073.48           1.0  38450   17:43:30  (  1 restrained)
# 5      14526.57           1.0  38450   17:56:51  (  1 restrained)
# 6      14651.52           1.0  38450   18:10:14  (  1 restrained)
# 7      14663.68           1.0  38450   18:23:38  (  1 restrained)
# 8      14630.02           1.0  38450   18:37:00  (  1 restrained)
# 9      14601.63           1.0  38450   18:50:18
# 10      14589.13           1.0  38450   19:03:34
# 11      14581.43           1.0  38450   19:16:48
# 12      14575.96           1.0  38450   19:30:01
# 13      14571.79           1.0  38450   19:43:20
# 14      14568.50           1.0  38450   19:56:36
# 15      14565.88           1.0  38450   20:09:49
# 16      14563.77           1.0  38450   20:23:03
# 17      14562.07           1.0  38450   20:36:16
# 18      14560.70           1.0  38450   20:49:29
# 19      14559.60           1.0  38450   21:02:42
# 20      14560.05           1.0  38450   21:15:55
# [1] "2025-01-14 00:37:07 CST"
# Time difference of 1.394645 days
# [1] "MTME-RR2"
# asreml(fixed = Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep, 
# Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse), sparse = ~TraitEnv:Gdrop, 
# residual = ~dsum(~ar1v(Col):ar1(Row) | TraitEnv), na.action = na.method(x = "include"), 
# data = ILYT_Pheno, maxit = 20, workspace = "88gb")
# [1] "AIC"
# [1] -28654.1
# attr(,"parameters")
# [1] 233
# (Intercept)                               TraitEnv 
# 1                                     38 
# rr(TraitEnv, 2):vm(Gkeep, Ginv.sparse)        TraitEnv:vm(Gkeep, Ginv.sparse) 
# 5956                                 116142 
# mv                         TraitEnv:Gdrop 
# 115                                    188 
# [1] "convergence = FALSE"
# Warning messages:
#   1: In asreml(Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep,  :
# Log-likelihood not converged
# 2: In asreml(Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep,  :
# Some components changed by more than 1% on the last iteration
# # Time to run
# MTME_RR2_time <- Sys.time() - MTME_RR2_start.time
# print(Sys.time())
# print(MTME_RR2_time)
# 
# # Print model info
# print('MTME-RR2')
# print(summary(MTME_RR2.asr)$call)
# print('AIC')
# print(summary(MTME_RR2.asr)$aic)
# print(MTME_RR2.asr$noeff)
# print(paste('convergence =', MTME_RR2.asr$converge))
# 
# save.image('Data/MTME_mod_rr.RData')

# #### Update model ----
# load('Data/MTME_mod_rr.RData')
# MTME_RR2.asr <- update(MTME_RR2.asr)
# # Print model info
# print('MTME-RR2 - Update 1')
# print(summary(MTME_RR2.asr)$call)
# print('AIC')
# print(summary(MTME_RR2.asr)$aic)
# print(MTME_RR2.asr$noeff)
# print(paste('convergence =', MTME_RR2.asr$converge))
# save.image('Data/MTME_mod_rr.RData')

# ASReml Version 4.2 14/01/2025 13:00:56
# Multi-section model using the sigma parameterization.
# LogLik        Sigma2     DF     wall
# 1      14560.55           1.0  38450   14:57:13
# 2      14560.44           1.0  38450   15:10:42
# 3      14558.30           1.0  38450   15:24:06
# 4      14557.28           1.0  38450   15:37:24
# 5      14556.78           1.0  38450   15:50:43
# 6      14556.50           1.0  38450   16:03:55
# 7      14556.29           1.0  38450   16:17:08
# 8      14556.26           1.0  38450   16:30:28
# 9      14556.50           1.0  38450   16:43:43
# 10      14557.29           1.0  38450   16:56:56
# 11      14557.33           1.0  38450   17:10:10
# 12      14557.61           1.0  38450   17:23:22
# 13      14558.24           1.0  38450   17:36:35
# 14      14559.08           1.0  38450   17:49:48
# 15      14559.93           1.0  38450   18:03:01
# 16      14560.69           1.0  38450   18:16:14
# 17      14561.33           1.0  38450   18:29:28
# 18      14561.86           1.0  38450   18:42:38
# 19      14562.28           1.0  38450   18:55:49
# 20      14562.63           1.0  38450   19:08:57
# [1] "MTME-RR2 - Update 1"
# asreml(fixed = Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep, 
#                                                                 Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse), sparse = ~TraitEnv:Gdrop, 
#        residual = ~dsum(~ar1v(Col):ar1(Row) | TraitEnv), G.param = MTME_RR2.asr$G.param, 
#        R.param = MTME_RR2.asr$R.param, na.action = na.method(x = "include"), 
#        data = ILYT_Pheno, maxit = 20, workspace = "88gb")
# [1] "AIC"
# [1] -28659.26
# attr(,"parameters")
# [1] 233
# (Intercept)                               TraitEnv 
# 1                                     38 
# rr(TraitEnv, 2):vm(Gkeep, Ginv.sparse)        TraitEnv:vm(Gkeep, Ginv.sparse) 
# 5956                                 116142 
# mv                         TraitEnv:Gdrop 
# 115                                    188 
# [1] "convergence = FALSE"

#### Update model ----
# load('Data/MTME_mod_rr.RData')
# MTME_RR2.asr <- update(MTME_RR2.asr)
# # Print model info
# print('MTME-RR2 - Update 2')
# print(summary(MTME_RR2.asr)$call)
# print('AIC')
# print(summary(MTME_RR2.asr)$aic)
# print(MTME_RR2.asr$noeff)
# print(paste('convergence =', MTME_RR2.asr$converge))
# save.image('Data/MTME_mod_rr.RData')

# ASReml Version 4.2 16/01/2025 03:55:06
# Multi-section model using the sigma parameterization.
# LogLik        Sigma2     DF     wall
# 1      14562.91           1.0  38450   05:55:09
# 2      14562.98           1.0  38450   06:08:53
# 3      14563.20           1.0  38450   06:22:23
# 4      14563.37           1.0  38450   06:35:47
# 5      14563.51           1.0  38450   06:49:09
# 6      14563.62           1.0  38450   07:02:37
# 7      14563.71           1.0  38450   07:16:03
# 8      14563.79           1.0  38450   07:29:19
# 9      14563.85           1.0  38450   07:42:38
# 10      14563.91           1.0  38450   07:55:55
# 11      14563.95           1.0  38450   08:09:15
# 12      14563.99           1.0  38450   08:22:38
# 13      14564.02           1.0  38450   08:35:54
# 14      14564.04           1.0  38450   08:49:06
# [1] "MTME-RR2 - Update 2"
# asreml(fixed = Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep, 
#        Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse), sparse = ~TraitEnv:Gdrop, 
#        residual = ~dsum(~ar1v(Col):ar1(Row) | TraitEnv), G.param = MTME_RR2.asr$G.param, 
#        R.param = MTME_RR2.asr$R.param, na.action = na.method(x = "include"), 
#        data = ILYT_Pheno, maxit = 20, workspace = "88gb")
# [1] "AIC"
# [1] -28662.09
# attr(,"parameters")
# [1] 233
# (Intercept)                               TraitEnv 
# 1                                     38 
# rr(TraitEnv, 2):vm(Gkeep, Ginv.sparse)        TraitEnv:vm(Gkeep, Ginv.sparse) 
# 5956                                 116142 
# mv                         TraitEnv:Gdrop 
# 115                                    188 
# [1] "convergence = TRUE"
# Warning message:
# In asreml(fixed = Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep,  :
# Some components changed by more than 1% on the last iteration

#### Update model ----
load('Data/MTME_mod_rr.RData')
MTME_RR2.asr <- update(MTME_RR2.asr)
# Print model info
print('MTME-RR2 - Update 3')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(MTME_RR2.asr$noeff)
print(paste('convergence =', MTME_RR2.asr$converge))
save.image('Data/MTME_mod_rr.RData')

# ASReml Version 4.2 17/01/2025 19:02:52
# Multi-section model using the sigma parameterization.
# LogLik        Sigma2     DF     wall
# 1      14564.07           1.0  38450   20:58:54
# 2      14564.07           1.0  38450   21:12:18
# 3      14564.09           1.0  38450   21:25:31
# 4      14564.10           1.0  38450   21:38:45
# 5      14564.11           1.0  38450   21:51:59
# 6      14564.12           1.0  38450   22:05:08
# [1] "MTME-RR2 - Update 3"
# asreml(fixed = Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep, 
#        Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse), sparse = ~TraitEnv:Gdrop, 
#        residual = ~dsum(~ar1v(Col):ar1(Row) | TraitEnv), G.param = MTME_RR2.asr$G.param, 
#        R.param = MTME_RR2.asr$R.param, na.action = na.method(x = "include"), 
#        data = ILYT_Pheno, maxit = 20, workspace = "88gb")
# [1] "AIC"
# [1] -28662.24
# attr(,"parameters")
# [1] 233
# (Intercept)                               TraitEnv 
# 1                                     38 
# rr(TraitEnv, 2):vm(Gkeep, Ginv.sparse)        TraitEnv:vm(Gkeep, Ginv.sparse) 
# 5956                                 116142 
# mv                         TraitEnv:Gdrop 
# 115                                    188 
# [1] "convergence = TRUE"

# # Predict
load('Data/MTME_mod_rr.RData')

MTME_RR2.pred <- predict(MTME_RR2.asr, classify='Gkeep:TraitEnv', pworkspace='8gb')$pvals

save.image('Data/MTME_mod_rr.RData')

# *** caught segfault ***
#   address 0x71a3dbf01000, cause 'memory not mapped'
# 
# Traceback:
#   1: doTryCatch(return(expr), name, parentenv, handler)
# 2: tryCatchOne(expr, names, parentenv, handlers[[1L]])
# 3: tryCatchList(expr, classes, parentenv, handlers)
# 4: tryCatch(expr, error = function(e) e)
# 5: withCallingHandlers(tryCatch(expr, error = function(e) e), warning = w.handler)
# 6: vsn.tryCatch.W.E(expr)
# 7: vsn.tryCatch.W.E.rethrow(.Call(.NAME, ..., PACKAGE = this_info$dll_name),     call. = this_call)
# 8: vs_Call("vs_main", data, list(info = ginverse, obj = asr_grm(ginverse,     data = data), mef = mkr(mef)), R.param, G.param, predict,     options, CORE_VERSION = ddd$core_version)
# 9: asreml(fixed = Pheno_z ~ TraitEnv, random = ~rr(TraitEnv, 2):vm(Gkeep,     Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse), sparse = ~TraitEnv:Gdrop,     residual = ~dsum(~ar1v(Col):ar1(Row) | TraitEnv), G.param = MTME_RR2.asr$G.param,     R.param = MTME_RR2.asr$R.param, na.action = na.method(x = "include"),     data = ILYT_Pheno, maxit = 20, workspace = "88gb", predict = list(        classify = c("Gkeep", "TraitEnv"), levels = list(), present = character(0),         ignore = "(Intercept)", use = character(0), except = character(0),         only = character(0), associate = list(), margin = FALSE,         average = list(), as.average = list(), vcov = FALSE,         sed = FALSE, parallel = FALSE, inrandom = TRUE, exrandom = FALSE,         aliased = FALSE, estimable = FALSE, design.points = list()))
# 10: eval(newcall, parent.frame())
# 11: eval(newcall, parent.frame())
# 12: predict.asreml(MTME_RR2.asr, classify = "Gkeep:TraitEnv", ignore = c("(Intercept)"))
# 13: predict(MTME_RR2.asr, classify = "Gkeep:TraitEnv", ignore = c("(Intercept)"))
# 14: eval(statements[[idx]], envir = globalenv())
# 15: eval(statements[[idx]], envir = globalenv())
# 16: .rs.sourceWithProgress(script = "/home/lucasb4/Documents/MTME-SI-ILWheat/05_MTME_mod_rr.R",     encoding = "UTF-8", con = stdout(), importRdata = NULL, exportRdata = NULL)
# An irrecoverable exception occurred. R is aborting now ...

## RR3() ----

# # Track time
# MTME_RR3_start.time <- Sys.time()
# print(MTME_RR3_start.time)
# 
# ILYT_Pheno |>
#   glimpse()
# 
# ### Run model ----
# MTME_RR3.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ rr(TraitEnv,3):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
#   residual = ~ dsum(~ ar1v(Col):ar1(Row) | TraitEnv),
#   sparse = ~ TraitEnv:Gdrop,
#   data = ILYT_Pheno,
#   na.action = na.method(x = "include"),
#   maxit = 20,
#   workspace = '88gb'
# )
# 
# # Time to run
# MTME_RR3_time <- Sys.time() - MTME_RR3_start.time
# print(Sys.time())
# print(MTME_RR3_time)
# 
# # Print model info
# print('MTME-RR3')
# print(summary(MTME_RR3.asr)$call)
# print('AIC')
# print(summary(MTME_RR3.asr)$aic)
# print(MTME_RR3.asr$noeff)
# print(paste('convergence =', MTME_RR3.asr$converge))
# 
# save.image('Data/MTME_mod_rr.RData')

