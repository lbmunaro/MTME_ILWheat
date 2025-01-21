# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

load('Data/ILYT_Pheno-Gmatrix.RData')
library(tidyverse)
library(asreml)

ILYT_Pheno2 <- ILYT_Pheno |>
  mutate(IDEU = paste(Env,Col,Row,sep = '-'), IDEU = as.factor(IDEU)) |>
  group_by(Env) |>
  mutate(nTrait = length(unique(Trait))) |>
  mutate(Tr2 = ifelse(nTrait==2,as.character(Trait),'GY'), Tr2 = as.factor(Tr2),
         Tr3 = ifelse(nTrait==3,as.character(Trait),'GY'), Tr3 = as.factor(Tr3),
         Tr4 = ifelse(nTrait==4,as.character(Trait),'GY'), Tr4 = as.factor(Tr4),
         Tr5 = ifelse(nTrait==5,as.character(Trait),'GY'), Tr5 = as.factor(Tr5)) |>
  ungroup() |>
  droplevels() |>
  arrange(IDEU, Trait) |>
  glimpse()

ILYT_Pheno2 |> filter(!is.na(Tr2)) |> droplevels() |> group_by(Env) |> 
  summarise(Env=unique(Env)) |> glimpse()
ILYT_Pheno2 |> filter(!is.na(Tr3)) |> droplevels() |> group_by(Env) |> 
  summarise(Env=unique(Env)) |> glimpse()
ILYT_Pheno2 |> filter(!is.na(Tr4)) |> droplevels() |> group_by(Env) |> 
  summarise(Env=unique(Env)) |> glimpse()
ILYT_Pheno2 |> filter(!is.na(Tr5)) |> droplevels() |> group_by(Env) |> 
  summarise(Env=unique(Env)) |> glimpse()

# Model ----
STSE_res_corr.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    at(Env):ar1v(Col):ar1(Row),
  residual = ~ dsum(~ IDEU:corgh(Tr2) | Env, levels = c('22-Adv','22-Stp','23-Adv',
                                                        '24-Adv','24-Stp')) +
    dsum(~ IDEU:corgh(Tr3) | Env, levels = c('23-Stp')) +
    dsum(~ IDEU:corgh(Tr4) | Env, levels = c('22-Neo','22-Urb','23-Neo','24-Neo')) +
    dsum(~ IDEU:corgh(Tr5) | Env, levels = c('23-Urb','24-Urb')),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno2,
  na.action = na.method(x = 'include'),
  maxit = 20,
  workspace = '12gb'
)

print('STSE_res_corr')
print(summary(STSE_res_corr.asr)$call)
STSE_res_corr.asr$loglik
print('AIC')
print(summary(STSE_res_corr.asr)$aic)
print(STSE_res_corr.asr$noeff)
print(paste('convergence =',STSE_res_corr.asr$converge))

save.image('Data/STSE_res_corr.RData')

# STSE_res_corr.asr <- update(STSE_res_corr.asr)
# 
# print('STSE_res_corr - Update 1')
# print(summary(STSE_res_corr.asr)$call)
# STSE_res_corr.asr$loglik
# print('AIC')
# print(summary(STSE_res_corr.asr)$aic)
# print(STSE_res_corr.asr$noeff)
# print(paste('convergence =',STSE_res_corr.asr$converge))
# 
# save.image('Data/STSE_res_corr.RData')
