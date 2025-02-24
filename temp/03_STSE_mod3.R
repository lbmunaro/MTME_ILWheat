# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

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

ILYT_Pheno <- ILYT_Pheno |>
  mutate(IDEU = paste(Env,Col,Row,sep = '-'), IDEU = as.factor(IDEU)) |>
  group_by(Env) |>
  mutate(nTrait = length(unique(Trait))) |>
  mutate(Tr2 = ifelse(nTrait==2,as.character(Trait),NA), Tr2 = as.factor(Tr2),
         Tr3 = ifelse(nTrait==3,as.character(Trait),NA), Tr3 = as.factor(Tr3),
         Tr4 = ifelse(nTrait==4,as.character(Trait),NA), Tr4 = as.factor(Tr4),
         Tr5 = ifelse(nTrait==5,as.character(Trait),NA), Tr5 = as.factor(Tr5)) |>
  ungroup() |>
  droplevels() |>
  arrange(IDEU, Trait) |>
  glimpse()

tr2 <- ILYT_Pheno |> filter(!is.na(Tr2)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr2

tr3 <- ILYT_Pheno |> filter(!is.na(Tr3)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr3

tr4 <- ILYT_Pheno |> filter(!is.na(Tr4)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr4

tr5 <- ILYT_Pheno |> filter(!is.na(Tr5)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr5

ILYT_Pheno <- ILYT_Pheno |>
  mutate(Env = factor(Env, levels = c(tr5,tr4,tr3,tr2))) |>
  arrange(Env, IDEU, Trait) |>
  glimpse()

# Model 3 ----

STSE_3.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    at(TraitEnv):ar1v(Col):ar1(Row),
  residual = ~ dsum(~ IDEU:corgh(Tr2) | Env, levels = tr2) +
    dsum(~ IDEU:corgh(Tr3) | Env, levels = tr3) +
    dsum(~ IDEU:corgh(Tr4) | Env, levels = tr4) +
    dsum(~ IDEU:corgh(Tr5) | Env, levels = tr5),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 30,
  workspace = '12gb'
)
print('STSE_3')
print(summary(STSE_3.asr)$call)
STSE_3.asr$loglik
print('AIC')
print(summary(STSE_3.asr)$aic)
print(paste('convergence =',STSE_3.asr$converge))

## Save data ----
save.image('Data/STSE_mod3.RData')

## Load
load('Data/STSE_mod3.RData')

STSE_3.asr <- update(STSE_3.asr)

print('STSE_3 - Update 1')
print(summary(STSE_3.asr)$call)
STSE_3.asr$loglik
print('AIC')
print(summary(STSE_3.asr)$aic)
print(paste('convergence =',STSE_3.asr$converge))
save.image('Data/STSE_mod3.RData')

## Load
load('Data/STSE_mod3.RData')

STSE_3.asr <- update(STSE_3.asr)

print('STSE_3 - Update 1')
print(summary(STSE_3.asr)$call)
STSE_3.asr$loglik
print('AIC')
print(summary(STSE_3.asr)$aic)
print(paste('convergence =',STSE_3.asr$converge))
save.image('Data/STSE_mod3.RData')