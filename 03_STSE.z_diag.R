# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science
library(asreml) # ASReml-R package
source('Functions_MTME.R')  # Load functions

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Fit model 0 ----
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

# Update model ----
STSE.z.asr0 <- update_asreml(STSE.z.asr0, 
                             max_updates = 10,
                             save_path = 'Data/STSE.z_diag.RData')

save.image('Data/STSE.z_diag.RData')

# Fit model 1 ----
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

# Update model ----
STSE.z.asr1 <- update_asreml(STSE.z.asr1, 
                             max_updates = 10,
                             save_path = 'Data/STSE.z_diag.RData')

save.image('Data/STSE.z_diag.RData')

# Fit model 2 ----
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

# Update model ----
STSE.z.asr2 <- update_asreml(STSE.z.asr2, 
                             max_updates = 10,
                             save_path = 'Data/STSE.z_diag.RData')

save.image('Data/STSE.z_diag.RData')

# Fit model 3 ----

# Create columns for each nlevels(Trait) within Env
ILYT_PhenoTr <- ILYT_Pheno |>
  mutate(IDEU = paste(Env,Col,Row,sep = '-'), IDEU = as.factor(IDEU)) |>
  group_by(Env) |>
  mutate(nTrait = length(unique(Trait))) |>
  mutate(Tr2 = ifelse(nTrait==2,as.character(Trait),NA), Tr2 = as.factor(Tr2),
         Tr3 = ifelse(nTrait==3,as.character(Trait),NA), Tr3 = as.factor(Tr3),
         Tr4 = ifelse(nTrait==4,as.character(Trait),NA), Tr4 = as.factor(Tr4),
         Tr5 = ifelse(nTrait==5,as.character(Trait),NA), Tr5 = as.factor(Tr5)) |>
  ungroup() |>
  droplevels() |>
  arrange(IDEU, TraitEnv) |>
  glimpse()

# Create vectors with the Env included in each of the Tr2:Tr5 columns
tr2 <- ILYT_PhenoTr |> filter(!is.na(Tr2)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr2
tr3 <- ILYT_PhenoTr |> filter(!is.na(Tr3)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr3
tr4 <- ILYT_PhenoTr |> filter(!is.na(Tr4)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr4
tr5 <- ILYT_PhenoTr |> filter(!is.na(Tr5)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr5

STSE.z.asr3 <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    diag(TraitEnv):Gkeep +
    diag(TraitEnv):Block +
    at(TraitEnv):ar1v(Col):ar1(Row),
  residual = ~ dsum(~ IDEU:corgh(Tr2) | Env, levels = tr2) +
    dsum(~ IDEU:corgh(Tr3) | Env, levels = tr3) +
    dsum(~ IDEU:corgh(Tr4) | Env, levels = tr4) +
    dsum(~ IDEU:corgh(Tr5) | Env, levels = tr5),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_PhenoTr,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '16gb')

# Update model ----
STSE.z.asr3 <- update_asreml(STSE.z.asr3, 
                             max_updates = 10,
                             save_path = 'Data/STSE.z_diag.RData')

save.image('Data/STSE.z_diag.RData')

# End ----
