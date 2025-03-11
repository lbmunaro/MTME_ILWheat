# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.
source('Functions_MTME.R')  # Load functions

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')

# Create columns for each nlevels(Trait) within each Env
ILYT_PhenoTr <- ILYT_Pheno |>
  # Create a unique identifier 'IDEU' for each experimental unit using Env, Col, and Row
  mutate(IDEU = paste(Env,Col,Row,sep = '-'), IDEU = as.factor(IDEU)) |>
  group_by(Env) |>
  mutate(nTrait = length(unique(Trait))) |>
  # Create new columns (Tr2 to Tr5) based on the number of traits present in each 'Env'
  mutate(Tr2 = ifelse(nTrait==2,as.character(Trait),NA), Tr2 = as.factor(Tr2),
         Tr3 = ifelse(nTrait==3,as.character(Trait),NA), Tr3 = as.factor(Tr3),
         Tr4 = ifelse(nTrait==4,as.character(Trait),NA), Tr4 = as.factor(Tr4),
         Tr5 = ifelse(nTrait==5,as.character(Trait),NA), Tr5 = as.factor(Tr5)) |>
  ungroup() |>
  droplevels() |>
  arrange(IDEU, TraitEnv) |>
  glimpse()

# Create vectors with the unique environments included in each of the Tr2:Tr5 columns
tr2 <- ILYT_PhenoTr |> filter(!is.na(Tr2)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr2
tr3 <- ILYT_PhenoTr |> filter(!is.na(Tr3)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr3
tr4 <- ILYT_PhenoTr |> filter(!is.na(Tr4)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr4
tr5 <- ILYT_PhenoTr |> filter(!is.na(Tr5)) |> droplevels() |> group_by(Env) |>
  summarise(Env = unique(Env)) |> pull(Env); tr5

# Fit rr4ap1rc model ----
k <- 4
## Run model ----
MTME.z_rr4ap1rc.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ rr(TraitEnv,4):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
    rr(TraitEnv,1):Gkeep + diag(TraitEnv):Gkeep +
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
  workspace = '80gb'
)

# Print model info
print(paste('convergence =', MTME.z_rr4ap1rc.asr$converge))
MTME.z_rr4ap1rc.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Save
save.image('Data/MTME.z_rr4ap1rc.RData')

# Update model ----
MTME.z_rr4ap1rc.asr <- update_asreml(MTME.z_rr4ap1rc.asr, 
                                 max_updates = 50,
                                 save_path = 'Data/MTME.z_rr4ap1rc.RData')

# Save
save.image('Data/MTME.z_rr4ap1rc.RData')