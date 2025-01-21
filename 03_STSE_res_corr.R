# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

load('Data/ILYT_Pheno-Gmatrix.RData')
library(tidyverse)
library(asreml)

ILYT_Pheno |>
  glimpse()
# Data ----
Toy_pheno <- ILYT_Pheno |>
  filter(Year==2024) |>
  mutate(IDEU = paste(Env,Col,Row,sep='-'),
         IDEU = as.factor(IDEU),
         Env = factor(Env, levels = c('24-Adv', '24-Stp', '24-Neo', '24-Urb'))
         ) |>
  group_by(Env) |>
  mutate(nTrait = length(unique(Trait))) |>
  mutate(Tr2 = ifelse(nTrait==2,as.character(Trait),'GY'), Tr2 = as.factor(Tr2),
         Tr4 = ifelse(nTrait==4,as.character(Trait),'GY'), Tr4 = as.factor(Tr4),
         Tr5 = ifelse(nTrait==5,as.character(Trait),'GY'), Tr5 = as.factor(Tr5)) |>
  ungroup() |>
  droplevels() |>
  arrange(IDEU, Trait) |>
  glimpse()

str(Toy_pheno)

Toy_pheno |> filter(!is.na(Tr2)) |> droplevels() |> group_by(Env) |> 
  summarise(Env=unique(Env)) |> glimpse()
# 24-Adv, 24-Stp

Toy_pheno |> filter(!is.na(Tr4)) |> droplevels() |> group_by(Env) |>
  summarise(Env=unique(Env)) |> glimpse()
# 24-Neo

Toy_pheno |> filter(!is.na(Tr5)) |> droplevels() |> group_by(Env) |>
  summarise(Env=unique(Env)) |> glimpse()
# 24-Urb

Toy_pheno |>
  str()

Toy_pheno <- Toy_pheno |>
  arrange(Env) |>
  glimpse()

unique(Toy_pheno$Env)

# Toy_pheno <- Toy_pheno |> filter(Env%in%c('24-Adv','24-Stp')) |>
#   droplevels() |>
#   arrange(IDEU, Tr2) |>
#   glimpse()


# STSE2.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ diag(TraitEnv):Gkeep,# +
#   #        at(Env):ar1v(Col):ar1(Row),
#   residual = ~ dsum(~ id(IDEU):corgh(Tr2) | Env),
#   sparse = ~ TraitEnv:Gdrop,
#   data = Toy_pheno,
#   na.action = na.method(x = 'include', y='include'),
#   maxit = 20,
#   workspace = '6gb'
# )
# 
# summary(STSE2.asr)$varcomp

# Model ----
STSE2.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):Gkeep +
        at(Env):ar1v(Col):ar1(Row),
    residual = ~ dsum(~ IDEU:corgh(Tr2) | Env, levels = c('24-Adv', '24-Stp')) +
    dsum(~ IDEU:corgh(Tr4) | Env, levels = c('24-Neo')) +
    dsum(~ IDEU:corgh(Tr5) | Env, levels = c('24-Urb')),
  sparse = ~ TraitEnv:Gdrop,
  data = Toy_pheno,
  na.action = na.method(x = 'include', y='include'),
  maxit = 20,
  workspace = '6gb'
)

summary(STSE2.asr)$varcomp

str(Toy_pheno)
table(Toy_pheno$Env, Toy_pheno$Tr2)
table(Toy_pheno$Env, Toy_pheno$Tr4)
table(Toy_pheno$Env, Toy_pheno$Tr5)
length(levels(Toy_pheno$IDEU))

# STSE2.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),# +
#   #        at(Env):ar1v(Col):ar1(Row),
#   residual = ~ dsum(~IDEU:corgh(Tr2) + IDEU:corgh(Tr4) + 
#                       IDEU:corgh(Tr5) | Env, 
#                     levels = list(c('24-Adv', '24-Stp'), 
#                     c('24-Neo'), c('24-Urb'))),
#   sparse = ~ TraitEnv:Gdrop,
#   data = Toy_pheno,
#   na.action = na.method(x = 'include'),
#   maxit = 20,
#   workspace = '6gb'
# )
# 
# 
# STSE2.asr <- asreml(
#   Pheno_z ~ TraitEnv,
#   random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),# +
#   #        at(Env):ar1v(Col):ar1(Row),
#   residual = ~ dsum(~ IDEU:corgh(Tr2) | Env, levels = c('24-Adv')) +
#     dsum(~ IDEU:corgh(Tr4) | Env, levels = c('24-Neo')) +
#     dsum(~ IDEU:corgh(Tr5) | Env, levels = c('24-Urb')) +
#     dsum(~ IDEU:corgh(Tr2) | Env, levels = c('24-Stp')),
#   sparse = ~ TraitEnv:Gdrop,
#   data = Toy_pheno,
#   na.action = na.method(x = 'include'),
#   maxit = 20,
#   workspace = '6gb'
# )
