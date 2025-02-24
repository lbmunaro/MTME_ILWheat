# ==========================================================================
# Compute Genomic Estimated Breeding Values (GEBVs) from Factor Analytic Model
# Based on Smith & Cullis (2018)
# ==========================================================================
rm(list = objects())  # Removes all objects from the environment.
# Load required libraries
library(tidyverse)  # Data manipulation and visualization
library(asreml)     # ASReml-R package for mixed models

# ==========================================================================
# Extract Random Effects (Variety Scores)
# ==========================================================================
load('Data/MTME.z_rr2a.RData')
# Extract variety BLUPs (random effects estimates)
randeff <- summary(MTME.z_rr1a.asr, coef = TRUE)$coef.random |> as.data.frame() |>
  glimpse()

randeff_df <- randeff |> 
  as.data.frame() |> 
  rownames_to_column(var = 'Effect') |>
  filter(!str_detect(Effect,'Comp')) |>
  glimpse()

# Keep only the variety scores associated with FA model terms
rr_scores <- randeff_df |> filter(str_detect(Effect, '^rr\\(TraitEnv,')) |> rename(RR = solution) |>
  mutate(TraitEnv = str_extract(Effect, '(?<=rr\\(TraitEnv, 1\\)_)[^:]+'),
         Gkeep = str_extract(Effect, '[^_]+$')) |>
  dplyr::select(TraitEnv,Gkeep,RR) |>
  glimpse()

diag_scores <- randeff_df |> filter(str_detect(Effect, '^TraitEnv_')) |> rename(D = solution) |>
  mutate(
    TraitEnv = str_extract(Effect, '(?<=TraitEnv_)[^:]+'),  # Extract TraitEnv
    Gkeep = str_extract(Effect, '[^_]+$')  # Extract Gkeep (last part after "_")
  ) |>  # Extract Gkeep (genotype identifier)
  dplyr::select(TraitEnv,Gkeep,D) |>
  glimpse()

gebvs <- rr_scores |>
  left_join(diag_scores) |>
  mutate(gebv=RR+D) |>
  glimpse()

hist(gebvs$gebv)

cor(gebvs$RR,gebvs$gebv)

gebvs <- rr_scores |>
  left_join(L_star |> as.data.frame() |> mutate(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |> glimpse()) |>
  rowwise() |>
  mutate(gebv = L_star1 * RR + L_star2 * RR) |>
  glimpse()


summary(MTME.z_rr2a.asr,coef=T)$coef.random |>
  as.data.frame() |> 
  rownames_to_column(var = 'Effect') |>
  filter(!str_detect(Effect,'Comp')) |>
  glimpse()
  filter(!str_detect(Effect, '^rr\\(TraitEnv,')) |>
  glimpse()
  
