# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Use for HPC only
setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')


ILYT_Pheno |> glimpse()

# Fit model 0----
STSE.z.asr <- asreml(
  Pheno_z ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = 'include'),
  maxit = 13,
  workspace = '12gb')

print('STSE.z')
print('AIC')
print(summary(STSE.z.asr)$aic)
print(paste('convergence =',STSE.z.asr$converge))
STSE.z.asr$trace |>
  as.data.frame() |> rownames_to_column('Iteration') |>
  filter(Iteration=='LogLik') |> print()

# Heritability ----
STSE.z_varcomp_df <- summary(STSE.z.asr)$varcomp |>
  as.data.frame() |>
  rownames_to_column() |>
  glimpse()

calculate_heritability <- function(varcomp_df, asreml_diagel) {
  # Function to perform one-by-one vpredict calculations
  vpredict_individual <- function(asreml_diagel, i, j) {
    formula <- as.formula(paste0('V', i, '~V', i, '/(V', i, '+V', j, ')'))
    result <- vpredict(asreml_diagel, formula)
    return(data.frame(Index = i, Formula = paste0('V', i, '~V', i, '/(V', i, '+V', j, ')'), Result = result))
  }
  
  # Initialize an empty dataframe to store results
  vpredict_results <- data.frame()
  
  # Loop through each pair of indices (V1 to V39 and V40 to V154 incrementing by 3)
  for (i in 1:39) {
    j <- 37 + i * 3
    if (j <= 154) {
      result <- vpredict_individual(asreml_diagel, i, j)
      vpredict_results <- rbind(vpredict_results, result)
    }
  }
  
  return(vpredict_results)
}

STSE.z_h2 <- cbind(
  unique(ILYT_Pheno$TraitEnv),
  calculate_heritability(varcomp_df = STSE.z_varcomp_df, asreml_diagel = STSE.z.asr)
)
STSE.z_h2

# Save data ----
save.image('Data/STSE.z_diag.RData')

# End ----