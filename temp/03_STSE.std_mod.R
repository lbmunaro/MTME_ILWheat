# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Use for HPC only
#setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
load('Data/ILYT_Pheno-Gmatrix.RData')


ILYT_Pheno |> glimpse()

# Fit model ----
## Run the model ----

STSE.std.asr <- asreml(
  Pheno_std ~ TraitEnv,
  random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
  residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
  sparse = ~ TraitEnv:Gdrop,
  data = ILYT_Pheno,
  na.action = na.method(x = "include"),
  maxit = 20,
  workspace = '12gb'
)
print('STSE.std')
print(summary(STSE.std.asr)$call)
STSE.std.asr$loglik
print('AIC')
print(summary(STSE.std.asr)$aic)
print(paste('convergence =',STSE.std.asr$converge))

# Heritability ----
STSE.std_varcomp_df <- summary(STSE.std.asr)$varcomp |>
  as.data.frame() |>
  rownames_to_column() |>
  glimpse()

calculate_heritability <- function(varcomp_df, asreml_model) {
  # Function to perform one-by-one vpredict calculations
  vpredict_individual <- function(asreml_model, i, j) {
    formula <- as.formula(paste0("V", i, "~V", i, "/(V", i, "+V", j, ")"))
    result <- vpredict(asreml_model, formula)
    return(data.frame(Index = i, Formula = paste0("V", i, "~V", i, "/(V", i, "+V", j, ")"), Result = result))
  }
  
  # Initialize an empty dataframe to store results
  vpredict_results <- data.frame()
  
  # Loop through each pair of indices (V1 to V39 and V40 to V154 incrementing by 3)
  for (i in 1:39) {
    j <- 37 + i * 3
    if (j <= 154) {
      result <- vpredict_individual(asreml_model, i, j)
      vpredict_results <- rbind(vpredict_results, result)
    }
  }
  
  return(vpredict_results)
}

STSE.std_h2 <- cbind(
  unique(ILYT_Pheno$TraitEnv),
  calculate_heritability(varcomp_df = STSE.std_varcomp_df, asreml_model = STSE.std.asr)
)

# Save data ----
save.image('Data/STSE.std_mod.RData')
# 
# load('Data/STSE.std_mod.RData')
# 
# print(STSE.std.asr$call)
# 
# STSE.std.asr <- update(STSE.std.asr, workspace = '80gb')
# 
# print(STSE.std.asr$call)
# 
# STSE.std_blup <- predict.asreml(STSE.std.asr, classify = 'TraitEnv:Gkeep',
#                               ignore = c('(Intercept)','TraitEnv'))
# 
# save.image('Data/STSE.std_mod.RData')


