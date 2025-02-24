# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models in parallel on SLURM

# Clean workspace
rm(list = objects())

# Packages ----
library(tidyverse)
library(asreml)
library(future.batchtools)  # Parallel processing on SLURM cluster
library(batchtools)

# Define HPC-specific parallel backend ----
n_cores <- 16  # Matching the allocated SLURM cores
plan(batchtools_slurm, template = "slurm_template.tmpl", workers = n_cores)

# Load data ----
load('Data/ILYT_Pheno-Gmatrix.RData')

# Function to fit models
fit_asreml_model <- function(model_id) {
  cat(paste("Fitting model:", model_id, "\n"))
  
  model <- switch(model_id,
                  "STSE.z0" = asreml(
                    Pheno_z ~ TraitEnv,
                    random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
                    residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
                    sparse = ~ TraitEnv:Gdrop,
                    data = ILYT_Pheno,
                    na.action = na.method(x = 'include'),
                    maxit = 13,
                    workspace = '12gb'
                  ),
                  
                  "STSE.z1" = asreml(
                    Pheno_z ~ TraitEnv,
                    random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
                      diag(TraitEnv):ide(Gkeep),
                    residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
                    sparse = ~ TraitEnv:Gdrop,
                    data = ILYT_Pheno,
                    na.action = na.method(x = 'include'),
                    maxit = 13,
                    workspace = '12gb'
                  ),
                  
                  "STSE.z2" = asreml(
                    Pheno_z ~ TraitEnv,
                    random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse) +
                      diag(TraitEnv):ide(Gkeep) +
                      diag(TraitEnv):Block,
                    residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
                    sparse = ~ TraitEnv:Gdrop,
                    data = ILYT_Pheno,
                    na.action = na.method(x = 'include'),
                    maxit = 13,
                    workspace = '12gb'
                  )
  )
  
  convergence_status <- model$converge
  logLik_val <- model$trace |>
    as.data.frame() |> rownames_to_column('Iteration') |>
    filter(Iteration == 'LogLik')
  
  cat(paste("Model:", model_id, "Convergence =", convergence_status, "\n"))
  print(logLik_val)
  
  return(list(model_id = model_id, model = model, convergence = convergence_status, logLik = logLik_val))
}

# Define model IDs
model_ids <- c("STSE.z0", "STSE.z1", "STSE.z2")

# Run models in parallel ----
results <- future_map(model_ids, fit_asreml_model, .progress = TRUE)

# Extract models
STSE_models <- setNames(lapply(results, `[[`, "model"), model_ids)

# Save models & environment ----
save(STSE_models, file = "Data/STSE.z_diag_parallel.RData")
save.image("Data/STSE.z_diag_parallel_workspace.RData")

# End ----