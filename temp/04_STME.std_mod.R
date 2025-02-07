# Load required libraries
library(tidyverse)
library(asreml)
library(doParallel)
library(foreach)

# Set working directory (HPC-specific)
setwd('~/MTME_ILWheat/')

# Load data
load('Data/ILYT_Pheno-Gmatrix.RData')

# Define function to fit models with error handling
fit_model <- function(trait, factor_analysis, workspace_size = '20gb') {
  result <- tryCatch({
    model <- asreml(
      Pheno_std ~ Env,
      random = as.formula(paste0("~ fa(Env,", factor_analysis, "):vm(Gkeep, Ginv.sparse)")),
      residual = ~ dsum(~ ar1(Col):ar1(Row) | Env),
      sparse = ~ Env:Gdrop,
      data = ILYT_Pheno |> filter(Trait == trait) |> droplevels(),
      na.action = na.method(x = "include"),
      maxit = 20,
      workspace = workspace_size
    )
    
    # Save model output
    save(model, file = paste0("Data/STME.std_", trait, "_fa", factor_analysis, ".RData"))
    
    # Return model summary
    list(
      trait = trait,
      fa = factor_analysis,
      aic = summary(model)$aic,
      convergence = model$converge
    )
  }, error = function(e) {
    # Log error and return a message
    error_msg <- paste(Sys.time(), "- Error fitting", trait, "fa", factor_analysis, ":", e$message)
    write(error_msg, file = "STME_errors.log", append = TRUE)
    return(error_msg)
  })
  
  return(result)
}

# Define traits and factor analyses
traits <- c("GY", "TW", "HD", "HT", "MAT")
factor_analyses <- c(1, 2, 3)

# Setup parallel processing
num_cores <- detectCores() - 1  # Use one less than available cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Run models in parallel
results <- foreach(trait = rep(traits, each = length(factor_analyses)), 
                   factor_analysis = rep(factor_analyses, times = length(traits)),
                   .packages = c("asreml", "tidyverse")) %dopar% {
                     fit_model(trait, factor_analysis)
                   }

# Stop parallel processing
stopCluster(cl)

# Save environment
save.image('Data/STME.std_mod.RData')

# Print completion message
print("All models have been processed. Check 'STME_errors.log' for any errors.")
