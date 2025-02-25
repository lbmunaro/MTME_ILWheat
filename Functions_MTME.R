# Functions

# Update ASReml-R ----
# This function updates ASReml-R until it converges
update_asreml <- function(mod, max_updates = 5, save_path) {
  count <- 0
  
  while (!mod$converge && count < max_updates) {
    count <- count + 1
    mod <- update(mod)
    
    # Print model update information
    print(paste('Update', count))
    print(paste('Convergence =', mod$converge))
    
    # Print LogLik value
    loglik <- mod$trace |>
      as.data.frame() |>
      rownames_to_column('Iteration') |>
      filter(Iteration == 'LogLik')
    
    print(loglik)
    
    # Save model state after each update
    save.image(save_path)
  }
  
  if (mod$converge) {
    print("Model successfully converged!")
  } else {
    print("Maximum updates reached. Model did not converge.")
  }
  
  return(mod) # Return the final model
}