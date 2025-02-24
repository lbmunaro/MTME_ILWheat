# STME & MTME model selection ----
# This script calculates the percentage of additive genetic variance that is
# explained by each model previously fit

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Use for HPC only
#setwd('~/MTME_ILWheat/')

# Load data ----
## MTME.z_rr2a
load('Data/MTME.z_rr2a.RData')

# Extract variance components
varcomp <- summary(MTME.z_rr2a.asr)$varcomp |> as.data.frame() |> rownames_to_column() |>
  glimpse()

# Extract specific variances (diagonal elements)
R <- varcomp$component[grep("^TraitEnv:vm", varcomp$rowname)]

# Extract loadings (factor 1 & factor 2) from FA model
L1 <- varcomp$component[grep("^rr\\(TraitEnv, 2\\):vm.*!fa1$", varcomp$rowname)]
L2 <- varcomp$component[grep("^rr\\(TraitEnv, 2\\):vm.*!fa2$", varcomp$rowname)]
L <- cbind(L1, L2)  # Combine factor loadings into a matrix
L[1:5,]
# Perform Singular Value Decomposition (SVD) for rotation
svd <- svd(L) # Perform SVD on the loadings matrix

# Extract SVD components
Ba <- svd$u   # Left singular vectors (orthogonal)
La <- diag(svd$d)  # Singular values (diagonal matrix)
Va <- svd$v   # Right singular vectors (orthogonal)

# Compute rotated estimated loadings
L_star <- L %*% Va

# Adjust sign to ensure majority of first column is positive
# If the sum of the first column is negative, multiply by -1
c_value <- ifelse(sum(L_star[,1]) < 0, -1, 1)
L_star <- c_value * L_star
colnames(L_star) <- c('L_star1', 'L_star2')

# Calculate Percentage of Additive Genetic Variance Explained
V_a <- levels(ILYT_Pheno$TraitEnv) |> as.data.frame(nm = 'TraitEnv') |>
  cbind(R, L_star) |>
  rowwise() |>
  mutate(V_a_h.L1 = 100*(L_star1^2)/(L_star1^2 + L_star2^2 + R), # % variance from factor 1
         V_a_h.L2 = 100*(L_star2^2)/(L_star1^2 + L_star2^2 + R), # % variance from factor 2
         V_a_h = V_a_h.L1 + V_a_h.L2) |> # Sum
  ungroup() |>
  mutate(V_a = mean(V_a_h)) |> # overall mean variance explained
  glimpse()
