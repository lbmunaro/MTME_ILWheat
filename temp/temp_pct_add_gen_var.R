
# Calculate the percentage of additive genetic variance


load('Data/MTME_mod_rr.RData')

vc <- summary(MTME_RR2.asr)$varcomp

vc |> as.data.frame() |> rownames_to_column()

R <- vc$component[1:39]
R

L1 <- vc$component[79:117]
L2 <- vc$component[118:156]

L <- cbind(L1,L2)
rownames(L) <- levels(ILYT_Pheno$TraitEnv)

VG <- (L%*%t(L))+diag(R)
VG
CG <- cov2cor(VG)
CG

# Given matrix Λa


Lambda_a <- L
Lambda_a
# Step 1: Perform Singular Value Decomposition (SVD)
svd_result <- svd(Lambda_a)

# Extract SVD components
Ba <- svd_result$u   # Left singular vectors (orthogonal)
La <- diag(svd_result$d)  # Singular values (diagonal matrix)
Va <- svd_result$v   # Right singular vectors (orthogonal)

# Step 2: Compute rotated estimated loadings
Lambda_a_star <- Lambda_a %*% Va

# Step 3: Adjust sign to ensure majority of first column is positive
# If the sum of the first column is negative, multiply by -1
c_value <- ifelse(sum(Lambda_a_star[,1]) < 0, -1, 1)
Lambda_a_star <- c_value * Lambda_a_star

Gvar <- Lambda_a_star |> as.data.frame() |> cbind(R) |>
  rownames_to_column() |>
  rowwise() %>%
  mutate(
    var_V1 = 100 * (V1^2) / (V1^2 + V2^2 + R),
    var_V2 = 100 * (V2^2) / (V1^2 + V2^2 + R),
    var_T = var_V1+var_V2
  ) %>%
  ungroup() |>
  #  mutate(var_T = sum(var_V1,var_V2)) |>
  arrange(desc(var_T))
Gvar
View(Gvar)

mean(Gvar$var_T)
