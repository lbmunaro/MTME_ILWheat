# Single-Trait Single-Environment model ----
# This script fits single-trait single-environment models

# Objective ----
# - 

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# # Load data ----
# ## Pheno & Ginv
# load('Data/ILYT_Pheno-Gmatrix.RData')
# 
# # Fit model 1 ----
# 
# # Track time
# STSE_start.time <- Sys.time()
# print(STSE_start.time)
# 
# # This includes a non-separable residual model, with a separate spatial model
# # for each trait by environment combination
# 
# ## Run the model ----
# 
# STSE.asr <- asreml(
#   Pheno_z ~ TraitEnv,  
#   random = ~ diag(TraitEnv):vm(Gkeep, Ginv.sparse),
#   residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
#   sparse = ~ TraitEnv:Gdrop,
#   data = ILYT_Pheno,
#   na.action = na.method(x = "include"), 
#   maxit = 20, 
#   workspace = '12gb' 
# )
# print('STSE')
# print(summary(STSE.asr)$call)
# STSE.asr$loglik
# print('AIC')
# print(summary(STSE.asr)$aic)
# print(STSE.asr$noeff)
# print(paste('convergence =',STSE.asr$converge))
# 
# # Time to run
# STSE_time <- Sys.time() - STSE_start.time
# print(Sys.time())
# print(STSE_time)
# 
# # ASReml Version 4.2 14/11/2024 11:26:23
# # Multi-section model using the sigma parameterization.
# # LogLik        Sigma2     DF     wall
# # 1      8848.809           1.0  38450   11:32:36
# # 2      10548.07           1.0  38450   11:38:24  (  2 restrained)
# # 3      12057.65           1.0  38450   11:44:13
# # 4      12995.94           1.0  38450   11:50:01
# # 5      13384.90           1.0  38450   11:55:52
# # 6      13465.26           1.0  38450   12:01:46
# # 7      13478.56           1.0  38450   12:07:35
# # 8      13480.53           1.0  38450   12:13:22
# # 9      13480.87           1.0  38450   12:19:10
# # 10      13480.94           1.0  38450   12:25:09
# # 11      13480.95           1.0  38450   12:31:09
# # 12      13480.96           1.0  38450   12:36:57
# # 13      13480.96           1.0  38450   12:42:53
# # 14      13480.96           1.0  38450   12:48:40
# 
# # Save data ----
# save.image('Data/STSE_mod.RData')

# End ----

# Heritability ----
load('Data/STSE_mod.RData')
varcomp_df <- summary(STSE.asr)$varcomp |>
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

TraitEnv <- unique(ILYT_Pheno$TraitEnv)

cbind(
  TraitEnv,
  calculate_heritability(varcomp_df = varcomp_df, asreml_model = STSE.asr)
)

# TraitEnv Index            Formula Result.Estimate  Result.SE
# V1   GY-22-Adv     1     V1~V1/(V1+V40)      0.23668745 0.03120932
# V2   GY-22-Neo     2     V2~V2/(V2+V43)      0.38328590 0.03527495
# V3   GY-22-Stp     3     V3~V3/(V3+V46)      0.42987523 0.04265158
# V4   GY-22-Urb     4     V4~V4/(V4+V49)      0.51827930 0.02818458
# V5   GY-23-Adv     5     V5~V5/(V5+V52)      0.28902432 0.03550119
# V6   GY-23-Neo     6     V6~V6/(V6+V55)      0.46344702 0.04134517
# V7   GY-23-Stp     7     V7~V7/(V7+V58)      0.07227687 0.02270804
# V8   GY-23-Urb     8     V8~V8/(V8+V61)      0.35284372 0.02940145
# V9   GY-24-Adv     9     V9~V9/(V9+V64)      0.49035691 0.04847124
# V10  GY-24-Neo    10  V10~V10/(V10+V67)      0.61120214 0.03253825
# V11  GY-24-Stp    11  V11~V11/(V11+V70)      0.56010213 0.04588486
# V12  GY-24-Urb    12  V12~V12/(V12+V73)      0.61692223 0.03068610
# V13  HD-22-Neo    13  V13~V13/(V13+V76)      0.61608623 0.03155129
# V14  HD-22-Urb    14  V14~V14/(V14+V79)      0.59183890 0.03400889
# V15  HD-23-Neo    15  V15~V15/(V15+V82)      0.68844901 0.03707239
# V16  HD-23-Urb    16  V16~V16/(V16+V85)      0.60310924 0.03469969
# V17  HD-24-Neo    17  V17~V17/(V17+V88)      0.60290259 0.04261115
# V18  HD-24-Urb    18  V18~V18/(V18+V91)      0.85095108 0.01962660
# V19  HT-22-Neo    19  V19~V19/(V19+V94)      0.42828874 0.03423667
# V20  HT-22-Urb    20  V20~V20/(V20+V97)      0.28711560 0.03130628
# V21  HT-23-Neo    21 V21~V21/(V21+V100)      0.33358888 0.04179079
# V22  HT-23-Stp    22 V22~V22/(V22+V103)      0.23643889 0.04394180
# V23  HT-23-Urb    23 V23~V23/(V23+V106)      0.50904045 0.03036474
# V24  HT-24-Neo    24 V24~V24/(V24+V109)      0.58829295 0.04294280
# V25  HT-24-Urb    25 V25~V25/(V25+V112)      0.66805446 0.03519712
# V26 MAT-23-Urb    26 V26~V26/(V26+V115)      0.55013402 0.03442666
# V27 MAT-24-Urb    27 V27~V27/(V27+V118)      0.51482190 0.04193519
# V28  TW-22-Adv    28 V28~V28/(V28+V121)      0.72884248 0.03138763
# V29  TW-22-Neo    29 V29~V29/(V29+V124)      0.81329634 0.01941466
# V30  TW-22-Stp    30 V30~V30/(V30+V127)      0.86568700 0.01679196
# V31  TW-22-Urb    31 V31~V31/(V31+V130)      0.67654799 0.02849670
# V32  TW-23-Adv    32 V32~V32/(V32+V133)      0.73557816 0.02872854
# V33  TW-23-Neo    33 V33~V33/(V33+V136)      0.72204278 0.02676387
# V34  TW-23-Stp    34 V34~V34/(V34+V139)      0.74599057 0.02770181
# V35  TW-23-Urb    35 V35~V35/(V35+V142)      0.37779554 0.04038692
# V36  TW-24-Adv    36 V36~V36/(V36+V145)      0.92484419 0.01143658
# V37  TW-24-Neo    37 V37~V37/(V37+V148)      0.81077334 0.02262705
# V38  TW-24-Stp    38 V38~V38/(V38+V151)      0.62083456 0.04539238
# V39  TW-24-Urb    39 V39~V39/(V39+V154)      0.69990426 0.03157315

#Identify possible outliers
plot(STSE.asr)

# Extract residuals from the model
residuals_STSE <- residuals(STSE.asr)
# Combine residuals with the original data to identify datapoints
data_with_residuals <- cbind(ILYT_Pheno, Residuals = residuals_STSE)
# Sort the data frame by the absolute value of residuals to spot potential outliers
data_with_residuals <- data_with_residuals[order(abs(data_with_residuals$Residuals), decreasing = TRUE), ]

ILYT_Pheno_2 <- data_with_residuals |>
  glimpse()

# Temp check
# Pheno
ILYT_Pheno_2 |>
  filter(Env=='23-Stp' & Trait=='GY') |>
  arrange(desc(Pheno)) |>
#  arrange(Pheno) |>
  glimpse() |>
  ggplot(aes(x=Row,y=Col, fill=Pheno)) +
  geom_tile() +
  geom_text(aes(label = round(Pheno, 1)), color = "white", size = 3) +
  scale_fill_viridis_c(name= 'Pheno',
                       option = 'viridis') +
  facet_wrap(~Trait)

# Residuals
ILYT_Pheno_2 |>
  filter(Env=='23-Stp' & Trait=='GY') |>
  arrange(desc(Residuals)) |>
#  arrange(Residuals) |>
  glimpse() |>
  ggplot(aes(x=Row,y=Col, fill=Residuals)) +
  geom_tile() +
  geom_text(aes(label = round(Residuals, 1)), color = "white", size = 3) +
  scale_fill_viridis_c(name= 'Residuals',
                       option = 'viridis') +
  facet_wrap(~Trait)

