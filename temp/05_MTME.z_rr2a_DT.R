# Multi-Trait Multi-Environment models ----
# This script fits Multi-Trait Multi-Environment models

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

# Use for HPC only
#setwd('~/MTME_ILWheat/')

# Load data ----
## Pheno & Ginv
#load("ILYT_Pheno-Gmatrix.RData")
 load('Data/ILYT_Pheno-Gmatrix.RData')

dim(ILYT_Pheno)
# 38792    19
nlevels(ILYT_Pheno$Env) * nlevels(ILYT_Pheno$Gkeep)
# 35724
sum(is.na(ILYT_Pheno$Pheno_z)) # this is standardised yeh?
# 115
sum(!is.na(ILYT_Pheno$Pheno_z))
# 115/38677 = 0.3% missing 
#sum(with(ILYT_Pheno[!is.na(Pheno_z)], table(Gkeep, Env)) > 0)
# 8588 / 35724 = 25% of GxE combos with pheno data

# Traits
nlevels(ILYT_Pheno$Trait)
# 5 
# will be interesting to see the structure here

with(ILYT_Pheno, table(Block,Env)) # looks like a combination of fully rep'd and p-rep'd designs for the different material
table(with(ILYT_Pheno, table(Gkeep,Block,Env,Trait)))
#       0       1       2 
# 1033324   38391       5 
# Ok cool, so mostly resolvable - what's going on with the extras (2's)?

head(ILYT_Pheno)
# closer look at one environment
(tt <- with(droplevels(ILYT_Pheno[ILYT_Pheno$Env == "22-Adv" & ILYT_Pheno$Trait == "GY",]), table(Gkeep,Block,Env)))
tt[rowSums(tt) == 2,,] # ok so i presume this is late stage material
tt[rowSums(tt) == 1,,] # and this is early stage material?
# all of this structure needs to be included in the LMM...
with(ILYT_Pheno, table(Block,Env))

str(ILYT_Pheno)
names(Ginv.sparse)

# Fit rr2a model ----
# DT, ok, but you are better to start with a diagonal model to assess the residual structure 
# and get good start values for subsequent models, 
# Will move forward with what you have here, but let me know if you want help here...
## Run model ----
k <- 2
MTME.z_rr2a.asr <- asreml(Pheno_z ~ TraitEnv,
                          random = ~ rr(TraitEnv,2):vm(Gkeep, Ginv.sparse) + diag(TraitEnv):vm(Gkeep, Ginv.sparse),
                          # where are your expt design / blocking terms? at least fit diag(TraitEnv):Block...
                          # what about your non-additive term, at least fit diag(TraitEnv):Gkeep...
                          residual = ~ dsum(~ ar1(Col):ar1(Row) | TraitEnv),
                          sparse = ~ TraitEnv:Gdrop, # what constitutes Gdrop?
                          data = droplevels(ILYT_Pheno[ILYT_Pheno$Env %in% c("22-Adv", "22-Neo", "22-Stp", "22-Urb"),]), # just subsetting to make it run faster
                          na.action = na.method(x = 'include'),
                          maxit = 13, # dont use maxit, it changes the step size, just use update
                          workspace = '24gb')
MTME.z_rr2a.asr <- update(MTME.z_rr2a.asr)
# 8906.019
summary(MTME.z_rr2a.asr)$varcom
save.image('Data/MTME.z_rr2a_DT.RData')

load('Data/MTME.z_rr2a_DT.RData')
# variance parameters
(vparams <- MTME.z_rr2a.asr$vparameters)
# latent environmental covariates (loadings)
(Lam <- matrix(vparams[grep("^rr.*fa", names(vparams))], ncol = k))
# specific variances
(Psi <- diag(vparams[grep("^TraitEnv.*vm", names(vparams))]))

# BLUPs
(coefs <- MTME.z_rr2a.asr$coefficients$random)
# genotype scores (slopes)
(f <- coefs[grep("Comp", rownames(coefs))])
Lamf <- c(matrix(f, ncol = k) %*% t(Lam)) # common GET effects
coefs2 <- coefs[grep("rr.*vm", rownames(coefs)),]
plot(Lamf, coefs2[grep("Comp", names(coefs2), invert = T)]);abline(a=0, b=1) # check
# genetic regressions residuals
(delta <- coefs[grep("Comp", rownames(coefs))]) # specific GET effects
# total GET effects
Lamfdelta <- Lamf + delta

# % var explained:
# You'd want to get a high %vaf for all (overall and individual) measures below, i.e., > 80%
# overall
# mean of ratios
mean(diag(Lam %*% t(Lam))/diag(Lam %*% t(Lam)+Psi)) * 100
# ratio of means
mean(diag(Lam %*% t(Lam)))/mean(diag(Lam %*% t(Lam)+Psi)) * 100
# note the difference in the above

# for each trait x env combo
sort(diag(Lam %*% t(Lam))/diag(Lam %*% t(Lam)+Psi) * 100)

# for each trait (averaged across envs)
# GY 1:4
# HD 5:6
# HT 7:8
# TW 9:12
(trait_lst <- list(1:4,5:6,7:8,9:12))
# mean of ratios
lapply(trait_lst, function(x) mean(diag(Lam %*% t(Lam))[x]/diag(Lam %*% t(Lam)+Psi)[x]) * 100)
# ratio of means
lapply(trait_lst, function(x) mean(diag(Lam %*% t(Lam))[x])/mean(diag(Lam %*% t(Lam)+Psi)[x]) * 100)

# for each env (averaged across traits
# 22-Adv 1,9
# 22-Neo 2,5,7,10
# 22-Stp 3,11
# 22-Urb 4,6,8,12
(trait_lst <- list(c(1,9),c(2,5,7,10),c(3,11),c(4,6,8,12)))
# mean of ratios
lapply(trait_lst, function(x) mean(diag(Lam %*% t(Lam))[x]/diag(Lam %*% t(Lam)+Psi)[x]) * 100)
# ratio of means
lapply(trait_lst, function(x) mean(diag(Lam %*% t(Lam))[x])/mean(diag(Lam %*% t(Lam)+Psi)[x]) * 100)
