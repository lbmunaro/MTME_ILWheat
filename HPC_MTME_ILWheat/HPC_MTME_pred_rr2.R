rm(list = ls())

# Packages ----
library(tidyverse) # R packages for data science.
library(asreml) # ASReml-R package.

load('MTME_mod_rr.RData')

summary(MTME_RR2.asr)$call

MTME_RR2.asr <- asreml(fixed = Pheno_z ~ TraitEnv,
                       random = ~ rr(TraitEnv, 2):vm(Gkeep, Ginv.sparse) +
                         diag(TraitEnv):vm(Gkeep, Ginv.sparse),
                       sparse = ~TraitEnv:Gdrop,
                       residual = ~ dsum(~ ar1v(Col):ar1(Row) | TraitEnv),
                       G.param = MTME_RR2.asr$G.param,
                       R.param = MTME_RR2.asr$R.param,
                       na.action = na.method(x = "include"),
                       data = ILYT_Pheno,
                       maxit = 20,
                       workspace = "152gb")

save.image('MTME_pred_rr2.RData')

# Print model info
print('MTME-RR2')
print(summary(MTME_RR2.asr)$call)
print('AIC')
print(summary(MTME_RR2.asr)$aic)
print(paste('convergence =', MTME_RR2.asr$converge))

summary(MTME_RR2.asr)$varcomp


MTME_RR2.pred0 <- predict.asreml(MTME_RR2.asr, classify='Gkeep:TraitEnv', pworkspace='16gb')$pvals

save.image('MTME_pred_rr2.RData')

MTME_RR2.pred1 <- predict.asreml(MTME_RR2.asr, classify='Gkeep:TraitEnv', 
                          ignore = c('(Intercept)', 'TraitEnv'),
                          pworkspace='16gb')$pvals

save.image('MTME_pred_rr2.RData')

