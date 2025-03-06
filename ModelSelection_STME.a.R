rm(list=ls())

library(asreml)
library(tidyverse)
load('Data/STME.z_rr.a.RData')
source('Functions_MTME.R')

# GY
lrt.asreml(GY_STME.z_rr1.asr, GY_STME.z_rr2.asr, GY_STME.z_rr3.asr)
# TW
lrt.asreml(TW_STME.z_rr1.asr, TW_STME.z_rr2.asr, TW_STME.z_rr3.asr)
# HD
lrt.asreml(HD_STME.z_rr1.asr, HD_STME.z_rr2.asr, HD_STME.z_rr3.asr)
# HT
lrt.asreml(HT_STME.z_rr1.asr, HT_STME.z_rr2.asr, HT_STME.z_rr3.asr)

rm(list = setdiff(ls(), c('GY_STME.z_rr3.asr', 
                          'TW_STME.z_rr3.asr', 
                          'HD_STME.z_rr2.asr', 
                          'HT_STME.z_rr2.asr', 
                          'MAT_STME.z_rr1.asr', 
                          'ILYT_Pheno')))

source('Functions_MTME.R')

VaPct(GY_STME.z_rr3.asr, k = 3, data = ILYT_Pheno, TE_fct = 'TraitEnv')
VaPct(TW_STME.z_rr3.asr, k = 3, data = ILYT_Pheno, TE_fct = 'TraitEnv')
VaPct(HD_STME.z_rr2.asr, k = 2, data = ILYT_Pheno, TE_fct = 'TraitEnv')
VaPct(HT_STME.z_rr2.asr, k = 2, data = ILYT_Pheno, TE_fct = 'TraitEnv')
VaPct(MAT_STME.z_rr1.asr, k = 1, data = ILYT_Pheno, TE_fct = 'TraitEnv')

STME_gebvs <- rbind(
  gebvs_asreml(GY_STME.z_rr3.asr, k = 3, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(TW_STME.z_rr3.asr, k = 3, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(HD_STME.z_rr2.asr, k = 2, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(HT_STME.z_rr2.asr, k = 2, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(MAT_STME.z_rr1.asr, k = 1, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv')
) |>
  rename(TraitEnv=TE_fct,
         Gkeep=G_fct) |>
  left_join(ILYT_Pheno |>
              group_by(TraitEnv) |>
              summarise(Pheno_mean=mean(Pheno_mean),
                        Pheno_sd=mean(Pheno_sd),
                        Trait=unique(Trait),
                        Env=unique(Env))
            ) |>
  mutate(gebv=blup*Pheno_sd) |>
  select(Gkeep, Trait, Env, gebv) |>
  glimpse()

STME_gebvs_overall <- STME_gebvs |>
  group_by(Gkeep, Trait) |>
  summarise(m_gebv=mean(gebv)) |>
  ungroup() |>
  pivot_wider(names_from = Trait, values_from = m_gebv) |>
  mutate(GSI=(213.75*GY/1000)+(0.41*TW)+(-13.32*HD)) |>
  arrange(desc(GSI)) |>
  mutate(rank=rank(-GSI)) |>
  glimpse()

STME_top50 <- STME_gebvs_overall |>
  filter(rank <= 50) |>
  pull(Gkeep)

