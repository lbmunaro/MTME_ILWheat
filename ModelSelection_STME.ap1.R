rm(list=ls())

library(asreml)
library(tidyverse)
load('Data/STME.z_rr.ap1.RData')
source('Functions_MTME.R')

# GY
lrt.asreml(GY_STME.z_rr1ap1.asr, GY_STME.z_rr2ap1.asr, GY_STME.z_rr3ap1.asr)
# TW
lrt.asreml(TW_STME.z_rr1ap1.asr, TW_STME.z_rr2ap1.asr, TW_STME.z_rr3ap1.asr)
# HD
lrt.asreml(HD_STME.z_rr1ap1.asr, HD_STME.z_rr2ap1.asr, HD_STME.z_rr3ap1.asr)
# HT
lrt.asreml(HT_STME.z_rr1ap1.asr, HT_STME.z_rr2ap1.asr, HT_STME.z_rr3ap1.asr)

rm(list = setdiff(ls(), c('GY_STME.z_rr3ap1.asr',
                          'TW_STME.z_rr3ap1.asr',
                          'HD_STME.z_rr2ap1.asr',
                          'HT_STME.z_rr3ap1.asr',
                          'MAT_STME.z_rr1ap1.asr',
                          'ILYT_Pheno')))

source('Functions_MTME.R')

GY_rr3ap <- VaPct(GY_STME.z_rr3ap1.asr, k = 3, data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
TW_rr3ap <- VaPct(TW_STME.z_rr3ap1.asr, k = 3, data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
HD_rr2ap <- VaPct(HD_STME.z_rr2ap1.asr, k = 2, data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
HT_rr3ap <- VaPct(HT_STME.z_rr3ap1.asr, k = 3, data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
MAT_rr1ap <- VaPct(MAT_STME.z_rr1ap1.asr, k = 1, data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct |>
  glimpse()

VaPct_STSE.ap <- data.frame(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |>
  left_join(ILYT_Pheno |>
              select(TraitEnv,Trait,Env,Year,Loc) |>
              group_by(TraitEnv) |>
              summarise_all(~unique(.))
  ) |>
  left_join(GY_rr3ap|>rename(TraitEnv=TE_fct,GY_rr3ap=VaPct)) |>
  left_join(TW_rr3ap|>rename(TraitEnv=TE_fct,TW_rr3ap=VaPct)) |>
  left_join(HD_rr2ap|>rename(TraitEnv=TE_fct,HD_rr2ap=VaPct)) |>
  left_join(HT_rr3ap|>rename(TraitEnv=TE_fct,HT_rr3ap=VaPct)) |>
  left_join(MAT_rr1ap|>rename(TraitEnv=TE_fct,MAT_rr1ap=VaPct)) |>
  pivot_longer(cols = c(GY_rr3ap:MAT_rr1ap), names_to = 'model', values_to = 'VaPct') |>
  arrange(model,TraitEnv) |>
  group_by(model) |>
  mutate(mean.rrk=mean(VaPct)) |>
  group_by(model,Trait) |>
  mutate(mean.rrkT=mean(VaPct)) |>
  glimpse()
unique(VaPct_STSE.ap$model)
colors <- c(
  'GY_rr3ap' = '#FCB316',
  'HD_rr2ap' = '#006230',
  'HT_rr3ap' = '#009FD4',
  'MAT_rr1ap' = '#5C0E41',
  'TW_rr3ap' = '#13294B'
)

ggplot(VaPct_STSE.ap,aes(x=Trait, y=VaPct, fill=model)) +
  geom_point(aes(color=model), alpha=1) +  # Dodge boxplots
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.5, alpha=0.5) +  # Dodge boxplots
    scale_y_continuous(name = bquote(V[a] ~ 'explained (%)'), limits = c(0,100),
                     breaks = seq(0, 100, by = 20)) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 10, family = 'Times New Roman'),
    axis.title = element_text(size = 10, family = 'Times New Roman')
  )
ggsave('Figures/Figure5.4.png', width = 7, height = 3, units = 'in', dpi = 300)

# gebvs ----
STME.ap_gebvs <- rbind(
  gebvs_asreml(GY_STME.z_rr3ap1.asr, k = 3, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(TW_STME.z_rr3ap1.asr, k = 3, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(HD_STME.z_rr2ap1.asr, k = 2, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(HT_STME.z_rr3ap1.asr, k = 3, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv'),
  gebvs_asreml(MAT_STME.z_rr1ap1.asr, k = 1, 
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

STME.ap_gebvs_overall <- STME.ap_gebvs |>
  group_by(Gkeep, Trait) |>
  summarise(m_gebv=mean(gebv)) |>
  ungroup() |>
  pivot_wider(names_from = Trait, values_from = m_gebv) |>
  mutate(GSI=(213.75*GY/1000)+(0.41*TW)+(-13.32*HD)) |>
  arrange(desc(GSI)) |>
  mutate(rank_GSI=rank(-GSI),
         rank_GY=rank(-GY)) |>
  glimpse()


save(STME.ap_gebvs,STME.ap_gebvs_overall, file='Data/STME.ap_gebvs.RData')
