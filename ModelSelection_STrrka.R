# Model selection

rm(list=ls())

library(tidyverse)
source('Functions_MTME.R')

load('Data/STME.z_rr.a.RData')

rr1a <- VaPct(mod = MTME.z_rr1a.asr, k = 1,
              data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr2a <- VaPct(mod = MTME.z_rr2a.asr, k = 2,
              data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr3a <- VaPct(mod = MTME.z_rr3a.asr, k = 3,
              data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr4a <- VaPct(mod = MTME.z_rr4a.asr, k = 4,
              data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr5a <- VaPct(mod = MTME.z_rr5a.asr, k = 5,
              data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
# rr6a <- VaPct(mod = MTME.z_rr5a.asr, k = 6,
#               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct

VaPct_rra <- data.frame(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |>
  left_join(ILYT_Pheno |>
              select(TraitEnv,Trait,Env,Year,Loc) |>
              group_by(TraitEnv) |>
              summarise_all(~unique(.))
            ) |>
  left_join(rr1a|>rename(TraitEnv=TE_fct,rr1a=VaPct)) |>
  left_join(rr2a|>rename(TraitEnv=TE_fct,rr2a=VaPct)) |>
  left_join(rr3a|>rename(TraitEnv=TE_fct,rr3a=VaPct)) |>
  left_join(rr4a|>rename(TraitEnv=TE_fct,rr4a=VaPct)) |>
  left_join(rr5a|>rename(TraitEnv=TE_fct,rr5a=VaPct)) |>
  pivot_longer(cols = c(rr1a:rr5a), names_to = 'model', values_to = 'VaPct') |>
  arrange(model,TraitEnv) |>
  group_by(model) |>
  mutate(mean.rrk=mean(VaPct)) |>
  group_by(model,Trait) |>
  mutate(mean.rrkT=mean(VaPct)) |>
  glimpse()

ggplot(VaPct_rra, aes(x=Trait, y=VaPct, fill=model)) +
  geom_jitter(aes(color=model),position = position_dodge(width = 0.8), alpha=0.5) +  # Dodge boxplots
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.5, alpha=0.5) +  # Dodge boxplots
  
  scale_y_continuous(name = bquote(V[a] ~ 'explained (%)'),
                     breaks = seq(0, 100, by = 20)) +
  
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 10, family = 'Times New Roman'),
    axis.title = element_text(size = 10, family = 'Times New Roman')
  )
