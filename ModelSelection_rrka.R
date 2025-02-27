# Model selection

rm(list=ls())

library(tidyverse)
source('Functions_MTME.R')

load('Data/MTME.z_rr1a.RData')
load('Data/MTME.z_rr2a.RData')
load('Data/MTME.z_rr3a.RData')
load('Data/MTME.z_rr4a.RData')
load('Data/MTME.z_rr5a.RData')

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

ggplot(VaPct_rra, aes(x=model, y=mean.rrkT, color=Trait, group=Trait)) +
  geom_point() +
  geom_line() +  # Add this line to connect the points
  ylab('Va explained (%)') +
  theme_bw()

# update ----

# By trait
VaPct_Trra <- data.frame(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |>
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
  arrange(model, TraitEnv) |>
  group_by(model, Trait) |>
  summarise(
    mean.rrkT = mean(VaPct, na.rm = TRUE),
    sd.rrkT = sd(VaPct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ymin = mean.rrkT - sd.rrkT, ymax = mean.rrkT + sd.rrkT) |>
  glimpse()

ggplot(VaPct_Trra, aes(x=Trait, y=mean.rrkT, fill=model, group=model)) +
  geom_col(position = "dodge", alpha=0.6, color='black') +  # Use geom_col() instead of geom_bar()
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.2, position = position_dodge(width = 0.9)) +  # Adjust error bars to match bars
  ylab('Va explained (%)') +
  theme_bw()

# By Env
VaPct_Erra <- data.frame(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |>
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
  arrange(model, TraitEnv) |>
  group_by(model, Env) |>
  summarise(
    mean.rrkT = mean(VaPct, na.rm = TRUE),
    sd.rrkT = sd(VaPct, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ymin = mean.rrkT - sd.rrkT, ymax = mean.rrkT + sd.rrkT) |>
  glimpse()

ggplot(VaPct_Erra|>filter(model=='rr5a'), aes(x=Env, y=mean.rrkT, fill=model, group=model)) +
  geom_col(position = "dodge", alpha=0.6, color='black') +  # Use geom_col() instead of geom_bar()
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=0.2, position = position_dodge(width = 0.9)) +  # Adjust error bars to match bars
  ylab('Va explained (%)') +
  theme_bw()


