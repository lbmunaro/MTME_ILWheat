# Model selection

rm(list=ls())
library(asreml)
library(tidyverse)
source('Functions_MTME.R')

load('Data/MTME.z_rr1ap1rc.RData')
load('Data/MTME.z_rr2ap1rc.RData')
load('Data/MTME.z_rr3ap1rc.RData')
load('Data/MTME.z_rr4ap1rc.RData')
load('Data/MTME.z_rr5ap1rc.RData')
# load('Data/MTME.z_rr6ap1rc.RData')

lrt.asreml(MTME.z_rr1ap1rc.asr,MTME.z_rr2ap1rc.asr
           #,MTME.z_rr3ap1rc.asr,MTME.z_rr4ap1rc.asr,MTME.z_rr5ap1rc.asr
           #MTME.z_rr6ap1rc.asr
           )

rr1ap1rc <- VaPct(mod = MTME.z_rr1ap1rc.asr, k = 1,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr2ap1rc <- VaPct(mod = MTME.z_rr2ap1rc.asr, k = 2,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
# rr3ap1rc <- VaPct(mod = MTME.z_rr3ap1rc.asr, k = 3,
#                data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
# rr4ap1rc <- VaPct(mod = MTME.z_rr4ap1rc.asr, k = 4,
#                data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
# rr5ap1rc <- VaPct(mod = MTME.z_rr5ap1rc.asr, k = 5,
#                data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
# rr6ap1rc <- VaPct(mod = MTME.z_rr6ap1rc.asr, k = 6,
#               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct

VaPct_rrap1rc <- data.frame(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |>
  left_join(ILYT_Pheno |>
              select(TraitEnv,Trait,Env,Year,Loc) |>
              group_by(TraitEnv) |>
              summarise_all(~unique(.))
            ) |>
  left_join(rr1ap1rc|>rename(TraitEnv=TE_fct,rr1ap1rc=VaPct)) |>
  left_join(rr2ap1rc|>rename(TraitEnv=TE_fct,rr2ap1rc=VaPct)) |>
  # left_join(rr3ap1rc|>rename(TraitEnv=TE_fct,rr3ap1rc=VaPct)) |>
  # left_join(rr4ap1rc|>rename(TraitEnv=TE_fct,rr4ap1rc=VaPct)) |>
  # left_join(rr5ap1rc|>rename(TraitEnv=TE_fct,rr5ap1rc=VaPct)) |>
  # left_join(rr6ap1rc|>rename(TraitEnv=TE_fct,rr6ap1rc=VaPct)) |>
  pivot_longer(cols = c(rr1ap1rc:rr2ap1rc), names_to = 'model', values_to = 'VaPct') |>
  arrange(model,TraitEnv) |>
  group_by(model) |>
  mutate(mean.rrk=mean(VaPct)) |>
  group_by(model,Trait) |>
  mutate(mean.rrkT=mean(VaPct)) |>
  glimpse()

colors <- c(
  'rr1ap1rc' = '#FCB316',
  'rr2ap1rc' = '#006230'#,
  # 'rr3ap1rc' = '#009FD4',
  # 'rr4ap1rc' = '#5C0E41',
  # 'rr5ap1rc' = '#13294B'#,
  # 'rr6ap1rc' = '#FF5F05'
)

ggplot(VaPct_rrap1rc, aes(x=Trait, y=VaPct, fill=model)) +
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.5, alpha=0.5) +  # Dodge boxplots
  geom_jitter(aes(color=model),position = position_dodge(width = 0.8),
              alpha=0.75, shape=3, size=1) +  # Dodge boxplots
  
  scale_y_continuous(name = bquote(V[a] ~ 'explained (%)'),
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

