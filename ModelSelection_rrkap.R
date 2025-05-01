# Model selection

rm(list=ls())
library(asreml)
library(tidyverse)
source('Functions_MTME.R')

load('Data/MTME.z_rr1ap.RData')
load('Data/MTME.z_rr2ap.RData')
load('Data/MTME.z_rr3ap.RData')
load('Data/MTME.z_rr4ap.RData')
load('Data/MTME.z_rr5ap.RData')
load('Data/MTME.z_rr6ap.RData')
load('Data/MTME.z_rr7ap.RData')
load('Data/MTME.z_rr8ap.RData')

lrt.asreml(MTME.z_rr1ap.asr,MTME.z_rr2ap.asr,MTME.z_rr3ap.asr,MTME.z_rr4ap.asr,
           MTME.z_rr5ap.asr,MTME.z_rr6ap.asr,MTME.z_rr7ap.asr,MTME.z_rr8ap.asr)

summary(MTME.z_rr1ap.asr)$aic
summary(MTME.z_rr2ap.asr)$aic
summary(MTME.z_rr3ap.asr)$aic
summary(MTME.z_rr4ap.asr)$aic
summary(MTME.z_rr5ap.asr)$aic
summary(MTME.z_rr6ap.asr)$aic
summary(MTME.z_rr7ap.asr)$aic
summary(MTME.z_rr8ap.asr)$aic

MTME.z_rr1ap.asr$loglik
MTME.z_rr2ap.asr$loglik
MTME.z_rr3ap.asr$loglik
MTME.z_rr4ap.asr$loglik
MTME.z_rr5ap.asr$loglik
MTME.z_rr6ap.asr$loglik
MTME.z_rr7ap.asr$loglik
MTME.z_rr8ap.asr$loglik

rr1ap <- VaPct(mod = MTME.z_rr1ap.asr, k = 1,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr2ap <- VaPct(mod = MTME.z_rr2ap.asr, k = 2,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr3ap <- VaPct(mod = MTME.z_rr3ap.asr, k = 3,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr4ap <- VaPct(mod = MTME.z_rr4ap.asr, k = 4,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr5ap <- VaPct(mod = MTME.z_rr5ap.asr, k = 5,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr6ap <- VaPct(mod = MTME.z_rr6ap.asr, k = 6,
              data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr7ap <- VaPct(mod = MTME.z_rr7ap.asr, k = 7,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct
rr8ap <- VaPct(mod = MTME.z_rr8ap.asr, k = 8,
               data = ILYT_Pheno, TE_fct = 'TraitEnv')$TraitEnv_VaPct

VaPct_rrap <- data.frame(TraitEnv = levels(ILYT_Pheno$TraitEnv)) |>
  left_join(ILYT_Pheno |>
              select(TraitEnv,Trait,Env,Year,Loc) |>
              group_by(TraitEnv) |>
              summarise_all(~unique(.))
            ) |>
  left_join(rr1ap|>rename(TraitEnv=TE_fct,rr1ap=VaPct)) |>
  left_join(rr2ap|>rename(TraitEnv=TE_fct,rr2ap=VaPct)) |>
  left_join(rr3ap|>rename(TraitEnv=TE_fct,rr3ap=VaPct)) |>
  left_join(rr4ap|>rename(TraitEnv=TE_fct,rr4ap=VaPct)) |>
  left_join(rr5ap|>rename(TraitEnv=TE_fct,rr5ap=VaPct)) |>
  left_join(rr6ap|>rename(TraitEnv=TE_fct,rr6ap=VaPct)) |>
  left_join(rr7ap|>rename(TraitEnv=TE_fct,rr7ap=VaPct)) |>
  left_join(rr8ap|>rename(TraitEnv=TE_fct,rr8ap=VaPct)) |>
  pivot_longer(cols = c(rr1ap:rr8ap), names_to = 'model', values_to = 'VaPct') |>
  mutate(model = str_replace(model, "rr([1-8])ap", "NFA\\1")) |>
  arrange(model,TraitEnv) |>
  group_by(model) |>
  mutate(mean.rrk=mean(VaPct)) |>
  group_by(model,Trait) |>
  mutate(mean.rrkT=mean(VaPct)) |>
  glimpse()


colors <- c(
  'NFA1' = '#FCB316',
  'NFA2' = '#006230',
  'NFA3' = '#009FD4',
  'NFA4' = '#5C0E41',
  'NFA5' = '#13294B',
  'NFA6' = '#FF5F05',
  'NFA7' = '#FF5F90',
  'NFA8' = '#F67F99'
)

ggplot(VaPct_rrap, aes(x=Trait, y=VaPct, fill=model)) +
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
ggsave('Figures/Figure5.2.png', width = 7, height = 3, units = 'in', dpi = 300)

# gebvs ----
MTME.ap_gebvs <- gebvs_asreml(MTME.z_rr6ap.asr, k = 6, 
               data = ILYT_Pheno, TE_fct = 'TraitEnv') |>
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

MTME.ap_common_gebvs <- gebvs_common_asreml(MTME.z_rr6ap.asr, k = 6, 
                              data = ILYT_Pheno, TE_fct = 'TraitEnv') |>
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

MTME.ap_gebvs_overall <- MTME.ap_gebvs |>
  group_by(Gkeep, Trait) |>
  summarise(m_gebv=mean(gebv)) |>
  ungroup() |>
  pivot_wider(names_from = Trait, values_from = m_gebv) |>
  mutate(GSI=(213.75*GY/1000)+(0.41*TW)+(-13.32*HD)) |>
  arrange(desc(GSI)) |>
  mutate(rank_GSI=rank(-GSI),
         rank_GY=rank(-GY)) |>
  glimpse()


load('Data/STME.ap_gebvs.RData')

# MTME vs. STME

library(ggplot2)
library(ggpubr)  # For stat_cor()

MTME.ap_gebvs |> 
  rename(gebv_MTME = gebv) |>
  left_join(STME.ap_gebvs |> rename(gebv_STME = gebv)) |>
  ggplot(aes(x = gebv_MTME, y = gebv_STME)) +
  geom_hex(alpha=1) +  # Use hexagonal binning
  geom_smooth(method = 'lm', se = FALSE, linewidth=0.25, color = 'gray', alpha = 0.5) +  # Add regression line
  stat_cor(method = 'pearson', label.x.npc = 'left', label.y.npc = 'top', 
           aes(label = ..r.label..)) +  # Show only correlation coefficient
  scale_fill_gradient(low = '#13294B', high = '#FF5F05') +  # Match color scale
  facet_wrap(~Trait, scales = 'free') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times New Roman'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = 'grey90'),
    strip.text = element_text(size = 12, face = 'bold'),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )

# Save the figure
ggsave('Figures/Figure6.1.png', width = 7, height = 4, units = 'in', dpi = 300)

# common only
MTME.ap_common_gebvs |> 
  rename(gebv_MTME = gebv) |>
  left_join(STME.ap_common_gebvs |> rename(gebv_STME = gebv)) |>
  ggplot(aes(x = gebv_MTME, y = gebv_STME)) +
  geom_hex(alpha=1) +  # Use hexagonal binning
  geom_smooth(method = 'lm', se = FALSE, linewidth=0.25, color = 'gray', alpha = 0.5) +  # Add regression line
  stat_cor(method = 'pearson', label.x.npc = 'left', label.y.npc = 'top', 
           aes(label = ..r.label..)) +  # Show only correlation coefficient
  scale_fill_gradient(low = '#13294B', high = '#FF5F05') +  # Match color scale
  facet_wrap(~Trait, scales = 'free') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times New Roman'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = 'grey90'),
    strip.text = element_text(size = 12, face = 'bold'),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
ggsave('Figures/Figure6.2.png', width = 7, height = 4, units = 'in', dpi = 300)

# GSI vs GY corr ----

MTME.ap_gebvs_overall |>
  select(-c(Gkeep,rank_GSI,rank_GY)) |>
  pivot_longer(cols = c(GY:TW), names_to = 'Trait', values_to = 'gebv') |>
  ggplot(aes(x = GSI, y = gebv)) +
  geom_hex(alpha=1) +  # Use hexagonal binning
  geom_smooth(method = 'lm', se = FALSE, linewidth=0.25, color = 'gray', alpha = 0.5) +  # Add regression line
  stat_cor(method = 'pearson', label.x.npc = 'left', label.y.npc = 'top', 
           aes(label = ..r.label..)) +  # Show only correlation coefficient
  scale_fill_gradient(low = '#13294B', high = '#FF5F05') +  # Match color scale
  facet_wrap(~Trait, scales='free') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times New Roman'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = 'grey90'),
    strip.text = element_text(size = 12, face = 'bold'),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
  )
ggsave('Figures/Figure6.3.png', width = 7, height = 5, units = 'in', dpi = 300)

# Response to selection ----

## MTME ----
MTME.ap_gebvs_overall |>
  mutate(model='MTME') |>
  bind_rows(STME.ap_gebvs_overall|>mutate(model='STME')) |>
  glimpse()

s = 20

MTME.ap_topSel_GSI <- MTME.ap_gebvs_overall |>
  filter(rank_GSI <= s) |>
  pull(Gkeep) |> glimpse()

MTME.ap_topSel_GY <- MTME.ap_gebvs_overall |>
  filter(rank_GY <= s) |>
  pull(Gkeep)

MTME_summary_longer <- MTME.ap_gebvs_overall |>
  summarise(
    # Means of all individuals
    all_mean_GSI = mean(GSI, na.rm = TRUE),
    all_mean_GY = mean(GY, na.rm = TRUE),
    all_mean_TW = mean(TW, na.rm = TRUE),
    all_mean_HD = mean(HD, na.rm = TRUE),
    all_mean_MAT = mean(MAT, na.rm = TRUE),
    all_mean_HT = mean(HT, na.rm = TRUE),
    
    # Means of top 20 individuals based on GSI-MTME
    MTME_GSI_GSI = mean(GSI[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_GY = mean(GY[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_TW = mean(TW[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_HD = mean(HD[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_MAT = mean(MAT[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_HT = mean(HT[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    
    # Means of top 20 individuals based on GY-MTME
    MTME_GY_GSI = mean(GSI[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE),
    MTME_GY_GY = mean(GY[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE),
    MTME_GY_TW = mean(TW[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE),
    MTME_GY_HD = mean(HD[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE),
    MTME_GY_MAT = mean(MAT[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE),
    MTME_GY_HT = mean(HT[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE)
  ) |>
    
  pivot_longer(cols = all_mean_GSI:MTME_GY_HT, 
               names_to = 'Selection_Trait', 
               values_to = 'Value') |>
  separate(Selection_Trait, into = c('Model', 'Selection', 'Trait'), sep = '_', extra = 'merge') |>
  pivot_wider(names_from = Model, values_from = Value) |>
  mutate(MTME=ifelse(is.na(MTME),all,MTME)) |>
  select(-all) |>
  pivot_longer(cols = MTME, names_to = 'Model', values_to = 'RespSel') |>
  pivot_wider(names_from = Selection, values_from = RespSel) |>
  pivot_longer(cols = GSI:GY, names_to = 'SelCrit', values_to = 'RespSel') |>
  mutate(RespSel=RespSel-mean) |>
  select(-mean) |>
  arrange(Model, SelCrit, Trait) |>
  glimpse()

### Plot ----

# Define a named vector for facet renaming and ordering
facet_labels <- c(
  'GSI' = 'GSI (USD/ha)', 
  'GY' = 'GY (kg/ha)', 
  'HD' = 'HD (day)', 
  'MAT' = 'MAT (day)', 
  'HT' = 'HT (cm)', 
  'TW' = 'TW (g/L)'
)

# Ensure proper facet order
MTME_summary_longer <- MTME_summary_longer |>
  mutate(
    Trait = factor(Trait, levels = c('GSI', 'GY', 'HD', 'MAT', 'HT', 'TW')),  # Set facet order
    # ModelSelCrit = factor(paste0(Model, ' (', SelCrit, ')'),
    #                       levels = c('MTME (GSI)', 'STME (GSI)', 'MTME (GY)', 'STME (GY)'))  # Order bars correctly
  )

# Generate plot
ggplot(MTME_summary_longer, aes(x = SelCrit, y = RespSel, fill = SelCrit)) +
  geom_col() +
  geom_label(aes(
    x = SelCrit, y = RespSel,
    label = ifelse(Trait %in% c('GY', 'GSI'), round(RespSel, 0), round(RespSel, 2)),  # Conditional rounding
    vjust = 0.5 + sign(RespSel) * 0.7),  # Adjust label position based on sign
    position = position_dodge(width = 1),
    size = 3, inherit.aes = FALSE, family = 'Times New Roman',
    color = 'black'
  ) +
  facet_wrap(~Trait, scales = 'free', ncol = 2, labeller = as_labeller(facet_labels)) +  # Apply facet renaming and ordering
  scale_fill_manual(
    values = c(
      'GSI' = '#FF5F05',
      'GY' = '#13294B'
    ),
  ) +  
  xlab('') +
  ylab('Response to Selection') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times New Roman'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = 'grey90'),
    strip.text = element_text(size = 12, face = 'bold', family = 'Times New Roman'),  # Ensure facet labels use Times New Roman
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 10, family = 'Times New Roman'),
    axis.title = element_text(size = 12, family = 'Times New Roman'),
    axis.text.x = element_text(angle = 0, hjust = 0.5, family = 'Times New Roman')  # Keep x-axis labels centered
  )
ggsave('Figures/Figure7.1.png', width = 7, height = 5, units = 'in', dpi = 300)

## STME vs. MTME ----
STME.ap_topSel_GSI <- STME.ap_gebvs_overall |>
  filter(rank_GSI <= s) |>
  pull(Gkeep)

STME_MTME_summary_longer <- MTME.ap_gebvs_overall |> 
  summarise(
    # Means of all individuals
    all_mean_GSI = mean(GSI, na.rm = TRUE),
    all_mean_GY = mean(GY, na.rm = TRUE),
    all_mean_TW = mean(TW, na.rm = TRUE),
    all_mean_HD = mean(HD, na.rm = TRUE),
    all_mean_MAT = mean(MAT, na.rm = TRUE),
    all_mean_HT = mean(HT, na.rm = TRUE),
    
    # Means of top 20 individuals based on GSI-MTME
    MTME_GSI_GSI = mean(GSI[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_GY = mean(GY[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_TW = mean(TW[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_HD = mean(HD[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_MAT = mean(MAT[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    MTME_GSI_HT = mean(HT[Gkeep %in% MTME.ap_topSel_GSI], na.rm = TRUE),
    
    # Means of top 20 individuals based on GSI-STME
    STME_GSI_GSI = mean(GSI[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_GY = mean(GY[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_TW = mean(TW[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_HD = mean(HD[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_MAT = mean(MAT[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_HT = mean(HT[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    
  ) |>
  pivot_longer(cols = all_mean_GSI:STME_GSI_HT, 
               names_to = 'Selection_Trait', 
               values_to = 'Value') |>
  separate(Selection_Trait, into = c('Model', 'Selection', 'Trait'), sep = '_', extra = 'merge') |>
  pivot_wider(names_from = Model, values_from = Value) |>
  mutate(MTME=ifelse(is.na(MTME),all,MTME)) |>
  mutate(STME=ifelse(is.na(STME),all,STME)) |>
  select(-all) |>
  pivot_longer(cols = MTME:STME, names_to = 'Model', values_to = 'RespSel') |>
  pivot_wider(names_from = Selection, values_from = RespSel) |>
  pivot_longer(cols = GSI, names_to = 'SelCrit', values_to = 'RespSel') |>
  mutate(RespSel=RespSel-mean) |>
  select(-mean) |>
  arrange(Model, SelCrit, Trait) |>
  glimpse()
  
### Plot ----

# Ensure proper facet order
STME_MTME_summary_longer <- STME_MTME_summary_longer |>
  mutate(
    Trait = factor(Trait, levels = c('GSI', 'GY', 'HD', 'MAT', 'HT', 'TW')),  # Set facet order
    # ModelSelCrit = factor(paste0(Model, ' (', SelCrit, ')'),
    #                       levels = c('MTME (GSI)', 'STME (GSI)', 'MTME (GY)', 'STME (GY)'))  # Order bars correctly
  )

ggplot(STME_MTME_summary_longer, aes(x = Model, y = RespSel, fill = Model)) +
  geom_col() +
  geom_label(aes(
    x = Model, y = RespSel,
    label = ifelse(Trait %in% c('GY', 'GSI'), round(RespSel, 0), round(RespSel, 2)),  # Conditional rounding
    vjust = 0.5 + sign(RespSel) * 0.7),  # Adjust label position based on sign
    position = position_dodge(width = 1),
    size = 3, inherit.aes = FALSE, family = 'Times New Roman',
    color = 'black'
  ) +
  facet_wrap(~Trait, scales = 'free', ncol = 2, labeller = as_labeller(facet_labels)) +  # Apply facet renaming and ordering
  scale_fill_manual(
    values = c(
      'MTME' = '#FF5F05',
      'STME' = '#13294B'
    ),
  ) +  
  xlab('') +
  ylab('Response to Selection') +
  theme_bw() +
  theme(
    text = element_text(family = 'Times New Roman'),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = 'grey90'),
    strip.text = element_text(size = 12, face = 'bold', family = 'Times New Roman'),  # Ensure facet labels use Times New Roman
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    axis.text = element_text(size = 10, family = 'Times New Roman'),
    axis.title = element_text(size = 12, family = 'Times New Roman'),
    axis.text.x = element_text(angle = 0, hjust = 0.5, family = 'Times New Roman')  # Keep x-axis labels centered
  )
ggsave('Figures/Figure7.2.png', width = 7, height = 5, units = 'in', dpi = 300)

# Gen corr ----

gcorr_MTME <- gcorr_asreml(mod = MTME.z_rr6ap.asr, k=6, data = ILYT_Pheno, TE_fct = 'TraitEnv')$gcorr

library(corrplot)

# Define color palette
my_colors <- colorRampPalette(c('#13294b', 'white', '#FF5F0F'))(100)

# Create correlation plot with specified colors
png('Figures/Figure8.1.png', width = 7, height = 7, units = 'in', res = 300)

# Set font to Times New Roman
par(family = "Times")

#png('figures/gen_corr.png', width = 8, height = 8, units = 'in', res = 320)
corrplot(gcorr_MTME, 
         method = 'square', 
         #type = 'lower', 
         col = my_colors,
         number.cex = 1.2, 
         number.digits = 2,
         diag = T,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 90)
dev.off()

# TEST ----
# Create a data frame version

corr_df <- as.data.frame(as.table(as.matrix(gcorr_MTME))) |>
  rename(Row = Var1, Col = Var2, Correlation = Freq) |>
  mutate(
    Row = as.character(Row),
    Col = as.character(Col),
    Trait_row = sapply(strsplit(Row, "-"), `[`, 1),
    Trait_col = sapply(strsplit(Col, "-"), `[`, 1),
    Year_row = sapply(strsplit(Row, "-"), `[`, 2),
    Year_col = sapply(strsplit(Col, "-"), `[`, 2),
    Loc_row = sapply(strsplit(Row, "-"), `[`, 3),
    Loc_col = sapply(strsplit(Col, "-"), `[`, 3),
    Env_row = sapply(strsplit(Row, "-"), function(x) paste(x[-1], collapse = "-")),
    Env_col = sapply(strsplit(Col, "-"), function(x) paste(x[-1], collapse = "-"))
  ) |>
  glimpse()


# Step 3: Create Trait × Trait average correlation matrix
trait_corr_mat <- corr_df |>
  group_by(Trait_row, Trait_col) |>
  summarise(mean_corr = mean(Correlation, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Trait_col, values_from = mean_corr) |>
  column_to_rownames("Trait_row") |>
  as.matrix()
trait_corr_mat

# Create correlation plot with specified colors
png('Figures/Figure8.2.png', width = 3.5, height = 3.5, units = 'in', res = 300)

# Set font to Times New Roman
par(family = "Times")
corrplot(trait_corr_mat, 
         method = 'square', 
         type = 'lower', 
         col = my_colors,
         number.cex = 0.75, 
         number.digits = 2,
         diag = TRUE,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 0,
         addCoef.col = "black")
dev.off()

### Grain yield ----
GY_corr_df <- corr_df |>
  filter(Trait_row=='GY') |>
  filter(Trait_col=='GY') |>
  glimpse()

# Year
year_corr_mat <- GY_corr_df |>
  group_by(Year_row, Year_col) |>
  summarise(mean_corr = mean(Correlation, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Year_col, values_from = mean_corr) |>
  column_to_rownames("Year_row") |>
  as.matrix()
year_corr_mat

corrplot(year_corr_mat, 
         method = 'square', 
         type = 'lower', 
         col = my_colors,
         number.cex = 1.2, 
         number.digits = 2,
         diag = TRUE,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 0,
         addCoef.col = "black")

# Loc
loc_corr_mat <- GY_corr_df |>
  group_by(Loc_row, Loc_col) |>
  summarise(mean_corr = mean(Correlation, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Loc_col, values_from = mean_corr) |>
  column_to_rownames("Loc_row") |>
  as.matrix()
loc_corr_mat

# Create correlation plot with specified colors
png('Figures/Figure8.3.png', width = 3.5, height = 3.5, units = 'in', res = 300)

# Set font to Times New Roman
par(family = "Times")
corrplot(loc_corr_mat, 
         method = 'square', 
         type = 'lower', 
         col = my_colors,
         number.cex = 0.75, 
         number.digits = 2,
         diag = TRUE,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 0,
         addCoef.col = "black")
dev.off()

# Env × Env average correlation matrix
env_corr_mat <- corr_df |>
  group_by(Env_row, Env_col) |>
  summarise(mean_corr = mean(Correlation, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = Env_col, values_from = mean_corr) |>
  column_to_rownames("Env_row") |>
#  mutate_if(is.numeric, ~round(.,1)) |>
  as.matrix()
env_corr_mat

# Create correlation plot with specified colors
png('Figures/Figure8.4.png', width = 7, height = 7, units = 'in', res = 300)

# Set font to Times New Roman
par(family = "Times")
corrplot(env_corr_mat, 
         method = 'square', 
         type = 'lower', 
         col = my_colors,
         number.cex = 1, 
         number.digits = 2,
         diag = T,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 0,
         addCoef.col = "black")
dev.off()


# Accuracy ----

load('Data/MTME.z_rr6ap.RData')

# variance parameters
(vparams <- MTME.z_rr6ap.asr$vparameters)
# latent environmental covariates (loadings)
k <- 6
(Lam <- matrix(vparams[grep("^rr.*fa", names(vparams))], ncol = k))
rownames(Lam) <- levels(ILYT_Pheno$TraitEnv)
# specific variances
(Psi <- diag(vparams[grep("^TraitEnv.*vm", names(vparams))]))
# in this example we dont have much/any specific variance.
colnames(Psi) <- rownames(Psi) <- levels(ILYT_Pheno$TraitEnv)


# additive effects
# cve effects
(BLUPs_cve <- MTME.z_rr6ap.asr$coef$random[grep("rr.*vm", rownames(MTME.z_rr6ap.asr$coef$random)),])
(PEV_cve_diag <- MTME.z_rr6ap.asr$vcoef$random[grep("rr.*vm", rownames(MTME.z_rr6ap.asr$coef$random))])
(PEV_cve_diag <- PEV_cve_diag[grep("Comp", names(BLUPs_cve), invert = T)])
(BLUPs_cve <- BLUPs_cve[grep("Comp", names(BLUPs_cve), invert = T)])
# specific effects
(BLUPs_sve <- MTME.z_rr6ap.asr$coef$random[grep("^TraitEnv.*vm", rownames(MTME.z_rr6ap.asr$coef$random)),])
PEV_sve_diag <- MTME.z_rr6ap.asr$vcoef$random[grep("^TraitEnv.*vm", rownames(MTME.z_rr6ap.asr$coef$random))]
# total (common + specific) effects
BLUPs_tve <- BLUPs_cve + BLUPs_sve
plot(BLUPs_tve, BLUPs_cve)
PEV_tve_diag_approx <- PEV_cve_diag + PEV_sve_diag
# non-additive effects
(BLUPs_ide <- MTME.z_rr6ap.asr$coef$random[grep("^TraitEnv.*ide", rownames(MTME.z_rr6ap.asr$coef$random)),])


############################
# (i) common GET effects

cve_pev <- predict(MTME.z_rr6ap.asr, classify = "TraitEnv:Gkeep",
                   only = "rr(TraitEnv, 6):vm(Gkeep, Ginv.sparse)",
                   vcov = TRUE, maxit = 1, pworkspace = 6e8)
head(cve_pev$pvals)
PEV_cve <- as.matrix(cve_pev$vcov)

# Quick check of BLUPs and PEVs
# BLUPs first
plot(BLUPs_cve, cve_pev$pvals$predicted.value); abline(a=0, b=1)
range(BLUPs_cve - cve_pev$pvals$predicted.value)
# PEVs next
plot(diag(PEV_cve), cve_pev$pvals$std.error^2); abline(a=0, b=1)
plot(diag(PEV_cve), PEV_cve_diag); abline(a=0, b=1)
range(diag(PEV_cve) - PEV_cve_diag)

# obtain accuracies
VAR_cve <- kronecker(Lam %*% t(Lam), G.dense.trim)
ACC_cve <- sqrt(1 - diag(PEV_cve)/diag(VAR_cve))
# note that this assumes the variance is given by g_ii lamlam', where g_ii is the ith diagonal element of G
plot(ACC_cve, BLUPs_cve)
# might be worth taking a look at the groupings here


############################
# (ii) common + specific GET effects

tve_pev <- predict(MTME.z_rr6ap.asr, classify = "TraitEnv:Gkeep",
                   only = c("rr(TraitEnv, 2):vm(Gkeep, G.dense.trim)", "TraitEnv:vm(Gkeep, G.dense.trim)"),
                   vcov = TRUE, maxit = 1, pworkspace = 6e8)
head(tve_pev$pvals)
PEV_tve <- as.matrix(tve_pev$vcov)

# Quick check of BLUPs and PEVs
# BLUPs first
plot(BLUPs_tve, tve_pev$pvals$predicted.value); abline(a=0, b=1)
range(BLUPs_tve - tve_pev$pvals$predicted.value)
# PEVs next
plot(diag(PEV_tve), tve_pev$pvals$std.error^2); abline(a=0, b=1)
# plot(diag(PEV_tve), PEV_tve_diag_approx); abline(a=0, b=1)
# range(diag(PEV_tve) - PEV_tve_diag_approx)

# obtain accuracies
VAR_tve <- kronecker(Lam %*% t(Lam) + Psi, G.dense.trim)
ACC_tve <- sqrt(1 - diag(PEV_tve)/diag(VAR_tve))
plot(ACC_tve, ACC_cve) # difference arising for that one environment where 
# Psi is non-zero, as expected.
# note that this assumes the variance is given by g_ii (lamlam' + Psi), where g_ii is the ith diagonal element of G
plot(ACC_tve, BLUPs_tve)


############################
# (iii) some linear combination of (i) or (ii)

# create weight vector
w <- rbind(0.5, -0.1, 2)
w <- w/sum(w) # sum to zero
w

# create BLUP for the linear combination - selection index (use the CVE effects as the example)
BLUPs_si <- matrix(BLUPs_cve, ncol = 3) %*% w
# obtain accuracies
VAR_cve_blups <- VAR_cve - PEV_cve
VAR_si_blups <- kronecker(t(w), diag(nvars)) %*% VAR_cve_blups %*% kronecker(w, diag(nvars))
VAR_si <- kronecker(t(w) %*% Lam %*% t(Lam) %*% w, G.dense.trim)
PEV_si <- VAR_si - VAR_si_blups
ACC_si <- sqrt(1 - diag(PEV_si)/diag(VAR_si))
plot(ACC_si, BLUPs_si)
# might be worth taking a look at the groupings here

# Might be of interest: implementing constraints into the index, e.g., desired gains 
# (atm its purely (trait/economic) importance to the aggregate genotype)

# end of script