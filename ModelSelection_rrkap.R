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

lrt.asreml(MTME.z_rr1ap.asr,MTME.z_rr2ap.asr,MTME.z_rr3ap.asr,MTME.z_rr4ap.asr,MTME.z_rr5ap.asr,MTME.z_rr6ap.asr)

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
  pivot_longer(cols = c(rr1ap:rr6ap), names_to = 'model', values_to = 'VaPct') |>
  arrange(model,TraitEnv) |>
  group_by(model) |>
  mutate(mean.rrk=mean(VaPct)) |>
  group_by(model,Trait) |>
  mutate(mean.rrkT=mean(VaPct)) |>
  glimpse()

colors <- c(
  'rr1ap' = '#FCB316',
  'rr2ap' = '#006230',
  'rr3ap' = '#009FD4',
  'rr4ap' = '#5C0E41',
  'rr5ap' = '#13294B',
  'rr6ap' = '#FF5F05'
)

ggplot(VaPct_rrap, aes(x=Trait, y=VaPct, fill=model)) +
  geom_jitter(aes(color=model),position = position_dodge(width = 0.8), alpha=1) +  # Dodge boxplots
  geom_boxplot(position = position_dodge(width = 0.8), width = 0.5, alpha=0.5) +  # Dodge boxplots
  
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


# Response to selection ----

MTME.ap_gebvs_overall |>
  mutate(model='MTME') |>
  bind_rows(STME.ap_gebvs_overall|>mutate(model='STME')) |>
  glimpse()

# No selected candidates
s = 50

STME.ap_topSel_GSI <- STME.ap_gebvs_overall |>
  filter(rank_GSI <= s) |>
  pull(Gkeep)

STME.ap_topSel_GY <- STME.ap_gebvs_overall |>
  filter(rank_GY <= s) |>
  pull(Gkeep)

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
    MTME_GY_HT = mean(HT[Gkeep %in% MTME.ap_topSel_GY], na.rm = TRUE),
    
    # Means of top 20 individuals based on GSI-STME
    STME_GSI_GSI = mean(GSI[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_GY = mean(GY[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_TW = mean(TW[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_HD = mean(HD[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_MAT = mean(MAT[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    STME_GSI_HT = mean(HT[Gkeep %in% STME.ap_topSel_GSI], na.rm = TRUE),
    
    # Means of top 20 individuals based on GY-STME
    STME_GY_GSI = mean(GSI[Gkeep %in% STME.ap_topSel_GY], na.rm = TRUE),
    STME_GY_GY = mean(GY[Gkeep %in% STME.ap_topSel_GY], na.rm = TRUE),
    STME_GY_TW = mean(TW[Gkeep %in% STME.ap_topSel_GY], na.rm = TRUE),
    STME_GY_HD = mean(HD[Gkeep %in% STME.ap_topSel_GY], na.rm = TRUE),
    STME_GY_MAT = mean(MAT[Gkeep %in% STME.ap_topSel_GY], na.rm = TRUE),
    STME_GY_HT = mean(HT[Gkeep %in% STME.ap_topSel_GY], na.rm = TRUE)
  ) |>
  pivot_longer(cols = all_mean_GSI:STME_GY_HT, 
               names_to = 'Selection_Trait', 
               values_to = 'Value') |>
  separate(Selection_Trait, into = c('Model', 'Selection', 'Trait'), sep = '_', extra = 'merge') |>
  pivot_wider(names_from = Model, values_from = Value) |>
  mutate(MTME=ifelse(is.na(MTME),all,MTME)) |>
  mutate(STME=ifelse(is.na(STME),all,STME)) |>
  select(-all) |>
  pivot_longer(cols = MTME:STME, names_to = 'Model', values_to = 'RespSel') |>
  pivot_wider(names_from = Selection, values_from = RespSel) |>
  pivot_longer(cols = GSI:GY, names_to = 'SelCrit', values_to = 'RespSel') |>
  mutate(RespSel=RespSel-mean) |>
  select(-mean) |>
  arrange(Model, SelCrit, Trait) |>
  glimpse()

## plot ----
library(ggpattern)

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
    ModelSelCrit = factor(paste0(Model, ' (', SelCrit, ')'),
                          levels = c('MTME (GSI)', 'STME (GSI)', 'MTME (GY)', 'STME (GY)'))  # Order bars correctly
  )

# Generate plot
ggplot(MTME_summary_longer, aes(x = ModelSelCrit, y = RespSel, fill = ModelSelCrit, pattern = SelCrit)) +
  geom_col_pattern(
    alpha = 1, 
    pattern_color = 'white',  # Color inside the pattern
    pattern_density = 0.1,    # Density of the pattern
    pattern_spacing = 0.02    # Spacing of the pattern
  ) +
  geom_label(aes(
    x = ModelSelCrit, y = RespSel,
    label = ifelse(Trait %in% c('GY', 'GSI'), round(RespSel, 0), round(RespSel, 2)),  # Conditional rounding
    vjust = 0.5 + sign(RespSel) * 0.7),  # Adjust label position based on sign
    position = position_dodge(width = 1),
    size = 3, inherit.aes = FALSE, family = 'Times New Roman',
    color = 'black'
  ) +
  facet_wrap(~Trait, scales = 'free', ncol = 2, labeller = as_labeller(facet_labels)) +  # Apply facet renaming and ordering
  scale_fill_manual(
    values = c(
      'MTME (GSI)' = '#13294B', 'STME (GSI)' = '#FF5F05',
      'MTME (GY)' = '#13294B', 'STME (GY)' = '#FF5F05'
    ),
    labels = c(
      'MTME (GSI)', 'STME (GSI)',
      'MTME (GY)', 'STME (GY)'
    )
  ) +  
  scale_pattern_manual(
    values = c('GSI' = 'none', 'GY' = 'stripe'),  # Ensure GSI is solid, GY is striped
    guide = 'none'  # Remove extra legend
  ) +
  guides(
    fill = guide_legend('Model & Selection Criterion',
                        override.aes = list(pattern = c('none', 'none','stripe', 'stripe')))  # Ensure correct legend display
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

# Save the figure
ggsave('Figures/Figure7.png', width = 12, height = 7, units = 'in', dpi = 300)


# Gen corr ----

gcorr_MTME <- gcorr_asreml(mod = MTME.z_rr6ap.asr, k=6, data = ILYT_Pheno, TE_fct = 'TraitEnv')$gcorr

summary(gcorr_MTME)
gcorr_MTME[1:5,1:5]

indices <- which(gcorr_MTME > 1, arr.ind = TRUE)
data.frame(Row = rownames(gcorr_MTME)[indices[,1]], 
           Column = colnames(gcorr_MTME)[indices[,2]], 
           Value = gcorr_MTME[indices])

gcorr_MTME2 <- gcorr_MTME
gcorr_MTME2[gcorr_MTME2 > 1] <- NA


library(corrplot)

# Define color palette
my_colors <- colorRampPalette(c('#FF5F0F', 'white', '#13294b'))(100)

# Create correlation plot with specified colors
png('Figures/Figure8.1.png', width = 12, height = 7, units = 'in', res = 300)
#png('figures/gen_corr.png', width = 8, height = 8, units = 'in', res = 320)
corrplot(gcorr_MTME2, 
         method = 'circle', 
         type = 'lower', 
         col = my_colors,
         number.cex = 1.2, 
         number.digits = 2,
         diag = T,
         tl.col = 'black', 
         tl.cex = 0.75,
         tl.srt = 10)
dev.off()
