# Tables & Figures

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.

load('Data/ILYT_Pheno-Gmatrix.RData')

ILYT_Pheno |> glimpse()

# Table 1 ----

ILYT_Pheno_summary <- ILYT_Pheno |>
  droplevels.data.frame() |>
  mutate(TotGenAll = length(unique(Gen))-1,
         TotPlots = n()) |>
  group_by(Trait) |>
  mutate(Pheno_mAll = mean(Pheno_SI, na.rm = T),
         naALL = sum(is.na(Pheno_SI))) |>
  group_by(TraitEnv) |>
  reframe(TotGen = length(unique(Gen)),
          Plots = n(),
          RepPct = Plots/TotGen-1,
          TotGenAll = unique(TotGenAll),
          TotPlots = unique(TotPlots),
          Trait = unique(Trait),
          Pheno_min = min(Pheno_SI, na.rm = T),
          Pheno_m = mean(Pheno_SI, na.rm = T),
          Pheno_max = max(Pheno_SI, na.rm = T),
          Pheno_na = sum(is.na(Pheno_SI)),
          Pheno_mAll = unique(Pheno_mAll),
          naALL = unique(naALL),
          Env = unique(Env),
          Year = unique(Year),
          Loc = unique(Loc)) |>
  glimpse()

ILYT_Pheno_summary_wide <- ILYT_Pheno_summary |>
  dplyr::select(-TraitEnv) |>
  pivot_wider(
    names_from = Trait,
    values_from = c(Pheno_m, Pheno_min, Pheno_max, Pheno_na, Pheno_mAll, naALL),
    names_glue = '{Trait}_{.value}'  # This renames the columns to include Trait
  ) |>
  arrange(Env) |>
  relocate(Env,.before = TotGen) |>
  write.csv('temp/Table1.csv') |>
  glimpse()

# Figure 1 ----
library(raster) # Geographic Data Analysis and Modeling
library(ggspatial) # Spatial Data Framework for ggplot2
library(RColorBrewer) # ColorBrewer Palettes

loc_coord <- data.frame(loc=c('Neo', 'Adv', 'Stp', 'Urb'),
                        lat=c(39.23274,38.415655,38.8712128,40.05833),
                        long=c(-88.38207,-89.463842,-88.8825766,-88.22937)) |>
  glimpse()


states <- c('illinois')
highlight_states <- c('illinois')

map_data <- map_data('state') %>%
  filter(region %in% states)

county <- map_data('county') |>
  filter(region %in% states) |>
  mutate(subregion=str_replace_all(subregion,' ',''),
         subregion=str_replace_all(subregion,'[.]',''))

wheat_ha <- read.csv('Data/2024_fsa_acres_web_080924.csv') |>
  mutate_if(is.character,~tolower(.)) |>
  dplyr::filter(Crop%in%'wheat') |>
  mutate(Planted.Acres=str_replace(Planted.Acres,',',''),
         Planted.Acres=as.numeric(Planted.Acres)) |>
  group_by(State,County,Crop) |>
  summarise(Planted.Acres=sum(Planted.Acres,na.rm=T)) |>
  mutate(region=State,
         subregion=County,
         subregion=str_replace_all(subregion,
                                   c('dewitt'='de witt','dupage'='du page','st. clair'='st clair','dekalb'='de kalb' ))) |>
  mutate(subregion=str_replace_all(subregion,' ',''),
         subregion=str_replace_all(subregion,'[.]',''))|>
  mutate(Planted.Hectares = round(Planted.Acres*0.404686,0)) |>
  glimpse()

breaks <- c(1, 500, 1000, 2500, 5000, 10000, 20000, 40000)
labels <- c('< 500', '501 to 1,000', '1,001 to 2,500', '2,501 to 5,000',
            '5,001 to 10,000', '10,001 to 20,000', '> 20,000')

county_ha <- county |>
  left_join(wheat_ha) |>
  mutate(Planted.Hectares = ifelse(Planted.Hectares == 0, NA, Planted.Hectares)) |>
  mutate(classes = cut(Planted.Hectares, breaks=breaks, labels = labels, include.lowest = TRUE, right = FALSE)) |>
  glimpse()

ggplot() +
  geom_polygon(data = county_ha, aes(x = long, y = lat, group = group, fill = classes), color = 'gray70') +
  scale_fill_brewer(name = 'Planted area (ha)', palette = 'Oranges', na.value = 'white', drop= TRUE, 
                    labels = c('< 500', '501 to 1,000', '1,001 to 2,500', '2,501 to 5,000',
                               '5,001 to 10,000', '10,001 to 20,000', '> 20,000',
                               'Not estimated')) +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), fill = 'transparent', color = 'gray50') +
  geom_point(data = loc_coord, aes(x = long, y = lat, group = NA), color = '#13294B', size = 1) +
  geom_point(data = loc_coord, aes(x = long, y = lat), color = '#13294B', size = 2, shape = 21) + 
  geom_text(data = loc_coord, aes(x = long, y = lat, group = NA, label = loc),
            angle = 0, vjust = 0.5, hjust = -0.2, color = '#13294B', size = 5, family = 'Times New Roman') +
  coord_fixed(1.3) +
  scale_x_continuous(name = 'Longitude', labels = function(x) paste0(x, '°')) +
  scale_y_continuous(name = 'Latitude', labels = function(y) paste0(y, '°')) +
  theme_bw() +
  theme(panel.grid = element_line(color = 'white'),
        panel.background = element_rect(fill = 'gray90'),
        panel.border = element_blank(),
        legend.title = element_text(size = 12, family = 'Times New Roman'),
        legend.text = element_text(size = 10, family = 'Times New Roman'),
        axis.text = element_text(size = 10, family = 'Times New Roman'),
        axis.title = element_text(size = 10, family = 'Times New Roman'))
ggsave('Figures/Figure1.tif', width = 7, height = 5, units = 'in', dpi = 300)
ggsave('Figures/Figure1.png', width = 7, height = 5, units = 'in', dpi = 300)

#library(scales) # for the label formatting function

# Figure 2 ----
# Connectivity

library(reshape2)

ILYT_Pheno |>
  glimpse()

# Prepare the data by creating a list of genotypes for each environment
env_genotypes <- ILYT_Pheno |>
  filter(!is.na(Gen)) |>
  droplevels() |>
  group_by(Env) |>
  summarize(Gen = list(unique(Gen))) |>
  glimpse()

# Create a connectivity matrix (number of shared genotypes)
env_list <- env_genotypes$Env
connectivity_matrix <- matrix(0, nrow = length(env_list), ncol = length(env_list),
                              dimnames = list(env_list, env_list))

# Compute shared genotypes for each pair of environments
for (i in seq_along(env_list)) {
  for (j in seq_along(env_list)) {
    common_genotypes <- intersect(env_genotypes$Gen[[i]], env_genotypes$Gen[[j]])
    connectivity_matrix[i, j] <- length(common_genotypes)
  }
}

# Convert the matrix to long format for ggplot2
connectivity_long <- melt(connectivity_matrix, varnames = c('Env1', 'Env2'), value.name = 'Shared')

# Create two subsets: upper and lower triangle
upper_triangle <- connectivity_long |> filter(as.integer(Env1) > as.integer(Env2))
diag_triangle <- connectivity_long |> filter(as.integer(Env1) == as.integer(Env2))
lower_triangle <- connectivity_long |> filter(as.integer(Env1) <= as.integer(Env2))

lower_triangle$Env2 <- factor(lower_triangle$Env2, levels = rev(levels(lower_triangle$Env2)))
upper_triangle$Env2 <- factor(upper_triangle$Env2, levels = rev(levels(upper_triangle$Env2)))
diag_triangle$Env2 <- factor(diag_triangle$Env2, levels = rev(levels(diag_triangle$Env2)))

## Plot ----
# Plot with clear alignment and appearance
ggplot() +
  # Lower triangle: heatmap
  geom_tile(data = lower_triangle, aes(Env1, Env2, fill = Shared), color = 'black') +
  #  geom_tile(data = lower_triangle, aes(Env1, Env2), fill = 'white', color = 'black') +
  # Upper triangle and diagonal: text
  geom_text(data = lower_triangle, aes(Env1, Env2, label = Shared), 
            size = 5, family = 'Times New Roman', color = 'black') +
  geom_text(data = diag_triangle, aes(Env1, Env2, label = Shared), fontface = 'bold', 
            size = 5, family = 'Times New Roman', color = 'black') +
  #  scale_fill_continuous2(low='blue', mid='green', high='red')
  scale_fill_viridis_c(name = 'Genotypes', option = 'turbo', alpha = 0.7) +
  #  scale_fill_gradient(low = 'blue', mid='green', high = 'red', na.value = 'white', limits = c(0, max(connectivity_long$Shared))) +
  theme_light() +
  theme(
    axis.text = element_text(size = 10, family = 'Times New Roman'),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_text(size = 12, family = 'Times New Roman'),
    legend.text = element_text(size = 10, family = 'Times New Roman'),
    panel.grid = element_blank(),
    panel.border = element_rect(color = 'white', fill = NA, size = 1),
    legend.position = c(0.98, 0.98),  # Move legend inside upper triangle
    legend.justification = c(1, 1)  # Adjust legend alignment
  ) +
  labs(fill = '')
ggsave('Figures/Figure2.png', width = 7, height = 4, units = 'in', dpi = 300)
ggsave('Figures/Figure2.tif', width = 7, height = 4, units = 'in', dpi = 300)

# End ----
length(unique(ILYT_Pheno$Gen))-1
length(unique(ILYT_Pheno$Gkeep))-1
length(unique(ILYT_Pheno$Gdrop))-1

ILYT_Pheno_summary |>
  ggplot(aes(x = Env, y = Pheno_m, fill = Year)) +
  geom_col() +
  facet_wrap(~Trait, scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ILYT_Pheno_summary |>
  ggplot(aes(x = Env, y = Pheno_na, fill = Year)) +
  geom_col() +
  facet_wrap(~Trait) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

length(unique(ILYT_Pheno$Env))

length(unique(ILYT_Pheno$TraitEnv))

ILYT_Pheno |>
  ggplot(aes(x=Env, y=Pheno_SI, fill=Year)) +
  geom_boxplot() +
  facet_wrap(~Trait, scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))