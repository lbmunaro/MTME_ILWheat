# Tables & Figures

# Clean workspace
rm(list = objects())  # Removes all objects from the environment.

# Packages ----
library(tidyverse) # R packages for data science.

load('Data/ILYT_Pheno-Gmatrix.RData')

# Table 1 ----
ILYT_Pheno2 <- ILYT_Pheno |>
  mutate(Loc = factor(recode(Loc,
                             "Addieville, IL" = "Adv",
                             "Neoga, IL" = "Neo",
                             "St Peter, IL" = "Stp",
                             "Urbana, IL" = "Urb"),
                      levels = c("Adv", "Stp", "Neo", "Urb"))) |>
  mutate(Year = factor(recode(Year,
                             "2022" = "22",
                             "2023" = "23",
                             "2024" = "24"),
                      levels = c("22", "23", "24"))) |>
  mutate(Labels=as.factor(paste0(Year,Loc))) |>
  glimpse()

ILYT_Pheno_summary <- ILYT_Pheno2 |>
#  filter(!is.na(Gen)) |>
  droplevels.data.frame() |>
  mutate(TotGenAll = length(unique(Gen))-1,
         TotPlots = n()) |>
  group_by(Trait) |>
  mutate(Pheno_mAll = mean(Pheno, na.rm = T),
         naALL = sum(is.na(Pheno))) |>
  group_by(TraitEnv) |>
  reframe(TotGen = length(unique(Gen)),
          Plots = n(),
          RepPct = Plots/TotGen-1,
          TotGenAll = unique(TotGenAll),
          TotPlots = unique(TotPlots),
          Trait = unique(Trait),
          Pheno_min = min(Pheno, na.rm = T),
          Pheno_m = mean(Pheno, na.rm = T),
          Pheno_max = max(Pheno, na.rm = T),
          Pheno_na = sum(is.na(Pheno)),
          Pheno_mAll = unique(Pheno_mAll),
          naALL = unique(naALL),
          Env = unique(Labels),
          Year = unique(Year),
          Loc = unique(Loc)) |>
  glimpse()

ILYT_Pheno_summary_wide <- ILYT_Pheno_summary |>
  select(-TraitEnv) |>
  #glimpse()
  pivot_wider(
    names_from = Trait,
    values_from = c(Pheno_m, Pheno_min, Pheno_max, Pheno_na, Pheno_mAll, naALL),
    names_glue = "{Trait}_{.value}"  # This renames the columns to include Trait
  ) |>
  arrange(desc(Loc), Year) |>
  write.csv('Tables/Table1.csv') |>
  glimpse()



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
  facet_wrap(~Trait, scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

length(unique(ILYT_Pheno$Env))

length(unique(ILYT_Pheno$TraitEnv))

ILYT_Pheno |>
  ggplot(aes(x=Env, y=Pheno, fill=Year)) +
  geom_boxplot() +
  facet_wrap(~Trait, scales = 'free') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Figure 1 ----

#library(sf) # Simple Features for R
library(raster) # Geographic Data Analysis and Modeling
library(ggspatial) # Spatial Data Framework for ggplot2
library(RColorBrewer) # ColorBrewer Palettes

loc_coord <- data.frame(loc=c("Neo", "Adv", "Stp", "Urb"),
                        lat=c(39.23274,38.415655,38.8712128,40.05833),
                        long=c(-88.38207,-89.463842,-88.8825766,-88.22937)) |>
  glimpse()

states <- c("illinois")
highlight_states <- c("illinois")

map_data <- map_data("state") %>%
  filter(region %in% states)

county <- map_data("county") |>
  filter(region %in% states) |>
  mutate(subregion=str_replace_all(subregion," ",""),
         subregion=str_replace_all(subregion,"[.]",""))

wheat_ac <- read.csv("Data/2024_fsa_acres_web_080924.csv") |>
  mutate_if(is.character,~tolower(.)) |>
  dplyr::filter(Crop%in%"wheat") |>
  mutate(Planted.Acres=str_replace(Planted.Acres,",",""),
         Planted.Acres=as.numeric(Planted.Acres)) |>
  group_by(State,County,Crop) |>
  summarise(Planted.Acres=sum(Planted.Acres,na.rm=T)) |>
  mutate(region=State,
         subregion=County) |>
  mutate(subregion=str_replace_all(subregion," ",""),
         subregion=str_replace_all(subregion,"[.]",""))|>
  glimpse()

breaks <- c(1, 1000, 5000, 10000, 25000, 50000, 100000)
labels <- c("< 1000", "1001 to 5000", "5001 to 10000", "10001 to 25000", "25001 to 50000", "> 50001")

county_ac <- county |>
  left_join(wheat_ac) |>
  mutate(Planted.Acres = ifelse(Planted.Acres == 0, NA, Planted.Acres)) |>
  mutate(classes = cut(Planted.Acres, breaks=breaks, labels = labels, include.lowest = TRUE, right = FALSE)) |>
  glimpse()

ggplot() +
  geom_polygon(data = county_ac, aes(x = long, y = lat, group = group, fill = classes), color = "gray70") +
  scale_fill_brewer(name = "Planted acres", palette = "Oranges", na.value = "white", drop= TRUE, 
                    labels = c("< 1000", "1001 to 5000", "5001 to 10000", "10001 to 25000", "25001 to 50000", "> 50001", "Not estimated")) +
  geom_polygon(data = map_data, aes(x = long, y = lat, group = group), fill = "transparent", color = "gray50") +
  geom_point(data = loc_coord, aes(x = long, y = lat, group = NA), color = "#13294B", size = 1) +
  geom_point(data = loc_coord, aes(x = long, y = lat), color = "#13294B", size = 2, shape = 21) + 
  geom_text(data = loc_coord, aes(x = long, y = lat, group = NA, label = loc),
            angle = 0, vjust = 0.5, hjust = -0.2, color = "#13294B", size = 6) +
  coord_fixed(1.3) +
  scale_x_continuous(name = "Longitude", labels = function(x) paste0(x, "°")) +
  scale_y_continuous(name = "Latitude", labels = function(y) paste0(y, "°")) +
  theme_bw() +
  theme(panel.grid = element_line(color = 'white'),
        panel.background = element_rect(fill = "gray90"),
        panel.border = element_blank(),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))
ggsave('Figures/Figure1.png', width = 5, height = 6, units = "in", dpi = 320)
ggsave('../PAG2025/projPAG2025/Figures/Fig1poster.png', width = 5.5, height = 6.5, units = "in", dpi = 320)

library(scales) # for the label formatting function




# Figure 2 ----
# Connectivity

library(reshape2)

ILYT_Pheno2 |>
  glimpse()

# Prepare the data by creating a list of genotypes for each environment
env_genotypes <- ILYT_Pheno2 |>
  filter(!is.na(Gen)) |>
  droplevels() |>
  group_by(Labels) |>
  summarize(Gen = list(unique(Gen))) |>
  glimpse()

# Create a connectivity matrix (number of shared genotypes)
env_list <- env_genotypes$Labels
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
connectivity_long <- melt(connectivity_matrix, varnames = c("Env1", "Env2"), value.name = "Shared")

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
  geom_tile(data = lower_triangle, aes(Env1, Env2, fill = Shared), color = "black") +
#  geom_tile(data = lower_triangle, aes(Env1, Env2), fill = 'white', color = "black") +
  # Upper triangle and diagonal: text
  geom_text(data = lower_triangle, aes(Env1, Env2, label = Shared), size = 5, color = "black") +
  geom_text(data = diag_triangle, aes(Env1, Env2, label = Shared), fontface = "bold", size = 5, color = "black") +
  #  scale_fill_continuous2(low='blue', mid='green', high='red')
  scale_fill_viridis_c(option = 'turbo', alpha = 0.7) +
  #  scale_fill_gradient(low = "blue", mid='green', high = "red", na.value = 'white', limits = c(0, max(connectivity_long$Shared))) +
  theme_light() +
  theme(
    #axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "white", fill = NA, size = 1)
        ) +
  labs(fill = "")
ggsave('Figures/Figure2.png', width = 6.5, height = 4, units = "in", dpi = 320)
ggsave('../PAG2025/projPAG2025/Figures/Fig2poster.png', width = 7, height = 4, units = "in", dpi = 320)
summary(connectivity_long)
summary(upper_triangle)
summary(diag_triangle)
