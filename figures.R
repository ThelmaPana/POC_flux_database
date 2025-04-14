#--------------------------------------------------------------------------#
# Project: POC_flux_database
# Script purpose: Generate figures for Simon’s paper
# Date: 27/01/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#


source("utils.R")
load("data/00.g_obs_flux.Rdata")


## Regularize coordinates grid ----
#--------------------------------------------------------------------------#
# Need to regularize the grid to match with observations
# First, get the grid for each model
pix_grid <- g_obs_flux %>% 
  #filter(depth == 100 & model == "Dunne 2007") %>% 
  group_by(model, depth) %>% 
  reframe(
    lon = list(unique(lon)),
    lat = list(unique(lat))
    ) %>% 
  group_by(model, depth) %>% 
  summarise(
    lon_pix = abs(median(unlist(lon) - lag(unlist(lon)), na.rm = TRUE)),
    lat_pix = abs(median(unlist(lat) - lag(unlist(lat)), na.rm = TRUE)),
    .groups = "drop"
  )
pix_grid

# Largest pixels are 2°, let’s refit everything on a 2° grid

prec <- 2 # round precision
g_obs_flux <- g_obs_flux %>% 
  mutate(
    lon = roundp(lon, precision = prec, f = floor) + prec / 2,
    lat = roundp(lat, precision = prec, f = floor) + prec / 2
  ) %>% 
  group_by(lon, lat, depth, model, colour) %>% 
  summarise(poc_flux = mean(poc_flux, na.rm = TRUE), .groups = "drop") %>% 
  arrange(depth, model)


## Read observations ----
#--------------------------------------------------------------------------#
load("data/02.obs_flux_targ.Rdata")
data_obs <- obs_flux_targ
rm(obs_flux_targ)

# Round on the same precision and join with models, keeping only pixels for which we have observations
df_100 <- data_obs %>% 
  filter(target_depth == 100) %>% 
  # Round lon and lat to match models (2° resolution on odd grid)
  mutate(
    lon = roundp(lon, f = floor, precision = prec) + prec / 2,
    lat = roundp(lat, f = floor, precision = prec) + prec / 2
  ) %>% 
  left_join(g_obs_flux %>% filter(depth == 100) %>% select(-depth), by = join_by(lon, lat), relationship = "many-to-many") # allow multiple models to join with observation
  
df_1000 <- data_obs %>% 
  filter(target_depth == 1000) %>% 
  mutate(
    lon = roundp(lon, f = floor, precision = prec) + prec / 2,
    lat = roundp(lat, f = floor, precision = prec) + prec / 2
  ) %>% 
  left_join(g_obs_flux %>% filter(depth == 1000) %>% select(-depth), by = join_by(lon, lat), relationship = "many-to-many") # allow multiple models to join with observation


## Prepare data ----
#--------------------------------------------------------------------------#
# Define type for each model
models <- tribble(
  ~model, ~type,
  "Dunne 2007", "Satellite",
  "Henson 2012", "Satellite",
  "Laws 2000", "Satellite",
  "Laws 2011", "Satellite",
  "Nowicki 2022", "Data assimilation",
  "Schlitzer 2000", "Inversion model",
  "Siegel 2014", "Satellite",
  "Wang 2023", "Inversion model"
)

# Define colours
colors <- c(
  "Satellite" = "#4AB2B8",
  "Data assimilation" = "#EDBA36",
  "Inversion model" = "#ED1C24"
)

# Join model type with data
df_100 <- df_100 %>% left_join(models, by = join_by(model))
df_1000 <- df_1000 %>% left_join(models, by = join_by(model))


## Density plots ----
#--------------------------------------------------------------------------#
p1 <- ggplot(df_100) +
  geom_histogram(aes(x = poc_flux_obs, y = after_stat(density)), fill = "grey80", colour = "white", binwidth = 10) +
  stat_density(aes(x = poc_flux, colour = type), adjust = 2, geom = "line", position = "identity", linewidth = 1) +
  stat_density(aes(x = poc_flux, group = model, colour = type), adjust = 2, geom = "line", position = "identity", linewidth = 0.2) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 500)) + scale_y_continuous(expand = c(0, 0.001)) +
  scale_colour_manual(values = colors) +
  labs(x = "POC flux (mg C m<sup>-2</sup> d<sup>-1</sup>)", y = "Density", colour = "Model type") +
  theme_classic() +
  theme(
    axis.title.x = element_markdown(),
    legend.position = c(0.7, 0.7)
  )

p2 <- ggplot(df_1000) +
  geom_histogram(aes(x = poc_flux_obs, y = after_stat(density)), fill = "grey80", colour = "white", binwidth = 2) +
  stat_density(aes(x = poc_flux, colour = type), adjust = 2, geom = "line", position = "identity", linewidth = 1, show.legend = FALSE) +
  stat_density(aes(x = poc_flux, group = model, colour = type), adjust = 2, geom = "line", position = "identity", linewidth = 0.2, show.legend = FALSE) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) + scale_y_continuous(expand = c(0, 0.001)) +
  scale_colour_manual(values = colors) +
  labs(x = "POC flux (mg C m<sup>-2</sup> d<sup>-1</sup>)", y = "Density", colour = "Model type") +
  theme_classic() +
  theme(
    axis.title.x = element_markdown(),
    legend.position = c(0.7, 0.7)
  )

p1 + p2


## Latitudinal plots ----
#--------------------------------------------------------------------------#
# For latitudinal plots, we first need to compute the average flux per lat band for observations
# 2° is a little small here, let’s use 5°
prec_lat <- 5
df_100_lat <- df_100 %>% 
  mutate(lat = roundp(lat, precision = prec_lat, f = floor) + prec_lat/2) %>% 
  group_by(lat) %>% 
  summarise(poc_flux_obs = mean(poc_flux_obs, na.rm = TRUE))

df_1000_lat <- df_1000 %>% 
  mutate(lat = roundp(lat, precision = prec_lat, f = floor) + prec_lat/2) %>% 
  group_by(lat) %>% 
  summarise(poc_flux_obs = mean(poc_flux_obs, na.rm = TRUE))

# Do a first plot with points showing all observations to make sure we are doing everything right.
ggplot(df_100) + 
  geom_col(data = df_100_lat, aes(x = poc_flux_obs, y = lat), orientation = "y", fill = "grey80", colour = "white") +
  geom_point(aes(x = poc_flux_obs, y = lat), size = 0.2, alpha = 0.5) +
  geom_point(aes(x = poc_flux, y = lat, colour = model), size = 0.2, alpha = 0.5) +
  geom_point(aes(x = poc_flux, y = lat, colour = type), size = 0.2, alpha = 0.5) +
  geom_smooth(aes(x = poc_flux_obs, y = lat), se = FALSE, orientation = "y", span = 0.3, linewidth = 1, show.legend = FALSE, colour = "black") +
  geom_smooth(aes(x = poc_flux, y = lat, group = model, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 0.2, show.legend = FALSE) +
  geom_smooth(aes(x = poc_flux, y = lat, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 1, show.legend = FALSE) +
  scale_colour_manual(values = colors) +
  labs(x = "POC flux (mg C m<sup>-2</sup> d<sup>-1</sup>)", y = "Latitude", colour = "Model type") +
  xlim(0, NA) +
  theme_classic() +
  theme(
    axis.title.x = element_markdown(),
  )
# Seems OK! Let’s drop the points
p3 <- ggplot(df_100) +
  geom_col(data = df_100_lat, aes(x = poc_flux_obs, y = lat), orientation = "y", fill = "grey80", colour = "white") +
  geom_smooth(aes(x = poc_flux, y = lat, group = model, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 0.2, show.legend = FALSE) +
  geom_smooth(aes(x = poc_flux, y = lat, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 1, show.legend = FALSE) +
  labs(x = "POC flux (mg C m<sup>-2</sup> d<sup>-1</sup>)", y = "Latitude", colour = "Model type") +
  scale_colour_manual(values = colors) +
  xlim(0, NA) +
  theme_classic() +
  theme(
    axis.title.x = element_markdown(),
  )

p4 <- ggplot(df_1000) +
  geom_col(data = df_1000_lat, aes(x = poc_flux_obs, y = lat), orientation = "y", fill = "grey80", colour = "white") +
  geom_smooth(aes(x = poc_flux, y = lat, group = model, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 0.2, show.legend = FALSE) +
  geom_smooth(aes(x = poc_flux, y = lat, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 1, show.legend = FALSE) +
  labs(x = "POC flux (mg C m<sup>-2</sup> d<sup>-1</sup>)", y = "Latitude", colour = "Model type") +
  scale_colour_manual(values = colors) +
  xlim(0, NA) +
  theme_classic() +
  theme(
    axis.title.x = element_markdown(),
  )

p3 + p4


## Assemble all ----
#--------------------------------------------------------------------------#
p <- ((p1 + p2 + plot_layout(ncol = 2, axes = "collect_y") ) / (p3 + plot_spacer() + p4 + plot_spacer() + plot_layout(ncol = 4))) + 
  plot_layout(heights = c(0.5, 1)) +
  plot_annotation(tag_levels = 'A')
p
ggsave(plot = p, filename = "plots/export_flux_obs.png", bg = "white", width = 2000, height = 1800, units = "px", dpi = 300)
