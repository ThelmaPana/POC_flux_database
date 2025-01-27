#--------------------------------------------------------------------------#
# Project: POC_flux_database
# Script purpose: Generate figures for Simon’s paper
# Date: 27/01/2025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#


source("utils.R")
load("data/00.g_obs_flux.Rdata")


## Regularize grid ----
#--------------------------------------------------------------------------#
#TODO: Do we really need to regularize the grid?


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
df <- g_obs_flux %>% 
  left_join(models, by = join_by(model))


## Density plots ----
#--------------------------------------------------------------------------#
p1 <- ggplot(df %>% filter(depth == 100)) +
  #geom_density(aes(x = poc_flux, group = model, colour = type)) +
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

p2 <- ggplot(df %>% filter(depth == 1000)) +
  #geom_density(aes(x = poc_flux, group = model, colour = type)) +
  stat_density(aes(x = poc_flux, colour = type), adjust = 2, geom = "line", position = "identity", linewidth = 1) +
  stat_density(aes(x = poc_flux, group = model, colour = type), adjust = 2, geom = "line", position = "identity", linewidth = 0.2) +
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
p3 <- ggplot(df %>% filter(depth == 100)) +
  geom_smooth( aes(x = poc_flux, y = lat, group = model, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 0.2, show.legend = FALSE) +
  geom_smooth( aes(x = poc_flux, y = lat, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 1, show.legend = FALSE) +
  labs(x = "POC flux (mg C m<sup>-2</sup> d<sup>-1</sup>)", y = "Latitude", colour = "Model type") +
  scale_colour_manual(values = colors) +
  xlim(0, NA) +
  theme_classic() +
  theme(
    axis.title.x = element_markdown(),
  )

p4 <- ggplot(df %>% filter(depth == 1000)) +
  geom_smooth( aes(x = poc_flux, y = lat, group = model, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 0.2, show.legend = FALSE) +
  geom_smooth( aes(x = poc_flux, y = lat, colour = type), se = FALSE, orientation = "y", span = 0.3, linewidth = 1, show.legend = FALSE) +
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
p <- (p1 + p2 + plot_layout(ncol = 1, axes = "collect_x")) + # p1 and p2 on top of each other, collect X axis
  (p3 + p4 + plot_layout(ncol = 2, axes = "collect")) + # p3 and p4 next to each other, collect X axis
  plot_layout(heights = c(0.5, 0.5, 1)) + # p3 and p4 have doubled height compared to p1 and p2
  plot_annotation(tag_levels = 'A') # annotate panels
ggsave(plot = p, filename = "plots/export_flux_obs.png", bg = "white", width = 1500, height = 2700, units = "px", dpi = 300)


