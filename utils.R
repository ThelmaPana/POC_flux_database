# Packages
library(tidyverse)
library(R.matlab)
library(readxl)
library(ggtext)
library(ncdf4)
library(purrr)
library(khroma)
library(castr)
library(paletteer)
library(patchwork)

# ggplot theme
theme_set(theme_minimal())

# Get world map data
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon = long)

# To compute inland points
coast <- read_csv("data/raw/gshhg_world_c.csv", col_types = cols())

# Earth radius
R <- 6378.137 # km # https://nssdc.gsfc.nasa.gov/planetary/factsheet/earthfact.html
