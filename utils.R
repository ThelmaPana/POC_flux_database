# Packages
library(tidyverse)
library(R.matlab)
library(ggtext)
library(ncdf4)
library(purrr)

# ggplot theme
theme_set(theme_minimal())

# Get world map data
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon = long)
