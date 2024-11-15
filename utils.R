# Packages
library(tidyverse)
library(R.matlab)
library(readxl)
library(ggtext)
library(ncdf4)
library(purrr)
library(khroma)
library(castr)

# ggplot theme
theme_set(theme_minimal())

# Get world map data
world <- fortify(map_data('world', wrap = c(-180, 180))) %>% rename(lon = long)
