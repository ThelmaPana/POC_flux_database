#--------------------------------------------------------------------------#
# Project: POC_flux_database
# Script purpose: Extract model POC flux estimates matching Simon’s observations
# Date: 07/0792025
# Author: Thelma Panaïotis
#--------------------------------------------------------------------------#

source("utils.R")

## Read data ----
#--------------------------------------------------------------------------#
# Model outputs
load("data/00.g_obs_flux.Rdata")
load("data/01.mod_flux.Rdata")

# Flag obs-based products and reccap2 products + retrieve colours
g_obs_flux <- g_obs_flux %>% mutate(type = "Glob. obs.")
g_obs_cols <- g_obs_flux %>% select(model, depth, colour) %>% distinct()

mod_flux <- mod_flux %>% mutate(type = "RECCAP2")
mod_cols <- mod_flux %>% select(model, depth, colour) %>% distinct()

# Observations
load("data/02.obs_flux_targ.Rdata")

# For each observation in obs_flux_targ, we want a match-up for each model.
# Keep only relevant columns
obs_flux_targ <- obs_flux_targ |> select(id, lon, lat, depth, target_depth, poc_flux_obs)


# For each model (each model has its own grid of coordinates)
# - get coordinates grid
# - regularize observations grid to match model grid

# List models and depths configurations
df_models <- g_obs_flux |> 
  bind_rows(mod_flux) |> 
  # Make model name variable name compatible
  mutate(
    model = str_to_lower(model),
    model = str_replace(model, " ", "_"),
    model = str_replace(model, "−", "_"),
    model = str_replace(model, "-", "_")
  )
model_configs <- df_models |> select(model, depth) |> distinct()

# Loop over configurations and match observations to models
match_obs <- lapply(1:nrow(model_configs), function(i) {
  
  print(i)
  
  # Get current configuration
  conf <- model_configs |> slice(i)  
  model <- conf |> pull(model)
  depth <- conf |> pull(depth)
  
  # Get data for the model of interest
  mod_data <- df_models |> filter(model == !!model & depth == !!depth)
  
  # Get coordinate grid both in resolution and values
  lon_values <- mod_data |> pull(lon) |> unique()
  lat_values <- mod_data |> pull(lat) |> unique()
  
  lon_grid <- lon_values |> diff() |> median() |> abs()
  lat_grid <- lat_values |> diff() |> median() |> abs()
  
  # Round lon and lat
  df <- obs_flux_targ |> 
    filter(depth == !!depth) |> 
    mutate(
      lon_grid = case_when(
        # Case 1: lon is given in .5, then we floor to lon_grid at 1° and add half of lon_grid
        median(lon_values %% lon_grid) == 0.5 ~ roundp(lon, precision = lon_grid, f = floor) + lon_grid/2,
        # Case 2: lon is given in natural number on a 2° grid, then we floor to lon_grid at 2° and we add 1
        median(lon_values %% lon_grid) == 1 ~ roundp(lon, precision = lon_grid, f = floor) + lon_grid/2,
        # Case 3: lon is given in natural number on a 1° grid, then we round to lon_grid
        median(lon_values %% lon_grid) == 0 ~ roundp(lon, precision = lon_grid, f = round),
        .default = NA
      ),
      lat_grid = case_when(
        # Case 1: lat is given in .5, then we floor to lat_grid at 1° and add half of lat_grid
        median(lat_values %% lat_grid) == 0.5 ~ roundp(lat, precision = lat_grid, f = floor) + lat_grid/2,
        # Case 2: lat is given in natural number on a 2° grid, then we floor to lat_grid at 2° and we add 1
        median(lat_values %% lat_grid) == 1 ~ roundp(lat, precision = lat_grid, f = floor) + lat_grid/2,
        # Case 3: lat is given in natural number on a 1° grid, then we round to lat_grid
        median(lat_values %% lat_grid) == 0 ~ roundp(lat, precision = lat_grid, f = round),
        .default = NA
      )
    )
  
  # Match with data from the model
  df <- df |> 
    select(id, lon, lat, lon_grid, lat_grid, depth, target_depth, poc_flux_obs) |> 
    left_join(mod_data |> select(-depth) |> rename(lon_grid = lon, lat_grid = lat), by = join_by(lon_grid, lat_grid))
  
  # Return result
  return(df)
}) |> 
  bind_rows()

# Separate into 100 and 1000 m fluxes
match_obs_100 <- match_obs |> filter(target_depth == 100)
match_obs_1000 <- match_obs |> filter(target_depth == 1000)

# List models at 100 and 1000 m
mod_list_100 <- match_obs_100 |> pull(model) |> unique()
mod_list_1000 <- match_obs_1000 |> pull(model) |> unique()

# Reshape to have one column per model
match_obs_wide_100 <- match_obs_100 |> 
  # Drop model metadata
  select(-c(lon_grid, lat_grid, colour, type)) |> 
  # Keep only listed model for that detph
  filter(model %in% mod_list_100) |> 
  # Make a column for each model
  pivot_wider(values_from = poc_flux, names_from = model)
match_obs_wide_1000 <- match_obs_1000 |> 
  # Drop model metadata
  select(-c(lon_grid, lat_grid, colour, type)) |> 
  # Keep only listed model for that detph
  filter(model %in% mod_list_1000) |> 
  # Make a column for each model
  pivot_wider(values_from = poc_flux, names_from = model)


# TODO: check unmatched models
# at 100 m
foo <- match_obs |> filter(id == "10333")
zoo <- match_obs |> filter(id == "5227")

bar <- match_obs |> filter(target_depth == 1000) |> drop_na(poc_flux)
summary(bar)


## Plots ----
#--------------------------------------------------------------------------#
# Model VS obs
match_obs |> 
  drop_na(model) |> # drop observations which did not match a model
  ggplot() + 
  geom_point(aes(x = poc_flux_obs, y = poc_flux, colour = factor(depth)), size = 0.5) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  labs(x = "Obs POC flux", y = "Mod POC flux") +
  coord_fixed() +
  theme_classic() +
  facet_wrap(~model)

# Model VS obs with colour for lat
match_obs |> 
  drop_na(model) |> # drop observations which did not match a model
  ggplot() + 
  geom_point(aes(x = poc_flux_obs, y = poc_flux, colour = abs(lat)), size = 0.5) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  scale_colour_viridis_c(option = "E") +
  labs(x = "Obs POC flux", y = "Mod POC flux", colour = "Lat (abs)") +
  coord_fixed() +
  theme_classic() +
  facet_wrap(~model)


# Map of values for each model at 100 m
match_obs |> 
  drop_na(model) |> # drop observations which did not match a model
  filter(target_depth == 100) |> 
  ggplot() + 
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "grey") +
  geom_point(aes(x = lon, y = lat, colour = poc_flux)) +
  scale_colour_viridis_c(trans = "log1p") +
  coord_quickmap(expand = 0) +
  facet_wrap(~model, ncol = 2) +
  labs(title = "Modelled POC flux at 100 m")


# Map of values for each model at 1000 m
match_obs |> 
  drop_na(model) |> # drop observations which did not match a model
  filter(target_depth == 1000) |> 
  ggplot() + 
  geom_polygon(data = world, aes(x = lon, y = lat, group = group), fill = "grey") +
  geom_point(aes(x = lon, y = lat, colour = poc_flux)) +
  scale_colour_viridis_c(trans = "log1p") +
  coord_quickmap() +
  facet_wrap(~model, ncol = 2) +
  labs(title = "Modelled POC flux at 1000 m")


## Check the content of match_obs_wide and why match-up did not work for some models ----
#--------------------------------------------------------------------------#
# Map precision
prec <- 50

# Choose an observation
foo <- match_obs_wide_1000 |> filter(id == "21483")
depth <- foo$target_depth
# Find models for which we do not have a match-up
mod_to_check <- foo |> 
  pivot_longer(nowicki_2022:simple_trim) |> 
  filter(is.na(value)) |> 
  pull(name)

# Plot a map of model
df_models |> 
  filter(model %in% mod_to_check) |> 
  filter(depth == !!depth) |> 
  ggplot() +
  geom_path(data = world, aes(x = lon, y = lat, group = group)) +
  geom_raster(aes(x = lon, y = lat, fill = poc_flux)) +
  geom_point(data = foo, aes(x = lon, y = lat), colour = "red") +
  xlim(
    max(roundp(foo$lon, precision = prec, f = floor), -180), 
    min(roundp(foo$lon, precision = prec, f = ceiling), 180)
  ) +
  ylim(
    max(roundp(foo$lat, precision = prec, f = floor), -90), 
    min(roundp(foo$lat, precision = prec, f = ceiling), 90)
  ) +
  scale_fill_viridis_c(na.value = NA) +
  facet_wrap(~model) +
  coord_quickmap(expand = 0)

