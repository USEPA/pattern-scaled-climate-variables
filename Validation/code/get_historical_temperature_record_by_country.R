##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'raster','sf','exactextractr','fasterize',
                      'cleangeo','rworldmap',
                      'ncdf4','tidync')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
########  recover patterns 
##########################

## set crs for analysis
crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0'

## read in down-scaled socioeconomic data
ses = 
  purrr::reduce(
    list(
      st_read('data/socioeconomics/grid.shp') %>%                    ## data grid
        st_transform(st_crs(crs)),
      read_csv(paste0('data/socioeconomics/gdp_ssp2.csv'),           ## gdp data
               show_col_types = F) %>%                             
        dplyr::select(gID, g2_2000, g2_2100),
      read_csv(paste0('data/socioeconomics/pop_ssp2.csv'),           ## population data
               show_col_types = F) %>%           
        dplyr::select(gID, p2_2000, p2_2100)
    ), 
    dplyr::left_join, 
    by = 'gID'
  )

## create sequence of months to span data
months = 
  tibble(
    date  = seq.Date(from = as.Date('1850-01-01'),
                     to   = as.Date('2024-10-01'),
                     by   = 'month'),
    layer = as.character(1:2098)
  )

# get country polygons
countries =
  st_as_sf(cleangeo::clgeo_Clean(getMap())) %>% 
  st_transform(st_crs(crs)) %>% 
  dplyr::select(NAME, ISO3, continent) %>% 
  rename_all(tolower)

## read in NOAA data from: https://berkeleyearth.org/data/
## open nc file
nc_file = 
  nc_open('data/berkeley/Land_and_Ocean_EqualArea.nc')

## get data from file
data = 
  data.frame(x    = ncvar_get(nc_file, 'longitude'),
             y    = ncvar_get(nc_file, 'latitude'),
             vals = ncvar_get(nc_file, 'temperature')) %>% 
  as_tibble

# create a raster object to recieve data
raster = 
  raster(xmn        = -180, 
         xmx        = 180, 
         ymn        = -90, 
         ymx        = 90, 
         resolution = c(2.5, 2.5))

# use rasterize to create desired rasterbrick
brick = 
  rasterize(x     = data[, 1:2],    # lon-lat data
            y     = raster,         # raster object
            field = data[, 3:2100], # values to fill raster with
            fun   = mean)           # aggregate function

# ## check projections, seems okay
# plot(brick$vals.1)
# plot(countries, add = T, color = NA)

## close nc file
nc_close(nc_file) 
rm(nc_file)

## clean house 
gc()

## extract weights to be used when aggregating data cells to country polygons
pop.weights.2000 = 
  fasterize(ses,  
            brick$vals.1, 
            field      = 'p2_2000', 
            fun        = 'sum', 
            background = mean(ses$p2_2000, na.rm=T))

# ## check projections, all look good
# plot(brick$vals.1)
# p2 = pop.weights.2000
# p2$layer[p2$layer == 0] = NA
# plot(p2, add = T)
# plot(countries, add = T, color = NA)
# 
# ## check US, looks about right
# plot(crop(p2, extent(countries %>% filter(iso3 == 'USA'))))
# plot(countries %>% filter(iso3 == 'USA'), add = T, color = NA)

## get annual gmst
gmst = 
  brick %>% 
  tabularaster::as_tibble() %>% 
  rename(temp    = cellvalue,
         cell.id = cellindex, 
         layer   = dimindex) %>%
  mutate(layer = as.character(layer)) %>% 
  left_join(months,
            by = 'layer') %>% 
  dplyr::select(-layer) %>% 
  mutate(year = year(date)) %>% 
  summarise(gmst = mean(temp, na.rm = T),
            .by  = year) 

## export gmst
gmst %>% 
  write_csv('results/historical_average_global_mean_surface_temperature.csv')

## extract patterns to country polygons
## area weighted
mean = 
  bind_cols(countries,
            exact_extract(brick, 
                          countries, 
                          fun = 'mean'))

## population weighted
pop2000 = 
  bind_cols(countries,
            exact_extract(brick, 
                          countries, 
                          weights = pop.weights.2000, 
                          fun     = 'weighted_mean'))

## pivot longer and combine
data = 
  left_join(
    mean %>% 
      st_drop_geometry %>% 
      pivot_longer(cols         = starts_with('mean'),
                   names_to     = 'layer',
                   values_to    = 'area.weighted',
                   names_prefix = 'mean.vals.') %>% 
      filter(!is.na(area.weighted)) %>% 
      left_join(months,
                by = 'layer') %>% 
      dplyr::select(-layer) %>% 
      mutate(year = year(date)),
    pop2000 %>% 
      st_drop_geometry %>% 
      pivot_longer(cols         = starts_with('weighted_mean'),
                   names_to     = 'layer',
                   values_to    = 'population.weighted',
                   names_prefix = 'weighted_mean.vals.') %>% 
      filter(!is.na(population.weighted)) %>% 
      left_join(months,
                by = 'layer') %>% 
      dplyr::select(-layer) %>% 
      mutate(year = year(date))
  )

## get yearly averages
data %<>% 
  group_by(name, iso3, year) %>% 
  summarize(area.weighted.mean       = mean(area.weighted, na.rm = T),
            population.weighted.mean = mean(population.weighted, na.rm = T),
            .groups = 'drop')

## export
data %>% 
  write_csv('results/historical_average_temperature.csv')

## end of script. have a great day!