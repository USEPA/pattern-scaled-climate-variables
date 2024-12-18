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
                      'ncdf4')
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

## read in NOAA data from: https://www.ncei.noaa.gov/access/monitoring/global-temperature-anomalies/grid
## open nc file
nc_file = nc_open('data/noaa_gridded_temp_and_precip/NOAAGlobalTemp_v5.0.0_gridded_s188001_e202212_c20230108T133308.nc')

## get dimension of time, looksl like 1716 months
length(ncvar_get(nc_file, 'time'))

## make brick of raster
brick =
  brick(ncvar_get(nc_file, 'anom'), 
        xmn = min(ncvar_get(nc_file, 'lat')), 
        xmx = max(ncvar_get(nc_file, 'lat')), 
        ymn = min(ncvar_get(nc_file, 'lon')), 
        ymx = max(ncvar_get(nc_file, 'lon')), 
        crs = CRS(crs)) 

## close nc file
nc_close(nc_file) 
rm(nc_file)

## clean house 
gc()

## transpose and rotate such that the projection matches others
brick %<>% 
  t %>%
  rotate

## clean house 
gc()

## create sequence of months to span data
months = 
  tibble(
    date  = seq.Date(from = as.Date('1900-01-01'),
                     to   = as.Date('2014-12-01'),
                     by   = 'month'),
    layer = as.character(1:1380)
  )

## get country polygons
countries =
  st_as_sf(cleangeo::clgeo_Clean(getMap())) %>% 
  st_transform(st_crs(crs)) %>% 
  dplyr::select(NAME, ISO3, continent) %>% 
  rename_all(tolower)

## extract weights to be used when aggregating data cells to country polygons
pop.weights.2000 = 
  fasterize(ses,  
            brick$layer.1, 
            field      = "p2_2000", 
            fun        = "sum", 
            background = mean(ses$p2_2000, na.rm=T))

## check projections, all look good
plot(brick$layer.1700)
plot(pop.weights.2000, add = T)
plot(countries, add = T, color = NA)

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
                   names_prefix = 'mean.layer.') %>% 
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
                   names_prefix = 'weighted_mean.layer.') %>% 
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
            # area.weighted.q05        = quantile(area.weighted, probs = 0.05, na.rm = T),
            # area.weighted.q95        = quantile(area.weighted, probs = 0.95, na.rm = T),
            population.weighted.mean = mean(population.weighted, na.rm = T),
            # population.weighted.q05  = quantile(population.weighted, probs = 0.05, na.rm = T),
            # population.weighted.q95  = quantile(population.weighted, probs = 0.95, na.rm = T),
            .groups = 'drop')


## export
data %>% 
  write_csv('results/historical_average_temperature.csv')

## end of script. have a great day!