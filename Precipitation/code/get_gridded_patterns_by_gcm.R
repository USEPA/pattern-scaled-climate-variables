## precipitation patterns from https://github.com/JGCRI/linear_pattern_scaling
## Cite as: Kravitz, Ben, & Snyder, Abigail. (2022). Pangeo-Enabled ESM Pattern Scaling (PEEPS): A customizable dataset of emulated Earth System Model output (1.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7557622

##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('tidyverse',
                      'arrow',
                      'ncdf4',
                      'sf','stars','exactextractr')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## function to read in patters and scale by gmst
get_patterns <- function(x) {
  
  ## get filename to use for export
  filename = str_remove(basename(x), '.nc')
  
  ## open netcdf file
  nc = nc_open(x)
  
  ## read file, make transformation, export
  pattern = 
    bind_cols(
      expand.grid(
        lon = ncdf4::ncvar_get(nc,'lon') ,
        lat = ncdf4::ncvar_get(nc, 'lat')
      ) %>%
        mutate(lon = if_else(lon >= 0 & lon <= 180, lon, lon - 360)),  ## shift (0,360) to (-180, 180)
      t(ncdf4::ncvar_get(nc, 'slope')) %>%
        as_tibble %>%
        rownames_to_column(var = 'row.id') %>%         ## create indicator for row of matrix
        pivot_longer(-row.id,
                     names_to = 'column.id') %>%       ## create indicator for column of matrix
        group_by(row.id) %>%
        mutate(column.id = as.character(seq(n())),     ## create indicator for column of matrix
               pattern   = value * 86400)              ## convert from kg/m2/s per degC to mm/day per deg C (slope gets multiplied by Tgav to give precip)
    ) %>% 
    # mutate(lmsp = pattern * gmst) %>% 
    st_as_sf(coords = c('lon', 'lat'),
             crs    = st_crs('+proj=longlat +datum=WGS84 +no_defs')) %>% 
    dplyr::select(pattern) %>% 
    st_rasterize %>% 
    as("Raster")
  
  ## close netcdf file
  nc_close(nc)
  
  ## extract patterns to 2 degree grid
  grid %>%
    mutate(pattern = exact_extract(pattern, 
                                   .,
                                   fun = 'mean'),
           lmsp    = pattern * gmst,
           gcm     = str_split(filename, '_')[[1]][1])
}

##########################
#################  process
##########################

## temperature from give
gmst = 
  read_parquet('data/TempNorm_1850to1900_global_temperature_norm.parquet') %>% 
  rename(year  = time, 
         gmst  = global_temperature_norm,
         trial = trialnum) %>% 
  filter(year %in% c(2100)) %>% 
  summarize(gmst = mean(gmst)) %>% 
  .$gmst

## path to files
path = 'data/annual/'

## all files in the directory
files = 
  list.files(path, pattern = 'ssp245_pr', full.names = T)

## get grid from get_gridded_patterns_by_gcm.R
grid = 
  st_read('data/data_grid/data_grid.shp',
          crs = st_crs('+proj=longlat +datum=WGS84 +no_defs')) %>%
  rename(grid.id = grid_id) 

## get each pattern by gcm
data = 
  files %>% 
  map_df(~get_patterns(.))

## export patterns
data %>% 
  st_drop_geometry %>% 
  write_parquet('results/cmip6_gridded_lmsp.parquet')

## end of script, have a great day!