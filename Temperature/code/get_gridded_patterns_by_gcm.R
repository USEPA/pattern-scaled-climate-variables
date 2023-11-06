##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr', 'tidyverse', 
                      'stringi',
                      'sf','raster','exactextractr',
                      'arrow')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## turn off planar geometry until package developers can fix this
sf_use_s2(FALSE)

## function to read in patters and scale by gmst
get_patterns <- function(x) {
  
  ## get filename to use for export
  filename = str_remove(basename(x), '.nc')
  
  ## get pattern and gcm
  grid %>%
    mutate(pattern = exact_extract(raster(x, 
                                          varname = "pattern") %>% 
                                     rotate, 
                                   .,
                                   fun = 'mean'),
           gcm    = str_split(filename, '_')[[1]][4],
           lmst   = pattern * gmst)
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

## path to patterns files
path = 'data/patterns/cmip6'

## all files in the directory
files =
  list.files('data/patterns/cmip6', full.names = T) %>%
  stringr::str_subset(., "broken", negate = T)

## get grid from Precipitation/code/get_gridded_patterns_by_gcm.R
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
  write_parquet('results/cmip6_gridded_lmst.parquet')

## end of script. have a great day!