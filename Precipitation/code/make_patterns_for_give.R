## precipitation patterns from https://github.com/JGCRI/linear_pattern_scaling
## Cite as: Kravitz, Ben, & Snyder, Abigail. (2022). Pangeo-Enabled ESM Pattern Scaling (PEEPS): A customizable dataset of emulated Earth System Model output (1.1) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7557622

##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('tidyverse','magrittr',
                      'arrow',
                      'ncdf4',
                      'sf','stars','exactextractr','fasterize',
                      'rworldmap',
                      'ggplot2','egg','wesanderson','showtext','ggridges')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## add fonts
font_add_google("Quattrocento Sans", "sans-serif")
showtext_auto()

## colorblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
# colors = c("#000000", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
colors = c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")

## get country polygons
shapes =
  st_as_sf(cleangeo::clgeo_Clean(getMap()[-which(getMap()$ADMIN=='Antarctica'),])) %>% 
  st_transform(st_crs('+proj=longlat +datum=WGS84 +no_defs')) %>% 
  dplyr::select(NAME, ISO3, continent) %>% 
  rename_all(tolower)

## all files in the directory
files = 
  list.files('data/annual/', pattern = 'ssp245_pr', full.names = T)

## base file
patterns = tibble()

for (SSP in c('ssp1','ssp2','ssp3')) {
  
  for (FILE in files) {
    
    ## get filename to use for export
    filename = str_remove(basename(FILE), '.nc')
    
    ## open netcdf file
    nc = nc_open(FILE)
    
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
                 pattern   = value * 86400 * 365)        ## convert from kg/m2/s per degC to mm/day per deg C (slope gets multiplied by Tgav to give precip), multiply by 365 to get sum of annual precip
      ) %>% 
      st_as_sf(coords = c('lon', 'lat'),
               crs    = st_crs('+proj=longlat +datum=WGS84 +no_defs')) %>% 
      dplyr::select(pattern) %>% 
      st_rasterize() %>% 
      as('Raster')
    
    ## close netcdf file
    nc_close(nc)
    
    ## clean house
    rm(nc)
    gc()
    
    if (SSP=='ssp1') {
      ## downscaled economic data
      ses = 
        purrr::reduce(
          list(
            st_read('data/socioeconomics/grid.shp') %>%                         ## data grid
              st_transform(st_crs(pattern)),
            read_csv(paste0('data/socioeconomics/gdp_', SSP, '.csv'),           ## gdp data
                     show_col_types = F) %>%                             
              dplyr::select(gID, g1_2000, g1_2100),
            read_csv(paste0('data/socioeconomics/pop_', SSP, '.csv'),           ## population data
                     show_col_types = F) %>%           
              dplyr::select(gID, p1_2000, p1_2100)
          ), 
          dplyr::left_join, 
          by = 'gID'
        )
      
      ## country temp patterns
      gdp.weights.2000 = fasterize(ses, pattern, field = "g1_2000", fun = "sum", background = mean(ses$g1_2000, na.rm = T))
      pop.weights.2000 = fasterize(ses, pattern, field = "p1_2000", fun = "sum", background = mean(ses$p1_2000, na.rm = T))
      gdp.weights.2100 = fasterize(ses, pattern, field = "g1_2100", fun = "sum", background = mean(ses$g1_2100, na.rm = T))
      pop.weights.2100 = fasterize(ses, pattern, field = "p1_2100", fun = "sum", background = mean(ses$p1_2100, na.rm = T))
      
    } else if (SSP=='ssp2') {
      ses = 
        purrr::reduce(
          list(
            st_read('data/socioeconomics/grid.shp') %>%                         ## data grid
              st_transform(st_crs(pattern)),
            read_csv(paste0('data/socioeconomics/gdp_', SSP, '.csv'),           ## gdp data
                     show_col_types = F) %>%                             
              dplyr::select(gID, g2_2000, g2_2100),
            read_csv(paste0('data/socioeconomics/pop_', SSP, '.csv'),           ## population data
                     show_col_types = F) %>%           
              dplyr::select(gID, p2_2000, p2_2100)
          ), 
          dplyr::left_join, 
          by = 'gID'
        )
      
      ## country temp patterns
      gdp.weights.2000 = fasterize(ses, pattern, field = "g2_2000", fun = "sum", background = mean(ses$g2_2000, na.rm=T))
      pop.weights.2000 = fasterize(ses, pattern, field = "p2_2000", fun = "sum", background = mean(ses$p2_2000, na.rm=T))
      gdp.weights.2100 = fasterize(ses, pattern, field = "g2_2100", fun = "sum", background = mean(ses$g2_2100, na.rm=T))
      pop.weights.2100 = fasterize(ses, pattern, field = "p2_2100", fun = "sum", background = mean(ses$p2_2100, na.rm=T))
      
    } else if (SSP=='ssp3') {
      ses = 
        purrr::reduce(
          list(
            st_read('data/socioeconomics/grid.shp') %>%                         ## data grid
              st_transform(st_crs(pattern)),
            read_csv(paste0('data/socioeconomics/gdp_', SSP, '.csv'),           ## gdp data
                     show_col_types = F) %>%                             
              dplyr::select(gID, g3_2000, g3_2100),
            read_csv(paste0('data/socioeconomics/pop_', SSP, '.csv'),           ## population data
                     show_col_types = F) %>%           
              dplyr::select(gID, p3_2000, p3_2100)
          ), 
          dplyr::left_join, 
          by = 'gID'
        )
      
      ## country temp patterns
      gdp.weights.2000 = fasterize(ses, pattern, field = "g3_2000", fun = "sum", background = mean(ses$g3_2000, na.rm=T))
      pop.weights.2000 = fasterize(ses, pattern, field = "p3_2000", fun = "sum", background = mean(ses$p3_2000, na.rm=T))
      gdp.weights.2100 = fasterize(ses, pattern, field = "g3_2100", fun = "sum", background = mean(ses$g3_2100, na.rm=T))
      pop.weights.2100 = fasterize(ses, pattern, field = "p3_2100", fun = "sum", background = mean(ses$p3_2100, na.rm=T))
    }
    
    ## extract patterns to country polygons
    shapes$patterns.area     = exact_extract(pattern, shapes, fun = 'mean')                                       ## area weighted
    shapes$patterns.gdp.2000 = exact_extract(pattern, shapes, weights = gdp.weights.2000, fun = 'weighted_mean')  ## gdp in the year 2000 weighted
    shapes$patterns.pop.2000 = exact_extract(pattern, shapes, weights = pop.weights.2000, fun = 'weighted_mean')  ## pop in the year 2000 weighted
    shapes$patterns.gdp.2100 = exact_extract(pattern, shapes, weights = gdp.weights.2100, fun = 'weighted_mean')  ## gdp in the year 2100 weighted
    shapes$patterns.pop.2100 = exact_extract(pattern, shapes, weights = pop.weights.2100, fun = 'weighted_mean')  ## pop in the year 2100 weighted
    
    if (SSP=='ssp2') {
      # convert to a dataframe for plotting
      pattern_df = 
        data.frame(raster::rasterToPoints(pattern, spatial = T)) %>% 
        rename(pattern = layer)
      
      ## get plot parameters for normalization
      plot.min = 
        min(shapes$patterns.area, 
            shapes$patterns.gdp.2000, 
            shapes$patterns.pop.2000)
      plot.max = 
        max(shapes$patterns.area, 
            shapes$patterns.gdp.2000, 
            shapes$patterns.pop.2000)
      
      ## specify plot colors and limits
      colors = RColorBrewer::brewer.pal(n = 3, 'BrBG')
      limits = c(-150, 150)
      
      ## plot
      plot =
        egg::ggarrange(
          top     = paste0('GCM: ', str_split(str_split(filename, pattern = '/'), pattern = '_')[[1]][1]),
          heights = c(0.5, 1, 1, 1),
          ncol    = 1,
          pattern_df %>% 
            ggplot() +
            geom_raster(aes(x    = x,
                            y    = y,
                            fill = pattern)) + 
            scale_fill_gradient2(low      = colors[1],
                                 mid      = colors[2],
                                 high     = colors[3],
                                 na.value = 'white',
                                 limits   = limits,
                                 oob      = scales::squish) +
            labs(fill = '    Annual \nPrecipitation \n    Pattern',
                 x    = '',
                 y    = '') +
            theme_minimal() + 
            theme(axis.title       = element_blank(),
                  axis.text        = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()),
          shapes %>% 
            ggplot() +
            geom_sf(aes(fill = patterns.area)) + 
            scale_fill_gradient2(low      = colors[1],
                                 mid      = colors[2],
                                 high     = colors[3],
                                 na.value = 'white',
                                 limits   = limits,
                                 oob      = scales::squish) +
            labs(fill = 'Area-weighted',
                 x    = '',
                 y    = '') +
            theme_minimal() + 
            theme(axis.title       = element_blank(),
                  axis.text        = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()),
          shapes %>% 
            ggplot() +
            geom_sf(aes(fill = patterns.gdp.2000)) + 
            scale_fill_gradient2(low      = colors[1],
                                 mid      = colors[2],
                                 high     = colors[3],
                                 na.value = 'white',
                                 limits   = limits,
                                 oob      = scales::squish) +
            labs(fill = paste0('GDP-weighted \nYear: 2000 \nSSP: ', toupper(SSP)),
                 x    = '',
                 y    = '') +
            theme_minimal() + 
            theme(axis.title       = element_blank(),
                  axis.text        = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()),
          shapes %>% 
            ggplot() +
            geom_sf(aes(fill = patterns.pop.2000)) + 
            scale_fill_gradient2(low      = colors[1],
                                 mid      = colors[2],
                                 high     = colors[3],
                                 na.value = 'white',
                                 limits   = limits,
                                 oob      = scales::squish) +
            labs(fill = paste0('Pop-weighted \nYear: 2000 \nSSP: ', toupper(SSP)), 
                 x    = '',
                 y    = '') +
            theme_minimal() + 
            theme(axis.title       = element_blank(),
                  axis.text        = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())
        )
      
      ## export
      ggsave(paste0('results/figures/total_annual_precip/', SSP, '_2000_', str_split(str_split(filename, pattern = '/'), pattern = '_')[[1]][1], '.svg'),
             plot,
             width  = 8.5,
             height = 11)
    }
    
    ## bind patterns 
    patterns = 
      bind_rows(
        patterns,
        shapes %>% 
          st_drop_geometry() %>% 
          mutate(scenario  = SSP,
                 source    = str_split(str_split(filename, pattern = '/'), pattern = '_')[[1]][1])
      )
  }
  
  ## clean house
  rm(pattern, pattern_df, plot, pop.weights.2000, pop.weights.2100, gdp.weights.2000, gdp.weights.2100, ses, shapes)
  gc()
  
}

## socioeconomic projections only contain ssp1, ssp2, and ssp3. We assume ssp1 is representative of ssp5, and ssp2 is representative of ssp4.
patterns %<>% 
  bind_rows(
    patterns %>% 
      filter(scenario == 'ssp1' | scenario == 'ssp2') %>% 
      mutate(scenario =  case_when(scenario == 'ssp1'~'ssp5',
                                   scenario == 'ssp2'~'ssp4'))
  ) %>% 
  arrange(scenario, iso3)

## get list of countries
iso3.list = 
  read_csv('data/iso3_countries_in_give.csv',
           show_col_types = F) %>% 
  .$iso3 

## filter to list of countries in give
patterns %<>% 
  filter(iso3 %in% iso3.list)

## rename things for give
patterns %<>%
  rename(patterns.gdp_2000 = patterns.gdp.2000,
         patterns.gdp_2100 = patterns.gdp.2100,
         patterns.pop_2000 = patterns.pop.2000,
         patterns.pop_2100 = patterns.pop.2100)

## export all
patterns %>% 
  write_csv('results/pattern_scaling_precipitation_cmip6/pattern_scaling_precipitation_by_country_full_sample.csv')

## export patterns that omit non-continuous GCM patterns rasters
patterns %>% 
  filter(!(source %in% c("ACCESS-ESM1-5", "MPI-ESM1-2-LR", "NESM3", "CanESM5", "MIROC-ES2L", "FGOALS-g3", "MIROC6", "IITM-ESM", "BCC-CSM2-MR", "CAMS-CSM1-0", "MRI-ESM2-0"))) %>% 
  write_csv('results/pattern_scaling_precipitation_cmip6/pattern_scaling_precipitation_by_country_restricted_sample.csv')

## export by averaging and SSP
for (METHOD in c('area', 'gdp_2000', 'pop_2000', 'gdp_2100', 'pop_2100')) {
  for (SSP in c('ssp1', 'ssp2', 'ssp3', 'ssp4', 'ssp5'))
    
    # ## test 
    # METHOD = 'area'
    # SSP    = 'ssp1'
    
    patterns %>% 
    filter(!(source %in% c("ACCESS-ESM1-5", "MPI-ESM1-2-LR", "NESM3", "CanESM5", "MIROC-ES2L", "FGOALS-g3", "MIROC6", "IITM-ESM", "BCC-CSM2-MR", "CAMS-CSM1-0", "MRI-ESM2-0")),
           scenario == SSP) %>% 
    select(iso3, paste0('patterns.', METHOD), source) %>% 
    pivot_wider(names_from  = source, 
                values_from = paste0('patterns.', METHOD)) %>% 
    arrange(factor(iso3, levels = iso3.list)) %>% 
    write_csv(paste0('results/pattern_scaling_precipitation_cmip6/PatternScaling_cmip6_precipitation_patterns_', METHOD, '_', toupper(SSP), '.csv'))
  
}

## collapse to an average pattern across cmip models by country
patterns %>% 
  dplyr::select(-c(source)) %>% 
  group_by(name, iso3, continent, scenario) %>% 
  summarise_all(mean, na.rm = T) %>% 
  ungroup %>% 
  write_csv('results/pattern_scaling_precipitation_cmip6/PatternScaling_cmip6_precipitation_patterns_average_by_country.csv')

## end of script, have a great day!