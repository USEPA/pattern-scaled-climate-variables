## Written by: US EPA, National Center for Environmental Economics; October 2021
## extracts CMIP6 patterns from: USGCRP contract with OSTP, developed for use in USG updated SC-GHGs, 10/6/2021
## weights by area, pop, and gdp from: http://www.cger.nies.go.jp/gcp/population-and-gdp.html

##########################
#################  library
##########################

## Clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse','data.table',
                      'raster','sf','exactextractr','fasterize',
                      'ggplot2','egg','wesanderson',
                      'cleangeo','rworldmap')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
########  recover patterns 
##########################

## all files in the directory
files =
  list.files('data/patterns/cmip6', full.names = T) %>%
  stringr::str_subset(., "broken", negate = T)

## base file
patterns = tibble()

for (SSP in c('ssp1','ssp2','ssp3')) {
  
  for (i in 1:length(files)) {
    
    ## read in pattern from GCM
    pattern = 
      raster(files[i], varname = "pattern") %>% 
      rotate
    
    ## get country polygons
    shapes =
      st_as_sf(cleangeo::clgeo_Clean(getMap()[-which(getMap()$ADMIN=='Antarctica'),])) %>% 
      st_transform(st_crs(pattern)) %>% 
      dplyr::select(NAME, ISO3, continent) %>% 
      rename_all(tolower)
    
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
    
    # convert to a dataframe for plotting in two steps,
    pattern_df  = data.frame(rasterToPoints(pattern, spatial = T))                          
    
    ## get plot parameters for normalization
    plot.min = 
      min(shapes$patterns.area, 
          shapes$patterns.gdp.2100, 
          shapes$patterns.pop.2100)
    plot.max = 
      max(shapes$patterns.area, 
          shapes$patterns.gdp.2100, 
          shapes$patterns.pop.2100)
    
    ## plot
    plot =
      egg::ggarrange(
      top     = paste0('GCM: ', substr(str_split(files[i],'/')[[1]][[4]], 17, nchar(str_split(files[i],'/')[[1]][[4]])-10)),
      heights = c(0.5, 1, 1, 1),
      ncol    = 1,
      pattern_df %>% 
        ggplot() +
        geom_raster(aes(x    = x,
                        y    = y,
                        fill = pattern)) + 
        scale_fill_gradientn(colors   = wes_palette("Zissou1"),
                             limits   = c(plot.min, plot.max),
                             na.value = "white") +
        labs(fill = 'CMIP6 Pattern',
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
        scale_fill_gradientn(colors   = wes_palette("Zissou1"),
                             limits   = c(plot.min, plot.max),
                             na.value ="white") +
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
        geom_sf(aes(fill = patterns.gdp.2100)) + 
        scale_fill_gradientn(colors   = wes_palette("Zissou1"),
                             limits   = c(plot.min, plot.max),
                             na.value = "white") +
        labs(fill = paste0('GDP-weighted \nYear: 2100 \nSSP: ', toupper(SSP)),
             x    = '',
             y    = '') +
        theme_minimal() + 
        theme(axis.title       = element_blank(),
              axis.text        = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank()),
      shapes %>% 
        ggplot() +
        geom_sf(aes(fill = patterns.pop.2100)) + 
        scale_fill_gradientn(colors   = wes_palette("Zissou1"),
                             limits   = c(plot.min, plot.max),
                             na.value = "white") +
        labs(fill = paste0('Pop-weighted \nYear: 2100 \nSSP: ', toupper(SSP)), 
             x    = '',
             y    = '') +
        theme_minimal() + 
        theme(axis.title       = element_blank(),
              axis.text        = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
    )
    
    ## export
    ggsave(paste0('results/figures/cmip6/', SSP, '_2100_', substr(str_split(files[i], '/')[[1]][[4]], 17, nchar(str_split(files[i], '/')[[1]][[4]])-10), '.svg'),
           plot,
           width  = 8.5,
           height = 11)
    
    ## bind patterns 
    patterns = 
      bind_rows(
        patterns,
        shapes %>% 
          st_drop_geometry() %>% 
          mutate(scenario  = SSP,
                 source    = substr(str_split(files[i], '/')[[1]][[4]], 17, nchar(str_split(files[i],'/')[[1]][[4]])-10), 
                 source.id = i)
      )
  }
}

## socioeconomic projections only contain ssp1, ssp2, and ssp3. We assume ssp1 is representative of ssp5, and ssp2 is representative of ssp4.
patterns %<>% 
  bind_rows(
    patterns %>% 
      filter(scenario == 'ssp1' | scenario == 'ssp2') %>% 
      mutate(scenario =  case_when(scenario == 'ssp1'~'ssp5',
                                   scenario == 'ssp2'~'ssp4'))
  ) %>% 
  arrange(source.id, scenario, iso3)

## export
patterns %>% 
  write_csv('results/cmip6_pattern_scaling_by_country.csv')

## collapse to an average pattern across cmip models by country
patterns %>% 
  dplyr::select(-c(source, source.id)) %>% 
  group_by(name, iso3, continent, scenario) %>% 
  summarise_all(mean, na.rm = T) %>% 
  write_csv('results/cmip6_pattern_scaling_by_country_mean.csv')

## end of script. have a great day!