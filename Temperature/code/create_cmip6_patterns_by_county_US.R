## Written by: US EPA, National Center for Environmental Economics; February 2022
## extracts CMIP6 patterns from: USGCRP contract with OSTP, developed for use in USG updated SC-GHGs, 10/6/2021

##########################
#################  library
##########################

# install.packages("remotes")
# remotes::install_github("ropensci/USAboundaries")
# remotes::install_github("ropensci/USAboundariesData")

library('installr')
install.Rtools()

## Clear worksace
rm(list = ls())
gc()

## This function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'raster','sf','exactextractr','fasterize','stars','egg',
                      'USAboundaries')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
########  recover patterns 
##########################

## all files in the directory
files <- list.files('us_scghg/data/gmst_patterns/cmip6', full.names=T) %>%
  stringr::str_subset(., "broken", negate =T)

## base file
patterns = tibble()

for (i in 1:length(files)) {
  pattern = raster(files[i], varname="pattern") %>% rotate()
  
  county = us_counties() %>% 
    st_transform(st_crs(pattern)) %>% 
    dplyr::select(geoid, name, state_abbr) %>% 
    rename(county.fips = geoid,
           state       = state_abbr) %>% 
    arrange(county.fips)
  
  # ## country polygons
  # county <- get_urbn_map("counties", sf = TRUE) %>% 
  #   st_transform(st_crs(pattern)) %>% 
  #   dplyr::select(county_fips, county_name, state_abbv) %>% 
  #   rename_all(tolower) %>% 
  #   arrange(county_fips)
  
  # county$patterns.gdp <- exact_extract(pattern, county,fun='mean')
  county$patterns.area <- exact_extract(pattern, county, fun='mean')
  
  patterns = bind_rows(patterns, 
                       county %>% 
                         st_drop_geometry() %>% 
                         mutate(source    = substr(str_split(files[i],'/')[[1]][[5]], 17, nchar(str_split(files[i],'/')[[1]][[5]])-10), 
                                source.id = i))
}

## save
write_csv(patterns,'us_scghg/data/gmst_patterns/cmip6_pattern_scaling_by_county.csv')

## END OF SCRIPT. Have a great day!