##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr', 'tidyverse', 
                      'sf',
                      'ggplot2', 'ggpattern', 'RColorBrewer', 'showtext',
                      'rworldmap')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
###################  parts
##########################

## fonts
font_add_google("Quattrocento Sans", "sans-serif")
showtext_auto()

##########################
#################  process
##########################

## world map
world = 
  st_as_sf(cleangeo::clgeo_Clean(getMap())) %>% 
  dplyr::select(NAME, ISO3, continent) %>% 
  rename_all(tolower) %>% 
  st_transform(st_crs('+proj=longlat +datum=WGS84 +no_defs'))

## get grid from get_gridded_patterns_by_gcm.R
grid = 
  st_read('data/data_grid/data_grid.shp',
          crs = st_crs('+proj=longlat +datum=WGS84 +no_defs')) %>%
  rename(grid.id = grid_id)

## get data from get_gridded_patterns_by_gcm.R
data = 
  read_parquet('results/cmip6_gridded_lmsp.parquet') %>% 
  group_by(grid.id) %>% 
  summarize(pattern.mean    = mean(pattern, na.rm = T),
            pattern.sd      = sd(pattern, na.rm = T),
            lmsp            = mean(lmsp, na.rm = T),
            sd.to.mean      = abs(pattern.sd/pattern.mean),
            model.agreement = case_when(sd.to.mean >= 7 ~ 'Low Model Agreement',
                                        T ~ 'High Model Agreement'),
            lmsp.model.agree = case_when(model.agreement == 'Low Model Agreement' ~ NA_real_,
                                         T ~ lmsp))

## add to grid
plot.data =  
  left_join(grid, data) %>% 
  st_intersection(world)

## plot without model disagreement
ggplot() +
  geom_sf(data = plot.data,
          aes(fill = lmsp),
          color = NA) +
  geom_sf(data = world,
          fill = NA) +
  scale_fill_distiller(palette  = 'BrBG',
                       direction = +1,
                       na.value = 'white',
                       breaks   = scales::pretty_breaks(n = 6),
                       guide    = guide_colorbar(title.position = 'top'),
                       limits   = c(-0.8, 2.9),
                       values   = scales::rescale(c(-0.8, -0.4, 0, 0.5, 2.5))
  ) +
  labs(fill    = 'Average Change in Local Mean Surface Precipitation in 2100 (mm/day)') +
  theme_void() +
  theme(legend.position    = 'bottom',
        legend.key.width   = unit(2.5, 'cm'),
        legend.margin      = margin(0, 0, 1.5, 0),
        legend.title.align = 0.5,
        text               = element_text(family = 'sans-serif'))

## export
ggsave('results/figures/lmsp_2100.svg', width = 9, height = 6)

## end of script. have a great day!