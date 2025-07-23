##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'sf',
                      'ggplot2','egg','showtext','RColorBrewer',
                      'rworldmap',
                      'arrow')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.rstudio.com/")
lapply(list.of.packages, library, character.only = TRUE)

##########################
#################### parts
##########################

## add fonts
font_add_google("Quattrocento Sans", "sans-serif")
showtext_auto()

## colorblind friendly palette from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
colors = c("#000000", "#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999")
# colors = c("#E69F00", "#009E73", "#56B4E9") ## blue to green to yellow
colors2 = brewer.pal(n = 9, name = "Set2")

##########################
##################### data 
##########################

## top 9 most populated countries
nine.countries = c('USA', 'CHN', 'IND', 'IDN', 'PAK', 'NGA', 'BRA', 'BGD', 'RUS')

## read data from get_historical_patterns_by_country.R
historical.berkeley = 
  read_csv('results/historical_average_temperature.csv',
           show_col_types = F)

## global mean surface temperature from berkeley
gmst.berkeley = 
  read_csv('results/historical_average_global_mean_surface_temperature.csv',
           show_col_types = F)

## temperatures from berkeley earth are: "in Celsius and reported as anomalies relative to the Jan 1951-Dec 1980 average"
## so we adjust down to the preindustrial average to match the fair data
## get baseline from berkeley
preindustrial.shift.for.berkeley = 
  gmst.berkeley %>% 
  filter(year %in% 1850:1899) %>% 
  .$gmst %>% 
  mean
## answer:  -0.2880234

## adjust berkeley gmst to be relative to preindustrial
gmst.berkeley %<>% 
  mutate(gmst = gmst - preindustrial.shift.for.berkeley)

## gmst from fair, change since preindustrial 1850-1900 averages
gmst.fair = 
  read_parquet('../Temperature/data/TempNorm_1850to1900_global_temperature_norm.parquet') %>% 
  rename(gmst = global_temperature_norm) %>% 
  filter(time %in% 1900:2023) %>% 
  rename(year = time) %>% 
  summarise(gmst.mean = mean(gmst, na.rm = T),
            gmst.01   = quantile(gmst, 0.01),
            gmst.05   = quantile(gmst, 0.05),
            gmst.95   = quantile(gmst, 0.95),
            gmst.99   = quantile(gmst, 0.99),
            .by = year)



## patterns from give
patterned.country.temperatures = 
  # read_csv('data/give_output/cmip6_patterns.csv',
  #          show_col_types = F) %>% 
  read_csv('../Temperature/results/cmip6_patterns_by_country.csv',
           show_col_types = F) %>%
  filter(scenario == 'ssp2',
         iso3 %in% nine.countries) %>% 
  dplyr::select(iso3, patterns.pop.2000, scenario, source) %>% 
  rename(pattern = patterns.pop.2000) %>% 
  distinct %>% 
  cross_join(gmst.fair) %>% 
  mutate(country.temp.mean = gmst.mean * pattern,
         country.temp.01   = gmst.01 * pattern,
         country.temp.05   = gmst.05 * pattern,
         country.temp.95   = gmst.95 * pattern,
         country.temp.99   = gmst.99 * pattern) %>% 
  summarise(country.temp.mean = mean(country.temp.mean, na.rm = T),
            country.temp.med  = quantile(country.temp.mean, probs = 0.50),
            country.temp.01   = quantile(country.temp.01, probs = 0.01),
            country.temp.05   = quantile(country.temp.05, probs = 0.05),
            country.temp.95   = quantile(country.temp.95, probs = 0.95),
            country.temp.99   = quantile(country.temp.99, probs = 0.99),
            .by = c('iso3', 'year'))

# pats =
#   read_csv('data/give_output/cmip6_patterns.csv',
#            show_col_types = F) %>%
#   rename(iso3 = country) %>%
#   dplyr::select(-trialnum) %>%
#   distinct
# 
# pats %>%
#   filter(iso3 == 'USA') %>%
#   summarise(mean(pattern))

##########################
##################### plot
##########################

## comparison of gmst 
# plot.gmst = 
ggplot() +
  
  ## gmst from fair
  geom_line(data = gmst.fair,
            aes(x     = year,
                y     = gmst.mean,
                color = 'GMST - FaIRv1.6.2'),
            linewidth = 1
  ) +
  geom_ribbon(data = gmst.fair,
              aes(x     = year,
                  ymin  = gmst.01,
                  ymax  = gmst.99,
                  fill  = 'GMST - FaIRv1.6.2'),
              alpha = 0.1
  ) +
  
  ## historical gmst from berkeley
  geom_line(data = gmst.berkeley,
            aes(x        = year,
                y        = gmst,
                color    = 'GMST - Berkeley Earth'),
            linewidth = 1
  ) +
  
  scale_x_continuous(limits = c(1900, 2027),
                     breaks = c(1900, 1925, 1950, 1975, 2000, 2023), 
                     expand = c(0, 0)) +
  scale_color_manual(values = colors,
                     breaks = c('GMST - FaIRv1.6.2', 'GMST - Berkeley Earth')) +
  scale_fill_manual(values = colors,
                    breaks = c('GMST - FaIRv1.6.2', 'GMST - Berkeley Earth')) +
  scale_linetype_manual(breaks = c('Area Weighted', 'Historical - Berkeley Earth'),
                        values = c('solid', 'dashed', 'dotted')) +
  labs(
    # title    = 'Area vs. Population Weighted Average Annual Temperatures',
    x        = 'Year',
    y        = 'Temperature Anomaly Relative to Preindustrial (°C, 1850-1900)',
    color    = '',
    linetype = ''
  ) +
  theme_classic() + 
  theme(
    text = element_text(family="serif"),
    legend.position = 'bottom',
    legend.key.spacing.x = unit(2, 'cm'),
    panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
    axis.text = element_text(size = rel(3), color = "black"),   # Increase size and set color to black for axis text
    axis.title = element_text(size = rel(3), color = "black"),  # Increase size and set color to black for axis titles
    legend.text = element_text(size = rel(3), color = "black"), # Increase size and set color to black for legend text
    legend.title = element_text(size = rel(3), color = "black") # Increase size and set color to black for legend title
  ) + 
  # theme_minimal() +
  # theme(legend.position = 'bottom',
  #       legend.title     = element_text(size = 14, color = 'grey20'),
  #       legend.text      = element_text(size = 14, color = 'grey20'),
  #       legend.key.width = unit(1.75, 'cm'),
  #       legend.margin    = margin(0, 0, 0, 0),
  #       legend.key.spacing.x = unit(2, 'cm'),
  #       axis.title       = element_text(size = 12),
  #       axis.text        = element_text(size = 14),
  #       axis.line.x      = element_line(color = 'black'),
  #       axis.ticks.x     = element_line(color = 'black', linewidth = 1),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
  #       panel.grid.minor = element_blank(),
  #       plot.caption     = element_text(size = 11, hjust = 0.5),
  #       plot.title       = element_text(size = 14, hjust = 0.5),
  #       text             = element_text(family = 'sans-serif', color = 'grey20')) +
  guides(color    = guide_legend(order = 1), 
         fill     = 'none',
         linetype = guide_legend(order = 2))

## export
# ggsave('results/gmst_comparison.svg', width = 9, height = 6)
ggsave('results/downscaling_1.png', width = 9, height = 6)

## country level temperature
# plot.countries =
ggplot() +
  
  ## facet
  facet_wrap(~fct_relevel(iso3, nine.countries), 
             ncol = 2,
             # scales = 'free_y',
             labeller = as_labeller(c(`USA` = 'United States', 
                                      `CHN` = 'China', 
                                      `IND` = 'India', 
                                      `IDN` = 'Indonesia', 
                                      `PAK` = 'Pakistan', 
                                      `NGA` = 'Nigeria', 
                                      `BRA` = 'Brazil', 
                                      `BGD` = 'Bangladesh', 
                                      `RUS` = 'Russia'))) +
  
  ## historical country-level from berkeley
  geom_line(data = historical.berkeley %>%
              filter(iso3 %in% nine.countries),
            aes(x        = year,
                y        = population.weighted.mean - preindustrial.shift.for.berkeley,
                color    = iso3,
                linetype = 'Historical'),
            linewidth = 0.5
  ) +
  
  ## implied historical from patterns and fair
  geom_line(data = patterned.country.temperatures %>%
              filter(iso3 %in% nine.countries),
            aes(x        = year,
                y        = country.temp.mean,
                color    = iso3,
                linetype = 'Implied Historical Patterned GMST from FaIR'),
            linewidth = 1
  ) +
  geom_ribbon(data = patterned.country.temperatures %>%
                filter(iso3 %in% nine.countries),
              aes(x    = year,
                  ymin = country.temp.01,
                  ymax = country.temp.99,
                  fill = iso3),
              alpha = 0.5
  ) +
  
  scale_x_continuous(limits = c(1900, 2027),
                     breaks = c(1900, 1925, 1950, 1975, 2000, 2023), 
                     expand = c(0, 0)) +
  scale_color_manual(values = c('grey10', colors2),
                     labels = c('USA' = 'United States', 
                                'CHN' = 'China', 
                                'IND' = 'India', 
                                'IDN' = 'Indonesia', 
                                'PAK' = 'Pakistan', 
                                'NGA' = 'Nigeria', 
                                'BRA' = 'Brazil', 
                                'BGD' = 'Bangladesh', 
                                'RUS' = 'Russia'),
                     breaks = nine.countries) +
  scale_fill_manual(values = c('grey10', colors2),
                    labels = c('USA' = 'United States', 
                               'CHN' = 'China', 
                               'IND' = 'India', 
                               'IDN' = 'Indonesia', 
                               'PAK' = 'Pakistan', 
                               'NGA' = 'Nigeria', 
                               'BRA' = 'Brazil', 
                               'BGD' = 'Bangladesh', 
                               'RUS' = 'Russia'),
                    breaks = nine.countries) +
  scale_linetype_manual(breaks = c('Implied Historical Patterned GMST from FaIR', 'Historical'),
                        values = c('solid', 'dashed', 'dotted')) +
  labs(
    x        = 'Year',
    y        = 'Change in Temperature Relative to Preindustrial (°C, 1850-1900)',
    color    = '',
    linetype = ''
  ) +
  theme_classic() + 
  theme(
    text = element_text(family="serif"),
    legend.position  = 'inside',
    legend.position.inside = c(0.75, 0.1),
    # legend.key.spacing.y = unit(0.5, 'cm'),
    legend.key.width = unit(1.75, 'cm'),
    strip.text       = element_text(size = rel(3)),
    panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
    panel.spacing.x  = unit(2, 'cm'), 
    axis.text = element_text(size = rel(2.5), color = "black"),   # Increase size and set color to black for axis text
    axis.title = element_text(size = rel(3), color = "black"),  # Increase size and set color to black for axis titles
    legend.text = element_text(size = rel(2.5), color = "black"), # Increase size and set color to black for legend text
    legend.title = element_text(size = rel(2), color = "black") # Increase size and set color to black for legend title
  ) + 
  # theme_minimal() +
  # theme(legend.position  = 'inside',
  #       legend.position.inside = c(0.75, 0.1),
  #       legend.title     = element_text(size = 14, color = 'grey20'),
  #       legend.text      = element_text(size = 14, color = 'grey20'),
  #       legend.key.width = unit(1.75, 'cm'),
  #       legend.margin    = margin(0, 0, 0, 0),
  #       legend.key.spacing.y = unit(0.5, 'cm'),
  #       axis.title       = element_text(size = 14),
  #       axis.text        = element_text(size = 14),
  #       axis.line.x      = element_line(color = 'black'),
  #       axis.ticks.x     = element_line(color = 'black', linewidth = 1),
  #       panel.grid.major.x = element_blank(),
  #       panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
  #       panel.grid.minor = element_blank(),
  #       plot.caption     = element_text(size = 11, hjust = 0.5),
  #       plot.title       = element_text(size = 14, hjust = 0.5),
  #       panel.spacing.x  = unit(2, 'cm'), 
  #       strip.text       = element_text(size = 12),
  #       text             = element_text(family = 'sans-serif', color = 'grey20')) +
  guides(color    = 'none', 
         fill     = 'none', 
         linetype = guide_legend(order = 2))

## export
# ggsave('results/country_temperaure_comparison.svg', width = 12, height = 12)
ggsave('results/downscaling_2.png', width = 9, height = 9)

## end of script. have a great day!