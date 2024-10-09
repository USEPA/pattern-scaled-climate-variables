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


##########################
####################  data
##########################

## import from make_patterns_for_give.R
patterns %>% 
  read_csv('results/pattern_scaling_precipitation_by_country_full_sample.csv',
           show_col_types = F)

## get rid of patterns that are noncontinuous, asked Ben Kravitz about this and it has to do with standardizing CMIP model output. 
## these GCMs produce NaNs for some small countries and island countries due to their gridded output
patterns.not.dropped = 
  patterns %>% 
  filter(!(source %in% c("ACCESS-ESM1-5", "MPI-ESM1-2-LR", "NESM3", "CanESM5", "MIROC-ES2L", "FGOALS-g3", "MIROC6", "IITM-ESM", "BCC-CSM2-MR", "CAMS-CSM1-0", "MRI-ESM2-0")))
patterns.dropped = 
  patterns %>% 
  filter(source %in% c("ACCESS-ESM1-5", "MPI-ESM1-2-LR", "NESM3", "CanESM5", "MIROC-ES2L", "FGOALS-g3", "MIROC6", "IITM-ESM", "BCC-CSM2-MR", "CAMS-CSM1-0", "MRI-ESM2-0"))

## summary, any big differences?
patterns.not.dropped %>% 
  filter(iso3 == 'USA') %>% 
  summary
patterns.dropped %>% 
  filter(iso3 == 'USA') %>% 
  summary

## make long for plotting
plot.data =
  bind_rows(
    patterns.dropped %>% 
      select(-patterns.gdp.2100, -patterns.pop.2100) %>% 
      pivot_longer(cols         = starts_with('patterns'),
                   names_to     = 'aggregation.method',
                   names_prefix = 'patterns.',
                   values_to    = 'pattern') %>% 
      mutate(filtering = 'Dropped Patterns'),
    patterns.not.dropped %>% 
      select(-patterns.gdp.2100, -patterns.pop.2100) %>% 
      pivot_longer(cols         = starts_with('patterns'),
                   names_to     = 'aggregation.method',
                   names_prefix = 'patterns.',
                   values_to    = 'pattern') %>% 
      mutate(filtering = 'Remaining Patterns')
  ) %>% 
  mutate(aggregation.method = case_when(aggregation.method == 'area'     ~ 'Area',
                                        aggregation.method == 'gdp.2000' ~ 'GDP',
                                        aggregation.method == 'pop.2000' ~ 'Population'))

##########################
####################  plot
##########################

## plot density global
ggplot() +
  geom_density_ridges(data = plot.data %>% 
                        filter(scenario %in% c('ssp2')),
                      aes(x     = pattern,
                          y     = filtering,
                          color = aggregation.method,
                          fill  = aggregation.method,
                          point_shape = aggregation.method),
                      alpha           = 0.05,
                      jittered_points = F, 
                      point_alpha     = .4) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(title       = 'Distribution of Global Precipitation Patterns',
       x           = '',
       y           = '',
       color       = 'Aggregation Method',
       fill        = 'Aggregation Method',
       point_shape = 'Aggregation Method',
       linetype    = '') +
  theme_minimal() +
  theme(legend.position  = 'right',
        legend.title     = element_text(size = 14, color = 'grey20'),
        legend.text      = element_text(size = 14, color = 'grey20'),
        legend.key.width = unit(1.75, 'cm'),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = 'black'),
        axis.ticks.x     = element_line(color = 'black', size = 1),
        panel.grid.major = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        text             = element_text(family = 'sans-serif', color = 'grey20'))

## export
ggsave('results/figures/distribtution_of_patterns_global.svg', 
       width  = 11, 
       height = 6)

## function to help with boxplots 
quantiles_95 <- function(x) {
  r <- quantile(x, probs=c(.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

## function for labels
means = 
  plot.data %>% 
  group_by(aggregation.method, scenario, filtering) %>% 
  summarise(mean  = round(mean(pattern, na.rm = T), 2),
            .groups = 'drop')

## plot global
ggplot() +
  geom_density_ridges(data = plot.data %>% 
                        filter(scenario %in% c('ssp2')),
                      aes(x     = pattern,
                          y     = filtering,
                          color = aggregation.method,
                          fill  = aggregation.method,
                          point_shape = aggregation.method),
                      scale           = 1,
                      alpha           = 0.05,
                      jittered_points = T, 
                      point_alpha     = .01) + 
  stat_summary(data = plot.data %>% 
                 filter(scenario %in% c('ssp2')),
               aes(x     = pattern,
                   y     = filtering,
                   color = aggregation.method,
                   fill  = aggregation.method),
               alpha       = 0.1,
               geom        = 'boxplot',
               fun.data    = quantiles_95, 
               width       = 0.1, 
               position    = position_dodge(.2),
               show.legend = F) + 
  stat_summary(data = plot.data %>% 
                 filter(scenario %in% c('ssp2')),
               aes(x     = pattern,
                   y     = filtering,
                   color = aggregation.method,
                   fill  = aggregation.method), 
               alpha       = 0.5,
               fun         = mean, 
               geom        = 'point', 
               shape       = 18, 
               size        = 4, 
               position    = position_dodge(.2),
               show.legend = F) +
  geom_text(data = means %>% 
              filter(scenario %in% c('ssp2')), 
            aes(x     = mean, 
                y     = filtering, 
                group = aggregation.method,
                label = mean,
                vjust = -0.8),
            position    = position_dodge(.2),
            color       = 'grey20',
            size        = 4,
            family      = 'sans-serif',
            show.legend = F) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_discrete(expand = expansion(mult = c(0.15, 0.25))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(title       = 'Distribution of Global Precipitation Patterns',
       x           = 'Annual Change in Precipitation per 1°C (mm/year)',
       y           = '',
       color       = 'Aggregation Method',
       fill        = 'Aggregation Method',
       point_shape = 'Aggregation Method',
       linetype    = '') +
  theme_minimal() +
  theme(legend.position  = 'right',
        legend.title     = element_text(size = 14, color = 'grey20'),
        legend.text      = element_text(size = 14, color = 'grey20'),
        legend.key.width = unit(1.75, 'cm'),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = 'black'),
        axis.ticks.x     = element_line(color = 'black', size = 1),
        panel.grid.major = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        text             = element_text(family = 'sans-serif', color = 'grey20')) +
  guides(color       = guide_legend(reverse = T, byrow = T),
         fill        = guide_legend(reverse = T, byrow = T),
         point_shape = guide_legend(reverse = T, byrow = T))

## export
ggsave('results/figures/distribtution_of_patterns_global.svg', 
       width  = 11, 
       height = 6)

## function for labels
means.by.country = 
  plot.data %>% 
  group_by(aggregation.method, scenario, filtering, iso3) %>% 
  summarise(mean  = round(mean(pattern, na.rm = T), 2),
            .groups = 'drop')

## plot usa
ggplot() +
  geom_density_ridges(data = plot.data %>% 
                        filter(iso3 %in% c('USA'),
                               scenario %in% c('ssp2')),
                      aes(x     = pattern,
                          y     = filtering,
                          color = aggregation.method,
                          fill  = aggregation.method,
                          point_shape = aggregation.method),
                      scale           = 1,
                      alpha           = 0.05,
                      jittered_points = T, 
                      point_alpha     = .4) + 
  stat_summary(data = plot.data %>% 
                 filter(iso3 %in% c('USA'),
                        scenario %in% c('ssp2')),
               aes(x     = pattern,
                   y     = filtering,
                   color = aggregation.method,
                   fill  = aggregation.method),
               alpha       = 0.1,
               geom        = 'boxplot',
               fun.data    = quantiles_95, 
               width       = 0.1, 
               position    = position_dodge(.2),
               show.legend = F) + 
  stat_summary(data = plot.data %>% 
                 filter(iso3 %in% c('USA'),
                        scenario %in% c('ssp2')),
               aes(x     = pattern,
                   y     = filtering,
                   color = aggregation.method,
                   fill  = aggregation.method), 
               alpha       = 0.5,
               fun         = mean, 
               geom        = 'point', 
               shape       = 18, 
               size        = 4, 
               position    = position_dodge(.2),
               show.legend = F) +
  geom_text(data = means.by.country %>% 
              filter(iso3 %in% c('USA'),
                     scenario %in% c('ssp2')), 
            aes(x     = mean, 
                y     = filtering, 
                group = aggregation.method,
                label = mean,
                vjust = -0.8),
            position    = position_dodge(.2),
            color       = 'grey20',
            size        = 4,
            family      = 'sans-serif',
            show.legend = F) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_discrete(expand = expansion(mult = c(0.15, 0.25))) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  labs(title       = 'Distribution of Domestic (USA) Precipitation Patterns',
       x           = 'Annual Change in Precipitation per 1°C (mm/year)',
       y           = '',
       color       = 'Aggregation Method',
       fill        = 'Aggregation Method',
       point_shape = 'Aggregation Method',
       linetype    = '') +
  theme_minimal() +
  theme(legend.position  = 'right',
        legend.title     = element_text(size = 14, color = 'grey20'),
        legend.text      = element_text(size = 14, color = 'grey20'),
        legend.key.width = unit(1.75, 'cm'),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = 'black'),
        axis.ticks.x     = element_line(color = 'black', size = 1),
        panel.grid.major = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        text             = element_text(family = 'sans-serif', color = 'grey20')) +
  guides(color       = guide_legend(reverse = T, byrow = T),
         fill        = guide_legend(reverse = T, byrow = T),
         point_shape = guide_legend(reverse = T, byrow = T))

## export
ggsave('results/figures/distribtution_of_patterns_usa.svg', 
       width  = 9, 
       height = 6)

## end of script, have a great day!