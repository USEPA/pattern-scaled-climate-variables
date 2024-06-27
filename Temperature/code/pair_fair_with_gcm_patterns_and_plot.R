##########################
#################  library
##########################

## clear workspace
rm(list = ls())
gc()

## this function will check if a package is installed, and if not, install it
list.of.packages <- c('magrittr','tidyverse',
                      'stringi',
                      'arrow',
                      'readxl',
                      'ggplot2','ggrepel','ggpubr',
                      'showtext')
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
colors2 = c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#CC79A7")

# ## function to condense large csv files from GIVE save_list to memory-friendly parquet
# csv_to_parquet <- function(x) {
#   filename = str_remove(basename(x), '.csv') 
#   read_csv(x, show_col_types = F) %>%
#     filter(time >= 1900) %>% 
#     write_parquet(paste0('results/mimifair/save_list/CO2-2020-', 
#                          str_split(str_split(x, pattern = '/')[[1]][5], pattern = '-')[[1]][3], 
#                          '/results/',
#                          str_split(x, pattern = '/')[[1]][7], 
#                          '/',
#                          filename, 
#                          '.parquet'))
# }

## function to read and join temperature data
read_gmst <- function(x) {
  read_parquet(x) %>%
    rename(year = time,
           gmst = global_temperature_norm,
           trial= trialnum) %>%
    # filter(year %in% c(2300),
    #        trial == 177) %>%
    mutate(model = str_split(str_split(x, pattern = '/')[[1]][4], pattern = '-')[[1]][3])
}

##########################
######################  do
##########################

# ## convert csv files from GIVE save_list to parquet to reduce file size
# save_list = list.files('results/mimifair/save_list/', pattern = '.csv', recursive = T, full.names = T)
# sapply(save_list, csv_to_parquet)

## read gmst from GIVE
gmst_path  = grep(pattern = glob2rx('*model_1*'), list.dirs('results/mimifair/save_list'), value = T)
gmst_files = list.files(gmst_path, pattern = 'TempNorm_1850to1900_global_temperature_norm.parquet', full.names = T)
data = 
  gmst_files %>% 
  map_df(~read_gmst(.))

## rank from hot to cool, there are only 18 models that overlap with tokarska et al 2020, and only 4 of CSIB's top 5 rankings
data %<>% 
  left_join( 
    data %>% 
      filter(year == 2100) %>% 
      group_by(model) %>% 
      arrange(model, gmst) %>% 
      mutate(rank.all  = ntile(gmst, 18),
             rank.csib = ntile(gmst, 4)) %>% 
      select(trial, model, rank.all, rank.csib),
    by = c('trial', 'model')
  )

## read and pair with rankings based on Marcus' 6/17 email: 
data %<>% 
  left_join(
    read_excel('data/gcm_rankings.xlsx',
               sheet = 'subset-in-give') %>%
      select('gcm', 'SUM-rank') %>%
      rename(gcm.all = gcm),
    by = c('rank.all' = 'SUM-rank')
  ) %>% 
  left_join(
    read_excel('data/gcm_rankings.xlsx',
               sheet = 'CSIB-picks') %>%
      select('gcm', 'SUM-rank') %>%
      rename(gcm.csib = gcm),
    by = c('rank.csib' = 'SUM-rank')
  )

## export
data %>% 
  filter(model %in% c('SSP245')) %>% 
  select(trial, gcm.all, gcm.csib) %>% 
  rename(fair_parameter_set = trial,
         all_gcm_patterns   = gcm.all,
         conus_gcm_patterns = gcm.csib) %>% 
  distinct %>% 
  write_csv('results/fair_parameter_sets_and_gcm_pattern_pairings.csv')

##########################
####################  plot
##########################

data %>%
  ggplot() +
  facet_wrap(~model, scales = 'free', ncol = 1) +
  geom_line(aes(x     = year, 
                y     = gmst, 
                color = fct_reorder(gcm.all, rank.all),
                group = interaction(gcm.all, trial)),
            alpha = 0.4) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = rev(colorRampPalette(colors)(18))) +
  scale_fill_manual(values = rev(colorRampPalette(colors)(18))) +
  labs(x         = 'Year',
       y         = 'Baseline GMST (relative to 1850-1900)',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() +
  theme(legend.position = 'right',
        legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 14, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text       = element_text(color = 'grey20', size = 14, face = 'bold'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        # plot.margin      = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), 'cm'),
        text             = element_text(family = 'sans-serif', color = 'grey20')) + 
  guides(color = guide_legend(reverse = T))

## export
ggsave('results/figures/fair_gcm_rankings_all.svg', 
       width  = 16, 
       height = 12)


data %>%
  ggplot() +
  facet_wrap(~model, scales = 'free', ncol = 1) +
  geom_line(aes(x     = year, 
                y     = gmst, 
                color = fct_reorder(gcm.csib, rank.csib),
                group = interaction(gcm.csib, trial)),
            alpha = 0.4) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = colors2) +
  labs(x         = 'Year',
       y         = 'Baseline GMST (relative to 1850-1900)',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() +
  theme(legend.position = 'right',
        legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 14, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text       = element_text(color = 'grey20', size = 14, face = 'bold'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        # plot.margin      = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), 'cm'),
        text             = element_text(family = 'sans-serif', color = 'grey20')) + 
  guides(color = guide_legend(reverse = T))

## export
ggsave('results/figures/fair_gcm_rankings_csib.svg', 
       width  = 16, 
       height = 12)


################################ scatter 
ggplot() +
  facet_wrap(~model, scales = 'free', ncol = 1) +
  geom_point(data = data %>%
               filter(year == 2100),
             aes(x     = rank.all, 
                 y     = gmst, 
                 color = fct_reorder(gcm.all, rank.all),
                 group = interaction(gcm.all, trial)),
             alpha = 0.4) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = rev(colorRampPalette(colors)(18))) +
  scale_fill_manual(values = rev(colorRampPalette(colors)(18))) +
  labs(x         = 'GCM Rank',
       y         = 'Baseline GMST in 2100 (relative to 1850-1900)',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() +
  theme(legend.position = 'right',
        legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 14, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text       = element_text(color = 'grey20', size = 14, face = 'bold'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        # plot.margin      = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), 'cm'),
        text             = element_text(family = 'sans-serif', color = 'grey20')) + 
  guides(color = guide_legend(reverse = T))

## export
ggsave('results/figures/fair_gcm_rankings_all_points.svg', 
       width  = 16, 
       height = 12)



ggplot() +
  facet_wrap(~model, scales = 'free', ncol = 1) +
  geom_point(data = data %>%
               filter(year == 2100),
             aes(x     = rank.all, 
                 y     = gmst, 
                 color = fct_reorder(gcm.all, rank.all),
                 group = interaction(gcm.all, trial)),
             alpha = 0.4) +
  geom_mark_ellipse(data = data %>% 
                      filter(year == 2100,
                             gcm.all %in% gcm.csib),
                    aes(x     = rank.all, 
                        y     = gmst,
                        group = gcm.all),
                    linewidth   = 0.75,
                    color       = 'red3',
                    inherit.aes = F) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = rev(colorRampPalette(colors)(18))) +
  scale_fill_manual(values = rev(colorRampPalette(colors)(18))) +
  labs(x         = 'GCM Rank',
       y         = 'Baseline GMST in 2100 (relative to 1850-1900)',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() +
  theme(legend.position = 'right',
        legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 14, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text       = element_text(color = 'grey20', size = 14, face = 'bold'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        # plot.margin      = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), 'cm'),
        text             = element_text(family = 'sans-serif', color = 'grey20')) + 
  guides(color = guide_legend(reverse = T))

## export
ggsave('results/figures/fair_gcm_rankings_all_points_marked_gcms.svg', 
       width  = 16, 
       height = 12)




ggplot() +
  facet_wrap(~model, scales = 'free', ncol = 1) +
  geom_point(data = data %>%
               filter(year == 2100),
             aes(x     = rank.csib, 
                 y     = gmst, 
                 color = fct_reorder(gcm.csib, rank.csib),
                 group = interaction(gcm.csib, trial)),
             alpha = 0.4) +
  annotation_custom(
    grob = grid::rectGrob(gp = grid::gpar(col = NA, fill = "white")),
    xmin = 2300
  ) +
  scale_color_manual(values = colors2) +
  scale_fill_manual(values = colors2) +
  labs(x         = 'GCM Rank',
       y         = 'Baseline GMST in 2100 (relative to 1850-1900)',
       color     = '',
       linetype  = '',
       group     = '',
       fill      = '') +
  theme_minimal() +
  theme(legend.position = 'right',
        legend.justification = c(1, 0),
        legend.title     = element_text(size = 14, color='grey20'),
        legend.text      = element_text(size = 14, color='grey20'),
        legend.key.size  = unit(1, 'cm'),
        legend.margin    = margin(0, 0, 0, 0),
        axis.title       = element_text(size = 14),
        axis.text        = element_text(size = 14),
        axis.line.x      = element_line(color = "black"),
        axis.ticks.x     = element_line(color = "black", size = 1),
        strip.text       = element_text(color = 'grey20', size = 14, face = 'bold'), 
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = 'grey70', linetype = 'dotted'),
        panel.grid.minor = element_blank(),
        plot.caption     = element_text(size = 12, hjust = 0.5),
        plot.title       = element_text(size = 12, hjust = 0.5),
        # plot.margin      = unit(c(t = 0, r = 0.5, b = 0, l = 0.5), 'cm'),
        text             = element_text(family = 'sans-serif', color = 'grey20')) + 
  guides(color = guide_legend(reverse = T))

## export
ggsave('results/figures/fair_gcm_rankings_csib_gcms.svg', 
       width  = 16, 
       height = 12)


## end of script, have a great day.