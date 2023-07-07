setwd('/Users/juusu53/Documents/projects/kipupotilaat/')
source('./code/helper_functions_for_r_analysis.R')

library(tidyverse)
library(ggsignif)
library(rcompanion)

subs <- read.csv('data/endometrioosi/endometriosis_with_activations_11_2021.csv',
                 na.strings = 'NaN')
subs$batch <- 'patient'
subs_control <- read.csv('data/endometrioosi/endometriosis_controls_with_activations_11_2021.csv',
                         na.strings = 'NaN')
subs_control$batch <- 'control'

subs_fixed <- make_total_colouring_columns(subs) %>% rename_emotions()
subs_control_fixed <- make_total_colouring_columns(subs_control) %>% rename_emotions()
subs_all_big <- subs_fixed %>%bind_rows(subs_control_fixed) 

## total pixels
data_long <- subs_all_big %>% 
  select(subid, sex, batch, pain_0_pos_color:sensitivity_2_pos_color)  %>% 
  rename(current = pain_0_pos_color, chronic = pain_1_pos_color,
         tactile = sensitivity_0_pos_color, pain = sensitivity_1_pos_color,
         hedonic = sensitivity_2_pos_color) %>% 
  mutate(subid = factor(subid), batch = factor(batch, levels=c('patient','control'))) %>% 
  rename(group = batch)

## normality check
data_real_long <- data_long %>% select(-c(tactile, hedonic, pain)) %>% pivot_longer(cols=current:chronic, 
                                                                              names_to = 'map_type', 
                                                                              values_to = 'pain')

outliers_total_pixels <- data_real_long %>% group_by(group, map_type) %>% identify_outliers(pain)
data_real_long %>% group_by(group, map_type) %>% shapiro_test(pain)
ggqqplot(data_real_long, "pain", ggtheme = theme_bw()) +
  facet_grid(map_type ~ group)

# not surprisingly, nothing's normal

anova_special <- bwtrim(pain ~ map_type * group, id= subid, data = data_real_long)
anova_special

## pain plot

data_long %>%
  group_by(group) %>%
  get_summary_stats(chronic, type = "median_iqr")

data_long %>%
  group_by(group) %>%
  get_summary_stats(current, type = "median_iqr")



u_chronic <- wilcox.test(data_long$chronic ~ data_long$group, conf.int=TRUE)
effect_chronic <- wilcoxonR(x = data_long$chronic,
                            g = data_long$group)

u_current <- wilcox.test(data_long$current ~ data_long$group, conf.int=TRUE)
effect_current <- wilcoxonR(x = data_long$current,
                            g = data_long$group)

p_pain <- p.adjust(c(u_current$p.value, u_chronic$p.value))

data_long %>% pivot_longer(current:chronic, names_to='pain type', values_to='prop_coloured') %>% 
  mutate(`pain type` = factor(`pain type`, levels=c('current', 'chronic'))) %>% 
  ggplot(aes(x=`pain type`, y=prop_coloured, col=group)) + 
  geom_boxplot(notch=TRUE, outlier.color='black') +
  ylab('proprtion of body coloured') +
  geom_jitter(position=position_jitterdodge(), alpha=0.6) + 
  geom_signif(y_position = c(0.6, 0.6), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
              annotation = paste('p =', round(p_pain,2)), col='black', tip_length=0.0, textsize = 5) + 
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text = element_text(size=20))
ggsave('/Users/juusu53/Documents/projects/kipupotilaat/figures/endometriosis/pain_coloring_comparison.pdf',
       width = 8, height = 8)

