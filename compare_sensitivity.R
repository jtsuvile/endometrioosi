setwd('/Users/juusu53/Documents/projects/kipupotilaat/')
source('./code/helper_functions_for_r_analysis.R')

library(tidyverse)
library(ggsignif)
library(rcompanion)
library(ggpubr)

subs <- read.csv('data/endometrioosi/endometriosis_with_activations_11_2021.csv',
                 na.strings = 'NaN')
subs$group <- 'patient'
subs_control <- read.csv('data/endometrioosi/endometriosis_controls_with_activations_11_2021.csv',
                         na.strings = 'NaN')
subs_control$group <- 'control'

# massage body data into tidy
subs_all <- subs %>%  select(subid, group, sex, age, emotions_0_pos_color:sensitivity_2_pos_color) %>% 
  bind_rows(subs_control %>% select(subid, sex, age, emotions_0_pos_color:group))  
subs_all <- rename_emotions(subs_all)
subs_all <- rename_sensitivity(subs_all)
subs_all <- rename_pain(subs_all)

subs_fixed <- make_total_colouring_columns(subs) %>% rename_emotions()
subs_control_fixed <- make_total_colouring_columns(subs_control) %>% rename_emotions()
subs_all_big <- subs_fixed %>%bind_rows(subs_control_fixed)

## whole body: total pixels
data_long <- subs_all_big %>% 
  select(subid, sex, group, pain_0_pos_color:sensitivity_2_pos_color)  %>% 
  rename(current = pain_0_pos_color, chronic = pain_1_pos_color,
         tactile = sensitivity_0_pos_color, pain = sensitivity_1_pos_color,
         hedonic = sensitivity_2_pos_color) %>% 
  mutate(subid = factor(subid), group = factor(group, levels=c('patient', 'control'))) %>% 
  rename(group = group)
u_tactile <- wilcox.test(data_long$tactile ~ data_long$group, conf.int=TRUE)
effect_tactile <- wilcoxonR(x = data_long$tactile,
                            g = data_long$group)
u_pain <- wilcox.test(data_long$pain ~ data_long$group, conf.int=TRUE)
effect_pain <- wilcoxonR(x = data_long$pain,
                         g = data_long$group)
u_hedonic <- wilcox.test(data_long$hedonic ~ data_long$group)
effect_hedonic <- wilcoxonR(x = data_long$hedonic,
                            g = data_long$group, ci=TRUE)

pvals <- p.adjust(c(u_tactile$p.value, u_pain$p.value, u_hedonic$p.value))

## plot whole body data
plot_all <- data_long %>% pivot_longer(tactile:hedonic, names_to='sensitivity type', values_to='prop_coloured') %>% 
  mutate(`sensitivity type` = factor(`sensitivity type`, levels=c('tactile', 'pain', 'hedonic'))) %>% 
  ggplot(aes(x=`sensitivity type`, y=prop_coloured, col=group)) + 
  geom_boxplot(notch=TRUE, outlier.color='black') +
  ylab('Proportion of whole body') +
  xlab('') + 
  scale_y_continuous(breaks=seq(0,1,0.25))+
  expand_limits(y = 1.1) +
  scale_x_discrete(breaks=c('tactile', 'pain', 'hedonic'), 
                   labels=c('Tactile','Nociceptive',' Hedonic')) + 
  geom_jitter(position=position_jitterdodge(), alpha=0.6) + 
  geom_signif(y_position = c(1.05, 1.05, 1.05), xmin = c(0.8, 1.8, 2.8), xmax = c(1.2, 2.2, 3.2),
              annotation = paste0('p = ', round(pvals,2)), col='black', tip_length=0.0, textsize = 5) + 
  theme_classic() +
  theme(text = element_text(size=20),
        axis.text = element_text(size=20),        
        # plot.margin = margin(0.8,0.1,-0.2,2.4, "cm"))
        axis.title.y = element_text(margin = margin(t = 0, r = 30, b = 0, l = 0)),
        plot.margin = margin(0.8,0.1,-0.2,1.45, "cm"))


plot_all

ggsave('/Users/juusu53/Documents/projects/kipupotilaat/figures/endometriosis/sensitivity_coloring_comparison.pdf',
       width = 12, height = 12)

# stats

data_real_long <- data_long %>% select(-c(current, chronic)) %>% pivot_longer(cols=tactile:hedonic, 
                                                                              names_to = 'map_type', 
                                                                              values_to = 'sensitivity')

outliers_total_pixels <- data_real_long %>% group_by(group, map_type) %>% identify_outliers(sensitivity)
data_real_long %>% group_by(group, map_type) %>% shapiro_test(sensitivity)
ggqqplot(data_real_long, "sensitivity", ggtheme = theme_bw()) +
  facet_grid(map_type ~ group)

anova_special <- bwtrim(sensitivity ~ map_type * group, id= subid, data = data_real_long)
anova_special

