setwd('/Users/juusu53/Documents/projects/kipupotilaat/')
source('./code/helper_functions_for_r_analysis.R')
library(psych)
library(RColorBrewer)
library(tidyverse)
library(apaTables)
library(rstatix)
library(ggpubr)
library(WRS2)
library(raincloudplots)
library(gghalves)
# NB: fix old style plot titles, send both new and old

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
  select(subid, sex, batch, sadness_pos_color:neutral_total) %>% select(-contains("pain")) %>% select(-contains("sensitivity")) %>% 
  pivot_longer(sadness_pos_color:neutral_total, names_to = "emotion", values_to="prop_coloured") %>% 
  separate(emotion, into=c("emotion", "type", NA)) %>% pivot_wider(names_from=type, values_from=prop_coloured) %>% 
  mutate(emotion = factor(emotion), subid = factor(subid), batch = factor(batch, levels=c('patient', 'control'))) %>% 
  rename(group = batch) 

outliers_total_pixels <- data_long %>% group_by(group, emotion) %>% identify_outliers(total)
data_long %>% group_by(group, emotion) %>% shapiro_test(total)
ggqqplot(data_long, "total", ggtheme = theme_bw()) +
  facet_grid(emotion ~ group)

# activations and deactivations

data_long_no_NA <- subs_all_big %>% 
  select(subid, sex, batch, sadness_pos_color:neutral_total) %>% select(-contains("pain")) %>% select(-contains("sensitivity")) %>% 
  drop_na() %>% 
  pivot_longer(sadness_pos_color:neutral_total, names_to = "emotion", values_to="prop_coloured") %>% 
  separate(emotion, into=c("emotion", "type", NA)) %>% pivot_wider(names_from=type, values_from=prop_coloured) %>% 
  mutate(emotion = factor(emotion), subid = factor(subid), batch = factor(batch, levels=c('patient', 'control'))) %>% 
  rename(group = batch)


## Create summaries to then report central tendencies
summarized_total <- data_long %>% group_by(emotion, group) %>% 
  summarise(coloured = mean(total, na.rm=T), sd = sd(total, na.rm=T), n = n(), na_nums= sum(is.na(total))) %>% 
  mutate(se = sd/sqrt(n))

summary_by_group <- data_long %>% group_by(group) %>% 
  summarise(mean_total = mean(total, na.rm=T), sd_total = sd(total, na.rm=T), 
            mean_pos = mean(pos, na.rm=T), sd_pos = sd(pos, na.rm = T),
            mean_neg = mean(neg, na.rm = T), sd_neg = sd(neg, na.rm=T),
            n = n(), na_nums= sum(is.na(total))) %>% 
  print()

summary_by_emotion <- data_long %>% group_by(emotion) %>% 
  summarise(mean_total = mean(total, na.rm=T), sd_total = sd(total, na.rm=T), 
            mean_pos = mean(pos, na.rm=T), sd_pos = sd(pos, na.rm = T),
            mean_neg = mean(neg, na.rm = T), sd_neg = sd(neg, na.rm=T),
            n = n(), na_nums= sum(is.na(total))) %>% 
  print()

# positive activations

summarized_pos <- data_long %>% group_by(emotion, group) %>% 
  summarise(coloured = mean(pos, na.rm=T), sd = sd(pos, na.rm=T), n = n(), na_nums= sum(is.na(pos))) %>% 
  mutate(se = sd/sqrt(n))

# negative (inactivations)

summarized_neg <- data_long %>% group_by(emotion, group) %>% 
  summarise(coloured = mean(neg, na.rm=T), sd = sd(neg, na.rm=T), n = n(), na_nums= sum(is.na(neg))) %>% 
  mutate(se = sd/sqrt(n))


g <- ggplot(data = data_long, aes(y = total, x = emotion, fill = group, col=group)) +
  # geom_half_violin(data=data_long, aes(y=total, x=emotion, fill=group), 
  #                  position = position_nudge(x = .2, y = 0), alpha = .8, side = "r") +
  geom_point(position = position_jitterdodge(jitter.width = .15, dodge.width = 0.6), size = .9, alpha = 0.8) +
  geom_boxplot(width=0.4, outlier.shape = NA, alpha = 0.5, position = position_dodge(width=0.6), notch=TRUE, col='black') +
  scale_x_discrete(limits=c('fear','happiness','sadness', 'anger','disgust','surprise','neutral')) +
  expand_limits(x = 5.25) +
  labs(y=' ', x='') + 
  #coord_flip() +
  ggtitle("C Combined") + 
  theme_classic() +
  theme(text = element_text(size=20),
        plot.margin = margin(1.5,0.1,0.1,0.1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))
#g

g2 <- ggplot(data = data_long, aes(y = pos, x = emotion, fill = group, col=group)) +
  # geom_half_violin(data=data_long, aes(y=total, x=emotion, fill=group), 
  #                  position = position_nudge(x = .2, y = 0), alpha = .8, side = "r") +
  geom_point(position = position_jitterdodge(jitter.width = .15, dodge.width = 0.6), size = .9, alpha = 0.8) +
  geom_boxplot(width=0.4, outlier.shape = NA, alpha = 0.5, position = position_dodge(width=0.6), notch=TRUE, col='black') +
  scale_x_discrete(limits=c('fear','happiness','sadness', 'anger','disgust','surprise','neutral')) +
  expand_limits(x = 5.25) +
  labs(y='Proportion of body area coloured', x='') + 
  #coord_flip() +
  ggtitle("A Activations") + 
  theme_classic() +
  theme(text = element_text(size=20),
        plot.margin = margin(1.5,0.1,0.1,0.1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))

g3 <- ggplot(data = data_long, aes(y = neg, x = emotion, fill = group, col=group)) +
  # geom_half_violin(data=data_long, aes(y=total, x=emotion, fill=group), 
  #                  position = position_nudge(x = .2, y = 0), alpha = .8, side = "r") +
  geom_point(position = position_jitterdodge(jitter.width = .15, dodge.width = 0.6), size = .9, alpha = 0.8) +
  geom_boxplot(width=0.4, outlier.shape = NA, alpha = 0.5, position = position_dodge(width=0.6), notch=TRUE, col='black') +
  scale_x_discrete(limits=c('fear','happiness','sadness', 'anger','disgust','surprise','neutral')) +
  expand_limits(x = 5.25) +
  labs(y=' ', x='') + 
  #coord_flip() +
  theme_classic() +
  ggtitle("B Deactivations") + 
  theme(text = element_text(size=20),
        plot.margin = margin(1.5,0.1,0.1,0.1, "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggarrange(g2, g3, g, 
          #labels = c("A activations", "B deactivations", "C activations and deactivations"), 
          font.label = c(size = 24),
          hjust = c(-0.2,-0.55,-0.45), 
          vjust = 1.5,
          ncol = 3, nrow = 1, 
          legend = 'right',
          common.legend = TRUE) %>%
  ggexport(filename = '/Users/juusu53/Documents/projects/kipupotilaat/figures/endometriosis/n_colored_pixels_patients_and_controls_dotandbox.png',
           width = 1000, height = 400, pointsize = 30)


## stats part
special_anova <- bwtrim(total ~ group * emotion, id=subid, data = data_long)
special_anova

special_anova_pos <- bwtrim(pos ~ group * emotion, id=subid, data = data_long)
special_anova_pos

special_anova_neg <- bwtrim(neg ~ group * emotion, id=subid, data = data_long)
special_anova_neg

pvals <- p.adjust(c(special_anova$A.p.value, special_anova$B.p.value, special_anova$AB.p.value,
           special_anova_pos$A.p.value, special_anova_pos$B.p.value, special_anova_pos$AB.p.value,
           special_anova_neg$A.p.value, special_anova_neg$B.p.value, special_anova_neg$AB.p.value))

