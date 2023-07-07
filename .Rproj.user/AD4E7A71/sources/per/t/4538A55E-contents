library(tidyverse)
library(WRS2)
data_folder <- '/Users/juusu53/Documents/projects/kipupotilaat/data/endometrioosi/'
controls <- read_csv(paste0(data_folder, '/endometriosis_controls_with_activations_11_2021.csv'))
pain <- read_csv(paste0(data_folder, '/endometriosis_with_activations_11_2021.csv'))

pain <- pain %>% select(subid, sex, age, starts_with('feels')) %>% mutate(group='pain')
data <- controls %>% select(subid, sex, age, starts_with('feels')) %>% mutate(group='control') %>% 
  rbind(pain)

plot_data_long <- data %>% 
  pivot_longer(cols="feels_pain":"feels_disgust", names_to = 'feeling', values_to='intensity') %>% 
  mutate(feeling = str_remove(feeling, 'feels_'), feeling=factor(feeling)) %>% 
  mutate(group = factor(group, levels=c('pain','control'), labels = c('patient','control')))

plot_data_summary <- plot_data_long %>% group_by(feeling, group) %>% 
  summarise(mean = mean(intensity), sd = sd(intensity), se=sd(intensity)/sqrt(n()), n()) %>% 
  ungroup()

pd <- position_dodge(0.7)

p1 <- ggplot(plot_data_long, aes(x=feeling, y=intensity, color=group, fill=group)) + 
  geom_point(position=position_jitterdodge(jitter.width=0.3, jitter.height = 0.5, dodge.width = 0.7), alpha=0.8) +
  geom_boxplot(width=0.4, outlier.shape = NA, alpha = 0.5, 
               position = position_dodge(width=0.7), notch=TRUE, col='black') +
  scale_x_discrete(limits=rev(c('pain', 'happy','anxiety','depression','sad','fear',
                                'surprise', 'angry','disgust')),
                   labels=rev(c('pain', 'happiness','anxiety','depression','sadness','fear',
                                'surprise','anger','disgust'))) +
  theme_classic() +
  coord_flip() +
  theme(text = element_text(size=20),
        #axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position='bottom') +
  #coord_fixed(ratio=0.5) +
  labs(color = "group", fill = 'group', x = 'Emotion', y = 'Intensity of emotion') 

pdf('/Users/juusu53/Documents/projects/kipupotilaat/figures/endometriosis/feels_now_boxplot.pdf')
p1
dev.off()

special_anova <- bwtrim(intensity ~ group * feeling, id=subid, data = plot_data_long)
special_anova

test_res <- wilcox.test(feels_happy ~ group, data=data)

N = length(unique(plot_data_long$feeling))
all_group_stats <- data.frame(emotion = unique(plot_data_long$feeling), 
                              test_stat = numeric(N),
                              p_val = numeric(N))

for(feeling in unique(plot_data_long$feeling)){
  test_res <- wilcox.test(as.formula(paste0('feels_', feeling, ' ~ group')), data=data)
  all_group_stats[all_group_stats['emotion']==feeling, 'test_stat'] = test_res$statistic
  all_group_stats[all_group_stats['emotion']==feeling, 'p_val'] = test_res$p.value
}

all_group_stats$p_corrected <- p.adjust(all_group_stats$p_val)
