library(tidyverse)

data <- read_csv('/Users/juusu53/Documents/projects/endometrioosi/age_and_gender_matched_subs_endo_helsinki_05_2023.csv')

sum(abs(data$age - data$control_age) > 5)

mean(data$age - data$control_age)

t.test(data$age, data$control_age)

data %>% 
ggplot() + 
  geom_histogram(aes(x=age), fill='pink', alpha=0.5) +
  geom_histogram(aes(x=control_age), fill='seagreen', alpha=0.5) +
  theme_minimal() +
  ggtitle('Endometrioosipotilaiden ja kivuttomien verrokkien ikäjakauma')
