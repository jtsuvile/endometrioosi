setwd('/Users/juusu53/Documents/projects/kipupotilaat/')
source('./code/helper_functions_for_r_analysis.R')

library(tidyverse)
library(ggsignif)
library(rcompanion)
library(table1)

subs <- read.csv('data/endometrioosi/endometriosis_with_activations_11_2021.csv',
                 na.strings = 'NaN')
subs$batch <- 'patient'
subs_control <- read.csv('data/endometrioosi/endometriosis_controls_with_activations_11_2021.csv',
                         na.strings = 'NaN')
subs_control$batch <- 'control'

subs_fixed <- make_total_colouring_columns(subs) %>% rename_emotions()
subs_control_fixed <- make_total_colouring_columns(subs_control) %>% rename_emotions()
subs_all_big <- subs_fixed %>%bind_rows(subs_control_fixed) 

subs_all_big <- subs_all_big %>% mutate(bmi = weight/((height/100)^2),
                        education = factor(education, levels=c(1,2,3,4), labels=c('Peruskoulu','Ammattikoulu',
                                                                                  'Ammattikorkeakoulu','Yliopisto')))

table1(~ age + bmi + factor(education) + work_sitting + work_physical | batch, data=subs_all_big)
