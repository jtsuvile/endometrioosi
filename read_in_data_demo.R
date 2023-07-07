# use libraries from the tidyverse family of packages
library(tidyverse)

# include helper functions from a file that is in the same folder
source('./helper_functions_for_r_analysis.R')


# read in data from patients
patients <- read.csv("~/Documents/projects/endometrioosi/data/endometriosis_with_activations_05_2023.csv") %>% 
  #add column with group definition
  mutate(group = 'patient')

# read in data from controls
controls <- read.csv("~/Documents/projects/endometrioosi/data/endometriosis_controls_with_activations_05_2023.csv") %>% 
  # add column with group definition
  mutate(group = 'control') %>% 
  # remove 'profession' column which we only have for controls
  select(-profession)

# combine patient data and data from controls
data <- bind_rows(patients, controls) %>% 
  # drop columns that we don't need
  select(-c(X, Unnamed..0)) %>% 
  # adjust emotion column names to be human readable
  rename_emotions() %>% 
  # adjust sensitivity column names to be human readable
  rename_sensitivity() %>% 
  # adjust pain column names to be human readable
  rename_pain()

# see all column names
colnames(data)

# a simple plot
ggplot(data, aes(x=feels_pain, y=currpain_pos_color, col=group)) + 
  geom_jitter() + 
  theme_minimal()
