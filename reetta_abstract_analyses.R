library(tidyverse)
library(janitor)
library(diffdf)
library(rstatix)
library(ggpubr)
library(ggplot2)

data <- read_csv2("/Users/juusu53/Documents/projects/endometrioosi/JUULIA_IMI_OMA_data_subidORDER.csv")

colnames(data)
emotion_cols <- c("emotions_0_total",
                  "emotions_1_total",
                  "emotions_2_total",
                  "emotions_3_total",
                  "emotions_4_total",
                  "emotions_5_total",
                  "emotions_6_total")

informative_emotion_cols <- c("sadness_0_total",
                              "happiness_1_total",
                              "anger_2_total",
                              "surprise_3_total",
                              "fear_4_total",
                              "disgust_5_total",
                              "neutral_6_total")

lower_torso_cols <- c("emotions_0_lower_torso_total",
                        "emotions_1_lower_torso_total",
                        "emotions_2_lower_torso_total",
                        "emotions_3_lower_torso_total",
                        "emotions_4_lower_torso_total",
                        "emotions_5_lower_torso_total",
                        "emotions_6_lower_torso_total")

maia_cols <- c("MAIA_Noticing",
               "MAIA_not_worrying",
               "MAIA_atten_regul",
               "MAIA_emot_awaren",
               "MAIA_self_regul",
               "MAIA_body_listen",
               "MAIA_trusting")

pain_and_sensitivity_cols <- c("pain_0_pos_color",
                              "pain_1_pos_color",
                              "sensitivity_0_pos_color",
                              "sensitivity_1_pos_color",
                              "sensitivity_2_pos_color")

data_new <- data %>% mutate(
  emotions_0_total = emotions_0_pos_color + emotions_0_neg_color,
  emotions_1_total = emotions_1_pos_color + emotions_1_neg_color,
  emotions_2_total = emotions_2_pos_color + emotions_2_neg_color,
  emotions_3_total = emotions_3_pos_color + emotions_3_neg_color,
  emotions_4_total = emotions_4_pos_color + emotions_4_neg_color,
  emotions_5_total = emotions_5_pos_color + emotions_5_neg_color,
  emotions_6_total = emotions_6_pos_color + emotions_6_neg_color
)

data <- data %>% mutate(
  emotions_0_total = sadness_0_total,
  emotions_1_total = happiness_1_total,
  emotions_2_total = anger_2_total,
  emotions_3_total = surprise_3_total,
  emotions_4_total = fear_4_total,
  emotions_5_total = disgust_5_total,
  emotions_6_total = neutral_6_total
)

compare_df_cols(data, data_new, return = "mismatch")
diffdf(data, data_new)

all_equal(data %>% select(emotion_cols), data_new %>% select(emotion_cols))
#rows seem to be different, why?!

data %>% select(emotion_cols) %>% tail()
data_new %>% select(emotion_cols) %>% tail()

# looks good, maybe a rounding issue?
all_equal(data %>% select(emotion_cols) %>% mutate_all(round), data_new %>% select(emotion_cols) %>% mutate_all(round))
# yup, good to go!


### let's make some correlation plots!

my.palette <- rev(get_palette("RdBu", 200))


# whole body emotion colorings
corr_list <- data %>% select(c(informative_emotion_cols, maia_cols)) %>% 
  rstatix::cor_test(vars=maia_cols, vars2=informative_emotion_cols) %>% 
  adjust_pvalue() %>% add_significance("p.adj") %>% 
  rename(p_unadj = p,
         p = p.adj)
corr_matrix <- as_cor_mat(corr_list)

pdf("/Users/juusu53/Documents/projects/endometrioosi/MAIA_vs_tunnevarit.pdf") 
corr_matrix %>%
  cor_plot(label = TRUE,
           palette = my.palette)
dev.off()

# lower torso emotion coloring
corr_list_lt <- data %>% select(c(lower_torso_cols, maia_cols)) %>% 
  rstatix::cor_test(vars=maia_cols, vars2=lower_torso_cols) %>% 
  adjust_pvalue() %>% add_significance("p.adj") %>% 
  rename(p_unadj = p,
         p = p.adj)
corr_matrix_lt <- as_cor_mat(corr_list_lt)

pdf("/Users/juusu53/Documents/projects/endometrioosi/MAIA_vs_tunne_lower_torso.pdf") 
corr_matrix_lt %>%
  cor_plot(label = TRUE,
           palette = my.palette)
dev.off()

# pain and sensitivity coloring
corr_list_ps <- data %>% select(c(pain_and_sensitivity_cols, maia_cols)) %>% 
  rstatix::cor_test(vars=maia_cols, vars2=pain_and_sensitivity_cols) %>% 
  adjust_pvalue() %>% add_significance("p.adj") %>% 
  rename(p_unadj = p,
         p = p.adj)
corr_matrix_ps <- as_cor_mat(corr_list_ps)

pdf("/Users/juusu53/Documents/projects/endometrioosi/MAIA_vs_pain_and_sensitivity.pdf") 
corr_matrix_ps %>%
  #cor_reorder() %>%
  cor_plot(label = TRUE,
           palette = my.palette,
           p.mat = data.matrix(p_vals),
           significant.level = 0.05)
dev.off()





