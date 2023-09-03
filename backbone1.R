library(tidyverse)
library(broom)
library(readxl)

# load the dataset
primary_univariate = read_excel("Data/Post-COVID Dataset for Yifei.xlsx", sheet = "primary univariate dataset")
secondary_univariate = read_excel("Data/Post-COVID Dataset for Yifei.xlsx", sheet = "secondary univariate dataset")

# merge two datasets as one:
secondary_univariate = secondary_univariate %>% 
  rename(record_id = record_id_2)
dat = left_join(primary_univariate, secondary_univariate, by = "record_id")

# Name of Covariates and outcomes:
covnames = c("age", "sex", "race", "ethnicity", "language", "education", "pmh_neuro", "pmh_enc_is", "pmh_covid_severe", "steroids",
             "ad8", "ecog", "ucla", "stress", "sleep", "dsm_depression", "dsm_anxiety", "language", "vaccine")
outnames = c("memory_cs", "executive_cs", "processing_cs", "verbal_cs", "global_cs", "barthel", "mrs", "dg8", "ecog", "pasc_cognition")


# univariate analysis:


