library(tidyverse)
library(broom)
library(broom.helpers)
library(readxl)
library(kableExtra)

# Load the dataset
primary_univariate = read_excel("Data/Post-COVID Dataset for Yifei.xlsx", sheet = "primary univariate dataset") %>% janitor::clean_names()
secondary_univariate = read_excel("Data/Post-COVID Dataset for Yifei.xlsx", sheet = "secondary univariate dataset") %>% janitor::clean_names()


# Merge two datasets as one:
secondary_univariate = secondary_univariate %>% 
  rename(record_id = record_id_2) 
  
# Data cleaning:
dat = left_join(primary_univariate, secondary_univariate, by = "record_id") %>% 
  mutate(barthel = case_when(
    barthel >= 0 & barthel <= 90 ~ 0,
    barthel >= 95 & barthel <= 100 ~ 1)) %>%
  mutate(mrs = case_when(
    mrs >= 0 & mrs <= 2 ~ 0,
    mrs >= 3 & mrs <= 6 ~ 1)) %>%
  mutate(ethnicity = ifelse(ethnicity == 3, NA, ethnicity),
         race = ifelse(race == 8, NA, race)) %>%
  mutate(education = case_when(
    education %in% c(0, 1, 2) ~ 1,
    education == 3 ~ 2,
    education == 4 ~ 3,
    education %in% c(5, 6, 7) ~ 4)) %>% 
  mutate(sex = factor(sex),
         race = factor(race),
         ethnicity = factor(ethnicity),
         language.x = factor(language.x),
         language.y = factor(language.y),
         education = factor(education),
         pmh_neuro = factor(pmh_neuro),
         pmh_enc_is_ich = factor(pmh_enc_is_ich),
         pmh_covid_severe = factor(pmh_covid_severe),
         icu = factor(icu),
         icu_2weeks = factor(icu_2weeks),
         steroids = factor(steroids),
         barthel = factor(barthel),
         mrs = factor(mrs),
         pasc_cognition = factor(pasc_cognition)) %>% 
  rename(language_primary = language.x,       ## language var in primary analysis
         language_secondary = language.y)     ## language var in secondary analysis

# Name of Covariates and outcomes:
covnames = c("age", "sex", "race", "ethnicity", "language_primary", "education", 
             "pmh_neuro", "pmh_enc_is_ich", "pmh_covid_severe", "icu", "icu_2weeks", "steroids",
             "ad8_14", "ecog_15", "ucla", "stress", "sleep", "dsm_depression", 
             "dsm_anxiety", "language_secondary", "vaccine")
outnames = c("memory_cs", "executive_cs", "processing_cs", "verbal_cs", 
             "global_cs", "barthel", "mrs", "ad8_28", "ecog_29", "pasc_cognition")


# Univariate analysis:
linear_models <- list()
logistic_models <- list()
## i for out, j for cov


##############################################################################################################################
### For linear regressions:
i_linear = c(1, 2, 3, 4, 5, 8, 9)
for (i in i_linear) {
  
  if (i > 7) {
    j_all <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15, 16, 17, 18, 19, 20, 21)
  } else {
    j_all <- 1:21
  }

  
  for (j in j_all) {
  fm <- paste0(outnames[i], "~", covnames[j])
  fm <- as.formula(fm)
  
  
  linear_models[[paste0("Model:", i, "~", j)]] <- lm(fm, data = dat) %>% tidy_and_attach(conf.int = TRUE) %>% tidy_remove_intercept()
  }
}

# print(linear_models)
  
  
# Create a kable to show the outputs:
linear_results_df <- data.frame(Model = character(), outcome = character(), covariate = character(), estimate = numeric(), std.error = numeric(), 
                                conf.low = numeric(), conf.high = numeric(), p.value = numeric())
for (model_name in names(linear_models)) {
  model <- linear_models[[model_name]]
  i_name <- as.numeric(sub("Model:(\\d+)~.*", "\\1", model_name))
  linear_results_df <- rbind(linear_results_df, 
                             data.frame(Model = model_name, outcome = outnames[i_name], covariate = model$term, 
                                        estimate = model$estimate, std.error = model$std.error, 
                                        conf.low = model$conf.low, conf.high = model$conf.high, p.value = model$p.value))
}
linear_table <- linear_results_df %>%
  mutate_at(vars(estimate, std.error, conf.low, conf.high), round, digits = 3) %>%
  kable(format = "html", escape = FALSE, caption = "Linear Regression Outputs") %>%
  kable_styling(full_width = FALSE)

# Display the table
linear_table



##############################################################################################################################
### For logistic regressions:
i_logistic = c(6, 7, 10)
for (i in i_logistic) {
  
  if (i > 5) {
    j_all <- 1:21
  }
  
  
  for (j in j_all) {
    fm2 <- paste0(outnames[i], "~", covnames[j])
    fm2 = as.formula(fm2)
    
    
    logistic_models[[paste0("Model:", i, "~", j)]] <- 
      glm(fm2, family = "binomial", data = dat) %>% 
      tidy_and_attach(conf.int = TRUE, exponentiate = TRUE) %>% tidy_remove_intercept()
  }
}

# print(logistic_models)


# Create a kable to show the outputs:
logistic_results_df <- data.frame(Model = character(), outcome = character(), covariate = character(), odds.ratio = numeric(), std.error = numeric(), 
                                  conf.low = numeric(), conf.high = numeric(), p.value = numeric())
for (model_name in names(logistic_models)) {
  model <- logistic_models[[model_name]]
  i_name <- as.numeric(sub("Model:(\\d+)~.*", "\\1", model_name))
  logistic_results_df <- rbind(logistic_results_df, 
                             data.frame(Model = model_name, outcome = outnames[i_name], covariate = model$term, odds.ratio = model$estimate, 
                                        std.error = model$std.error, conf.low = model$conf.low, conf.high = model$conf.high, 
                                        p.value = model$p.value))
}
logistic_table <- logistic_results_df %>%
  mutate_at(vars(odds.ratio, std.error, conf.low, conf.high), round, digits = 3) %>%
  kable(format = "html", escape = FALSE, caption = "Logistic Regression Outputs") %>%
  kable_styling(full_width = FALSE)

# Display the table
logistic_table

##############################################################################################################################