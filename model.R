# ordinal meta-analysis script
# data publicly available from published manuscripts
# goal: estimate treatment effect of thrombectomy using full ordinal data
# background: stroke trials have ordinal outcomes but existing meta-analyses don't model it

# libraries ---------------------------------------------------------------
library(here)
library(tidyverse)
library(cmdstanr)
library(brms)

# set up data -------------------------------------------------------------

data <- read.csv(here("data.csv")) %>% 
  as_tibble() %>% 
  mutate(treatment = case_when(
    treatment == 1 ~ "thrombectomy",
    treatment == 0 ~ "medical"))

data$treatment <- factor(data$treatment)
data$trial <- factor(data$trial)

data_long <- data %>%
  pivot_longer(
    cols = -c(trial, year, treatment),
    names_to = "ordinal_value", 
    values_to = "count"
  ) %>% 
  uncount(count) %>%
  arrange(treatment, ordinal_value) %>% 
  mutate(ordinal_value = case_when(
    ordinal_value == "zero" ~ 7,
    ordinal_value == "one" ~ 6, 
    ordinal_value == "two" ~ 5,
    ordinal_value == "three" ~ 4,
    ordinal_value == "four" ~ 3,
    ordinal_value == "five" ~ 2,
    ordinal_value == "six" ~ 1
  ))

# fit model to data -------------------------------------------------------

seed <- 123

formula <- ordinal_value ~ treatment + (1 + treatment | trial)

priors <- c(prior(normal(0, 1), class = "b"),
            prior(normal(0, 1), class = "sd"),
            prior(normal(0, 1), class = "Intercept"))

model <- brm(
  formula = formula,
  prior = priors,
  sample_prior = TRUE,
  data = data_long,
  family = cumulative(link = "logit"),
  chains = 4,
  cores = 4,
  threads = threading(4),
  backend = "cmdstanr",
  silent = 0,
  seed = seed
)

model <- add_criterion(model, "loo")

saveRDS(model, here("model.rds"))
