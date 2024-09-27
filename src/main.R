library("daltoolbox")
library(tidyr)
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library(gridExtra)
library(tidyverse)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/tutorial/graphics_extra.R")


# Loads bfd data set
load("data/flights/bfd_2023.rdata")
# head(bfd)
# summary(bfd)


# Filters
jk_flights <- bfd %>%
  filter(depart == "SBBR") %>%
  select(-c("route", "real_depart", "real_arrival", "delay_arrival", "real_flight_length"), -starts_with("arrival_"), -starts_with("outlier_")) %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(status != "CANCELADO") %>%
  mutate(target = delay_depart > 15) %>%
  select(-status, -depart, -delay_depart)
  

# Factors (todo: is any of these factors ordered?)
jk_flights$arrival <- as.factor(jk_flights$arrival)
jk_flights$company <- as.factor(jk_flights$company)
jk_flights$flight <- as.factor(jk_flights$flight)
jk_flights$di <- as.factor(jk_flights$di)
jk_flights$type <- as.factor(jk_flights$type)
jk_flights$depart_sky_coverage <- as.factor(jk_flights$depart_sky_coverage)
jk_flights$target <- as.factor(jk_flights$target)


# Pre-processing
df <- jk_flights %>%
  drop_na() %>%
  select(-flight, -expected_depart, -expected_arrival)


# Extracting levels
slevels <- as.factor(unique(df$target))


# Random sampling
set.seed(1)
sr <- train_test(sample_random(), df)
df_train <- sr$train
df_test <- sr$test


# Model training
model <- cla_rf("target", slevels)
model <- fit(model, df_train)
train_prediction <- predict(model, df_train)


# Model adjustment
df_train_predictand <- adjust_class_label(df_train[,"target"])
train_eval <- evaluate(model, df_train_predictand, train_prediction)
print(train_eval$metrics)


# Model testing
test_prediction <- predict(model, df_test)
df_test_predictand <- adjust_class_label(df_test[,"target"])
test_eval <- evaluate(model, df_test_predictand, test_prediction)
print(test_eval$metrics)

