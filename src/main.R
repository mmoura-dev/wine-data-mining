# setwd("~/wine-data-mining")
library("daltoolbox")
library(tidyr)
library("dplyr")
library("RColorBrewer")
library("ggplot2")
library(gridExtra)
library(tidyverse)
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/tutorial/graphics_extra.R")
source("src/flights_data.R")


# Loading data
jk_flights <- get_flight_data(2023, "SBBR")


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
