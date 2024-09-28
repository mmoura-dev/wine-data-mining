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
  select(-c(expected_depart, expected_arrival, di, type, depart_visibility,
            depart_pressure, depart_sky_coverage, depart_wind_direction_cat, flight))


# Min-Max
norm <- minmax()
norm <- fit(norm, df)
df <- transform(norm, df)
summary(df)


# Category mapping
cat_cols_to_map <- c("arrival", "company", "depart_wind_speed_scale",
                     "depart_day_period")
for (col in cat_cols_to_map) {
  cm <- categ_mapping(col)
  aux_cm <- transform(cm, df)
  df <- df %>% select(-one_of(col))
  df <- cbind(df, aux_cm)
}


# Extracting levels
slevels <- as.factor(unique(df$target))


# Random sampling
set.seed(1)
sr <- train_test(sample_random(), df)
df_train <- sr$train
df_test <- sr$test


# Model training
model <- cla_rf("target", slevels, mtry=8, ntree=128)
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
