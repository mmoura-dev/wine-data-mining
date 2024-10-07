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
source("src/confusion_matrix.R")


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


model_evaluate <- function(model_name, model, df_train, df_test) {
  print(model_name)
  model <- fit(model, df_train)
  
  
  print("Model adjustment metrics")
  train_prediction <- unlist(lapply(predict(model, df_train)[,2], function(x) ifelse(x > 0.1, 1, 0)))
  df_train_predictand <- adjust_class_label(df_train[,"target"])[,2]
  print(get_confusion_matrix(df_train_predictand, train_prediction))
  
  
  print("Evaluation metrics")
  test_prediction <- unlist(lapply(predict(model, df_test)[,2], function(x) ifelse(x > 0.1, 1, 0)))
  df_test_predictand <- adjust_class_label(df_test[,"target"])[,2]
  print(get_confusion_matrix(df_test_predictand, test_prediction))

  # return (get_confusion_matrix(df_test_predictand, test_prediction))
}


model_evaluate("Random Forest", cla_rf("target", slevels, mtry=9, ntree=81), df_train, df_test)
model_evaluate("KNN", cla_knn("target", slevels, k=5), df_train, df_test)
# model_evaluate("Majority", cla_majority("target", slevels), df_train, df_test)
# model_evaluate("Decision Tree", cla_dtree("target", slevels), df_train, df_test)
model_evaluate("MLP", cla_mlp("target", slevels, size=3, decay=0.03), df_train, df_test)
model_evaluate("Naive Bayes", cla_nb("target", slevels), df_train, df_test)
model_evaluate("SVM", cla_svm("target", slevels, epsilon=0.0,cost=20.000), df_train, df_test)
