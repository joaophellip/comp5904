#### Multiclass Support Vector Machine ####
## This script implements the first part of the automated job that
## fits a support vector machine for earthquake classification problem ##

cat("--importing dependencies----\n")

# importing dataset
source("src/data/earthquake_damage.R")
source("src/models/metric.R")
data <- Earthquake_data()
data$generate()

# importing SVM implementation package
if (!require("e1071")) {
  install.packages("e1071")
  library("e1071")
}
if (!require("fastDummies")) {
  install.packages("fastDummies")
  library("fastDummies")
}

cat("--pre processing dataset----\n")

# dropping some predictors from merged_dataset
#drops <- c("geo_level_2_id", "geo_level_3_id")
#dataset <- data$merged_dataset[, !(names(data$merged_dataset) %in% drops)]
dataset <- data$merged_dataset

# reserving 40% of dataset for test
n <- dim(dataset)[1]
l <- ceiling(0.6*n)
training_indexes <- sample(c(1:n), l, FALSE)
test_indexes <- setdiff(c(1:n), training_indexes)
dataset <- dataset[training_indexes, ]

# generating a training subset of size l
n_training <- length(training_indexes)
l <- 40000
subset_training_indexes <- sample(c(1:n_training), l, FALSE)
test_indexes <- setdiff(c(1:n_training), subset_training_indexes)
sampled_dataset <- dataset[subset_training_indexes, ]
sampled_dataset$damage_grade <- factor(sampled_dataset$damage_grade)
test_dataset <- dataset[test_indexes, ]
test_dataset$damage_grade <- factor(test_dataset$damage_grade)

# transforming factors in dummy variables and scaling training and test datasets linearly
select_columns <- c("geo_level_1_id", "geo_level_2_id", "count_floors_pre_eq", "land_surface_condition", "foundation_type", "roof_type",
                    "ground_floor_type", "other_floor_type", "position", "plan_configuration",
                    "legal_ownership_status")
sampled_dataset <- dummy_cols(sampled_dataset, remove_selected_columns = TRUE, select_columns = select_columns)
test_dataset <- dummy_cols(test_dataset, remove_selected_columns = TRUE, select_columns = select_columns)
scaled_training_dataset <- sampled_dataset
scaled_test_dataset <- test_dataset
scaling_factors <- rep(0, dim(sampled_dataset)[2])
scaling_shifts <- rep(0, dim(sampled_dataset)[2])
k = 1
for (predictor in sampled_dataset) {
  if (!is.factor(predictor)) {
    if ((max(predictor) - min(predictor)) != 0) {
      scaling_factors[k] <- 1/(max(predictor) - min(predictor))
      scaling_shifts[k] <- min(predictor)/(max(predictor) - min(predictor))
      scaled_training_dataset[, k] <- predictor*scaling_factors[k] - scaling_shifts[k]
      scaled_test_dataset[, k] <- scaled_test_dataset[, k]*scaling_factors[k] - scaling_shifts[k]
    } else if (max(predictor) != 0) {
      scaling_factors[k] <- 1/max(predictor)
      scaling_shifts[k] <- 0
      scaled_training_dataset[, k] <- predictor*scaling_factors[k] - scaling_shifts[k]
      scaled_test_dataset[, k] <- scaled_test_dataset[, k]*scaling_factors[k] - scaling_shifts[k]
    } else {
      scaling_factors[k] <- 1
      scaling_shifts[k] <- 0
      scaled_training_dataset[, k] <- predictor*scaling_factors[k] - scaling_shifts[k]
      scaled_test_dataset[, k] <- scaled_test_dataset[, k]*scaling_factors[k] - scaling_shifts[k]
    }
  }
  k = k + 1
}

cat("--fitting a SVM----\n")

# tuning SVM classifier over a grid search
type <- "C-classification"
kernel <- "radial"
svm_classifier <- svm(scaled_training_dataset$damage_grade ~ .,
                       data = scaled_training_dataset[, !(names(scaled_training_dataset) %in% c("damage_grade"))],
                       scale = FALSE, kernel = kernel, type = type, cachesize = 200)

# in_sample_predictions <- predict(svm_classifier, newdata = scaled_training_dataset[, !(names(scaled_training_dataset) %in% c("damage_grade"))])
# in_sample_accuracy <-  table(scaled_training_dataset$damage_grade == in_sample_predictions)["TRUE"] / length(scaled_training_dataset$damage_grade)

# cat("--predicting samples from test_dataset (reamining 40% of original set)----\n")
# 
# out_of_sample_predictions <- predict(svm_classifier, newdata = scaled_test_dataset[, !(names(scaled_test_dataset) %in% c("damage_grade"))])
# out_of_sample_accuracy <-  table(scaled_test_dataset$damage_grade == out_of_sample_predictions)["TRUE"] / length(scaled_test_dataset$damage_grade)