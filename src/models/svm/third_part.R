#### Multiclass Support Vector Machine ####
## This script implements the third and last part of the automated job that
## fits a support vector machine for earthquake classification problem ##

cat("--importing dependencies----\n")

# importing SVM implementation package
if (!require("e1071")) {
  install.packages("e1071")
  library("e1071")
}
if (!require("fastDummies")) {
  install.packages("fastDummies")
  library("fastDummies")
}

# retrieving data from imported environment
test_indexes <- export_first_part$test_indexes
dataset <- export_first_part$dataset[test_indexes, ]
dataset$damage_grade <- factor(dataset$damage_grade)

cat("--predicting samples from test_dataset (reamining 40% of original set)----\n")

# transforming factors in dummy variables and scaling test datasets linearly
select_columns <- c("geo_level_1_id", "land_surface_condition", "foundation_type", "roof_type",
                    "ground_floor_type", "other_floor_type", "position", "plan_configuration",
                    "legal_ownership_status")
test_dataset <- dummy_cols(dataset, remove_selected_columns = TRUE, select_columns = select_columns)
scaled_test_dataset <- test_dataset

scaling_factors <- export_second_part$scaling_factors
scaling_shifts <- export_second_part$scaling_shifts
k = 1
for (predictor in test_dataset) {
  if (!is.factor(predictor)) {
    scaled_test_dataset[, k] <- scaled_test_dataset[, k]*scaling_factors[k] - scaling_shifts[k]
  }
  k = k + 1
}

# classifying unseen test samples
validation_predictions <- predict(second_part$svm_classifier, newdata = scaled_test_dataset[, !(names(scaled_test_dataset) %in% c("damage_grade"))])
validation_accuracy <-  table(scaled_test_dataset$damage_grade == validation_predictions)["TRUE"] / length(scaled_test_dataset$damage_grade)
validation_confusion_matrix <- table(scaled_complete_training_dataset$damage_grade, validation_predictions)
