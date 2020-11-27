#### Multiclass Support Vector Machine ####
## This script implements the second part of the automated job that
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
training_indexes <- export_first_part$training_indexes
dataset <- export_first_part$dataset

cat("--subsampling a larger dataset for fine-grained grid search----\n")

# generating a training subset of size l
n_training <- length(training_indexes)
l <- 20000
subset_training_indexes <- sample(c(1:n_training), l, FALSE)
test_indexes <- setdiff(c(1:n_training), subset_training_indexes)
sampled_dataset <- dataset[subset_training_indexes, ]
sampled_dataset$damage_grade <- factor(sampled_dataset$damage_grade)
test_dataset <- dataset[test_indexes, ]
test_dataset$damage_grade <- factor(test_dataset$damage_grade)

# transforming factors in dummy variables and scaling training and test datasets linearly
select_columns <- c("geo_level_1_id", "land_surface_condition", "foundation_type", "roof_type",
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

cat("--running 5-fold CV for fine-grained grid search----\n")

# tuning SVM classifier over a grid search
# IMPORTANT: adjust gamma and cost MANUALLY as function of the best_parameters imported from first_part environment
type <- "C-classification"
kernel <- "radial"
gamma <- 2^seq(-5, -1, by=1)
cost <- 2^seq(0, 4, by=1)

tuned_grid_search_svm <- tune.svm(damage_grade ~ .,
                            data = scaled_training_dataset,
                            gamma = gamma, cost = cost,
                            scale = FALSE, kernel = kernel, type = type, cachesize = 200,
                            tune.control(sampling = c("cross"), cross = 5))
print(tuned_grid_search_svm)

cat("--training final classifier over the whole training set (60% of original set)----\n")

# transforming factors in dummy variables and scaling whole training set
training_dataset <- dummy_cols(dataset, remove_selected_columns = TRUE, select_columns = select_columns)
training_dataset$damage_grade <- factor(training_dataset$damage_grade)
scaled_complete_training_dataset <- training_dataset
scaling_factors <- rep(0, dim(training_dataset)[2])
scaling_shifts <- rep(0, dim(training_dataset)[2])
k = 1
for (predictor in training_dataset) {
  if (!is.factor(predictor)) {
    if ((max(predictor) - min(predictor)) != 0) {
      scaling_factors[k] <- 1/(max(predictor) - min(predictor))
      scaling_shifts[k] <- min(predictor)/(max(predictor) - min(predictor))
      scaled_complete_training_dataset[, k] <- predictor*scaling_factors[k] - scaling_shifts[k]
    } else if (max(predictor) != 0) {
      scaling_factors[k] <- 1/max(predictor)
      scaling_shifts[k] <- 0
      scaled_complete_training_dataset[, k] <- predictor*scaling_factors[k] - scaling_shifts[k]
    } else {
      scaling_factors[k] <- 1
      scaling_shifts[k] <- 0
      scaled_complete_training_dataset[, k] <- predictor*scaling_factors[k] - scaling_shifts[k]
    }
  }
  k = k + 1
}

# training SVM over the whole training dataset
best_gamma <- tuned_grid_search_svm$best.parameters$gamma
best_cost <- tuned_grid_search_svm$best.parameters$cost
svm_classifier <- svm(scaled_complete_training_dataset$damage_grade ~ .,
                      data = scaled_complete_training_dataset[, !(names(scaled_complete_training_dataset) %in% c("damage_grade"))],
                      gamma = best_gamma, cost = best_cost,
                      scale = FALSE, kernel = kernel, type = type, cachesize = 200)

# calculating in-sample metrics
in_sample_predictions <- predict(svm_classifier, newdata = scaled_complete_training_dataset[, !(names(scaled_complete_training_dataset) %in% c("damage_grade"))])
in_sample_accuracy <-  table(scaled_complete_training_dataset$damage_grade == in_sample_predictions)["TRUE"] / length(scaled_complete_training_dataset$damage_grade)
in_sample_confusion_matrix <- table(scaled_complete_training_dataset$damage_grade, in_sample_predictions)
