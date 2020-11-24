#### Multiclass Support Vector Machine ####
## This script fits a support vector machine for earthquake classification problem ##

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
drops <- c("geo_level_2_id", "geo_level_3_id")
dataset <- data$merged_dataset[, !(names(data$merged_dataset) %in% drops)]
#dataset <- data$merged_dataset

# reserving 40% of dataset for test
n <- dim(dataset)[1]
l <- ceiling(0.6*n)
training_indexes <- sample(c(1:n), l, FALSE)
test_indexes <- setdiff(c(1:n), training_indexes)
dataset <- dataset[training_indexes, ]

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

cat("--running 5-fold CV for a first grid search----\n")

# training SVM classifier
type <- "C-classification"
kernel <- "radial"
obj <- tune.svm(damage_grade ~ .,
         data = scaled_training_dataset,
         gamma = 2^seq(-15, 3, by=3), cost = 2^seq(-5, 15, by=3),
         scale = FALSE, kernel = kernel, type = type, cachesize = 200,
         tune.control(sampling = c("cross"), cross = 5))

cat("--done----\n")
print(obj)

cat("--running 5-fold CV for fine-grained grid search----\n")

# 1) split again training_dataset with l = 100000.
# 2) scale sampled_dataset
# 3) call tune.svm with gamma and cost around obj$best$gamma and obj$best$cost resp

cat("--training final classifier over the whole training set (60% of original set)----\n")

# 1) scale whole training_dataset
# 2) call svm with gamma and cost from best of fine-grained

# svm_classifier <- svm(scaled_training_dataset$damage_grade ~ .,
#                       data = scaled_training_dataset[, !(names(scaled_training_dataset) %in% c("damage_grade"))],
#                       scale = FALSE, kernel = kernel, type = type, cachesize = 200)
# in_sample_predictions <- predict(svm_classifier, newdata = scaled_training_dataset[, !(names(scaled_training_dataset) %in% c("damage_grade"))])
# in_sample_accuracy <-  table(scaled_training_dataset$damage_grade == in_sample_predictions)["TRUE"] / length(scaled_training_dataset$damage_grade)

cat("--predicting samples from test_dataset (reamining 40% of original set)----\n")

# 1) scale whole test_dataset
# 2) call predict

# out_of_sample_predictions <- predict(svm_classifier, newdata = scaled_test_dataset[, !(names(scaled_test_dataset) %in% c("damage_grade"))])
# out_of_sample_accuracy <-  table(scaled_test_dataset$damage_grade == out_of_sample_predictions)["TRUE"] / length(scaled_test_dataset$damage_grade)