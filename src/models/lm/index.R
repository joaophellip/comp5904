#### Linear regression of an Indication Matrix ####
## This script runs a regression model over the indicator matrix for the earthquake classification problem ##
## outputs objects:
##  - in_sample_f1: micro averaged f1 score taken over the training sample.
##  - in_sample_performance: hit ratio over the training sample.
##  - data: object generated using class Earthquake_data. This object holds the dataset for the earthquake competition.
##    See class definition at src/data/earthquake_damage.R for reference.
####

# script configuration values
plot_graphs = FALSE
enable_cross_validation <- FALSE
diagnostic_mode <- FALSE
enable_basis_transformation <- c(TRUE, 2)
# if first element of enable_basis_transformation is TRUE, the second is
# polynomial order for the transformation, e.g. an order 2 means a transformation 
# to include the square of the input variable space an their cross-products

cat("--importing dependencies----\n")

source("src/data/earthquake_damage.R")
source("src/models/metric.R")

data <- Earthquake_data()
data$generate()

# dropping some variables from dataset in order to speed up execution time
drops <- c("geo_level_2_id", "geo_level_3_id")

# expanding basis if basis transtormation is enabled
if (enable_basis_transformation[1] == TRUE) {
  dataset <- data$dataset[, !(names(data$dataset) %in% drops)]
  if (enable_basis_transformation[2] == 2) {
    # todo: add quadratic transformation to variables in dataset
  }
} else {
  dataset <- data$dataset[, !(names(data$dataset) %in% drops)]
}

cat("--fitting regression models for each response indicator variable----\n")

# fitting linear models
lm_low_damage <- lm(data$indicator_response$low_damage ~ ., dataset)
lm_medium_damage <- lm(data$indicator_response$medium_damage ~ ., dataset)
lm_severe_damage <- lm(data$indicator_response$severe_damage ~ ., dataset)
lm_predictions <- data.frame(lm_low_damage$fitted.values, lm_medium_damage$fitted.values,
                            lm_severe_damage$fitted.values)

# classifying to the class with largest fit, i.e. the higher estimated conditional probability
training_classification <- as.integer(apply(lm_predictions, 1, which.max))

# calculating in-sample hit ratio (number of correct classifications over the total number of samples)
in_sample_performance <- table(training_classification == data$class_response)["TRUE"] / length(training_classification)

# calculating in-sample micro avg F1 score (competition performance metric)
# PS: f1, precision, and recall should be the same as we taking the contribution of all classes as equally weighted
# PS: as total number of false positives/negatives sums uo to the total number of incorrect predictions, it turns out f1 is equal the hit ratio (in_sample_performance)
low_TP <- table(training_classification == data$class_response & data$class_response == 1)["TRUE"]
low_FP <- table(training_classification != data$class_response & training_classification == 1)["TRUE"]
low_FN <- table(training_classification != data$class_response & data$class_response == 1)["TRUE"]
medium_TP <- table(training_classification == data$class_response & data$class_response == 2)["TRUE"]
medium_FP <- table(training_classification != data$class_response & training_classification == 2)["TRUE"]
medium_FN <- table(training_classification != data$class_response & data$class_response == 2)["TRUE"]
severe_TP <- table(training_classification == data$class_response & data$class_response == 3)["TRUE"]
severe_FP <- table(training_classification != data$class_response & training_classification == 3)["TRUE"]
severe_FN <- table(training_classification != data$class_response & data$class_response == 3)["TRUE"]
in_sample_f1 <- fMicro(low_TP + medium_TP + severe_TP, low_FP + medium_FP + severe_FP, low_FN + medium_FN + severe_FN)

# plotting predicted ranges for all three class predictions
if (plot_graphs) {
  boxplot(lm_predictions$lm_low_damage.fitted.values, lm_predictions$lm_medium_damage.fitted.values, 
          lm_predictions$lm_severe_damage.fitted.values, names = c("low damage", "medium damage", "severe damage"))
  title("Estimated conditional probability")
}

# removing debuggable variables
if (diagnostic_mode == FALSE) {
  rm(list = c("drops", "dataset", "lm_low_damage", "lm_medium_damage", "lm_severe_damage"))
  rm(list = c("low_TP", "medium_TP", "severe_TP", "low_FP", "medium_FP", "severe_FP", "low_FN", "medium_FN", "severe_FN"))
  rm(list = c("plot_graphs", "enable_cross_validation"))
  rm(list = c("fMicro", "Fmicro"))
  rm(list = c("Earthquake_data"))
}