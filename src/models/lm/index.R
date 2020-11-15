#### Linear regression of an Indication Matrix ####
## This script runs a regression model over the indicator matrix for the earthquake classification problem ##
## outputs objects:
##  - Earthquake_data: class builder. Use it to construct an object containing the earthquake dataset. See
##    src/data/earthquake_damage.R for reference.
##  - Fmicro: function that calculates the micro-averaged F1 score. See src/models/metric.R for reference.
####

# configuration values
plot_graphs = FALSE
enable_cross_validation <- FALSE

cat("--importing dependencies----\n")

source("src/data/earthquake_damage.R")
source("src/models/metric.R")

data <- Earthquake_data()
data$generate()

cat("--fitting regression models for each response indicator variable----\n")

# dropping some variables from dataset in order to speed up execution time
drops <- c("geo_level_2_id", "geo_level_3_id")

lm_low_damage <- lm(data$indicator_response$low_damage ~ ., data$dataset[, !(names(data$dataset) %in% drops)])
lm_medium_damage <- lm(data$indicator_response$medium_damage ~ ., data$dataset[, !(names(data$dataset) %in% drops)])
lm_severe_damage <- lm(data$indicator_response$severe_damage ~ ., data$dataset[, !(names(data$dataset) %in% drops)])
 
lm_predictions <- data.frame(lm_low_damage$fitted.values, lm_medium_damage$fitted.values,
                            lm_severe_damage$fitted.values)

# classifying to the class with largest fit, e.g. the higher estimated conditional probability
training_classification <- as.integer(apply(lm_predictions, 1, which.max))

# calculating in-sample hit ratio (number of correct classifications over the total number of samples)
in_sample_performance <- table(training_classification == data$class_response)["TRUE"] / length(training_classification)

# calculating in-sample micro avg F1 score (competition performance metric)
# PS: f1, precision, and recall should be the same for this problem as we taking the contribution of all classes as equal
# PS2: as total number of false positives/negatives total of incorrect predictions, it turns out f1 is equal the hit ratio (in_sample_performance)
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

if (plot_graphs) {
  # plotting ranges for all three class predictions
  boxplot(lm_predictions$lm_low_damage.fitted.values, lm_predictions$lm_medium_damage.fitted.values, 
          lm_predictions$lm_severe_damage.fitted.values, names = c("low damage", "medium damage", "severe damage"))
  title("Estimated conditional probability")  
}

# calculating the decision boundary: coefficients that define the hyperplane, i.e. the set of points with same probability for all classes
coefficients <- data.frame(lm_low_damage$coefficients, lm_medium_damage$coefficients,
                           lm_severe_damage$coefficients)
boundary_coefficients <- coefficients[,1] - coefficients[,2] - coefficients[,3]

# todo: plotting pairwise decision boundary for variables 1 and 4. Holding remaining variables to a default value

# todo : use cross validation flag to implement a K-folding CV

rm(list = c("drops", "lm_low_damage", "lm_medium_damage", "lm_severe_damage"))
rm(list = c("low_TP", "medium_TP", "severe_TP", "low_FP", "medium_FP", "severe_FP", "low_FN", "medium_FN", "severe_FN"))
rm(list = c("plot_graphs", "enable_cross_validation"))