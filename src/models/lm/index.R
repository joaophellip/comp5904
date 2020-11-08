# Linear regression of an Indication Matrix

cat("--importing dataset----\n")

source("src/data/earthquake_damage.R")

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

# calculating in sample performance by taking the hit ratio (number of correct classifications over the total number of samples)
in_sample_performance <- table(training_classification == data$class_response)["TRUE"] / length(training_classification)

# plotting ranges for all three class predictions
boxplot(lm_predictions$lm_low_damage.fitted.values, lm_predictions$lm_medium_damage.fitted.values, 
        lm_predictions$lm_severe_damage.fitted.values, names = c("low damage", "medium damage", "severe damage"))
title("Estimated conditional probability")

# todo: add graph with boundary (points with same probability for all classes) 

rm(list = c("drops", "lm_low_damage", "lm_medium_damage", "lm_severe_damage"))