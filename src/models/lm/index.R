# Linear regression of an Indication Matrix

cat("--importing dataset----\n")

source("src/data/earthquake_damage.R")

data <- Earthquake_data()
data$generate()

cat("--fitting regression models for each response indicator variable----\n")

drops <- c("geo_level_2_id", "geo_level_3_id")

lm_low_damage <- lm(data$indicator_response$low_damage ~ ., data$dataset[, !(names(data$dataset) %in% drops)])
lm_medium_damage <- lm(data$indicator_response$medium_damage ~ ., data$dataset[, !(names(data$dataset) %in% drops)])
lm_severe_damage <- lm(data$indicator_response$severe_damage ~ ., data$dataset[, !(names(data$dataset) %in% drops)])
 
lm_predictions <- data.frame(lm_low_damage$fitted.values, lm_medium_damage$fitted.values,
                            lm_severe_damage$fitted.values)

training_classification <- as.integer(apply(lm_predictions, 1, which.max))

in_sample_performance <- table(training_classification == data$class_response)["TRUE"] / length(training_classification)

rm(list = c("drops", "lm_low_damage", "lm_medium_damage", "lm_severe_damage"))