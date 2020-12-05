#### Multiclass Support Vector Machine ####
## This script fits a support vector machine for earthquake classification problem ##
####

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

# dropping some variables from dataset in order to speed up execution time
drops <- c("geo_level_2_id", "geo_level_3_id") # with sampling size of 10k, when we drop geo 2 e 3 we get the highest the oos accuracy. Possible explanation: for this size class 1 may offer information without too many levels that would make more difficult (i suppose) to separate in higher dimensions
dataset <- data$dataset[, !(names(data$dataset) %in% drops)] # total acc ~ 66
#dataset <- data$dataset # total acc ~ 56 if all factors or all numeric ; 66 if geo1 factor with geo2 and geo3 numeric

# generating a subset of size l from the dataset
# a subset of size 10K can be trained in a reasonable time frame (a few minutes if all predictors are included)
# note that using a 5-fold CV in svm, means 8k samples will be used for training and 2k for test in each iteration
n = dim(dataset)[1]
l = 10000
subsetTrainingIndexes <- sample(c(1:n), l, FALSE)
trainingDataset <- dataset[subsetTrainingIndexes, ]

cat("--fitting a C-Classification SVM with RFB Kernel. Using 5-fold cross validation and grid-search for C and gamma----")

# fitting SVM
scale - TRUE
k_fold = 5
type = "C-classification"
kernel= "radial"
gamma = 2^(-1:1)
cost = 2^(2:4)
svm <- svm(as.factor(data$class_response[subsetTrainingIndexes]) ~ ., data = trainingDataset, scale = scale, 
    kernel = kernel, type = type, cross = k_fold,
    cachesize = 400) #, gamma = gamma, cost = cost)