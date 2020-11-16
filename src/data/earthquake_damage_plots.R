# Plots and descriptive analysis

cat("----loading libs----\n")

if (!require("mclust")) {
  install.packages("mclust")
  library("mclust")
}
source("src/data/earthquake_damage.R")

cat("----plotting pairwise scatter----\n")

data <- Earthquake_data()
data$generate()

# plotting pairwise scatter for variables 1 and 4
clPairs(data$dataset[, c(1,4)], cl = data$class_response)

clp <- clPairs(data$dataset[, c(1,4)], cl = data$class_response,
               lower.panel = NULL)

clPairsLegend(0.1, 0.3, class = c("low", "medium", "severe"),
              col = clp$col, pch = clp$pch,
              title = "Earthquake damage grade")