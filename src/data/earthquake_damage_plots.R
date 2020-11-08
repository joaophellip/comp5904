# Plots and descriptive analysis

cat("----loading libs----\n")

if (!require("mclust")) {
  install.packages("mclust")
  library("mclust")
}

cat("----plotting pairwise scatter----\n")

clPairs(dataset[1:1000, 2:4], cl = predictedDamage$damage_grade[1:1000])

clp <- clPairs(dataset[1:1000, 2:4], cl = predictedDamage$damage_grade[1:1000],
               lower.panel = NULL)

clPairsLegend(0.1, 0.3, class = c("low", "medium", "severe"),
              col = clp$col, pch = clp$pch,
              title = "Earthquake damage grade")