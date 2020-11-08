# fields:
# dataset: design matrix X
# indicator_response: response matrix with three column vectors. For each observation, a value of 1 indicates sample belongs that class, otherwise 0
# class_response: response vector that contains class to which observation belong. Coded as numeric values {1,2,3}
Earthquake_data <- setRefClass("EarthquakeDataProcessing",
                                 fields = list(dataset = "data.frame",
                                               indicator_response = "data.frame", class_response = "numeric"),
                                 methods = list(
                                   generate = function() {
                                     
                                     # importing datasets from local csv files
                                     data <- read.csv("./datasets/trainingDataset_earthquake.csv")
                                     predictedDamage <- read.csv("./datasets/trainingDatasetLabels_earthquake.csv")

                                     # factoring variables based on pre defined list of variables to be interpreted as factors
                                     merged_dataset <- merge(data, predictedDamage, by = c("building_id") )
                                     list_factors <- c("geo_level_1_id", "geo_level_2_id", "geo_level_3_id", "land_surface_condition", "foundation_type",
                                                       "roof_type", "ground_floor_type", "other_floor_type", "position", "plan_configuration",
                                                       "has_superstructure_adobe_mud", "has_superstructure_mud_mortar_stone", "has_superstructure_stone_flag",
                                                       "has_superstructure_cement_mortar_stone", "has_superstructure_mud_mortar_brick",
                                                       "has_superstructure_cement_mortar_brick", "has_superstructure_timber", "has_superstructure_bamboo",
                                                       "has_superstructure_rc_non_engineered", "has_superstructure_rc_engineered", "has_superstructure_other",
                                                       "legal_ownership_status", "has_secondary_use", "has_secondary_use_agriculture", "has_secondary_use_hotel",
                                                       "has_secondary_use_rental", "has_secondary_use_institution", "has_secondary_use_school",
                                                       "has_secondary_use_industry", "has_secondary_use_health_post", "has_secondary_use_gov_office",
                                                       "has_secondary_use_use_police", "has_secondary_use_other")
                                     
                                     for (j in 1:length(list_factors)) {
                                       merged_dataset[list_factors[j]] <- factor(merged_dataset[list_factors[j]][,1])
                                     }
                                     
                                     # generating indicator_matrix
                                     n <- length(merged_dataset$damage_grade)
                                     
                                     indicator_matrix <- matrix(rep(0, 3*n), ncol = 3)
                                     indicator_matrix[, 1] <- ifelse(merged_dataset$damage_grade==1, 1, 0)
                                     indicator_matrix[, 2] <- ifelse(merged_dataset$damage_grade==2, 1, 0)
                                     indicator_matrix[, 3] <- ifelse(merged_dataset$damage_grade==3, 1, 0)
                                     
                                     # exporting dataset, indicator_response, and class_response
                                     dataset <<- as.data.frame(merged_dataset[,c(-1,-40)])
                                     response <- as.data.frame(indicator_matrix)
                                     
                                     names(response)[names(response) == "V1"] <- "low_damage"
                                     names(response)[names(response) == "V2"] <- "medium_damage"
                                     names(response)[names(response) == "V3"] <- "severe_damage"
                                     
                                     indicator_response <<- response
                                     class_response <<- merged_dataset$damage_grade

                                     rm(list = c("data", "predictedDamage", "n", "j", "list_factors", "merged_dataset", "indicator_matrix", "response"))
                                   }
                                 ))