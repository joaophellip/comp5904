library(xgboost)
library(vtreat)
library (tidyverse)
library(reshape2)
#source("src/data/earthquake_damage.R")

data <- Earthquake_data()
data$generate()

# Juntando as features com a variável resposta
damage <- as_tibble(cbind(data$dataset,
                          damage_grade = factor(data$class_response, 
                                                labels = c("low", "medium", "severe"))))

# Separando entre conjunto de treinamento e teste
set.seed(135)
index <- sample(1:260601, ceiling(0.6 * 260601))
damage_test <- damage[-index, ]
damage <- damage[index, ]

damage <- damage %>% mutate(geo_level_2_id = scale(as.integer(geo_level_2_id)),
                            geo_level_3_id = scale(as.integer(geo_level_3_id)),
                            count_floors_pre_eq = scale(count_floors_pre_eq),
                            age = scale(age),
                            area_percentage = scale(area_percentage),
                            height_percentage = scale(height_percentage),
                            count_families = scale(count_families))

damage_test <- damage_test %>% mutate(geo_level_2_id = scale(as.integer(geo_level_2_id)),
                                      geo_level_3_id = scale(as.integer(geo_level_3_id)),
                                      count_floors_pre_eq = scale(count_floors_pre_eq),
                                      age = scale(age),
                                      area_percentage = scale(area_percentage),
                                      height_percentage = scale(height_percentage),
                                      count_families = scale(count_families))

#Manipulação de dados para o uso do pacote
drop <- c("damage_grade", "building_id", "count_floors_pre_eq")
damage_boo <- damage[,!(names(damage) %in% drop)]
test_boo <- damage_test[,!(names(damage_test) %in% drop)]


features <- setdiff(names(damage_boo), damage$damage_grade)
treatplan <- vtreat::designTreatmentsZ(damage_boo, features, verbose = FALSE)
new_vars <- treatplan %>%
  magrittr::use_series(scoreFrame) %>%        
  dplyr::filter(code %in% c("clean", "lev")) %>% 
  magrittr::use_series(varName)     

features_train <- vtreat::prepare(treatplan, damage_boo, varRestriction = new_vars) %>% as.matrix()
response_train <- damage$damage_grade

features_test <- vtreat::prepare(treatplan, test_boo, varRestriction = new_vars) %>% as.matrix()
response_test <- damage_test$damage_grade


# create hyperparameter grid
hyper_grid <- expand.grid(
  eta = c(0.2, 0.3),  
  max_depth = c(6, 10),  
  min_child_weight = c(5, 10, 100),
  gamma=c(1, 2, 5),
  subsample = c(.6, .8, 1), 
  colsample_bytree = c(.6, .8, 1),
  optimal_trees = 0,               
  min_Merror = 0                   
)


for(i in 1:nrow(hyper_grid)) {
  # create parameter list
  params <- list(
    eta = hyper_grid$eta[i],
    gamma = hyper_grid$gamma[i],
    max_depth = hyper_grid$max_depth[i],
    min_child_weight = hyper_grid$min_child_weight[i],
    subsample = hyper_grid$subsample[i],
    colsample_bytree = hyper_grid$colsample_bytree[i]
  )
  
  # reproducibility
  set.seed(123)
  
  # train model
  xgb.tune<-xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 250,
    nfold = 3,
    objective = "multi:softmax",
    num_class = 4,
    nthread = 2,
    #verbose = 0,               # silent,
    early_stopping_rounds = 3 # stop if no improvement for 3 consecutive trees
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_merror_mean)
  hyper_grid$min_merror[i] <- min(xgb.tune$evaluation_log$test_merror_mean)

  
  
}

hyper_grid %>%
  dplyr::arrange(min_merror) %>%
  head(10)


#FINAL
set.seed(123)

xgb.fit1 <- xgboost(
  data = features_train,
  label = response_train,
  eta=0.2 ,
  max_depth = 10,
  min_child_weight = 5,
  nrounds = 149,
  objective = "multi:softmax",
  num_class = 4,
  verbose = 0,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 0.6
)



#Predição na base de teste
pred <- predict(xgb.fit1, features_test , type = "class")
tab<-table(pred,response_test)
#Acurácia na base de teste
sum(diag(tab)) / sum(tab)

