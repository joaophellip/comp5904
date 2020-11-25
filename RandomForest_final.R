library(randomForest)
library(tidyverse)
library(caret)
library(gridExtra)
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

#Florestas
#ESCOLHA DO MTRY.
drop <- c("building_id","count_floors_pre_eq")
damage_RF <- damage[,!(names(damage) %in% drop)]
damage_features <- damage_RF[,!(names(damage_RF) %in% "damage_grade")]

set.seed(100)
control <- trainControl(method="cv", number=10,returnResamp="all")
tunegrid <- expand.grid(.mtry=c(6:17))
caret_teste <- train(damage_grade ~., data=damage_RF, method="rf", metric="accuracy", 
                   tuneGrid=tunegrid, ntree = 25, trControl=control)

#Desbalanceado
set.seed(100)
fit.flor <- randomForest(damage_grade ~  ., 
                          data = damage_RF, ntree = 200, mtry = 8, 
                          importance = TRUE)

summary(fit.flor)
fit.flor

importance(fit.flor)
varImpPlot(fit.flor, scale = FALSE)
imp<-varImpPlot(fit.flor)
imp <- as.data.frame(imp)
imp$varnames <- rownames(imp) # row names to column
rownames(imp) <- NULL  

g1<-ggplot(imp, aes(x=reorder(varnames, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, )) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=MeanDecreaseAccuracy)) +
  ylab("MeanDecreaseAccuracy") +
  xlab("Variable Name") +
  coord_flip() + ggtitle("Sem tratamento")

#Predição na base de teste
predValid1 <- predict(fit.flor, damage_test, type = "class")
#Acurácia na base de teste
tab<-table(predValid1,damage_test$damage_grade)
sum(diag(tab)) / sum(tab)

#Balanceado
set.seed(100)
fit.flor.ba <- randomForest(damage_grade ~  ., 
                         data = damage_RF, ntree = 200, mtry = 8, 
                         importance = TRUE, sampsize = c(15040, 15040, 15040))

summary(fit.flor.ba)
fit.flor.ba

importance(fit.flor.ba)
varImpPlot(fit.flor.ba)

imp2<-varImpPlot(fit.flor.ba)
imp2 <- as.data.frame(imp2)
imp2$varnames <- rownames(imp2) # row names to column
rownames(imp2) <- NULL  

g2<-ggplot(imp2, aes(x=reorder(varnames, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy, )) + 
  geom_point() +
  geom_segment(aes(x=varnames,xend=varnames,y=0,yend=MeanDecreaseAccuracy)) +
  ylab("MeanDecreaseAccuracy") +
  xlab("")+
  coord_flip() + ggtitle("Com tratamento")

#Predição na base de teste
predValid1 <- predict(fit.flor.ba, damage_test, type = "class")
#Acurácia na base de teste
tab<-table(predValid1,damage_test$damage_grade)
sum(diag(tab)) / sum(tab)

grid.arrange(g1,g2,ncol=2, nrow=1)


