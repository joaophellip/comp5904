################## Modelo multinomial ##############################

# Utilizando o pacote 'nnet'
library(nnet)

# Transformando as variáveis geo_level_2_id e geo_level_3_id em inteiras,
# e padronizando as demais variáveis inteiras
damage <- damage %>% mutate(geo_level_2_id = scale(as.integer(geo_level_2_id)),
                            geo_level_3_id = scale(as.integer(geo_level_3_id)),
                            age = scale(age),
                            area_percentage = scale(area_percentage),
                            height_percentage = scale(height_percentage),
                            count_families = scale(count_families)) %>% 
  dplyr::select(-count_floors_pre_eq)

damage_test <- damage_test %>% mutate(geo_level_2_id = scale(as.integer(geo_level_2_id)),
                                      geo_level_3_id = scale(as.integer(geo_level_3_id)),
                                      age = scale(age),
                                      area_percentage = scale(area_percentage),
                                      height_percentage = scale(height_percentage),
                                      count_families = scale(count_families)) %>% 
  dplyr::select(-count_floors_pre_eq)

library(doParallel)

no_cores <- detectCores() - 1  
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cores = no_cores) 

# Ajuste do modelo multinomial
mod <- multinom(damage_grade ~ ., data = damage, Hess = TRUE)

stopCluster(cl)  

# Predições do conjunto de validação -------------------------------------------
pred <- predict(mod, newdata = damage_test)

(tab <- table(train = damage_test$damage_grade, prediction = pred))

# Taxa de acertos
sum(diag(tab))/sum(tab)

# Submissão --------------------------------------------------------------------

test <- read.csv("./datasets/testDataset_earthquake.csv")

list_factors <- c("geo_level_1_id", "land_surface_condition", "foundation_type",
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
  test[list_factors[j]] <- factor(test[list_factors[j]][,1])
}


test <- test %>% mutate(geo_level_2_id = scale(as.integer(geo_level_2_id)),
                       geo_level_3_id = scale(as.integer(geo_level_3_id)),
                       count_floors_pre_eq = scale(count_floors_pre_eq),
                       age = scale(age),
                       area_percentage = scale(area_percentage),
                       height_percentage = scale(height_percentage),
                       count_families = scale(count_families))


pred <- predict(mod, newdata = test[,-1])
pred <- as.integer(factor(pred, labels = c("1", "2", "3")))


write.csv(data.frame(building_id = test$building_id,
                      damage_grade = pred),
                       "submission.csv", row.names = FALSE)


# Seleção de variáveis --------------------------------------------------------

# Coeficientes
EST <- c(coef(mod)[1,], coef(mod)[2,])

# Erros padrões
SE <-sqrt(diag(solve(mod$Hessian)))

# Valor Z
Z <- EST/SE

# VISUALIZANDO
sig <- tibble(Z2 = abs(Z)[1:89],
              Z3 = abs(Z)[90:178],
              coef = mod$coefnames)

### Coeficientes associados à classe 2 em relação à classe 1 ###

# Variáveis geográficas
sig[2:33,] %>% 
  ggplot(aes(x = coef, y = Z2)) +
    theme_classic() +
    geom_point() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
    geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
    labs(x = "Coeficiente", y = "Valor Z")

# Demais
sig[c(34:63, 75:78),] %>% 
  ggplot(aes(x = coef, y = Z2)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")


# Superestrutura
sig[64:74,] %>% 
  ggplot(aes(x = coef, y = Z2)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")

# Uso secundário
sig[79:89,] %>% 
  ggplot(aes(x = coef, y = Z2)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")



### Coeficientes associados à classe 3 em relação à classe 1 ###

# Variáveis geográficas
sig[2:33,] %>% 
  ggplot(aes(x = coef, y = Z3)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")

# Demais
sig[c(34:63, 75:78),] %>% 
  ggplot(aes(x = coef, y = Z3)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")


# Superestrutura
sig[64:74,] %>% 
  ggplot(aes(x = coef, y = Z3)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")

# Uso secundário
sig[79:89,] %>% 
  ggplot(aes(x = coef, y = Z3)) +
  theme_classic() +
  geom_point() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_hline(yintercept = qnorm(1-0.05/2), linetype = "dashed") +
  labs(x = "Coeficiente", y = "Valor Z")


