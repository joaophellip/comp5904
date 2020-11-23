#setwd("~/MAE0501 - Aprendizagem Estatística/Terremotos/comp5904/datasets")

#https://www.drivendata.org/competitions/57/nepal-earthquake/page/134/
#--------------------Lendo a base de dados-----------------------
treino <- read.csv('trainingDataset_earthquake.csv')
teste <- read.csv('testDataset_earthquake.csv')
resp_treino <- read.csv('trainingDatasetLabels_earthquake.csv')

#base completa
total <- rbind(treino, teste)

treino <- merge(treino, resp_treino, by = 'building_id')


#OBJETIVO: PREDIZER A VARIÁVEL ORDINAL "DAMAGE_GRADE", NA QUAL 1 REPRESENTA DANO PEQUENO, 2 REPRESENTA DANO MÉDIO E 3 DESTRUIÇÃO COMPLETA.

#--------------------Análise Descritiva das variáveis------------------

#Região geográfica
#quanto menor o valor mais genérico é a localização, level 1: 0-30, level 2: 0-1427, level 3: 0-12567
boxplot(treino$geo_level_1_id,
        xlab = 'Nível 1', ylab = 'localização',
        main = 'Boxplot para o nível 1 de localização')

boxplot(treino$geo_level_2_id,
        xlab = 'Nível 2', ylab = 'localização',
        main = 'Boxplot para o nível 2 de localização')

boxplot(treino$geo_level_3_id,
        xlab = 'Nível 3', ylab = 'localização',
        main = 'Boxplot para o nível 3 de localização')

boxplot(treino$geo_level_1_id~treino$damage_grade,
        xlab = 'Grau de destruição', ylab = 'Localização',
        main = 'Nível 1 de localização contra grau de destruição',
        names = c('Baixa','Média','Completa'))

boxplot(treino$geo_level_2_id~treino$damage_grade,
        xlab = 'Grau de destruição', ylab = 'Localização',
        main = 'Nível 2 de localização contra grau de destruição',
        names = c('Baixa','Média','Completa'))

boxplot(treino$geo_level_3_id~treino$damage_grade,
        xlab = 'Grau de destruição', ylab = 'Localização',
        main = 'Nível 3 de localização contra grau de destruição',
        names = c('Baixa','Média','Completa'))

#Número de andares
cont <- table(treino$count_floors_pre_eq,treino$damage_grade)
barplot(cont, main="Andares contra grau de destruição",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(9),
        legend = rownames(cont))

#Idade da construção
boxplot(treino$age~treino$damage_grade,
        xlab = 'Grau de destruição', ylab = 'Idade',
        main = 'Idade da construção contra grau de destruição',
        names = c('Baixa','Média','Completa'))

boxplot(treino$age[treino$age < 300]~treino$damage_grade[treino$age < 300],
        xlab = 'Grau de destruição', ylab = 'Idade',
        main = 'Idade da construção contra grau de destruição',
        names = c('Baixa','Média','Completa'))

#Area da construção (normalizado)
boxplot(treino$area_percentage~treino$damage_grade,
        xlab = 'Grau de destruição', ylab = 'Area',
        main = 'Area da construção contra grau de destruição',
        names = c('Baixa','Média','Completa'))

#Altura da construção (normalizado)
boxplot(treino$height_percentage~treino$damage_grade,
        xlab = 'Grau de destruição', ylab = 'Altura',
        main = 'Altura da construção contra grau de destruição',
        names = c('Baixa','Média','Completa'))

#Condição da superficie 
cont <- table(treino$land_surface_condition,treino$damage_grade)
barplot(cont, main="Condição da superficie ",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(3),
        legend = rownames(cont))

#Tipo de fundação
cont <- table(treino$foundation_type,treino$damage_grade)
barplot(cont, main="Tipo de fundação",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(5),
        legend = rownames(cont))

#Tipo de telhado usado
cont <- table(treino$roof_type,treino$damage_grade)
barplot(cont, main="Tipo de telhado usado",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(3),
        legend = rownames(cont))

#Tipo de solo
cont <- table(treino$ground_floor_type,treino$damage_grade)
barplot(cont, main="Tipo de solo",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(5),
        legend = rownames(cont))

#Tipo das construções acima do solo
cont <- table(treino$other_floor_type,treino$damage_grade)
barplot(cont, main="Tipo das construções acima do solo",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(4),
        legend = rownames(cont))

#Posição da construção
cont <- table(treino$position,treino$damage_grade)
barplot(cont, main="osição da construção",
        xlab="Grau de destruição", col=colorspace::sequential_hcl(4),
        legend = rownames(cont))

#Configuração do plano da construção
cont <- table(treino$plan_configuration,treino$damage_grade)
barplot(cont, main="Configuração do plano da construção",
        xlab="Grau de destruição", col=colorspace::rainbow_hcl(10),
        legend = rownames(cont))

#Juntando os tipos de materiais de construção
treino$structure <- 0
treino$structure[treino$has_superstructure_adobe_mud == 1] <- 1
treino$structure[treino$has_superstructure_mud_mortar_stone == 1] <- 2
treino$structure[treino$has_superstructure_stone_flag  == 1] <- 3
treino$structure[treino$has_superstructure_cement_mortar_stone  == 1] <- 4
treino$structure[treino$has_superstructure_mud_mortar_brick  == 1] <- 5
treino$structure[treino$has_superstructure_cement_mortar_brick  == 1] <- 6
treino$structure[treino$has_superstructure_timber  == 1] <- 7
treino$structure[treino$has_superstructure_bamboo  == 1] <- 8
treino$structure[treino$has_superstructure_rc_non_engineered == 1] <- 9
treino$structure[treino$has_superstructure_rc_engineered  == 1] <- 10
treino$structure[treino$has_superstructure_other  == 1] <- 11

table(treino$structure)

treino$structure <- as.factor(treino$structure)

#Construção foi feita de?
library(RColorBrewer)
cont <- table(treino$structure,treino$damage_grade)
barplot(cont, main="Andares contra grau de destruição",
        xlab="Grau de destruição", col=brewer.pal(11,"Spectral"),
        legend = rownames(cont))

#Situação legal do local?
cont <- table(treino$legal_ownership_status ,treino$damage_grade)
barplot(cont, main="Andares contra grau de destruição",
        xlab="Grau de destruição", col=brewer.pal(4,"Spectral"),
        legend = rownames(cont))

#Número de familias no local?
cont <- table(treino$count_families  ,treino$damage_grade)
barplot(cont, main="Andares contra grau de destruição",
        xlab="Grau de destruição", col=brewer.pal(9,"Spectral"),
        legend = rownames(cont))

#Tem uso secundário?
cont <- table(treino$has_secondary_use  ,treino$damage_grade)
barplot(cont, main="Andares contra grau de destruição",
        xlab="Grau de destruição", col=brewer.pal(2,"Spectral"),
        legend = rownames(cont))

#Juntando os tipos de uso secundário 
treino$secondary <- 0
treino$secondary[treino$has_secondary_use_agriculture == 1] <- 1
treino$secondary[treino$has_secondary_use_hotel == 1] <- 2
treino$secondary[treino$has_secondary_use_rental   == 1] <- 3
treino$secondary[treino$has_secondary_use_institution   == 1] <- 4
treino$secondary[treino$has_secondary_use_school  == 1] <- 5
treino$secondary[treino$has_secondary_use_industry  == 1] <- 6
treino$secondary[treino$has_secondary_use_health_post  == 1] <- 7
treino$secondary[treino$has_secondary_use_gov_office  == 1] <- 8
treino$secondary[treino$has_secondary_use_use_police == 1] <- 9
treino$secondary[treino$has_secondary_use_other  == 1] <- 10


table(treino$secondary)

#Tem uso secundário para...?
cont <- table(treino$secondary  ,treino$damage_grade)
barplot(cont, main="Andares contra grau de destruição",
        xlab="Grau de destruição", col=brewer.pal(11,"RdBu"),
        legend = rownames(cont))

#--------------------Análise Descritiva das variáveis------------------


#--------------------Utilizando Modelo logito cumulativo---------------


#https://towardsdatascience.com/implementing-and-interpreting-ordinal-logistic-regression-1ee699274cf5
library(VGAM)
library(MASS)
library(generalhoslem)

library(dplyr)
treino_novo <- dplyr::select(treino, geo_level_1_id, geo_level_2_id, geo_level_3_id, count_floors_pre_eq , age , area_percentage , land_surface_condition , foundation_type , roof_type , 
                             ground_floor_type , other_floor_type , position , plan_configuration , structure , legal_ownership_status ,
                             count_families , secondary,damage_grade)

#Dividindo a base de treino
ntreino <- sample(nrow(treino_novo), 0.7*nrow(treino_novo))
base.treino <- treino_novo[ntreino,]
base.teste <- treino_novo[-ntreino,]


#ajustando o modelo de logito cumulativo com chances proporcionais
mlc <- polr(as.factor(damage_grade) ~ count_floors_pre_eq + age + area_percentage + land_surface_condition + foundation_type + roof_type + 
              ground_floor_type + other_floor_type + position + plan_configuration + as.factor(structure) + legal_ownership_status +
              count_families + as.factor(secondary), Hess = TRUE, data = base.treino)

summary(mlc)

summary_table <- coef(summary(mlc))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

#Predição
#Testando para o valores da base de teste 
result <- as.data.frame(round(predict(mlc,base.teste,type = "p"), 3))

colnames(result) <- c('b','m','d','cat')
result$cat <- 0
result$cat[result$d >= result$b & result$d >= result$m] <- 3
result$cat[result$m >= result$b & result$m >= result$d] <- 2
result$cat[result$b >= result$m & result$b >= result$d] <- 1

result[result$cat == 0,]


prop.table(table(result$cat, base.teste$damage_grade))
#Somando as colunas obtemos : 0.0213351070 + 0.5002877937 + 0.0616006447 = 58%


#Refazendo o modelo utilizando as variaveis de posição geográfica

#ajustando o modelo de logito cumulativo com chances proporcionais
mlc2 <- polr(as.factor(damage_grade) ~ geo_level_1_id + geo_level_2_id + geo_level_3_id + count_floors_pre_eq + age + area_percentage + land_surface_condition + foundation_type + roof_type + 
               ground_floor_type + other_floor_type + position + plan_configuration + as.factor(structure) + legal_ownership_status +
               count_families + as.factor(secondary), Hess = TRUE, data = base.treino)

summary(mlc2)

summary_table <- coef(summary(mlc2))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval,3))
summary_table

#Predição
#Testando para o valores da base de teste 
result <- as.data.frame(round(predict(mlc2,base.teste,type = "p"), 3))

colnames(result) <- c('b','m','d','cat')
result$cat <- 0
result$cat[result$d >= result$b & result$d >= result$m] <- 3
result$cat[result$m >= result$b & result$m >= result$d] <- 2
result$cat[result$b >= result$m & result$b >= result$d] <- 1

result[result$cat == 0,]


prop.table(table(result$cat, base.teste$damage_grade))
#Somando as colunas obtemos : 0.0213351070 + 0.5002877937 + 0.0616006447 = 58%





















#Verificando a qualidade do ajuste
lipsitz.test(mlc)
#Rejeito a hipótese de chances proporcionais pelo teste de lipsitz

logitgof(damage_grade, fitted(mlc), g = 10, ord = TRUE)

pulkrob.chisq(mlc, c("count_floors_pre_eq","structure"))

pulkrob.deviance(mlc, c("count_floors_pre_eq","structure"))

#Testar a suposição de chances proporcionais

mlc2 <- vglm(damage_grade ~ count_floors_pre_eq + structure,
             cumulative(parallel = TRUE, reverse = FALSE))

mlc3 <- vglm(as.factor(damage_grade) ~ as.factor(count_floors_pre_eq) + as.factor(structure),
             cumulative(parallel = FALSE, reverse = FALSE))
lrtest(mlc2,mlc3)

#Concluimos que o modelo com chances não proporcionais é melhor que o de chances proporcionais 

summary(mlc3)

fitted(mlc3)

table(fitted(mlc3), damage_grade)

require(ROCR)
pred = prediction(fitted(mlc3), damage_grade)
perf = performance(pred, "tpr", "fpr")
plot(perf)
abline(0,1,lty=2)




#----------------------------------------------------------------------------------
# AS CHANCES NÃO SÃO PROPORCIONAIS E NO PACOTE MASS NÃO ENCONTREI UMA VARIAÇÃO DA FUNÇÃO POLR
# VOU USAR O PACOTE VGAM

teste <- table(damage_grade, structure)
teste <- cbind(teste, c(1,2,3))
teste <- as.data.frame(as.matrix(teste))


mteste <- vglm(cbind(teste$1,teste$2,teste$3,teste$4,teste$5,teste$6,teste$7,teste$8,teste$9,teste$10,teste$11) ~ teste$V12,  family=cumulative, data=teste)

summary(mteste)

vglm(base_sna$damage_grade ~ base_sna$structure,
     family = cumulative, data = base_sna)


base_sna <- na.omit(treino)

modelo_logit <- vglm(base_sna$damage_grade ~ base_sna$structure,
                     family = cumulative, data = base_sna)

summary(modelo_logit)


