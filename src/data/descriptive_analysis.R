library(tidyverse)

source("src/data/earthquake_damage.R")

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

# Estrutura 
glimpse(damage)

# Análise descritiva no conjunto de treinamento --------------------------------

### Resposta
damage %>% ggplot(aes(x = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar() + 
  scale_x_discrete(labels = c("Baixo", "Médio", "Severo")) +
  labs(x = "Grau de dano",
       y = "Número de construções")

# Proporções
round(100 * prop.table(table(damage$damage_grade)), 2)

---

### Região geográfica mais geral -- geo_level_1_id
damage %>% ggplot(aes(x = geo_level_1_id, fill = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade)) + 
  scale_x_discrete(breaks = seq(0, 30, 5)) +
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Região geográfica",
       y = "Número de construções")

# A grande maioria das regiões apresentam construções que sofreram
# um dano médio;

# As regiões em que a maioria das construções soferam danos mais severos
# foram as regiões 8, 17, 18 e 21. Entre elas destaca-se a região 17

# Proporções dos danos dentro de cada região
round(100 * prop.table(table(damage$geo_level_1_id, damage$damage_grade), 1), 2)

---

### Área -- area_percentage

# Boxplot  
damage %>% ggplot(aes(x = damage_grade, y = area_percentage)) +
  theme_classic() +
  geom_boxplot() + 
  scale_x_discrete(labels = c("Baixo", "Médio", "Severo")) +
  labs(x = "Grau de dano",
       y = "Área normalizada")

# Barplot  
damage %>% ggplot(aes(x = area_percentage, fill = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano",
       x = "Área normalizada", y = "Frequência")

---

### Altura -- height_percentage
  
# Boxplot
damage %>% ggplot(aes(x = damage_grade, y = height_percentage)) +
  theme_classic() +
  geom_boxplot() + 
  scale_x_discrete(labels = c("Baixo", "Médio", "Severo")) +
  labs(x = "Grau de dano",
       y = "Altura normalizada")

# Barplot
damage %>% ggplot(aes(x = height_percentage, fill = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano",
       x = "Altura normalizada", y = "Frequência")

---

### Número de andares antes do terremoto -- count_floors_pre_eq

# Boxplot
damage %>% ggplot(aes(x = damage_grade, y = count_floors_pre_eq)) +
  theme_classic() +
  geom_boxplot() + 
  scale_y_continuous(breaks = 1:9) +
  scale_x_discrete(labels = c("Baixo", "Médio", "Severo")) +
  labs(x = "Grau de dano",
       y = "Número de andares")

# Barplot
damage %>% ggplot(aes(x = count_floors_pre_eq, fill = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  scale_x_continuous(breaks = 1:9) +
  labs(fill = "Grau de dano", x = "Número de andares",
       y = "Número de construções")

# Summary
summary(damage$count_floors_pre_eq); sd(damage$count_floors_pre_eq)

# Tabela de frequências
table(damage$count_floors_pre_eq)

# A maioria das construções possuem 2 andares

# 95% das construções possuem até 3 andares
quantile(damage$count_floors_pre_eq, probs = 0.95)

# Proporções de dano dentro de cada nível dos andares
prop.table(table(damage$count_floors_pre_eq, damage$damage_grade), margin = 1)

---

### Idade -- age

# Boxplot  
damage %>% ggplot(aes(x = damage_grade, y = age)) +
  theme_classic() +
  geom_boxplot() + 
  scale_y_continuous(breaks = 1:9) +
  scale_x_discrete(labels = c("Baixo", "Médio", "Severo")) +
  labs(x = "Grau de dano",
       y = "Idade")

# Retirando os outliers
damage %>% filter(age < 200) %>% 
ggplot(aes(x = damage_grade, y = age)) +
  theme_classic() +
  geom_boxplot() + 
  scale_y_continuous(breaks = 1:9) +
  scale_x_discrete(labels = c("Baixo", "Médio", "Severo")) +
  labs(x = "Grau de dano",
       y = "Idade")  

# Barplots
damage %>% ggplot(aes(x = age, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Idade", y = "Número de construções")

# Retirando o outlier
damage %>% filter(age < 250) %>% 
  ggplot(aes(x = age, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Idade", y = "Número de construções")

# Note que o número de construções que sofreram danos leves decai
# exponencialmente quando a idade aumenta

# Summary
summary(damage$age); sd(damage$age)

# Tabela de frequências
table(damage$age)

# Distribuição das construções com idade igual a 995
prop.table(table(damage$damage_grade[which(damage$age == 995)]))

---

### Condição da superfície -- land_surface_condition
  
# Distribuição geral
prop.table(table(damage$land_surface_condition))
  
# Proporções de dano dentro de cada nível das condições
prop.table(table(damage$land_surface_condition,
                   damage$damage_grade), margin = 1)

# Note que dentro de cada nível das condições as proporções de danos
# são semelhantes sugerindo o tipo de dano independe da condição da 
# superfície

damage %>% ggplot(aes(x = land_surface_condition, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  geom_text(aes(y =..count..,
                label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position = position_dodge(0.9), vjust = -0.9, size = 3) +
  coord_cartesian(ylim = c(0, 80000)) +
  labs(fill = "Grau de dano", x = "Condição da superfície", y = "Número de construções")

---

### Tipo de fundação - foundation_type
damage %>% ggplot(aes(x = foundation_type, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  geom_text(aes(y =..count..,
                label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position = position_dodge(0.9), vjust = -0.9, size = 3) +
  coord_cartesian(ylim = c(0, 80000)) +
  labs(fill = "Grau de dano", x = "Tipo de fundação", y = "Número de construções")

# Proporções dos tipos de fundação
round(100*prop.table(table(damage$foundation_type)), 2)

# Proporções de dano dentro de cada nível dos tipos de fundação
prop.table(table(damage$foundation_type,
                 damage$damage_grade), margin = 1)

# As proporções de dano dentro de cada nível do tipo de fundação sugerem
# associação entre as variáveis.

# As fundações "h" e "r" apresentaram maiores danos do tipo severo,
# enquanto que cerca de 98% de construções com tipo de fundação
# "i" sofreram danos de leves a médios.

---

### Tipo de andar térreo -- ground_floor_type
damage %>% ggplot(aes(x = ground_floor_type, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  geom_text(aes(y =..count..,
                label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position = position_dodge(0.9), vjust = -0.9, size = 3) +
  coord_cartesian(ylim = c(0, 80000)) +
  labs(fill = "Grau de dano", x = "Tipo de andar térreo", y = "Número de construções")


# Proporções dos tipos de andar térreo
round(100*prop.table(table(damage$ground_floor_type)), 2)

# Proporções de dano dentro de cada nível do tipo de andar térreo
prop.table(table(damage$ground_floor_type,
                 damage$damage_grade), margin = 1)

# As proporções sugerem associação. Consruções com o tipo de andar térreo
# "f" e "x" sofreram danos semelhantes.

---

### Tipo de piso utilizado (exceto telhado e térreo) -- other_floor_type
damage %>% ggplot(aes(x = other_floor_type, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  geom_text(aes(y =..count..,
                label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position = position_dodge(0.9), vjust = -0.9, size = 3) +
  coord_cartesian(ylim = c(0, 80000)) +
  labs(fill = "Grau de dano", x = "Tipo de piso utilizado (exceto telhado e térreo)", y = "Número de construções")

# Proporções de dano dentro de cada nível do tipo de piso
prop.table(table(damage$other_floor_type,
                 damage$damage_grade), margin = 1)

# Aqui é possível perceber que construções com tipo de piso "s" sofreram
# menos danos severos em comparação com os outros tipos. As construções
# com pisos "q" e "x" destacam-se pelos altos níveis de danos sofridos.

---

  ### Tipo de telhado -- roof_type
  damage %>% ggplot(aes(x = roof_type, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  geom_text(aes(y =..count..,
                label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position = position_dodge(0.9), vjust = -0.9, size = 3) +
  coord_cartesian(ylim = c(0, 80000)) +
  labs(fill = "Grau de dano", x = "Tipo de telhado", y = "Número de construções")

# Proporções de dano dentro de cada nível do tipo de telhado
prop.table(table(damage$roof_type,
                 damage$damage_grade), margin = 1)

# As proporções de dano dentro de cada nível do tipo de telhado sugerem
# associação entre as variáveis.

# Construções com o tipo de telhado "x" sofreram menos dados severos do
# que construções com outros tipos de telhados.

---
  
### Posição -- position
damage %>% ggplot(aes(x = position, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  geom_text(aes(y =..count..,
                label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position = position_dodge(0.9), vjust = -0.9, size = 3) +
  coord_cartesian(ylim = c(0, 80000)) +
  labs(fill = "Grau de dano", x = "Posição", y = "Número de construções")

# Proporções de dano dentro de cada nível da posição
prop.table(table(damage$position,
                 damage$damage_grade), margin = 1)

# As distribuições dos danos sofridos em cada nível da posição 
# são relativamente semelhantes entre si, indicando que o dano sofrido
# independe da posição da construção 

# Configuração do plano de construção -- plan_configuration
damage %>% ggplot(aes(x = plan_configuration, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Configuração do plano de construção", y = "Número de construções")

# Proporções de dano dentro de cada nível da configuração do plano
prop.table(table(damage$plan_configuration,
                 damage$damage_grade), margin = 1)


---

### Status -- legal_ownership_status
damage %>% ggplot(aes(x = legal_ownership_status, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Status legal de propriedade do terreno", y = "Número de construções")

# Proporções de dano dentro de cada nível do status
prop.table(table(damage$legal_ownership_status,
                 damage$damage_grade), margin = 1)

# As porporções sugerem que os danos sofridos possuem associação com
# o status legal da construção. Note que construções com status "a"
# tiveram mais danos leves e menos danos severos do que construções com
# status "w".

---

### Número de famílias -- count_families
damage %>% ggplot(aes(x = count_families, group = damage_grade)) +
  theme_classic() +
  theme(legend.position = "top") +
  geom_bar(aes(fill = damage_grade), position = "dodge") + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("low"  = "#482677FF",
                               "medium" = "#2D708EFF",
                               "severe" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Número de famílias", y = "Número de construções")

# Proporções de dano dentro de cada nível do número de famílias
prop.table(table(damage$count_families,
                 damage$damage_grade), margin = 1)

# Levando em conta que existem poucas construções com mais de 6 famílias, 
# não parece haver associação entre o grau de dano e o número de famílias
# vivendo na construção. 

---

### Superstructure

# ANA:

library(tigerstats)
library(reshape2)
library(plyr)

tab0 <- colPerc(xtabs(~damage_grade+has_superstructure_adobe_mud,data=damage))
tab1 <- colPerc(xtabs(~damage_grade+has_superstructure_mud_mortar_stone,data=damage))
tab2 <- colPerc(xtabs(~damage_grade+has_superstructure_stone_flag,data=damage))
tab3 <- colPerc(xtabs(~damage_grade+has_superstructure_cement_mortar_stone,data=damage))
tab4 <- colPerc(xtabs(~damage_grade+has_superstructure_mud_mortar_brick,data=damage))
tab5 <- colPerc(xtabs(~damage_grade+has_superstructure_cement_mortar_brick,data=damage))
tab6 <- colPerc(xtabs(~damage_grade+has_superstructure_timber,data=damage))
tab7 <- colPerc(xtabs(~damage_grade+has_superstructure_bamboo,data=damage))
tab8 <- colPerc(xtabs(~damage_grade+has_superstructure_rc_non_engineered,data=damage))
tab9 <- colPerc(xtabs(~damage_grade+has_superstructure_rc_engineered,data=damage))
tab10 <- colPerc(xtabs(~damage_grade+has_superstructure_other,data=damage))

tab <- cbind(tab0, tab1, tab2, tab3, tab4, tab5, 
             tab6, tab7, tab8, tab9, tab10)

names <- c("Adobe/barro", "Barro e pedra", "Pedra",
           "Cimento e pedra", "Barro e tijolo",
           "Cimento e tijolo", "Madeira", "Bamboo",
           "Concreto armado", "Concreto armado projetado", "Outro material" )

#names <- c("adobe_mud", "mud_mortar_stone", "stone_flag",
#           "cement_mortar_stone", "mud_mortar_brick",
#           "cement_mortar_brick", "timber", "bamboo",
#           "non_engineered", "engineered", "other" )

teste <- data.frame(tab)
drop <- c("X0", "X0.1", "X0.2", "X0.3", "X0.4", "X0.5", "X0.6", "X0.7", "X0.8","X0.9", "X0.10")
tab_f <- teste[,!(names(teste) %in% drop)]
colnames(tab_f)<-names
Cat <- seq(1, 4, 1)
tab_f <- cbind(tab_f, Cat)
plot_teste <- melt(tab_f, id.vars = "Cat")
plot_teste_fim <- plot_teste[plot_teste$Cat < 4,]
plot_teste_fim_sor <- arrange(plot_teste_fim, variable, desc(Cat)) 
plot_teste_fim2 <- ddply(plot_teste_fim_sor, "variable",
                         transform,
                         label_ypos = cumsum(value))

# A intenção aqui foi destacar como os materiais utilizados nas construções
# influenciam o nível de dano. De repente cabe mencionar na análise quando
# o percentual de 1 foi menor que o de 3 e quando não.
# E destacar a distribuição da 'has_superstructure_rc_engineered'.
ggplot(plot_teste_fim2, aes(x = variable, y = value, fill = factor(Cat))) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_text(aes(y = label_ypos, label = value),
                vjust = 0.9, color = "white", size = 3.5) + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("1"  = "#482677FF",
                               "2" = "#2D708EFF",
                               "3" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Superestrutura",
       y = "Distribuição do grau de dano")

---

### Secondary use

# ANA:

# O próximo gráfico mostra cada uma das variáveis a partir da
# has_secondary_use até a has_secondary_use_other. Peguei todas elas quando
# valiam '1' e calculei a distribuição com relação a variável damage_grade
tab0 <- colPerc(xtabs(~damage_grade+has_secondary_use,data=damage))
tab1 <- colPerc(xtabs(~damage_grade+has_secondary_use_agriculture,data=damage))
tab2 <- colPerc(xtabs(~damage_grade+has_secondary_use_hotel,data=damage))
tab3 <- colPerc(xtabs(~damage_grade+has_secondary_use_rental,data=damage))
tab4 <- colPerc(xtabs(~damage_grade+has_secondary_use_institution,data=damage))
tab5 <- colPerc(xtabs(~damage_grade+has_secondary_use_school,data=damage))
tab6 <- colPerc(xtabs(~damage_grade+has_secondary_use_industry,data=damage))
tab7 <- colPerc(xtabs(~damage_grade+has_secondary_use_health_post,data=damage))
tab8 <- colPerc(xtabs(~damage_grade+has_secondary_use_gov_office,data=damage))
tab9 <- colPerc(xtabs(~damage_grade+has_secondary_use_use_police,data=damage))
tab10 <- colPerc(xtabs(~damage_grade+has_secondary_use_other,data=damage))

tab <- cbind(tab0, tab1, tab2, tab3, tab4, tab5,
             tab6, tab7, tab8, tab9, tab10)

names <- c("Possui algum", "Agricultura", "Hotel", "Aluguel",
           "Instituição", "Escola", "Indústria", "Posto de saúde",
           "Escritório de governo", "Polícia", "Outro")
#names<-c("secondary_use", "agriculture", "hotel", "rental", "institution", "school", "industry", "health_post", "gov_office", "police", "other" )

teste<-data.frame(tab)

drop <- c("X0", "X0.1", "X0.2", "X0.3", "X0.4", "X0.5", "X0.6", "X0.7", "X0.8","X0.9", "X0.10")
tab_f <- teste[,!(names(teste) %in% drop)]
colnames(tab_f)<-names
Cat <- seq(1, 4, 1)
tab_f<-cbind(tab_f, Cat)
plot_teste<-melt(tab_f, id.vars = "Cat")
plot_teste_fim<- plot_teste[plot_teste$Cat < 4,]
plot_teste_fim_sor <- arrange(plot_teste_fim, variable, desc(Cat)) 
plot_teste_fim2 <- ddply(plot_teste_fim_sor, "variable",
                         transform, label_ypos=cumsum(value))

# A intenção aqui foi destacar como estruturas utilizadas para determinados
# fins (hoteis, postos de saúde, ...) sofreram danos menores.
# De repente cabe mencionar na análise quando o percentual de 1 foi menor
# que o de 3 e quando não.

ggplot(plot_teste_fim2, aes(x = variable, y = value, fill = factor(Cat))) + 
  geom_bar(stat = "identity") + 
  theme_classic() + 
  theme(legend.position = "top", 
        axis.text.x = element_text(angle = 30, hjust = 1, size = 8)) +
  geom_text(aes(y = label_ypos, label = value),
            vjust = 0.9, color = "white", size = 3.5) + 
  scale_fill_manual(labels = c("Baixo", "Médio", "Severo"),
                    values = c("1"  = "#482677FF",
                               "2" = "#2D708EFF",
                               "3" = "#73D055FF")) +
  labs(fill = "Grau de dano", x = "Uso secundário",
       y = "Distribuição do grau de dano")



