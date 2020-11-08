# comp5904
código para competição da disciplina MAE5904

## problema
modeling earthquake damage : [link aqui](https://www.drivendata.org/competitions/57/nepal-earthquake/)

## relatório
O relatório com o detalhamento dos métodos e discussões sobre os resultados pode ser consultado [aqui](https://docs.google.com/document/d/15c1aUz4iZqGZI0TAWmVIH_WEBB1Mw9esFBzNu6KwDro/edit?usp=sharing)

## estrutura das pastas

* datasets : contém os conjuntos de dados repartidos em treinamento e teste. 
 - `trainingDataset_earthquake.csv` contém os dados de treinamento para a competição modeling earthquake damage.
 - `testDataset_earthquase.csv` contém os dados de teste para a competição modeling earthquake damage.

* src : contém os arquivos .R do projeto.
 - `data` contém scripts para carregar e pré processar os arquivos csv.
 - `models` contém scripts para implementar os modelos de aprendizagem

* models
 - `lm` contém scripts para implementação de Regressão Linear sobre uma matriz indicadora

*  utils (??)

## execução dos scripts

para gerar predições para cada modelo, basta rodar os arquivos index.R dentro das pastas que identificam os modelos. Exemplo: `source('./src/models/lm/index.R')`