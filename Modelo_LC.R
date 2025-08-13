# Limpar ambiente
rm(list = ls())

# Carregar pacotes
library(dplyr)
library(ggplot2)
library(demography)
library(tidyr)
library(tidyverse)
library(forecast)

# Carregar dados
dados <- read.demogdata("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/mort_BR.txt", 
                        "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/pop_BR.txt",
                        type = "mortality", label = "Brasil")

# Inverter age e year
dados$age <- dados$year
dados$year <- c(2000:2019)

# extrair anos de treinamento
anos_treino <- 2000:2015

# Ajustar modelo de Lee-Carter
lca_fem <- lca(dados, 
               series = names(dados$rate)[1],
               years = anos_treino,
               ages = dados$age,
               max.age = 90,
               adjust = "dt",
               restype = "logrates")

summary(lca_fem)
plot(lca_fem)

# Função para cálculo do RMSE
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Função para cálculo do MAE
mae <- function(actual, predicted) {
  mean(abs(actual - predicted))
}

# Função para cálculo do sMAPE
smape <- function(actual, predicted) {
  mean(2 * abs(actual - predicted) / (abs(actual) + abs(predicted)), na.rm = TRUE)
}

# Previsão de mortalidade para os anos de 2016 a 2019
anos_previsao <- 2016:2019
previsao_fem <- forecast(lca_fem, h = length(anos_previsao), restype = "logrates")

# Construir data frame com previsões e valores reais
prev_fem <- data.frame(previsao_fem$rate)
# Incluir coluna sexo e coluna idade
# Renomeando as colunas de cada data frame das previsoes
for (i in 1:4) {
  novo_nome <- paste0("previsto_20", 15 + i)
  nome_lmt_inf <- paste0("lmt_inf_20", 15+i)
  nome_lmt_max <- paste0("lmt_max_20", 15+i)
  prev_fem <- rename(prev_fem, !!novo_nome := paste0("mulheres.", i))
  prev_fem <- rename(prev_fem, !!nome_lmt_inf := paste0("lower.", i))
  prev_fem <- rename(prev_fem, !!nome_lmt_max := paste0("upper.", i))
}

prev_fem <- prev_fem %>%
  tibble::rownames_to_column("index") %>%
  pivot_longer(
    cols = -index,
    names_pattern = "(previsto|lmt_inf|lmt_max)_(\\d{4})",
    names_to = c("tipo", "ano"),
    values_to = "valor"
  ) %>%
  pivot_wider(
    names_from = tipo,
    values_from = valor
  ) %>%
  mutate(sexo = "Feminino", ano = as.integer(ano)) %>%
  select(index, sexo, ano, previsto, inferior = lmt_inf, superior = lmt_max)

# Ordenar por ano e renomear index como idade
prev_fem <- prev_fem %>%
  mutate(index = as.integer(index)) %>%
  rename(idade = index) %>%
  select(sexo, idade, ano, previsto, inferior, superior) %>%
  arrange(ano, idade)

# Incluir dados observados de 2016 a 2019
observados <- read.table("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/mort_BR.txt")
# Renomear colunas com os nomes da linha 2 e eliminar as duas primeiras linhas
colnames(observados) <- observados[2, ]
observados <- observados[-c(1, 2), ]

# Filtrar os dados observados para os anos de 2016 a 2019
observados <- observados %>%
  filter(ANO %in% anos_previsao)

# Incluir dados observados de mulheres no data frame de previsões
prev_fem$observado <- NA
prev_fem$observado <- observados$Mulheres

# Função para calcular sMAPE
smape <- function(actual, forecast) {
  n <- length(actual)
  return(100/n * sum(2*abs(forecast-actual)/(abs(actual)+abs(forecast))))
}

prev_fem$observado <- as.numeric(prev_fem$observado)

# Calcular métricas por idade
metricas_fem <- prev_fem %>%
  group_by(idade) %>%
  summarise(
    RMSE = sqrt(mean((observado - previsto)^2)),  # RMSE
    MAE = mean(abs(observado - previsto)),         # MAE
    sMAPE = smape(observado, previsto)      # sMAPE (%)
  )

# Resultado
print(metricas_fem)

# Média das métricas
metricas_fem_media <- metricas_fem %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    sMAPE = mean(sMAPE, na.rm = TRUE)
  )
print(metricas_fem_media)
