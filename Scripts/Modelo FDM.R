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

# Incluir dados observados
observados <- read.table("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/mort_BR.txt")
colnames(observados) <- observados[2, ]
observados <- observados[-c(1,2), ] %>%
  filter(ANO %in% 2016:2019) %>%
  mutate(across(c(Mulheres, Homens, Ambos), as.numeric))

# Inverter age e year
dados$age <- dados$year
dados$year <- c(2000:2019)

# extrair anos de treinamento
anos_treino <- 2000:2015
anos_previsao <- 2016:2019

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
  mean(200 * abs(actual - predicted) / (abs(actual) + abs(predicted)), na.rm = TRUE)
}

# Extrair anos de treino
dados_treino <- extract.years(dados, anos_treino)

#Modelos
fdm_fem <- fdm(dados_treino, series = names(dados$rate)[1], max.age = 90, years.fit = anos_treino)
fdm_mas <- fdm(dados_treino, series = names(dados$rate)[2], max.age = 90, years.fit = anos_treino)
fdm_ambos <- fdm(dados_treino, series = names(dados$rate)[3], max.age = 90, years.fit = anos_treino)

# Previsões
previsao_fdm_fem <- forecast(fdm_fem, h = length(anos_previsao), level = 95)
previsao_fdm_mas <- forecast(fdm_mas, h = length(anos_previsao), level = 95)
previsao_fdm_ambos <- forecast(fdm_ambos, h = length(anos_previsao), level = 95)

# Contruir Dataframes com as previsões e intervalos
prev_fem <- data.frame(previsao_fdm_fem$rate)
prev_mas <- data.frame(previsao_fdm_mas$rate)
prev_ambos <- data.frame(previsao_fdm_ambos$rate)

# Transformar em long format
# Se a coluna de idade não existir, transforme o índice em coluna
prev_fem <- prev_fem %>%
  tibble::rownames_to_column(var = "idade")

# Pivot único
prev_fem <- prev_fem %>%
  pivot_longer(
    cols = -idade,
    names_to = c(".value", "ano"),
    names_pattern = "(mulheres|lower|upper)\\.(\\d+)"
  ) %>%
  rename(
    previsto = mulheres,
    inferior = lower,
    superior = upper
  ) %>%
  # nova coluna para sexo
  mutate(sexo = "Feminino") %>%
  # idade como numérica
  mutate(idade = as.numeric(idade)) %>%
  arrange(ano, idade) %>%
  # incluir observados
  mutate(observado = observados$Mulheres)

# Repetir para masculino
prev_mas <- prev_mas %>%
  tibble::rownames_to_column(var = "idade")
# Pivot único
prev_mas <- prev_mas %>%
  pivot_longer(
    cols = -idade,
    names_to = c(".value", "ano"),
    names_pattern = "(homens|lower|upper)\\.(\\d+)"
  ) %>%
  rename(
    previsto = homens,
    inferior = lower,
    superior = upper
  ) %>%
  # nova coluna para sexo
  mutate(sexo = "Masculino") %>%
  # idade como numérica
  mutate(idade = as.numeric(idade)) %>%
  arrange(ano, idade) %>%
  # incluir observados
  mutate(observado = observados$Homens)

# Repetir para ambos
prev_ambos <- prev_ambos %>%
  tibble::rownames_to_column(var = "idade")
prev_ambos <- prev_ambos %>%
  pivot_longer(
    cols = -idade,
    names_to = c(".value", "ano"),
    names_pattern = "(ambos|lower|upper)\\.(\\d+)"
  ) %>%
  rename(
    previsto = ambos,
    inferior = lower,
    superior = upper
  ) %>%
  # nova coluna para sexo
  mutate(sexo = "Ambos") %>%
  # idade como numérica
  mutate(idade = as.numeric(idade)) %>%
  arrange(ano, idade) %>%
  # incluir observados
  mutate(observado = observados$Ambos)

# Combinar resultados
prev_total <- bind_rows(prev_fem, prev_mas, prev_ambos)


# Calcular métricas por sexo e idade
metricas_total <- prev_total %>%
  group_by(sexo, idade) %>%
  summarise(
    RMSE = sqrt(mean((observado - previsto)^2)),
    MAE = mean(abs(observado - previsto)),
    sMAPE = smape(observado, previsto),
    .groups = 'drop'
  )


# Calcular médias das métricas por sexo
metricas_media_por_sexo <- metricas_total %>%
  group_by(sexo) %>%
  summarise(
    RMSE = mean(RMSE, na.rm = TRUE),
    MAE = mean(MAE, na.rm = TRUE),
    sMAPE = mean(sMAPE, na.rm = TRUE),
    .groups = 'drop'
  )

# Exibir resultados
cat("\nMétricas por sexo e idade:\n")
print(metricas_total)

cat("\nMétricas médias por sexo:\n")
print(metricas_media_por_sexo)

# Gravar resultados em CSV
write.csv(prev_total, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/FDM/FDM_prev_total.csv", row.names = FALSE)
write.csv(metricas_total, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/FDM/FDM_metricas_por_sexo.csv", row.names = FALSE)

