#### Modelagem ARIMA
# limpar o ambiente
rm(list = ls())

# Carregar pacotes
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tidyr)

# Carregar os dados
tabua_br <- read.csv("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/tabuas_BR.csv")

# Verificar as primeiras linhas do dataset
head(tabua_br)

# Verificar a estrutura do dataset
str(tabua_br)

# Filtrar apenas as colunas necessárias (IDADE, SEXO, ANO, nMx)
tabua <- tabua_br %>%
  select(IDADE, SEXO, ANO, nMx)

# Excluir dados do ano 2020 em diante
tabua <- tabua %>% filter(ANO < 2020)

##################### MODELAGEM #########################
####### ARIMA ########
# Definir períodos de treino e teste
treino_inicio <- 2000
treino_fim <- 2015
teste_inicio <- 2016
teste_fim <- 2019

# Criar função para calcular sMAPE
smape <- function(actual, forecast) {
  n <- length(actual)
  return(100/n * sum(2*abs(forecast-actual)/(abs(actual)+abs(forecast))))
}

# Dataframes para armazenar resultados
modelos_metricas <- data.frame()
previsoes_intervalos <- data.frame()
metricas_globais <- data.frame()

# Loop por sexo e idade
for (sexo in unique(tabua$SEXO)) {
  for (idade in unique(tabua$IDADE)) {
    # Filtrar dados
    subdados <- tabua %>% 
      filter(SEXO == sexo, IDADE == idade) %>%
      arrange(ANO)
    
    # Separar treino e teste
    treino <- subdados %>% filter(ANO >= treino_inicio, ANO <= treino_fim)
    teste <- subdados %>% filter(ANO >= teste_inicio, ANO <= teste_fim)
    
    if (nrow(treino) == 0 | nrow(teste) == 0) next
    
    # Modelagem em escala logarítmica
    ts_treino <- ts(log(treino$nMx), start = treino_inicio)
    modelo <- auto.arima(ts_treino)
    ordem <- arimaorder(modelo)
    
    # Previsão
    horizonte <- nrow(teste)
    previsao <- forecast(modelo, h = horizonte, level = 95)
    
    # Converter para escala original
    previsao_original <- exp(previsao$mean)
    lower_original <- exp(previsao$lower)
    upper_original <- exp(previsao$upper)
    
    # Métricas de erro
    actual <- teste$nMx
    forecast <- as.numeric(previsao_original)
    
    # Armazenar resultados
    modelos_metricas <- rbind(modelos_metricas, data.frame(
      IDADE = idade,
      SEXO = sexo,
      ARIMA_p = ordem[1],
      ARIMA_d = ordem[2],
      ARIMA_q = ordem[3],
      RMSE = sqrt(mean((actual - forecast)^2)),
      MAE = mean(abs(actual - forecast)),
      sMAPE = smape(actual, forecast)
    ))
    
    previsoes_intervalos <- rbind(previsoes_intervalos, data.frame(
      IDADE = idade,
      SEXO = sexo,
      ANO = teste$ANO,
      Observado = actual,
      Previsao = forecast,
      Lower_95 = lower_original,
      Upper_95 = upper_original
    ))
  }
  
  # Métricas globais por sexo
  prev_sexo <- previsoes_intervalos %>% filter(SEXO == sexo)
  actual_global <- prev_sexo$Observado
  forecast_global <- prev_sexo$Previsao
  
  metricas_globais <- rbind(metricas_globais, data.frame(
    SEXO = sexo,
    RMSE = sqrt(mean((actual_global - forecast_global)^2)),
    MAE = mean(abs(actual_global - forecast_global)),
    sMAPE = smape(actual_global, forecast_global)
  ))
}

# ---------------------------
# VISUALIZAÇÃO DOS RESULTADOS
# ---------------------------
# 1. Sumário dos modelos
print("Modelos ARIMA e Métricas por Idade/Sexo:")
print(modelos_metricas)

# 2. Métricas agregadas por sexo
print("\nMétricas Globais por Sexo:")
print(metricas_globais)

# 3. Exemplo gráfico para uma idade específica
idade_exemplo <- 5
sexo_exemplo <- "Homens"

dados_plot <- previsoes_intervalos %>% 
  filter(IDADE == idade_exemplo, SEXO == sexo_exemplo)

ggplot(dados_plot, aes(x = ANO)) +
  geom_line(aes(y = Observado, color = "Observado"), linewidth = 1) +
  geom_line(aes(y = Previsao, color = "Previsão"), linewidth = 1) +
  geom_ribbon(aes(ymin = X95., ymax = X95..1), alpha = 0.2, fill = "blue") +
  labs(title = paste("Previsão de nMx para", sexo_exemplo, "de", idade_exemplo, "anos"),
       y = "Taxa de Mortalidade (nMx)", x = "Ano") +
  scale_color_manual(values = c("Observado" = "black", "Previsão" = "red")) +
  theme_minimal()

# 4. Gráfico de todas as previsões
ggplot(previsoes_intervalos, aes(x = ANO, y = Previsao, color = SEXO)) +
  geom_line() +
  geom_ribbon(aes(ymin = X95., ymax = X95..1), alpha = 0.2) +
  facet_wrap(~ IDADE, scales = "free_y") +
  labs(title = "Previsões de nMx por Idade e Sexo",
       y = "Taxa de Mortalidade (nMx)", x = "Ano") +
  theme_minimal()

# Organizar Previsões por Sexo, Ano e Idade
previsoes_intervalos <- previsoes_intervalos %>%
  arrange(SEXO, ANO, IDADE)

# Gravar os resultados em CSV
write.csv(modelos_metricas, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/modelos_metricas_arima.csv", row.names = FALSE)
write.csv(previsoes_intervalos, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/previsoes_intervalos_arima.csv", row.names = FALSE)
write.csv(metricas_globais, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/metricas_globais_arima.csv", row.names = FALSE)
