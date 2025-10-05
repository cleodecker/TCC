#### Modelagem ETS
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
####### ETS ########
# Definir períodos de treino e teste
treino_inicio <- 2000
treino_fim <- 2015
teste_inicio <- 2016
teste_fim <- 2019

# Função para cálculo do sMAPE
smape <- function(actual, predicted) {
  mean(200 * abs(actual - predicted) / (abs(actual) + abs(predicted)), na.rm = TRUE)
}


# Dataframes para armazenar resultados
ets_metricas <- data.frame()
ets_previsoes <- data.frame()
ets_globais <- data.frame()

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
    modelo <- ets(ts_treino)
    metodo <- modelo$method  # Extrai o método ETS (ex: "AAN")
    
    # Extrair resíduos
    residuos <- residuals(modelo)
    
    # Testes de ruído branco
    # 1. Teste de Ljung-Box
    lb_test <- tryCatch({
      Box.test(residuos, lag = min(10, length(residuos) - 1), type = "Ljung-Box")
    }, error = function(e) {
      list(p.value = NA)
    })
    
    ljung_box_pvalue <- lb_test$p.value
    ruido_branco_lb <- ifelse(is.na(ljung_box_pvalue), NA, ljung_box_pvalue > 0.05)
    
    # 2. Verificar ACF dos resíduos (proporção de lags significativos)
    acf_result <- tryCatch({
      acf(residuos, plot = FALSE, lag.max = min(10, length(residuos) - 1))
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(acf_result)) {
      # Limite de significância: 1.96/sqrt(n)
      limite_sig <- 1.96 / sqrt(length(residuos))
      acf_valores <- acf_result$acf[-1]  # Remove lag 0
      prop_sig <- mean(abs(acf_valores) > limite_sig)
    } else {
      prop_sig <- NA
    }
    
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
    ets_metricas <- rbind(ets_metricas, data.frame(
      sexo = sexo,
      idade = idade,
      ETS_Metodo = metodo,
      RMSE = sqrt(mean((actual - forecast)^2)),
      MAE = mean(abs(actual - forecast)),
      sMAPE = smape(actual, forecast),
      Ljung_Box_pvalue = ljung_box_pvalue,
      Ruido_Branco_LB = ruido_branco_lb,
      ACF_prop_significativa = prop_sig
    ))
    
    ets_previsoes <- rbind(ets_previsoes, data.frame(
      sexo = sexo,
      idade = idade,
      ano = teste$ANO,
      previsto = forecast,
      inferior = lower_original,
      superior = upper_original,
      observado = actual
    ))
  }
  
  # Métricas globais por sexo
  prev_sexo <- ets_previsoes %>% filter(sexo == sexo)
  actual_global <- prev_sexo$observado
  forecast_global <- prev_sexo$previsto
  
  ets_globais <- rbind(ets_globais, data.frame(
    SEXO = sexo,
    Modelo = "ETS",
    RMSE = sqrt(mean((actual_global - forecast_global)^2)),
    MAE = mean(abs(actual_global - forecast_global)),
    sMAPE = smape(actual_global, forecast_global)
  ))
}

# Substituir Mulheres por Feminino e Homens por Masculino
ets_metricas <- ets_metricas %>%
  mutate(sexo = recode(sexo,
                       "Homens" = "Masculino",
                       "Mulheres" = "Feminino"))
ets_previsoes <- ets_previsoes %>%
  mutate(sexo = recode(sexo,
                       "Homens" = "Masculino",
                       "Mulheres" = "Feminino"))

# ---------------------------
# VISUALIZAÇÃO DOS RESULTADOS ETS
# ---------------------------
# 1. Sumário dos modelos
print("Modelos ETS e Métricas por Idade/Sexo:")
print(ets_metricas)

# 2. Sumário dos testes de ruído branco
print("\nResumo dos Testes de Ruído Branco:")
print(paste("Modelos com resíduos ruído branco (Ljung-Box):", 
            sum(ets_metricas$Ruido_Branco_LB, na.rm = TRUE), "/", 
            sum(!is.na(ets_metricas$Ruido_Branco_LB))))

# Organizar Previsões por Sexo, Ano e Idade
ets_previsoes <- ets_previsoes %>%
  arrange(sexo, ano, idade)

# Renomear colunas para clareza
colnames(ets_previsoes) <- c("sexo", "idade", "ano", "previsto", "inferior",
                             "superior", "observado")

# Salvar os resultados em arquivos CSV
write.csv(ets_metricas, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/ETS/ETS_metricas.csv", row.names = FALSE)
write.csv(ets_previsoes, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/ETS/ETS_previsoes.csv", row.names = FALSE)
