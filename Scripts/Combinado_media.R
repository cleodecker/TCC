rm(list = ls())

library(dplyr)
library(readr)
library(Metrics)

# -------------------------
# 1) Leitura dos CSVs de previsões individuais
# -------------------------
arima <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/ARIMA_previsoes_intervalos_arima.csv") %>%
  rename(prev_arima = previsto)
ets <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/ETS_previsoes.csv") %>%
  rename(prev_ets = previsto)
fdm <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/FDM_prev_total.csv") %>%
  rename(prev_fdm = previsto)
lc <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/LC_prev_total.csv") %>%
  rename(prev_lc = previsto)
tl_cnn <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/transfer_cnn_previsoes.csv") %>%
  rename(prev_cnn = previsto)
tl_gru <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/transfer_gru_previsoes.csv") %>%
  rename(prev_gru = previsto)
tl_cnn_gru <- read_csv("https://raw.githubusercontent.com/cleodecker/TCC/refs/heads/main/Previs%C3%B5es/transfer_cnn_gru_previsoes.csv") %>%
  rename(prev_cnn_gru = previsto)

# -------------------------
# 2) Consolidar previsões
# -------------------------
dados1 <- data.frame(sexo = arima$sexo, idade = arima$idade, ano = arima$ano, observado = arima$observado)
dados1 <- dados %>%
  mutate(
    prev_arima   = arima$prev_arima,
    prev_ets     = ets$prev_ets,
    prev_fdm     = fdm$prev_fdm,
    prev_lc      = lc$prev_lc,
    prev_cnn     = tl_cnn$prev_cnn,
    prev_gru     = tl_gru$prev_gru,
    prev_cnn_gru = tl_cnn_gru$prev_cnn_gru
  )

# -------------------------
# 3) Combinação média direta + bootstrap para IC
# -------------------------
set.seed(123)
B <- 1000

# Função bootstrap apenas para IC
bootstrap_ic <- function(vals, B = 1000, nivel = 0.95) {
  n <- length(vals)
  boot_means <- replicate(B, mean(sample(vals, n, replace = TRUE), na.rm = TRUE))
  alpha <- (1 - nivel)/2
  as.list(c(lower = quantile(boot_means, probs = alpha),
            upper = quantile(boot_means, probs = 1 - alpha)))
}

prev_media_boot <- dados %>%
  rowwise() %>%
  mutate(
    previsto = mean(c(prev_arima, prev_ets, prev_fdm, prev_lc, prev_cnn, prev_gru, prev_cnn_gru), na.rm = TRUE),
    # calcular IC direto
    tmp_boot = list({
      n <- length(c(prev_arima, prev_ets, prev_fdm, prev_lc, prev_cnn, prev_gru, prev_cnn_gru))
      boot_means <- replicate(B, mean(sample(c(prev_arima, prev_ets, prev_fdm, prev_lc, prev_cnn, prev_gru, prev_cnn_gru),
                                             n, replace = TRUE), na.rm = TRUE))
      alpha <- 0.025
      list(lower = quantile(boot_means, probs = alpha),
           upper = quantile(boot_means, probs = 1 - alpha))
    }),
    lower = tmp_boot$lower,
    upper = tmp_boot$upper
  ) %>%
  ungroup() %>%
  select(sexo, idade, ano, observado, previsto, lower, upper) %>%
  arrange(sexo, ano, idade)


# -------------------------
# 4) Métricas no período de teste
# -------------------------
teste <- 2016:2019
metrica_media1 <- prev_media_boot %>%
  filter(ano %in% teste) %>%
  group_by(sexo, idade) %>%
  summarise(
    RMSE  = rmse(observado, previsto),
    MAE   = mae(observado, previsto),
    sMAPE = mean(200 * abs(observado - previsto) / (abs(observado) + abs(previsto)), na.rm = TRUE),
    .groups = "drop"
  )



# -------------------------
# 5) Salvar resultados
# -------------------------
write_csv(prev_media_boot, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/prev_media_simples.csv")
write_csv(metrica_media1, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/metrica_media_simples.csv")

