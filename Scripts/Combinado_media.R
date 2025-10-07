rm(list = ls())
library(dplyr)
library(readr)
library(Metrics)

# -------------------------
# 1) Leitura dos CSVs
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
# DIAGNÓSTICO
# -------------------------
cat("=== DIAGNÓSTICO ===\n")
cat("Dimensões:\n")
cat("ARIMA:", nrow(arima), "linhas\n")
cat("ETS:", nrow(ets), "linhas\n")
cat("FDM:", nrow(fdm), "linhas\n")
cat("LC:", nrow(lc), "linhas\n")
cat("CNN:", nrow(tl_cnn), "linhas\n")
cat("GRU:", nrow(tl_gru), "linhas\n")
cat("CNN-GRU:", nrow(tl_cnn_gru), "linhas\n\n")

# -------------------------
# 2) Consolidar COM JOIN
# -------------------------
dados <- arima %>%
  select(sexo, idade, ano, observado, prev_arima) %>%
  full_join(ets %>% select(sexo, idade, ano, prev_ets), 
            by = c("sexo", "idade", "ano")) %>%
  full_join(fdm %>% select(sexo, idade, ano, prev_fdm), 
            by = c("sexo", "idade", "ano")) %>%
  full_join(lc %>% select(sexo, idade, ano, prev_lc), 
            by = c("sexo", "idade", "ano")) %>%
  full_join(tl_cnn %>% select(sexo, idade, ano, prev_cnn), 
            by = c("sexo", "idade", "ano")) %>%
  full_join(tl_gru %>% select(sexo, idade, ano, prev_gru), 
            by = c("sexo", "idade", "ano")) %>%
  full_join(tl_cnn_gru %>% select(sexo, idade, ano, prev_cnn_gru), 
            by = c("sexo", "idade", "ano"))

# Verificar NAs por idade
cat("\nNAs por idade:\n")
dados %>%
  group_by(idade) %>%
  summarise(
    na_arima = sum(is.na(prev_arima)),
    na_ets = sum(is.na(prev_ets)),
    na_fdm = sum(is.na(prev_fdm)),
    na_lc = sum(is.na(prev_lc)),
    na_cnn = sum(is.na(prev_cnn)),
    na_gru = sum(is.na(prev_gru)),
    na_cnn_gru = sum(is.na(prev_cnn_gru)),
    .groups = "drop"
  ) %>%
  filter(if_any(starts_with("na_"), ~. > 0)) %>%
  print(n = Inf)

# -------------------------
# 3) Combinação média + bootstrap
# -------------------------
set.seed(123)
B <- 1000

dados <- dados %>%
  mutate(
    previsto = rowMeans(select(., starts_with("prev_")), na.rm = TRUE)
  )

# Função bootstrap
calc_bootstrap_ic <- function(row_data, B = 1000) {
  vals <- as.numeric(row_data)
  vals <- vals[!is.na(vals)]
  
  if(length(vals) == 0) {
    return(data.frame(lower = NA, upper = NA))
  }
  
  boot_means <- replicate(B, mean(sample(vals, length(vals), replace = TRUE)))
  
  return(data.frame(
    lower = quantile(boot_means, probs = 0.025, names = FALSE),
    upper = quantile(boot_means, probs = 0.975, names = FALSE)
  ))
}

message("Calculando intervalos de confiança...")
ic_results <- dados %>%
  select(starts_with("prev_")) %>%
  apply(1, calc_bootstrap_ic, B = B) %>%
  bind_rows()

prev_media_boot <- dados %>%
  select(sexo, idade, ano, observado, previsto) %>%
  bind_cols(ic_results) %>%
  arrange(sexo, ano, idade)

# -------------------------
# 4) Métricas
# -------------------------
teste <- 2016:2019
metrica_media1 <- prev_media_boot %>%
  filter(ano %in% teste, !is.na(previsto)) %>%
  group_by(sexo, idade) %>%
  summarise(
    n_obs = n(),
    RMSE  = rmse(observado, previsto),
    MAE   = mae(observado, previsto),
    sMAPE = mean(200 * abs(observado - previsto) / (abs(observado) + abs(previsto)), na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------
# 5) Salvar
# -------------------------
write_csv(prev_media_boot, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/prev_media_simples.csv")
write_csv(metrica_media1, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/metrica_media_simples.csv")

message("Concluído!")
