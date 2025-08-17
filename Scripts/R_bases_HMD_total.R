# Limpar o ambiente
rm(list = ls())

#install.packages("HMDHFDplus")
library(HMDHFDplus)
library(dplyr)
library(arrow)
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(viridis)
library(tidyverse)

# Definir as credenciais
username <- "sigerip.ufpb@gmail.com"
password <- "Sigerip@2024"

# Lista de países disponíveis
countries_df <- getHMDcountries()  # Puxar a lista de países
countries <- countries_df[[3]]  # Extrair a terceira coluna (CNTRY)

# Tábuas de mortalidade por Grupos Etários
data_type_life_bs <- "bltper_5x1"  # Ambos os sexos
data_type_life_m <- "mltper_5x1"   # Masculino
data_type_life_f <- "fltper_5x1"   # Feminino

# Função para baixar dados de um país e tipo de tábua de vida
download_life_table <- function(country, data_type, username, password) {
  tryCatch({
    data <- readHMDweb(CNTRY = country, item = data_type, username = username, password = password)
    data$Country <- country  # Adicionar coluna com o código do país
    return(data)
  }, error = function(e) {
    message("Erro ao baixar dados para o país: ", country, " - Tipo: ", data_type)
    return(NULL)
  })
}

# Baixar dados para todos os países e tipos de tábuas de vida
all_data_bs <- lapply(countries, download_life_table, data_type = data_type_life_bs, username = username, password = password)
all_data_m <- lapply(countries, download_life_table, data_type = data_type_life_m, username = username, password = password)
all_data_f <- lapply(countries, download_life_table, data_type = data_type_life_f, username = username, password = password)

# Excluir a coluna OpenInterval
all_data_bs <- lapply(all_data_bs, function(x) x %>% select(-OpenInterval))
all_data_m <- lapply(all_data_m, function(x) x %>% select(-OpenInterval))
all_data_f <- lapply(all_data_f, function(x) x %>% select(-OpenInterval))

# Função para processar cada data frame
process_life_table <- function(df) {
  # 1. Eliminar linhas com Age >= 95
  df <- df %>% filter(Age < 95)
  
  # 2. Modificar a linha com Age == 90
  df <- df %>%
    mutate(
      mx = ifelse(Age == 90, 1 / ex, mx),  # mx = 1/ex
      ax = ifelse(Age == 90, ex, ax),      # ax = ex
      qx = ifelse(Age == 90, 1.0000, qx),  # qx = 1.0000
      dx = ifelse(Age == 90, lx, dx),      # dx = lx
      Lx = ifelse(Age == 90, Tx, Lx)       # Lx = Tx
    )
  
  return(df)
}

# Aplicar a função a todas as listas de data frames
all_data_bs_processed <- lapply(all_data_bs, process_life_table)
all_data_m_processed <- lapply(all_data_m, process_life_table)
all_data_f_processed <- lapply(all_data_f, process_life_table)

# Combinar os dados processados em três data frames
combined_bs_processed <- bind_rows(all_data_bs_processed)
combined_m_processed <- bind_rows(all_data_m_processed)
combined_f_processed <- bind_rows(all_data_f_processed)

# Gravar os data frames em arquivos Parquet
write_parquet(combined_bs_processed, "life_tables_both_sexes_processed.parquet")
write_parquet(combined_m_processed, "life_tables_male_processed.parquet")
write_parquet(combined_f_processed, "life_tables_female_processed.parquet")

# Filtrar Year, Age, Country e mx
combined_bs_filtered <- combined_bs_processed %>%
  select(Year, Age, Country, mx)
combined_m_filtered <- combined_m_processed %>%
  select(Year, Age, Country, mx)
combined_f_filtered <- combined_f_processed %>%
  select(Year, Age, Country, mx)

# Pivotar os data frames para o formato longo, colocando Age como coluna e ordenando por Year e Country
combined_bs_long <- combined_bs_filtered %>%
  pivot_wider(names_from = Age, values_from = mx) %>%
  arrange(Country, Year)
combined_m_long <- combined_m_filtered %>%
  pivot_wider(names_from = Age, values_from = mx) %>%
  arrange(Country, Year)
combined_f_long <- combined_f_filtered %>%
  pivot_wider(names_from = Age, values_from = mx) %>%
  arrange(Country, Year)

# Selecionar apenas anos de 1950 a 2019
combined_bs_long <- combined_bs_long %>%
  filter(Year >= 1950 & Year <= 2019)
combined_m_long <- combined_m_long %>%
  filter(Year >= 1950 & Year <= 2019)
combined_f_long <- combined_f_long %>%
  filter(Year >= 1950 & Year <= 2019)

# Transformar as taxas das colunas anos em ln
combined_bs_long <- combined_bs_long %>%
  mutate(across(`0`:`90`, log))
combined_m_long <- combined_m_long %>%
  mutate(across(`0`:`90`, log))
combined_f_long <- combined_f_long %>%
  mutate(across(`0`:`90`, log))

# Gravar os data frames longos em arquivos csv
write.csv(combined_bs_long, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/hmd_ambos.csv", row.names = FALSE)
write.csv(combined_m_long, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/hmd_masculino.csv", row.names = FALSE)
write.csv(combined_f_long, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/hmd_feminino.csv", row.names = FALSE)

