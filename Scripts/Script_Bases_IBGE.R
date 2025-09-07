### Projeções IBGE - 2024
# Limpar ambiente
rm(list = ls())

# Carregar pacotes necessários
library(forecast)
library(tseries)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)
library(viridis)
library(tidyverse)

# Carregar os dados
tabua_BR <- read_excel("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/tabuas_BR_IBGE.xlsx")

# Verificar as primeiras linhas dos dados
head(tabua_BR)

# Excluir as 4 primeiras linhas e converter a 5 linha em cabeçalho
colnames(tabua_BR) <- tabua_BR[5, ]
tabua_BR <- tabua_BR[-(1:5), ]

# Filtrar apenas dados para o Brasil e eliminar colunas SIGLA e CÓD
tabua_BR <- tabua_BR %>%
  filter(LOCAL == "Brasil") %>%
  select(-SIGLA, -CÓD.)

# Converter todas as colunas para númerico, exceto LOCAL e SEXO
tabua_BR <- tabua_BR %>%
  mutate(across(-c(LOCAL, SEXO), as.numeric))

# Substituir valores NA por 0
tabua_BR[is.na(tabua_BR)] <- 0

# Gerar gráfico da evolução das taxas de mortalidade por idade
plot_evolucao_mortalidade <- function(dados, grupo_etario = NULL, anos = c(2000, 2019)) {
  # Filtros básicos
  dados_filtrados <- dados %>%
    filter(ANO >= anos[1] & ANO <= anos[2])
  
  # Aplicar filtro por grupo etário se especificado
  if (!is.null(grupo_etario)) {
    dados_filtrados <- dados_filtrados %>%
      filter(IDADE %in% grupo_etario)
    
    titulo <- paste0("Evolução da Mortalidade - Grupo ", 
                     paste(grupo_etario, collapse = ", "), 
                     " (", anos[1], "-", anos[2], ")")
  } else {
    titulo <- paste0("Evolução da Mortalidade - Todos os Grupos (", 
                     anos[1], "-", anos[2], ")")
  }
  
  # Criar o gráfico
  p <- dados_filtrados %>%
    ggplot(aes(x = ANO, y = log(nMx), color = SEXO, group = interaction(SEXO, IDADE))) +
    geom_line() +
    geom_point(size = 1) +
    labs(title = titulo,
         x = "Ano",
         y = "Log da Taxa de Mortalidade",
         color = "Sexo") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(anos[1], anos[2], by = 2)) +
    theme(legend.position = "bottom")
  
  # Se mostrar todos os grupos, adicionar facetas
  if (is.null(grupo_etario)) {
    p <- p + facet_wrap(~IDADE, scales = "free_y")
  }
  
  return(p)
}

# Gráficos individuais da evolução das taxas de mortalidade
grupos_etarios <- unique(tabua_BR$IDADE)
for (grupo in grupos_etarios) {
  print(plot_evolucao_mortalidade(tabua_BR, grupo_etario = grupo))
}

# Gráfico da evolução das taxas de mortalidade para todos os grupos
plot_evolucao_mortalidade(tabua_BR)

# Função para a curva de mortalidade
plot_evolucao_mortalidade_gradiente <- function(dados, sexo_selecionado = "Masculino") {
  # Verificar se os dados já estão no formato correto
  if(!all(c("IDADE", "ANO", "nMx", "SEXO") %in% names(dados))) {
    stop("Os dados devem conter as colunas: IDADE, ANO, nMx e SEXO")
  }
  
  # Criar o gráfico
  p <- dados %>%
    filter(SEXO == sexo_selecionado,
           ANO >= 2000, ANO <= 2019) %>%
    ggplot(aes(x = IDADE, y = log(nMx), group = ANO, color = ANO)) +
    geom_line(linewidth = 0.8, alpha = 0.7) +
    scale_color_viridis_c(
      name = "Ano",
      option = "viridis",
      breaks = seq(2000, 2019, by = 3),
      guide = guide_colorbar(
        barwidth = 15,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    scale_x_continuous(
      breaks = seq(0, 90, by = 10),
      minor_breaks = seq(0, 90, by = 5)
    ) +
    labs(
      title = paste("Evolução da Mortalidade por Idade -", sexo_selecionado),
      subtitle = "2000-2019 (gradiente temporal)",
      x = "Idade (anos)",
      y = "Log da Taxa de Mortalidade (nMx)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "gray40"),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_line(color = "gray95")
    )
  
  return(p)
}

# Para o sexo masculino
plot_evolucao_mortalidade_gradiente(tabua_BR, "Homens")
# Para o sexo feminino
plot_evolucao_mortalidade_gradiente(tabua_BR, "Mulheres")
# Para ambos os sexos
plot_evolucao_mortalidade_gradiente(tabua_BR, "Ambos")

# Gravar csv com a tábua brasileira
write.csv(tabua_BR, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/tabuas_BR.csv", row.names = FALSE)


################
# Carregar dados populacionais
pop_br <- read_excel("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/pop_BR_IBGE.xlsx")
pop_br_is <- read_excel("C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/pop_BR_IBGE_is.xlsx")

# Verificar as primeiras linhas dos dados populacionais
head(pop_br)
head(pop_br_is)

# Excluir as 4 primeiras linhas e converter a 5 linha em cabeçalho
colnames(pop_br) <- pop_br[5, ]
pop_br <- pop_br[-(1:5), ]
colnames(pop_br_is) <- pop_br_is[5, ]
pop_br_is <- pop_br_is[-(1:5), ]

# Filtrar apenas dados para o Brasil e eliminar colunas SIGLA e CÓD
pop_br <- pop_br %>%
  filter(LOCAL == "Brasil") %>%
  select(-SIGLA, -CÓD.)
pop_br_is <- pop_br_is %>%
  filter(LOCAL == "Brasil") %>%
  select(-SIGLA, -CÓD.)

# Converter as colunas 2000, 2001, ..., 2019 para numérico
pop_br <- pop_br %>%
  mutate(across(starts_with("20"), as.numeric))
# Converter as colunas 2000, 2001, ..., 2019 para numérico no dataset de IS
pop_br_is <- pop_br_is %>%
  mutate(across(starts_with("20"), as.numeric))

# Converter as colunas com os anos para linhas, com o novo nome da coluna ANO
pop_br <- pop_br %>%
  pivot_longer(cols = starts_with("20"), names_to = "ANO", values_to = "POP") %>%
  mutate(ANO = as.numeric(ANO))
pop_br_is <- pop_br_is %>%
  pivot_longer(cols = starts_with("20"), names_to = "ANO", values_to = "POP") %>%
  mutate(ANO = as.numeric(ANO))

# Excluir dados do ano 2020 em diante
pop_br <- pop_br %>% filter(ANO < 2020)
pop_br_is <- pop_br_is %>% filter(ANO < 2020)

# Filtrar apenas IDADE 0 na pop_br_is
pop_br_is_0 <- pop_br_is %>%
  filter(IDADE == "0")

# Transformar a coluna GRUPO ETÁRIO em numérica e mudar o nome para IDADE, extraindo apenas o número antes de -
pop_br <- pop_br %>%
  mutate(IDADE = as.numeric(sub("^(\\d+)-.*", "\\1", `GRUPO ETÁRIO`))) %>%
  select(-`GRUPO ETÁRIO`)

# Transformar a coluna IDADE em númerica para pop_br_is_0
pop_br_is_0 <- pop_br_is_0 %>%
  mutate(IDADE = as.numeric(IDADE))

# Substituir NA por 90
pop_br$IDADE[is.na(pop_br$IDADE)] <- 90

# Ordenar por ano, sexo e idade
pop_br <- pop_br %>%
  arrange(ANO, SEXO, IDADE)

pop_br_is <- pop_br_is %>%
  arrange(ANO, SEXO, IDADE)

# Substituir a IDADE 0 por 1
pop_br$IDADE[pop_br$IDADE == 0] <- 1

# Concatenar os dados populacionais
pop_br <- bind_rows(pop_br, pop_br_is_0)

# Reordenar os dados após a concatenação
pop_br <- pop_br %>%
  arrange(ANO, SEXO, IDADE)

# Diminuir a população da IDADE 1 o valor da população da IDADE 0
pop_br <- pop_br %>%
  group_by(ANO, SEXO) %>%
  mutate(POP = ifelse(IDADE == 1, POP - POP[IDADE == 0], POP)) %>%
  ungroup()

# Converter a coluna POP para numérico
pop_br$POP <- as.numeric(pop_br$POP)

# Converter sexo para coluna e excluir a coluna LOCAL
pop_br <- pop_br %>%
  select(-LOCAL) %>%
  pivot_wider(names_from = SEXO, values_from = POP)

# Gravar csv com a população brasileira
write.csv(pop_br, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/pop_BR.csv", row.names = FALSE)

# Dividir tabua em três df: Mulheres, Homens e Ambos
tabua_mulheres <- tabua %>% 
  select(IDADE, ANO, Mulheres) %>%
  rename(nMx = Mulheres)
  
tabua_homens <- tabua %>% 
  select(IDADE, ANO, Homens) %>%
  rename(nMx = Homens)

tabua_ambos <- tabua %>% 
  select(IDADE, ANO, Ambos) %>%
  rename(nMx = Ambos)

# Pivotar os dados de tabua_mulheres, tabua_homens e tabua_ambos e converter nMx para ln
tabua_mulheres <- tabua_mulheres %>%
  mutate(nMx = log(nMx)) %>%
  pivot_wider(names_from = IDADE, values_from = nMx)

tabua_homens <- tabua_homens %>%
  mutate(nMx = log(nMx)) %>%
  pivot_wider(names_from = IDADE, values_from = nMx)

tabua_ambos <- tabua_ambos %>%
  mutate(nMx = log(nMx)) %>%
  pivot_wider(names_from = IDADE, values_from = nMx)

# Gravar os dataframes em arquivos CSV
write.csv(tabua_mulheres, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/tabua_mulheres.csv", row.names = FALSE)
write.csv(tabua_homens, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/tabua_homens.csv", row.names = FALSE)
write.csv(tabua_ambos, "C:/Users/cleod/OneDrive/Documentos/Documentos/Estudos/Ciências Atuariais/TCC/tabua_ambos.csv", row.names = FALSE)

