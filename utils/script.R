# Pré-processamento para salvar base leve de escolas, agora com nomes de municípios

# 1. Carregar bibliotecas
suppressMessages({
  if (!require(pacman)) install.packages("pacman");
  pacman::p_load(dplyr, readr)
})

# 2. Carregar os dados brutos e a nova tabela de consulta de municípios
escolas_raw <- read.csv("data/microdados_ed_basica_2024.csv", sep = ";")
municipios_lookup <- readRDS("data/municipios_lookup.rds")

# 3. Processar e enriquecer os dados das escolas
escolas_enriquecidas <- escolas_raw %>%
  # Garante que as colunas de código sejam do tipo texto para a junção
  mutate(
    CO_ENTIDADE = as.character(CO_ENTIDADE),
    CO_MUNICIPIO = as.character(CO_MUNICIPIO)
  ) %>%
  # Junta a base de escolas com a nossa nova tabela de consulta de municípios
  left_join(municipios_lookup, by = c("CO_MUNICIPIO" = "id_municipio")) %>%
  # Seleciona apenas as colunas que nos interessam para a busca
  distinct(CO_ENTIDADE, NO_ENTIDADE, nome_municipio) %>%
  arrange(NO_ENTIDADE)

# 4. Salvar a nova base de escolas enriquecida
saveRDS(escolas_enriquecidas, "data/escolas_privadas_nomelista.rds")

cat("SUCESSO! A base de escolas foi enriquecida com os nomes dos municípios e salva.\n")

