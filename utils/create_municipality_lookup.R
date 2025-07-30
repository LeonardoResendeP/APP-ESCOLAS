# Script para criar uma lista definitiva de municípios brasileiros
# a partir do basedosdados. Execute este script UMA VEZ a partir da consola do R.

# 1. Carregar bibliotecas
suppressMessages({
  if (!require(pacman)) install.packages("pacman");
  pacman::p_load(basedosdados, dplyr, readr)
})

# 2. Definir o seu billing project ID
set_billing_id("able-study-331220")

# 3. Baixar a tabela de municípios do diretório brasileiro do basedosdados
# Esta tabela contém o ID e o nome de todos os municípios do Brasil
query <- "SELECT id_municipio, nome, sigla_uf FROM `basedosdados.br_bd_diretorios_brasil.municipio`"
municipios_df <- read_sql(query)

# 4. Limpar e preparar os dados
municipios_clean <- municipios_df %>%
  mutate(
    # Garante que o ID do município seja texto para a junção
    id_municipio = as.character(id_municipio),
    # Cria um nome de exibição para a seleção manual, ex: "Alecrim - RS"
    display_name = paste0(nome, " - ", sigla_uf)
  ) %>%
  # Renomeia a coluna 'nome' para ser mais claro no futuro
  rename(nome_municipio = nome) %>%
  select(id_municipio, nome_municipio, sigla_uf, display_name) %>%
  arrange(display_name)

# 5. Salvar o dataframe como um ficheiro .rds na pasta 'data/'
dir.create("data", showWarnings = FALSE)
saveRDS(municipios_clean, "data/municipios_lookup.rds")

cat("SUCESSO! A lista de municípios foi salva em 'data/municipios_lookup.rds'.\n")
