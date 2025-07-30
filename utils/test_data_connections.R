# --- AMBIENTE DE TESTE PARA NOVAS CONEXÕES DE DADOS ---
# Este script não faz parte da aplicação Shiny.
# Use-o para desenvolver e testar as funções de busca de dados de forma isolada.

# 1. Carregar as bibliotecas necessárias
suppressMessages({
  if (!require(pacman)) install.packages("pacman");
  pacman::p_load(DBI, bigrquery, basedosdados, tidyverse)
})

# 2. Configurar a conexão com o BigQuery
set_billing_id("able-study-331220") # O seu ID de faturação
con <- dbConnect(
  bigrquery::bigquery(),
  project = "basedosdados",
  billing = "able-study-331220"
)

# 3. Definir uma escola para os testes
# Vamos usar o CODIN_INEP da escola que você já processou com sucesso.
codinep_teste <- "35136931" 

# --- FUNÇÃO DE TESTE PARA O IDEB ---
fetch_ideb_data <- function(cod_inep, bq_connection) {
  
  cat("\n--- Testando busca de dados do IDEB para a escola:", cod_inep, "---\n")
  
  query_ideb <- glue::glue_sql("
    SELECT 
      ano,
      anos_escolares,
      ideb
    FROM `basedosdados.br_inep_ideb.escola` 
    WHERE id_escola = {cod_inep}
  ", .con = bq_connection)
  
  cat("Query a ser executada:\n")
  print(query_ideb)
  
  dados_ideb <- tryCatch({
    dbGetQuery(bq_connection, query_ideb)
  }, error = function(e) {
    cat("ERRO ao buscar dados do IDEB:", e$message, "\n")
    return(NULL)
  })
  
  return(dados_ideb)
}


# --- FUNÇÃO DE TESTE PARA O ENEM ---
fetch_enem_data <- function(cod_inep, bq_connection) {
  
  cat("\n--- Testando busca de dados do ENEM para a escola:", cod_inep, "---\n")
  
  query_enem <- glue::glue_sql("
    SELECT
      ano,
      media_ciencias_natureza,
      media_ciencias_humanas,
      media_linguagens_codigos,
      media_matematica,
      media_redacao
    FROM `basedosdados.br_inep_indicadores_educacionais.taxa_evolucao_escola`
    WHERE id_escola = {cod_inep} AND ano >= 2019
    ORDER BY ano DESC
  ", .con = bq_connection)
  
  cat("Query a ser executada:\n")
  print(query_enem)
  
  dados_enem <- tryCatch({
    dbGetQuery(bq_connection, query_enem)
  }, error = function(e) {
    cat("ERRO ao buscar dados do ENEM:", e$message, "\n")
    return(NULL)
  })
  
  return(dados_enem)
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVA FUNÇÃO DE TESTE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# --- FUNÇÃO DE TESTE PARA O INSE (Nível Socioeconómico) ---
fetch_inse_data <- function(cod_inep, bq_connection) {
  
  cat("\n--- Testando busca de dados do INSE para a escola:", cod_inep, "---\n")
  
  # A tabela do INSE parece ser `br_inep_saeb.escola` e os dados mais recentes são de 2021
  # Vamos usar a tabela do SAEB que contém o INSE.
  query_inse <- glue::glue_sql("
    SELECT
      ano,
      id_escola,
      inse
    FROM `basedosdados.br_inep_indicador_nivel_socioeconomico.escola`
    WHERE id_escola = {cod_inep}
    ORDER BY ano DESC
  ", .con = bq_connection)
  
  cat("Query a ser executada:\n")
  print(query_inse)
  
  dados_inse <- tryCatch({
    dbGetQuery(bq_connection, query_inse)
  }, error = function(e) {
    cat("ERRO ao buscar dados do INSE:", e$message, "\n")
    return(NULL)
  })
  
  return(dados_inse)
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA NOVA FUNÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# --- EXECUÇÃO DOS TESTES ---
# Para executar, selecione as linhas abaixo e pressione Ctrl+Enter no RStudio.

cat("\n\n>>> INICIANDO TESTES DE CONEXÃO <<<\n")

# Teste 1: Buscar dados do IDEB
ideb_resultado <- fetch_ideb_data(codinep_teste, con)

if (!is.null(ideb_resultado)) {
  cat("\n✅ SUCESSO! Dados do IDEB encontrados:\n")
  print(ideb_resultado)
}

# Teste 2: Buscar dados do ENEM
enem_resultado <- fetch_enem_data(codinep_teste, con)

if (!is.null(enem_resultado)) {
  cat("\n✅ SUCESSO! Dados do ENEM encontrados:\n")
  print(enem_resultado)
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVO TESTE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Teste 3: Buscar dados do INSE
inse_resultado <- fetch_inse_data(codinep_teste, con)

if (!is.null(inse_resultado)) {
  cat("\n✅ SUCESSO! Dados do INSE encontrados:\n")
  print(inse_resultado)
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DO NOVO TESTE >>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Fechar a conexão no final
dbDisconnect(con)
cat("\n\n>>> TESTES CONCLUÍDOS <<<\n")
