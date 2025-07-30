# --- AMBIENTE DE TESTE PARA DADOS DE INFRAESTRUTURA ---
# Este script não faz parte da aplicação Shiny.
# Use-o para desenvolver e testar a busca de dados de infraestrutura do Censo Escolar.

# 1. Carregar as bibliotecas necessárias
suppressMessages({
  if (!require(pacman)) install.packages("pacman");
  # Adicionadas bibliotecas 'sf' e 'geosphere' para a lógica de proximidade
  pacman::p_load(DBI, bigrquery, basedosdados, tidyverse, glue, sf, geosphere)
})

# 2. Configurar a conexão com o BigQuery
# Substitua pelo seu ID de faturação do Google Cloud
set_billing_id("able-study-331220") 
con <- dbConnect(
  bigrquery::bigquery(),
  project = "basedosdados",
  billing = "able-study-331220"
)

# 3. Carregar dados locais necessários
cat("--- Carregando dados locais ---\n")
# Dados geográficos (code_school -> lat, lon)
geo_all_df <- readRDS("data/escolas_geo_com_empty_flag.rds") %>%
  st_drop_geometry() %>%
  mutate(code_school = as.character(code_school))

# Dados de escolas (CO_ENTIDADE -> nome_municipio)
escolas_lista_df <- readRDS("data/escolas_privadas_nomelista.rds") %>%
  mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))

# Tabela de consulta de municípios (nome_municipio -> id_municipio)
municipios_lookup_df <- readRDS("data/municipios_lookup.rds")

# 4. Definir parâmetros para o teste
codinep_teste <- "35136931" # COL BANDEIRANTES
ano_referencia <- 2024 # Testando com 2023

# --- LÓGICA DINÂMICA PARA ENCONTRAR MUNICÍPIO E CONCORRENTES ---
cat("--- Encontrando município e os 5 concorrentes mais próximos ---\n")

# A. Encontrar o nome do município da escola principal
escola_info <- escolas_lista_df %>%
  filter(CO_ENTIDADE == codinep_teste)

if (nrow(escola_info) == 0) {
  stop("Cód. INEP da escola principal não encontrado na lista de escolas (escolas_privadas_nomelista.rds).")
}
nome_municipio_principal <- escola_info$nome_municipio[1]

# B. Usar o nome para encontrar o ID do município na tabela de consulta
municipio_info <- municipios_lookup_df %>%
  filter(nome_municipio == nome_municipio_principal)

if (nrow(municipio_info) == 0) {
  stop(paste("Não foi possível encontrar o ID para o município:", nome_municipio_principal, "em municipios_lookup.rds"))
}
id_municipio_teste <- municipio_info$id_municipio[1]

cat("Escola Principal:", codinep_teste, "no município de", nome_municipio_principal, "(ID:", id_municipio_teste, ")\n")

# C. Encontrar as coordenadas da escola principal na base geográfica
geo_principal <- geo_all_df %>%
  filter(code_school == codinep_teste)

if (nrow(geo_principal) == 0 || is.na(geo_principal$latitude) || is.na(geo_principal$longitude)) {
  stop("Não foi possível encontrar a geolocalização da escola principal (escolas_geo_com_empty_flag.rds).")
}
lat_principal <- geo_principal$latitude
lon_principal <- geo_principal$longitude

# D. Encontrar todos os concorrentes no mesmo município
concorrentes_no_municipio <- escolas_lista_df %>%
  filter(nome_municipio == nome_municipio_principal,
         CO_ENTIDADE != codinep_teste)

# E. Juntar com dados geográficos para obter coordenadas e calcular distância
concorrentes_df <- concorrentes_no_municipio %>%
  inner_join(geo_all_df, by = c("CO_ENTIDADE" = "code_school")) %>%
  filter(!is.na(latitude), !is.na(longitude)) %>%
  mutate(dist_metros = distHaversine(
    matrix(c(lon_principal, lat_principal), nrow = 1),
    matrix(c(longitude, latitude), ncol = 2)
  )) %>%
  arrange(dist_metros) %>%
  slice(1:5)

# Extrai a lista de códigos INEP dos concorrentes
concorrentes_teste <- concorrentes_df$CO_ENTIDADE
cat("Concorrentes encontrados:", paste(concorrentes_teste, collapse = ", "), "\n")


# --- FUNÇÃO PARA BUSCAR DADOS DE INFRAESTRUTURA ---
fetch_infra_data <- function(codigos_inep, id_municipio, ano, bq_connection) {
  
  cat("\n--- Buscando dados de infraestrutura para o ano:", ano, "---\n")
  
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CORREÇÃO DA QUERY <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Alterado para somar as colunas granulares de computadores, que são mais confiáveis.
  query_infra <- glue::glue_sql("
    SELECT
      CAST(id_escola AS STRING) AS id_escola,
      
      -- Categoria: Infraestrutura Essencial
      COALESCE(agua_potavel, 0) AS essencial_agua_potavel,
      COALESCE(biblioteca, 0) AS essencial_biblioteca,
      COALESCE(laboratorio_informatica, 0) AS essencial_lab_informatica,
      COALESCE(laboratorio_ciencias, 0) AS essencial_lab_ciencias,
      COALESCE(quadra_esportes, 0) AS essencial_quadra_esportes,
      COALESCE(dependencia_pne, 0) AS essencial_acessibilidade_pne,
      COALESCE(refeitorio, 0) AS essencial_refeitorio,

      -- Categoria: Lazer e Bem-Estar
      COALESCE(area_verde, 0) AS lazer_area_verde,
      COALESCE(parque_infantil, 0) AS lazer_parque_infantil,
      COALESCE(quadra_esportes_coberta, 0) AS lazer_quadra_coberta,

      -- Categoria: Tecnologia para Alunos
      COALESCE(internet, 0) AS tec_internet_geral,
      COALESCE(banda_larga, 0) AS tec_banda_larga,
      COALESCE(internet_alunos, 0) AS tec_internet_para_alunos,
      COALESCE(equipamento_lousa_digital, 0) AS tec_lousa_digital,
      (COALESCE(quantidade_desktop_aluno, 0) + COALESCE(quantidade_computador_portatil_aluno, 0) + COALESCE(quantidade_tablet_aluno, 0)) AS tec_total_dispositivos_aluno,

      -- Categoria: Equipe de Apoio
      COALESCE(profissional_psicologo, 0) AS apoio_psicologo,
      COALESCE(profissional_bibliotecario, 0) AS apoio_bibliotecario

    FROM `basedosdados.br_inep_censo_escolar.escola` 
    WHERE ano = {ano} AND CAST(id_escola AS STRING) IN ({codigos_inep*})
  ", .con = bq_connection)
  
  cat("Query para escolas específicas:\n")
  print(query_infra)
  
  dados_escolas <- tryCatch({
    dbGetQuery(bq_connection, query_infra)
  }, error = function(e) {
    cat("ERRO ao buscar dados de infraestrutura para escolas:", e$message, "\n")
    return(NULL)
  })
  
  # Query para a média municipal com as novas variáveis
  query_media_municipio <- glue::glue_sql("
    SELECT
      'Média Municipal' AS id_escola,
      AVG(CAST(COALESCE(agua_potavel, 0) AS INT64)) AS essencial_agua_potavel,
      AVG(CAST(COALESCE(biblioteca, 0) AS INT64)) AS essencial_biblioteca,
      AVG(CAST(COALESCE(laboratorio_informatica, 0) AS INT64)) AS essencial_lab_informatica,
      AVG(CAST(COALESCE(laboratorio_ciencias, 0) AS INT64)) AS essencial_lab_ciencias,
      AVG(CAST(COALESCE(quadra_esportes, 0) AS INT64)) AS essencial_quadra_esportes,
      AVG(CAST(COALESCE(dependencia_pne, 0) AS INT64)) AS essencial_acessibilidade_pne,
      AVG(CAST(COALESCE(refeitorio, 0) AS INT64)) AS essencial_refeitorio,
      AVG(CAST(COALESCE(area_verde, 0) AS INT64)) AS lazer_area_verde,
      AVG(CAST(COALESCE(parque_infantil, 0) AS INT64)) AS lazer_parque_infantil,
      AVG(CAST(COALESCE(quadra_esportes_coberta, 0) AS INT64)) AS lazer_quadra_coberta,
      AVG(CAST(COALESCE(internet, 0) AS INT64)) AS tec_internet_geral,
      AVG(CAST(COALESCE(banda_larga, 0) AS INT64)) AS tec_banda_larga,
      AVG(CAST(COALESCE(internet_alunos, 0) AS INT64)) AS tec_internet_para_alunos,
      AVG(CAST(COALESCE(equipamento_lousa_digital, 0) AS INT64)) AS tec_lousa_digital,
      AVG(COALESCE(quantidade_desktop_aluno, 0) + COALESCE(quantidade_computador_portatil_aluno, 0) + COALESCE(quantidade_tablet_aluno, 0)) AS tec_total_dispositivos_aluno,
      AVG(CAST(COALESCE(profissional_psicologo, 0) AS INT64)) AS apoio_psicologo,
      AVG(CAST(COALESCE(profissional_bibliotecario, 0) AS INT64)) AS apoio_bibliotecario
    FROM `basedosdados.br_inep_censo_escolar.escola`
    WHERE ano = {ano} AND id_municipio = {id_municipio}
  ", .con = bq_connection)
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  cat("\nQuery para a média municipal:\n")
  print(query_media_municipio)
  
  media_municipio <- tryCatch({
    dbGetQuery(bq_connection, query_media_municipio)
  }, error = function(e) {
    cat("ERRO ao buscar a média de infraestrutura do município:", e$message, "\n")
    return(NULL)
  })
  
  # Combina todos os resultados em um único dataframe
  resultados_finais <- bind_rows(dados_escolas, media_municipio)
  
  return(resultados_finais)
}


# --- EXECUÇÃO DO TESTE ---
cat("\n\n>>> INICIANDO TESTE DE DADOS DE INFRAESTRUTURA <<<\n")

# Combina o INEP da escola principal com seus concorrentes
todos_os_ineps <- c(codinep_teste, concorrentes_teste)

# Executa a função de busca
infra_resultados <- fetch_infra_data(todos_os_ineps, id_municipio_teste, ano_referencia, con)

if (!is.null(infra_resultados) && nrow(infra_resultados) > 0) {
  cat("\n\n✅ SUCESSO! Dados de infraestrutura encontrados:\n\n")
  
  # Formata os resultados para melhor visualização
  infra_formatado <- infra_resultados %>%
    pivot_longer(
      cols = -id_escola,
      names_to = "indicador",
      values_to = "valor"
    ) %>%
    mutate(
      # Formata como porcentagem para indicadores binários
      # e como número para contagens (ex: computadores)
      valor_formatado = case_when(
        str_detect(indicador, "total_dispositivos") & id_escola != "Média Municipal" ~ as.character(round(valor, 0)),
        str_detect(indicador, "total_dispositivos") & id_escola == "Média Municipal" ~ as.character(round(valor, 1)),
        TRUE ~ paste0(round(as.numeric(valor) * 100, 1), "%")
      )
    ) %>%
    select(-valor) %>% # Remove a coluna numérica original
    pivot_wider(
      names_from = id_escola,
      values_from = valor_formatado
    ) %>%
    # Adiciona uma coluna de categoria para agrupar os indicadores
    mutate(categoria = str_extract(indicador, "^[^_]+")) %>%
    relocate(categoria) %>%
    arrange(categoria)
  
  print(infra_formatado, n = 30)
  
  cat("\n\n* Valores em % representam a proporção de escolas com o indicador.\n")
  cat("* Valores numéricos (ex: dispositivos) representam a quantidade ou a média municipal.\n")
} else {
  cat("\n\n⚠️ A busca não retornou resultados. Verifique os códigos INEP e o ano de referência. Se o erro persistir, os dados para 2023 podem não estar totalmente disponíveis para consulta via API.\n")
}

# Fechar a conexão no final
dbDisconnect(con)
cat("\n\n>>> TESTE CONCLUÍDO <<<\n")

