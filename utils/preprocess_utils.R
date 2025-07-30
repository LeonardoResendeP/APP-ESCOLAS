# --- FUNÇÕES AUXILIARES DE BUSCA DE DADOS ---

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVA FUNÇÃO INTEGRADA <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Função movida do script de teste para buscar dados de infraestrutura.
fetch_infra_data <- function(codigos_inep, id_municipio, ano, bq_connection) {
  
  cat("--- Buscando dados de infraestrutura para o ano:", ano, "---\n")
  
  # Query para buscar os indicadores de infraestrutura para escolas específicas.
  query_infra <- glue::glue_sql("
    SELECT
      CAST(id_escola AS STRING) AS id_escola,
      COALESCE(agua_potavel, 0) AS essencial_agua_potavel,
      COALESCE(biblioteca, 0) AS essencial_biblioteca,
      COALESCE(laboratorio_informatica, 0) AS essencial_lab_informatica,
      COALESCE(laboratorio_ciencias, 0) AS essencial_lab_ciencias,
      COALESCE(quadra_esportes, 0) AS essencial_quadra_esportes,
      COALESCE(dependencia_pne, 0) AS essencial_acessibilidade_pne,
      COALESCE(refeitorio, 0) AS essencial_refeitorio,
      COALESCE(area_verde, 0) AS lazer_area_verde,
      COALESCE(parque_infantil, 0) AS lazer_parque_infantil,
      COALESCE(quadra_esportes_coberta, 0) AS lazer_quadra_coberta,
      COALESCE(internet, 0) AS tec_internet_geral,
      COALESCE(banda_larga, 0) AS tec_banda_larga,
      COALESCE(internet_alunos, 0) AS tec_internet_para_alunos,
      COALESCE(equipamento_lousa_digital, 0) AS tec_lousa_digital,
      (COALESCE(quantidade_desktop_aluno, 0) + COALESCE(quantidade_computador_portatil_aluno, 0) + COALESCE(quantidade_tablet_aluno, 0)) AS tec_total_dispositivos_aluno,
      COALESCE(profissional_psicologo, 0) AS apoio_psicologo,
      COALESCE(profissional_bibliotecario, 0) AS apoio_bibliotecario
    FROM `basedosdados.br_inep_censo_escolar.escola` 
    WHERE ano = {ano} AND CAST(id_escola AS STRING) IN ({codigos_inep*})
  ", .con = bq_connection)
  
  dados_escolas <- tryCatch(dbGetQuery(bq_connection, query_infra), error = function(e) {
    cat("ERRO ao buscar dados de infraestrutura para escolas:", e$message, "\n"); return(NULL)
  })
  
  # Query para a média municipal dos mesmos indicadores.
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
  
  media_municipio <- tryCatch(dbGetQuery(bq_connection, query_media_municipio), error = function(e) {
    cat("ERRO ao buscar a média de infraestrutura do município:", e$message, "\n"); return(NULL)
  })
  
  # Combina os resultados e retorna um único dataframe.
  return(bind_rows(dados_escolas, media_municipio))
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA NOVA FUNÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# --- FUNÇÃO PRINCIPAL DE PRÉ-PROCESSAMENTO ---
preprocessar_escola <- function(codinep, lat = NULL, lon = NULL, nome_muni_manual = NULL) {
  # Carrega as bibliotecas necessárias
  suppressMessages({
    if (!require(pacman)) install.packages("pacman");
    pacman::p_load(DBI, bigrquery, basedosdados, tidyverse, sf, glue, geosphere)
  })
  
  codinep <- as.character(codinep)
  cat("\n\U0001F504 Iniciando processamento da escola:", codinep, "\n")
  
  # Define os anos de referência para as buscas
  anos_matricula <- c(2023,2024)
  ano_infra <- 2024 # Usando o ano que você definiu
  
  # Configura e conecta ao BigQuery
  set_billing_id("able-study-331220")
  cat("\U0001F4F1 Conectando ao BigQuery...\n")
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "basedosdados",
    dataset = "br_inep_censo_escolar",
    billing = "able-study-331220"
  )
  on.exit(dbDisconnect(con)) # Garante que a conexão seja fechada no final
  
  cat("\U0001F4E5 Buscando dados da escola principal...\n")
  
  # Carrega os dados locais
  nomes_all <- readRDS("data/escolas_privadas_nomelista.rds") %>% dplyr::mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
  geo_all_sf <- readRDS("data/escolas_geo_com_empty_flag.rds")
  
  geo_all_df <- sf::st_drop_geometry(geo_all_sf) %>%
    dplyr::mutate(code_school = as.character(code_school))
  
  nome_escola <- nomes_all %>% dplyr::filter(CO_ENTIDADE == codinep) %>% dplyr::pull(NO_ENTIDADE) %>% first()
  if (is.na(nome_escola)) nome_escola <- "N/D"
  
  cat("\U0001F30E Verificando geolocalização e concorrentes...\n")
  
  if (!is.null(nome_muni_manual) && nzchar(nome_muni_manual)) {
    nome_muni <- nome_muni_manual
    cat("Usando município fornecido manualmente:", nome_muni, "\n")
    
    geo_escola <- geo_all_df %>% dplyr::filter(code_school == codinep)
    if (is.null(lat) || is.null(lon)) {
      lat <- if (nrow(geo_escola) > 0 && !is.na(geo_escola$latitude[1])) geo_escola$latitude[1] else NA
      lon <- if (nrow(geo_escola) > 0 && !is.na(geo_escola$longitude[1])) geo_escola$longitude[1] else NA
    }
    
  } else if (is.null(lat) || is.null(lon)) {
    geo_escola <- geo_all_df %>% dplyr::filter(code_school == codinep)
    if (nrow(geo_escola) == 0 || is.na(geo_escola$latitude) || is.na(geo_escola$longitude) || is.na(geo_escola$name_muni)) {
      stop("Geolocalização para a escola principal não encontrada e nenhum município foi fornecido manualmente.")
    }
    lat <- geo_escola$latitude[1]
    lon <- geo_escola$longitude[1]
    nome_muni <- geo_escola$name_muni[1]
  } else {
    geo_escola <- geo_all_df %>% dplyr::filter(code_school == codinep)
    nome_muni <- geo_escola$name_muni[1]
    if(is.na(nome_muni)) stop("Não foi possível determinar o município da escola. Por favor, selecione manualmente.")
  }
  
  concorrentes_locais <- geo_all_df %>%
    dplyr::filter(name_muni == nome_muni,
                  code_school != codinep)
  
  concorrentes_com_nomes <- concorrentes_locais %>%
    dplyr::left_join(nomes_all, by = c("code_school" = "CO_ENTIDADE"))
  
  if (nrow(concorrentes_com_nomes %>% filter(!is.na(latitude), !is.na(longitude))) > 0 && !is.na(lat) && !is.na(lon)) {
    concorrentes_finais <- concorrentes_com_nomes %>%
      filter(!is.na(latitude), !is.na(longitude)) %>%
      dplyr::mutate(dist_metros = distHaversine(
        matrix(c(lon, lat), nrow = 1),
        matrix(c(longitude, latitude), ncol = 2)
      )) %>%
      dplyr::arrange(dist_metros) %>%
      dplyr::slice(1:5)
  } else {
    concorrentes_finais <- concorrentes_com_nomes %>%
      dplyr::arrange(NO_ENTIDADE) %>%
      dplyr::slice(1:5) %>%
      mutate(dist_metros = NA)
  }
  
  ids_concorrentes_proximos <- concorrentes_finais$code_school
  
  cat("\U0001F4E5 Buscando dados no BigQuery...\n")
  
  todos_os_ids <- c(codinep, ids_concorrentes_proximos)
  
  if(length(todos_os_ids) == 0 || (length(todos_os_ids) == 1 && is.na(todos_os_ids[1]))) {
    stop("Não foi possível gerar uma lista de IDs de escolas para a consulta.")
  }
  
  query_matriculas <- glue::glue_sql(
    "SELECT
     ano, CAST(id_escola AS STRING) AS id_escola, CAST(id_municipio AS STRING) AS id_municipio,
     COALESCE(quantidade_matricula_infantil_creche, 0) + COALESCE(quantidade_matricula_infantil_pre_escola, 0) AS mat_infantil,
     COALESCE(quantidade_matricula_fundamental_anos_iniciais, 0) + COALESCE(quantidade_matricula_fundamental_anos_finais, 0) AS mat_fundamental,
     COALESCE(quantidade_matricula_medio, 0) AS mat_medio
   FROM `basedosdados.br_inep_censo_escolar.escola`
   WHERE CAST(id_escola AS STRING) IN ({todos_os_ids*}) AND ano IN ({anos_matricula*})",
    .con = con
  )
  
  df_escolas_selecionadas <- dbGetQuery(con, query_matriculas)
  
  cat("\U0001F4CA Processando e estruturando os resultados...\n")
  
  tryCatch({
    
    df_escola_principal <- df_escolas_selecionadas %>% dplyr::filter(id_escola == codinep)
    df_concorrentes <- df_escolas_selecionadas %>% dplyr::filter(id_escola != codinep)
    
    id_muni_bq <- if(nrow(df_escola_principal) > 0) df_escola_principal$id_municipio[1] else NA
    if(is.na(id_muni_bq)) {
      muni_info <- geo_all_df %>% filter(name_muni == nome_muni) %>% select(id_municipio) %>% slice(1)
      id_muni_bq <- if(nrow(muni_info) > 0) muni_info$id_municipio else NA
    }
    if(is.na(id_muni_bq)) stop("Não foi possível obter o ID do município.")
    
    make_bloco_matriculas <- function(df, anos) {
      if (nrow(df) == 0) return(list())
      df_wide <- df %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(across(starts_with("mat_"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = ano, values_from = starts_with("mat_"))
      segmentos <- c("mat_infantil", "mat_fundamental", "mat_medio")
      blocos <- list()
      for (seg in segmentos) {
        v1_col <- paste0(seg, "_", anos[1]); v2_col <- paste0(seg, "_", anos[2])
        v1 <- if(v1_col %in% names(df_wide)) df_wide[[v1_col]][1] else 0
        v2 <- if(v2_col %in% names(df_wide)) df_wide[[v2_col]][1] else 0
        v1 <- ifelse(is.na(v1), 0, v1); v2 <- ifelse(is.na(v2), 0, v2)
        var <- ifelse(v1 == 0, NA, round((v2 - v1) / v1 * 100, 1))
        blocos[[seg]] <- list(label = paste("Matrículas -", stringr::str_to_title(gsub("mat_", "", seg))),
                              valor_ano_1 = v1, valor_ano_2 = v2,
                              taxa_de_variacao = ifelse(is.na(var), "N/A", paste0(var, "%")))
      }
      blocos
    }
    
    dados_propria <- make_bloco_matriculas(df_escola_principal, anos_matricula)
    dados_concorrentes <- make_bloco_matriculas(df_concorrentes, anos_matricula)
    
    query_mercado <- glue::glue_sql("
      SELECT ano,
        COALESCE(SUM(quantidade_matricula_infantil_creche), 0) + COALESCE(SUM(quantidade_matricula_infantil_pre_escola), 0) AS mat_infantil,
        COALESCE(SUM(quantidade_matricula_fundamental_anos_iniciais), 0) + COALESCE(SUM(quantidade_matricula_fundamental_anos_finais), 0) AS mat_fundamental,
        COALESCE(SUM(quantidade_matricula_medio), 0) AS mat_medio
      FROM `basedosdados.br_inep_censo_escolar.escola`
      WHERE CAST(id_municipio AS STRING) = {id_muni_bq} AND ano IN ({anos_matricula*})
      GROUP BY ano
    ", .con = con)
    
    dados_mercado <- dbGetQuery(con, query_mercado)
    dados_mercado_municipio <- make_bloco_matriculas(dados_mercado, anos_matricula)
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CHAMADA DA NOVA FUNÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Busca os dados de infraestrutura usando a função auxiliar.
    dados_infraestrutura <- fetch_infra_data(
      codigos_inep = todos_os_ids,
      id_municipio = id_muni_bq,
      ano = ano_infra,
      bq_connection = con
    )
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CHAMADA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< RESULTADO FINAL ENRIQUECIDO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Adiciona os novos dataframes à lista de resultados que será salva.
    resultado <- list(
      id_escola = codinep, nome_escola = nome_escola, id_municipio = id_muni_bq,
      nome_municipio = nome_muni, latitude = lat, longitude = lon,
      lista_concorrentes = concorrentes_finais %>% 
        dplyr::select(id_escola = code_school, nome_escola = NO_ENTIDADE, latitude, longitude, dist_metros),
      dados_propria_escola = dados_propria,
      dados_concorrentes_proximos = dados_concorrentes,
      dados_mercado_municipio = dados_mercado_municipio,
      # Novos dados adicionados aqui:
      dados_infraestrutura = dados_infraestrutura,
      data_extracao = Sys.time() # Boa prática para saber quando os dados foram gerados
    )
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DO ENRIQUECIMENTO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    dir.create("data/escolas", recursive = TRUE, showWarnings = FALSE)
    path_out <- file.path("data", "escolas", paste0(codinep, ".rds"))
    saveRDS(resultado, path_out)
    cat("\U00002705 Dados processados e salvos com sucesso em:", path_out, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("--- ERRO DURANTE O PROCESSAMENTO DOS DADOS ---\n"); print(e); cat("--------------------------------------------\n")
    stop(e)
  })
}
