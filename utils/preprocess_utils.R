# preprocessar_escola.R
preprocessar_escola <- function(codinep, lat = NULL, lon = NULL, nome_muni_manual = NULL) {
  # Carrega as bibliotecas necessárias
  suppressMessages({
    if (!require(pacman)) install.packages("pacman");
    pacman::p_load(DBI, bigrquery, basedosdados, tidyverse, sf, glue, geosphere)
  })
  
  codinep <- as.character(codinep)
  cat("\n\U0001F504 Iniciando processamento da escola:", codinep, "\n")
  
  # Configura e conecta ao BigQuery
  set_billing_id("able-study-331220")
  cat("\U0001F4F1 Conectando ao BigQuery...\n")
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "basedosdados",
    dataset = "br_inep_censo_escolar",
    billing = "able-study-331220"
  )
  
  cat("\U0001F4E5 Buscando dados da escola principal...\n")
  
  # Carrega os dados locais
  nomes_all <- readRDS("data/escolas_privadas_nomelista.rds") %>% dplyr::mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
  geo_all_sf <- readRDS("data/escolas_geo_com_empty_flag.rds")
  
  geo_all_df <- sf::st_drop_geometry(geo_all_sf) %>%
    dplyr::mutate(code_school = as.character(code_school))
  
  nome_escola <- nomes_all %>% dplyr::filter(CO_ENTIDADE == codinep) %>% dplyr::pull(NO_ENTIDADE) %>% first()
  if (is.na(nome_escola)) nome_escola <- "N/D"
  
  cat("\U0001F30E Verificando geolocalização...\n")
  
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
  
  cat("\U0001F680 Otimização: Buscando 5 concorrentes mais próximos localmente...\n")
  
  concorrentes_locais <- geo_all_df %>%
    dplyr::filter(name_muni == nome_muni,
                  code_school != codinep)
  
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CORREÇÃO IMPORTANTE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Junta com os nomes das escolas ANTES de decidir como ordenar e fatiar.
  # Isto garante que a coluna NO_ENTIDADE esteja sempre disponível.
  concorrentes_com_nomes <- concorrentes_locais %>%
    dplyr::left_join(nomes_all, by = c("code_school" = "CO_ENTIDADE"))
  
  # Apenas calcula a distância se houver lat/lon para a escola principal E concorrentes com geo
  if (nrow(concorrentes_com_nomes %>% filter(!is.na(latitude), !is.na(longitude))) > 0 && !is.na(lat) && !is.na(lon)) {
    concorrentes_finais <- concorrentes_com_nomes %>%
      # Filtra apenas os que têm geo para o cálculo
      filter(!is.na(latitude), !is.na(longitude)) %>%
      dplyr::mutate(dist_metros = distHaversine(
        matrix(c(lon, lat), nrow = 1),
        matrix(c(longitude, latitude), ncol = 2)
      )) %>%
      dplyr::arrange(dist_metros) %>%
      dplyr::slice(1:5)
  } else {
    # Se não houver geo, apenas ordena alfabeticamente
    concorrentes_finais <- concorrentes_com_nomes %>%
      dplyr::arrange(NO_ENTIDADE) %>%
      dplyr::slice(1:5) %>%
      mutate(dist_metros = NA)
  }
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  ids_concorrentes_proximos <- concorrentes_finais$code_school
  
  cat("\U0001F4E5 Buscando dados no BigQuery apenas para as escolas selecionadas...\n")
  
  todos_os_ids <- c(codinep, ids_concorrentes_proximos)
  
  if(length(todos_os_ids) == 0 || (length(todos_os_ids) == 1 && is.na(todos_os_ids[1]))) {
    stop("Não foi possível gerar uma lista de IDs de escolas para a consulta.")
  }
  
  query_otimizada <- glue::glue_sql(
    "SELECT
     ano,
     CAST(id_escola AS STRING) AS id_escola,
     CAST(id_municipio AS STRING) AS id_municipio,
     COALESCE(quantidade_matricula_infantil_creche, 0) + 
     COALESCE(quantidade_matricula_infantil_pre_escola, 0) AS mat_infantil,
     COALESCE(quantidade_matricula_fundamental_anos_iniciais, 0) + 
     COALESCE(quantidade_matricula_fundamental_anos_finais, 0) AS mat_fundamental,
     COALESCE(quantidade_matricula_medio, 0) AS mat_medio,
     COALESCE(quantidade_matricula_eja, 0) AS mat_eja,
     COALESCE(quantidade_matricula_profissional, 0) AS mat_profissional
   FROM `basedosdados.br_inep_censo_escolar.escola`
   WHERE CAST(id_escola AS STRING) IN ({todos_os_ids*}) 
   AND ano IN (2022, 2023)",
    .con = con
  )
  
  df_escolas_selecionadas <- dbGetQuery(con, query_otimizada)
  
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
    
    make_bloco_matriculas <- function(df) {
      if (nrow(df) == 0) return(list())
      df_wide <- df %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(across(starts_with("mat_"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = ano, values_from = starts_with("mat_"))
      segmentos <- c("mat_infantil", "mat_fundamental", "mat_medio", "mat_eja", "mat_profissional")
      blocos <- list()
      for (seg in segmentos) {
        v1_col <- paste0(seg, "_2022"); v2_col <- paste0(seg, "_2023")
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
    
    dados_propria <- make_bloco_matriculas(df_escola_principal)
    dados_concorrentes <- make_bloco_matriculas(df_concorrentes)
    
    cat("\U0001F4C8 Buscando dados de mercado para o município...\n")
    query_mercado <- glue::glue_sql("
      SELECT ano,
        COALESCE(SUM(quantidade_matricula_infantil_creche), 0) + COALESCE(SUM(quantidade_matricula_infantil_pre_escola), 0) AS mat_infantil,
        COALESCE(SUM(quantidade_matricula_fundamental_anos_iniciais), 0) + COALESCE(SUM(quantidade_matricula_fundamental_anos_finais), 0) AS mat_fundamental,
        COALESCE(SUM(quantidade_matricula_medio), 0) AS mat_medio,
        COALESCE(SUM(quantidade_matricula_eja), 0) AS mat_eja,
        COALESCE(SUM(quantidade_matricula_profissional), 0) AS mat_profissional
      FROM `basedosdados.br_inep_censo_escolar.escola`
      WHERE CAST(id_municipio AS STRING) = {id_muni_bq} AND ano IN (2022, 2023)
      GROUP BY ano
    ", .con = con)
    
    dados_mercado <- dbGetQuery(con, query_mercado)
    dados_mercado_municipio <- make_bloco_matriculas(dados_mercado)
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CORREÇÃO IMPORTANTE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # O join com 'nomes_all' foi removido daqui porque já foi feito antes,
    # garantindo que 'concorrentes_finais' já tem todas as colunas necessárias.
    resultado <- list(
      id_escola = codinep, nome_escola = nome_escola, id_municipio = id_muni_bq,
      nome_municipio = nome_muni, latitude = lat, longitude = lon,
      lista_concorrentes = concorrentes_finais %>% 
        dplyr::select(id_escola = code_school, nome_escola = NO_ENTIDADE, latitude, longitude, dist_metros),
      dados_propria_escola = dados_propria,
      dados_concorrentes_proximos = dados_concorrentes,
      dados_mercado_municipio = dados_mercado_municipio
    )
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
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
