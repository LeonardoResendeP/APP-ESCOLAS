# =========================================================
#   ARQUIVO DE FUNÇÕES AUXILIARES PARA PRÉ-PROCESSAMENTO
# =========================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(sf)
  library(stringr)
  library(readr)
  library(DBI)
  library(bigrquery)
  library(basedosdados)
  library(glue)
  library(geosphere)
})

# ---
# NOVA SEÇÃO: Análise de Território
# ---

analisar_territorio <- function(escolas_sf, setores_sf) {
  old_s2 <- sf::sf_use_s2()          # guarda estado
  on.exit(sf::sf_use_s2(old_s2), add = TRUE)
  sf::sf_use_s2(TRUE)                # usa geodésico
  
  if (sf::st_crs(escolas_sf) != sf::st_crs(setores_sf)) {
    escolas_sf <- sf::st_transform(escolas_sf, sf::st_crs(setores_sf))
  }
  
  escolas_com_setor <- sf::st_join(escolas_sf, setores_sf, join = sf::st_intersects)
  
  dados_territorio <- escolas_com_setor %>%
    sf::st_drop_geometry() %>%
    dplyr::select(
      id_escola = id_escola,
      cd_setor,
      mercado_infantil_0_4       = pop_0_4,
      mercado_fundamental1_5_9   = pop_5_9,
      mercado_fundamental2_10_14 = pop_10_14,
      mercado_medio_15_17        = pop_15_17,
      mercado_total_0_17         = tam_total_0_17,
      contexto_alfabet_15mais    = alfabet_15mais,
      contexto_domicilios_pp     = dpp_total
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::matches("^(mercado_|contexto_)"),
                                ~ suppressWarnings(as.numeric(.x))))
  dados_territorio
}

# ---
# FUNÇÃO PRINCIPAL DE PRÉ-PROCESSAMENTO (CORRIGIDA E INTEGRADA)
# ---

preprocessar_escola <- function(codinep, lat = NULL, lon = NULL) {
  
  codinep <- as.character(codinep)
  cat("\n\U0001F504 Iniciando processamento da escola:", codinep, "\n")
  
  cat("--- Carregando bases de dados unificadas ---\n")
  nomes_df <- readRDS("data/escolas_privadas_nomelista.rds")
  geo_sf <- readRDS("data/processed/escolas_privadas_unificadas_sf.rds")
  municipios_lookup <- readRDS("data/municipios_lookup.rds")
  
  escolas_unificadas_sf <- geo_sf %>%
    left_join(nomes_df, by = c("id_escola" = "CO_ENTIDADE")) %>%
    left_join(municipios_lookup, by = "nome_municipio") %>%
    rename(CO_MUNICIPIO = id_municipio)
  
  setores_com_dados_sf  <- sf::st_read("data/processed/censo2022_universo_setor_joined_clean.gpkg", quiet = TRUE)
  
  escola_principal_info <- escolas_unificadas_sf %>%
    filter(id_escola == codinep) %>%
    slice(1)
  
  if (nrow(escola_principal_info) == 0) {
    stop(paste("ERRO: Código INEP", codinep, "não encontrado na base de dados unificada."))
  }
  
  if (!is.null(lat) && !is.null(lon) && !is.na(lat) && !is.na(lon)) {
    cat("--- Usando coordenadas manuais fornecidas pelo admin ---\n")
    new_geom <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = sf::st_crs(escolas_unificadas_sf))
    sf::st_geometry(escola_principal_info) <- new_geom
  }
  
  nome_escola <- escola_principal_info$NO_ENTIDADE
  id_muni_bq  <- as.character(escola_principal_info$CO_MUNICIPIO)
  nome_muni   <- escola_principal_info$nome_municipio
  
  cat("--- Identificando concorrentes mais próximos ---\n")
  concorrentes_finais <- escolas_unificadas_sf %>%
    mutate(CO_MUNICIPIO = as.character(CO_MUNICIPIO)) %>%
    filter(CO_MUNICIPIO == id_muni_bq, id_escola != codinep, !sf::st_is_empty(.)) %>%
    mutate(
      dist_metros = as.numeric(sf::st_distance(sf::st_geometry(.), sf::st_geometry(escola_principal_info)))
    ) %>%
    arrange(dist_metros) %>%
    slice(1:5)
  
  ids_concorrentes_proximos <- concorrentes_finais$id_escola
  
  escolas_para_analise_sf <- bind_rows(escola_principal_info, concorrentes_finais)
  
  dados_territorio_full <- analisar_territorio(escolas_para_analise_sf, setores_com_dados_sf)
  
  dados_territorio_escola <- dados_territorio_full %>%
    filter(id_escola == codinep)
  
  dados_territorio_concorrentes_media <- dados_territorio_full %>%
    filter(id_escola %in% ids_concorrentes_proximos) %>%
    summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))
  
  dados_processados_bg <- reprocessar_dados_concorrentes(codinep, ids_concorrentes_proximos, id_muni_bq)
  
  # --- Consolidando e salvando resultado final (substituir bloco atual) ---
  # garantir mesmo CRS para a extração de lat/lon do leaflet (WGS84)
  escola_principal_wgs84 <- tryCatch(
    sf::st_transform(escola_principal_info, 4326),
    error = function(e) escola_principal_info
  )
  
  resultado <- list(
    id_escola      = codinep, 
    nome_escola    = nome_escola, 
    id_municipio   = id_muni_bq,
    nome_municipio = nome_muni, 
    latitude       = sf::st_coordinates(escola_principal_wgs84)[, "Y"], 
    longitude      = sf::st_coordinates(escola_principal_wgs84)[, "X"],
    
    # >>> manter geometry aqui <<<
    lista_concorrentes = concorrentes_finais %>% 
      dplyr::select(id_escola, nome_escola = NO_ENTIDADE, dist_metros, geometry),
    
    dados_propria_escola        = dados_processados_bg$dados_propria_escola,
    dados_concorrentes_proximos = dados_processados_bg$dados_concorrentes_proximos,
    dados_mercado_municipio     = dados_processados_bg$dados_mercado_municipio,
    dados_infraestrutura        = dados_processados_bg$dados_infraestrutura,
    dados_enem_areas            = dados_processados_bg$dados_enem_areas,
    dados_enem_redacao          = dados_processados_bg$dados_enem_redacao,
    
    dados_territorio_escola             = dados_territorio_escola,
    dados_territorio_concorrentes_media = dados_territorio_concorrentes_media,
    
    data_extracao = Sys.time()
  )
  
  dir.create("data/escolas", recursive = TRUE, showWarnings = FALSE)
  path_out <- file.path("data", "escolas", paste0(codinep, ".rds"))
  saveRDS(resultado, path_out)
  cat("\U00002705 Dados processados e salvos com sucesso em:", path_out, "\n")
  
  return(TRUE)
}

# ---
# FUNÇÕES DE BUSCA DE DADOS (EXISTENTES)
# ---

fetch_infra_data <- function(codigos_inep, id_municipio, ano, bq_connection) {
  
  cat("--- Buscando dados de infraestrutura para o ano:", ano, "---\n")
  
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
  
  if (!is.null(dados_escolas) && !is.null(media_municipio)) {
    resultados_finais <- bind_rows(dados_escolas, media_municipio)
  } else {
    resultados_finais <- dados_escolas 
  }
  
  return(resultados_finais)
}

get_enem_municipio_df <- function(env) {
  # aceita 'enem_consoliado_municipio' (typo) ou 'enem_consolidado_municipio'
  if (exists("enem_consoliado_municipio", envir = env)) {
    return(get("enem_consoliado_municipio", envir = env))
  } else if (exists("enem_consolidado_municipio", envir = env)) {
    return(get("enem_consolidado_municipio", envir = env))
  } else {
    stop("Objeto de ENEM municipal não encontrado no .RData (nem 'enem_consoliado_municipio' nem 'enem_consolidado_municipio').")
  }
}

processar_enem_local <- function(codinep, ids_concorrentes, id_municipio) {
  cat("--- Processando dados do ENEM a partir de arquivo local ---\n")
  e <- new.env(parent = emptyenv())
  load("data/enem_consolidados.RData", envir = e)
  
  codinep_num <- as.numeric(codinep)
  ids_concorrentes_num <- as.numeric(ids_concorrentes)
  id_municipio_num <- as.numeric(id_municipio)
  
  nomes_all <- readRDS("data/escolas_privadas_nomelista.rds") %>% 
    mutate(CO_ENTIDADE = as.character(CO_ENTIDADE)) %>%
    select(id_escola = CO_ENTIDADE, nome_escola = NO_ENTIDADE)
  
  dados_individuais <- e$enem_consolidado_escola %>%
    filter(CO_ESCOLA %in% c(codinep_num, ids_concorrentes_num)) %>%
    mutate(id_escola = as.character(CO_ESCOLA))
  
  enem_muni_df <- get_enem_municipio_df(e)
  
  concorrentes_encontrados <- dados_individuais %>% filter(id_escola %in% ids_concorrentes)
  num_encontrados <- dplyr::n_distinct(concorrentes_encontrados$id_escola)
  
  dados_concorrentes_media <- concorrentes_encontrados %>%
    summarise(across(starts_with("NU_NOTA"), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(id_escola = "Média Concorrentes")
  
  dados_municipio_media <- enem_muni_df %>%
    filter(CO_MUNICIPIO_ESC == id_municipio_num) %>%
    mutate(id_escola = "Média Municipal") %>%
    select(-CO_MUNICIPIO_ESC)
  
  dados_combinados_full <- bind_rows(
    dados_individuais,
    dados_concorrentes_media,
    dados_municipio_media
  ) %>%
    left_join(nomes_all, by = "id_escola") %>%
    mutate(
      tipo = case_when(
        id_escola == codinep ~ "Sua Escola",
        id_escola %in% ids_concorrentes ~ "Concorrente",
        id_escola == "Média Concorrentes" ~ "Média Concorrentes",
        id_escola == "Média Municipal" ~ "Média Municipal"
      ),
      escola_label = case_when(
        tipo == "Sua Escola" ~ "Sua Escola",
        tipo == "Média Concorrentes" ~ paste0("Média Concorrentes (", num_encontrados, " de ", length(ids_concorrentes), ")"),
        tipo == "Média Municipal" ~ "Média Municipal",
        TRUE ~ nome_escola
      )
    )
  
  df_areas <- dados_combinados_full %>%
    select(id_escola, tipo, escola_label,
           `Ciências da Natureza` = NU_NOTA_CN,
           `Ciências Humanas` = NU_NOTA_CH,
           `Linguagens e Códigos` = NU_NOTA_LC,
           `Matemática` = NU_NOTA_MT,
           `Redação` = NU_NOTA_REDACAO) %>%
    pivot_longer(
      cols = -c(id_escola, tipo, escola_label),
      names_to = "area",
      values_to = "nota"
    )
  
  df_redacao <- dados_combinados_full %>%
    select(id_escola, tipo, escola_label,
           `Competência 1` = NU_NOTA_COMP1,
           `Competência 2` = NU_NOTA_COMP2,
           `Competência 3` = NU_NOTA_COMP3,
           `Competência 4` = NU_NOTA_COMP4,
           `Competência 5` = NU_NOTA_COMP5) %>%
    pivot_longer(
      cols = -c(id_escola, tipo, escola_label),
      names_to = "competencia",
      values_to = "nota"
    )
  
  return(list(areas = df_areas, redacao = df_redacao))
}

reprocessar_dados_concorrentes <- function(codinep, ids_concorrentes, id_municipio) {
  
  cat("--- Iniciando busca de dados para a escola", codinep, "e seus concorrentes ---\n")
  
  suppressMessages({
    if (!require(pacman)) install.packages("pacman");
    pacman::p_load(DBI, bigrquery, basedosdados, tidyverse, glue)
  })
  
  anos_matricula <- c(2023, 2024)
  ano_infra <- 2024
  todos_os_ids <- c(codinep, ids_concorrentes)
  
  set_billing_id("able-study-331220")
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "basedosdados",
    billing = "able-study-331220"
  )
  on.exit(dbDisconnect(con))
  
  query_matriculas <- glue::glue_sql(
    "SELECT
     ano, CAST(id_escola AS STRING) AS id_escola,
     COALESCE(quantidade_matricula_infantil_creche, 0) + COALESCE(quantidade_matricula_infantil_pre_escola, 0) AS mat_infantil,
     COALESCE(quantidade_matricula_fundamental_anos_iniciais, 0) + COALESCE(quantidade_matricula_fundamental_anos_finais, 0) AS mat_fundamental,
     COALESCE(quantidade_matricula_medio, 0) AS mat_medio
   FROM `basedosdados.br_inep_censo_escolar.escola`
   WHERE CAST(id_escola AS STRING) IN ({todos_os_ids*}) AND ano IN ({anos_matricula*})",
    .con = con
  )
  df_matriculas <- dbGetQuery(con, query_matriculas)
  
  df_infra <- fetch_infra_data(todos_os_ids, id_municipio, ano_infra, con)
  
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
  
  dados_propria_escola <- make_bloco_matriculas(df_matriculas %>% filter(id_escola == codinep), anos_matricula)
  dados_concorrentes_proximos <- make_bloco_matriculas(df_matriculas %>% filter(id_escola != codinep), anos_matricula)
  
  query_mercado <- glue::glue_sql("
      SELECT ano,
        COALESCE(SUM(quantidade_matricula_infantil_creche), 0) + COALESCE(SUM(quantidade_matricula_infantil_pre_escola), 0) AS mat_infantil,
        COALESCE(SUM(quantidade_matricula_fundamental_anos_iniciais), 0) + COALESCE(SUM(quantidade_matricula_fundamental_anos_finais), 0) AS mat_fundamental,
        COALESCE(SUM(quantidade_matricula_medio), 0) AS mat_medio
      FROM `basedosdados.br_inep_censo_escolar.escola`
      WHERE CAST(id_municipio AS STRING) = {id_municipio} AND ano IN ({anos_matricula*})
      GROUP BY ano
    ", .con = con)
  dados_mercado <- dbGetQuery(con, query_mercado)
  dados_mercado_municipio <- make_bloco_matriculas(dados_mercado, anos_matricula)
  
  dados_enem_local <- processar_enem_local(codinep, ids_concorrentes, id_municipio)
  
  return(
    list(
      dados_propria_escola = dados_propria_escola,
      dados_concorrentes_proximos = dados_concorrentes_proximos,
      dados_mercado_municipio = dados_mercado_municipio,
      dados_infraestrutura = df_infra,
      dados_enem_areas = dados_enem_local$areas,
      dados_enem_redacao = dados_enem_local$redacao
    )
  )
}