# =========================================================
#   PREPROCESS UTILS — bases otimizadas + robustez
# =========================================================

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(stringr); library(readr)
  library(DBI); library(bigrquery); library(basedosdados)
  library(glue); library(tidyr)
})

# -------------- Helpers de caminho (DATA_DIR) --------------
path_data <- function(...) file.path(Sys.getenv("DATA_DIR", unset = "data"), ...)

# -------------- Helpers genéricos --------------
`%||%` <- function(a,b) if (!is.null(a)) a else b
coalesce_cols <- function(df, cols){
  present <- intersect(cols, names(df)); if(!length(present)) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]; for(nm in present[-1]) out <- dplyr::coalesce(out, df[[nm]]); out
}
pick_nome_escola <- function(df) as.character(coalesce_cols(df, c("nome_escola","NO_ENTIDADE","NM_ENTIDADE","NO_NOME_ENTIDADE","id_escola")))

# -------------- Bases principais --------------
.carregar_nomes_df <- function(){
  f <- path_data("escolas_privadas_nomelista.rds")
  if(!file.exists(f)) stop("nomes_df ausente: ", f)
  readRDS(f)
}
.carregar_escolas_sf <- function(){
  cand <- c(
    path_data("processed/escolas_enriquecidas.rds"),
    path_data("processed/escolas_SP_RJ_MG.rds"),
    path_data("processed/escolas_privadas_unificadas_sf.rds")
  )
  f <- cand[file.exists(cand)][1]
  if (is.na(f)) stop("Nenhuma base SF de escolas encontrada em processed/.")
  sf <- readRDS(f); if (is.na(sf::st_crs(sf))) sf <- sf::st_set_crs(sf, 4326); sf
}
.carregar_escolas_sf_fallback <- function(){
  f <- path_data("processed/escolas_privadas_unificadas_sf.rds")
  if (file.exists(f)) {
    sf <- readRDS(f); if (is.na(sf::st_crs(sf))) sf <- sf::st_set_crs(sf, 4326); return(sf)
  }
  return(NULL)
}
.carregar_muni_lookup <- function(){
  f <- path_data("municipios_lookup.rds")
  if(!file.exists(f)) stop("municipios_lookup.rds ausente em DATA_DIR.")
  readRDS(f)
}

# -------------- Setores (otimizados por UF) --------------
.get_setores_gpkg_uf <- function(uf) {
  cand <- c(
    path_data(file.path("cache_setores", paste0("setores_", toupper(uf), ".gpkg"))),
    path_data("processed/setores_slim.gpkg")
  )
  f <- cand[file.exists(cand)][1]; if (is.na(f)) NA_character_ else f
}

# -------------- Território (join pt->setor) --------------
.analisar_territorio <- function(escolas_sf, setores_gpkg) {
  if (is.na(setores_gpkg) || !file.exists(setores_gpkg)) return(NULL)
  lay <- sf::st_layers(setores_gpkg)$name[1]
  setores_sf <- suppressMessages(sf::st_read(setores_gpkg, layer = lay, quiet = TRUE))
  if (is.na(sf::st_crs(setores_sf))) setores_sf <- sf::st_set_crs(setores_sf, 4326)
  
  old_s2 <- sf::sf_use_s2(); on.exit(sf::sf_use_s2(old_s2), add = TRUE); sf::sf_use_s2(TRUE)
  if (sf::st_crs(escolas_sf) != sf::st_crs(setores_sf)) escolas_sf <- sf::st_transform(escolas_sf, sf::st_crs(setores_sf))
  
  j <- suppressWarnings(sf::st_join(escolas_sf, setores_sf, join = sf::st_intersects, left = TRUE))
  cols <- names(j)
  first_col <- function(cands){ x <- cands[cands %in% cols]; if(length(x)) x[1] else NA_character_ }
  
  col_setor  <- first_col(c("cd_setor","CD_SETOR","code_setor","id_setor"))
  c_0_4      <- first_col(c("pop_0_4","POP_0_4","p_0_4"))
  c_5_9      <- first_col(c("pop_5_9","POP_5_9","p_5_9"))
  c_10_14    <- first_col(c("pop_10_14","POP_10_14","p_10_14"))
  c_15_17    <- first_col(c("pop_15_17","POP_15_17","p_15_17"))
  c_tot_0_17 <- first_col(c("tam_total_0_17","TAM_TOTAL_0_17","pop_0_17"))
  c_alf_15m  <- first_col(c("alfabet_15mais","ALFABET_15MAIS","alfab_15m"))
  c_dpp      <- first_col(c("dpp_total","DPP_TOTAL","domicilios_pp"))
  
  out <- j |>
    sf::st_drop_geometry() |>
    dplyr::transmute(
      id_escola = as.character(id_escola),
      cd_setor  = if (!is.na(col_setor)) .data[[col_setor]] else NA,
      mercado_infantil_0_4       = suppressWarnings(as.numeric(if (!is.na(c_0_4))   .data[[c_0_4]]   else NA)),
      mercado_fundamental1_5_9   = suppressWarnings(as.numeric(if (!is.na(c_5_9))   .data[[c_5_9]]   else NA)),
      mercado_fundamental2_10_14 = suppressWarnings(as.numeric(if (!is.na(c_10_14)) .data[[c_10_14]] else NA)),
      mercado_medio_15_17        = suppressWarnings(as.numeric(if (!is.na(c_15_17)) .data[[c_15_17]] else NA)),
      mercado_total_0_17         = suppressWarnings(as.numeric(if (!is.na(c_tot_0_17)) .data[[c_tot_0_17]] else NA)),
      contexto_alfabet_15mais    = suppressWarnings(as.numeric(if (!is.na(c_alf_15m))  .data[[c_alf_15m]]  else NA)),
      contexto_domicilios_pp     = suppressWarnings(as.numeric(if (!is.na(c_dpp))      .data[[c_dpp]]      else NA))
    )
  out
}

# -------------- Infraestrutura (BigQuery) --------------
fetch_infra_data <- function(codigos_inep, id_municipio, ano, con) {
  cat("--- Buscando INFRA (ano=", ano, ") ---\n")
  codigos_inep <- as.character(codigos_inep)
  query_escolas <- glue::glue_sql("
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
  ", .con = con)
  
  dados_escolas <- tryCatch(DBI::dbGetQuery(con, query_escolas), error = function(e) NULL)
  
  query_media_muni <- glue::glue_sql("
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
  ", .con = con)
  
  media_municipio <- tryCatch(DBI::dbGetQuery(con, query_media_muni), error = function(e) NULL)
  
  if (!is.null(dados_escolas) && !is.null(media_municipio)) {
    dplyr::bind_rows(dados_escolas, media_municipio)
  } else dados_escolas
}

# -------------- ENEM local (RData) --------------
get_enem_municipio_df <- function(env){
  if (exists("enem_consoliado_municipio", envir = env)) get("enem_consoliado_municipio", envir = env)
  else if (exists("enem_consolidado_municipio", envir = env)) get("enem_consolidado_municipio", envir = env)
  else stop("Objeto ENEM municipal não encontrado no RData.")
}
processar_enem_local <- function(codinep, ids_concorrentes, id_municipio){
  e <- new.env(parent = emptyenv()); rd <- path_data("enem_consolidados.RData")
  if(!file.exists(rd)) stop("ENEM RData ausente: ", rd)
  load(rd, envir = e)
  
  codinep_num <- suppressWarnings(as.numeric(codinep))
  ids_conc_num <- suppressWarnings(as.numeric(ids_concorrentes))
  id_muni_num <- suppressWarnings(as.numeric(id_municipio))
  
  nomes_all <- .carregar_nomes_df() %>% mutate(CO_ENTIDADE = as.character(CO_ENTIDADE)) %>% select(id_escola = CO_ENTIDADE, nome_escola = NO_ENTIDADE)
  
  dados_indiv <- e$enem_consolidado_escola %>% filter(CO_ESCOLA %in% c(codinep_num, ids_conc_num)) %>% mutate(id_escola = as.character(CO_ESCOLA))
  enem_muni_df <- get_enem_municipio_df(e)
  
  conc_encontrados <- dados_indiv %>% filter(id_escola %in% as.character(ids_concorrentes))
  n_found <- dplyr::n_distinct(conc_encontrados$id_escola)
  
  dados_conc_media <- conc_encontrados %>% summarise(across(starts_with("NU_NOTA"), ~ mean(.x, na.rm = TRUE))) %>% mutate(id_escola = "Média Concorrentes")
  dados_muni_media <- enem_muni_df %>% filter(CO_MUNICIPIO_ESC == id_muni_num) %>% mutate(id_escola = "Média Municipal") %>% select(-CO_MUNICIPIO_ESC)
  
  dados_full <- bind_rows(dados_indiv, dados_conc_media, dados_muni_media) %>%
    left_join(nomes_all, by = "id_escola") %>%
    mutate(
      tipo = case_when(
        id_escola == codinep ~ "Sua Escola",
        id_escola %in% as.character(ids_concorrentes) ~ "Concorrente",
        id_escola == "Média Concorrentes" ~ "Média Concorrentes",
        id_escola == "Média Municipal" ~ "Média Municipal"
      ),
      escola_label = case_when(
        tipo == "Sua Escola" ~ "Sua Escola",
        tipo == "Média Concorrentes" ~ paste0("Média Concorrentes (", n_found, " de ", length(ids_concorrentes), ")"),
        tipo == "Média Municipal" ~ "Média Municipal",
        TRUE ~ (nome_escola %||% id_escola)
      )
    )
  
  df_areas <- dados_full %>%
    select(id_escola, tipo, escola_label,
           `Ciências da Natureza` = NU_NOTA_CN,
           `Ciências Humanas` = NU_NOTA_CH,
           `Linguagens e Códigos` = NU_NOTA_LC,
           `Matemática` = NU_NOTA_MT,
           `Redação` = NU_NOTA_REDACAO) %>%
    pivot_longer(cols = -c(id_escola, tipo, escola_label), names_to = "area", values_to = "nota")
  
  df_red <- dados_full %>%
    select(id_escola, tipo, escola_label,
           `Competência 1` = NU_NOTA_COMP1,
           `Competência 2` = NU_NOTA_COMP2,
           `Competência 3` = NU_NOTA_COMP3,
           `Competência 4` = NU_NOTA_COMP4,
           `Competência 5` = NU_NOTA_COMP5) %>%
    pivot_longer(cols = -c(id_escola, tipo, escola_label), names_to = "competencia", values_to = "nota")
  
  list(areas = df_areas, redacao = df_red)
}

# -------------- Matrículas (com Iniciais/Finais) --------------

.make_bloco_matriculas <- function(df, anos) {
  if (nrow(df) == 0) return(list())
  
  df_wide <- df %>%
    group_by(ano) %>%
    summarise(across(starts_with("mat_"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = ano, values_from = starts_with("mat_"))
  
  # segmentos que queremos exibir
  segs <- c("mat_infantil", "mat_fund_iniciais", "mat_fund_finais",
            "mat_fundamental", "mat_medio")
  
  # rótulos bonitinhos
  rotulos <- c(
    mat_infantil      = "Matrículas - Infantil",
    mat_fund_iniciais = "Matrículas - Fundamental — Iniciais",
    mat_fund_finais   = "Matrículas - Fundamental — Finais",
    mat_fundamental   = "Matrículas - Fundamental (Total)",
    mat_medio         = "Matrículas - Médio"
  )
  
  out <- list()
  for (seg in segs) {
    v1 <- df_wide[[paste0(seg, "_", anos[1])]] %||% 0
    v2 <- df_wide[[paste0(seg, "_", anos[2])]] %||% 0
    v1 <- ifelse(is.na(v1), 0, v1)
    v2 <- ifelse(is.na(v2), 0, v2)
    var <- ifelse(v1 == 0, NA, round((v2 - v1) / v1 * 100, 1))
    
    out[[seg]] <- list(
      label = rotulos[[seg]],
      valor_ano_1 = as.numeric(v1),
      valor_ano_2 = as.numeric(v2),
      taxa_de_variacao = ifelse(is.na(var), "N/A", paste0(var, "%"))
    )
  }
  
  out
}

.matriculas_tabela_detalhada <- function(df){
  # retorna data.frame com Infantil, Fund. Iniciais, Fund. Finais, Médio — colunas 2023/2024/Var_%
  if (nrow(df) == 0) return(data.frame())
  agg <- df %>%
    group_by(ano) %>%
    summarise(
      Infantil = sum(mat_infantil, na.rm = TRUE),
      `Fundamental – Iniciais` = sum(mat_fund_iniciais, na.rm = TRUE),
      `Fundamental – Finais`   = sum(mat_fund_finais,   na.rm = TRUE),
      Médio     = sum(mat_medio, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_longer(-ano, names_to = "segmento", values_to = "valor") %>%
    tidyr::pivot_wider(names_from = ano, values_from = valor) %>%
    mutate(`Var_%` = ifelse(`2023` %in% c(NA,0), NA_real_, round((`2024`-`2023`)/`2023`*100,1)))
  agg
}

# -------------- Reprocessamento (concorrentes selecionados) --------------
reprocessar_dados_concorrentes <- function(codinep, ids_concorrentes, id_municipio){
  anos_matricula <- c(2023, 2024)
  ano_infra <- 2024
  todos_ids <- as.character(c(codinep, ids_concorrentes))
  
  basedosdados::set_billing_id(Sys.getenv("BD_BILLING_ID", unset = "able-study-331220"))
  con <- DBI::dbConnect(bigrquery::bigquery(), project = "basedosdados",
                        billing = Sys.getenv("BD_BILLING_ID", unset = "able-study-331220"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # >>> inclui iniciais/finais explícitos
  q_mat <- glue::glue_sql("
    SELECT
      ano, CAST(id_escola AS STRING) AS id_escola,
      COALESCE(quantidade_matricula_infantil_creche, 0) + COALESCE(quantidade_matricula_infantil_pre_escola, 0) AS mat_infantil,
      COALESCE(quantidade_matricula_fundamental_anos_iniciais, 0) AS mat_fund_iniciais,
      COALESCE(quantidade_matricula_fundamental_anos_finais,   0) AS mat_fund_finais,
      (COALESCE(quantidade_matricula_fundamental_anos_iniciais,0) + 
       COALESCE(quantidade_matricula_fundamental_anos_finais,  0)) AS mat_fundamental,
      COALESCE(quantidade_matricula_medio, 0) AS mat_medio
    FROM `basedosdados.br_inep_censo_escolar.escola`
    WHERE CAST(id_escola AS STRING) IN ({todos_ids*}) AND ano IN ({anos_matricula*})
  ", .con = con)
  
  df_matriculas <- DBI::dbGetQuery(con, q_mat)
  df_infra      <- fetch_infra_data(todos_ids, id_municipio, ano_infra, con)
  
  dados_propria_escola        <- .make_bloco_matriculas(df_matriculas %>% filter(id_escola == codinep), anos_matricula)
  dados_concorrentes_proximos <- .make_bloco_matriculas(df_matriculas %>% filter(id_escola != codinep), anos_matricula)
  
  q_mkt <- glue::glue_sql("
    SELECT ano,
      COALESCE(SUM(quantidade_matricula_infantil_creche), 0) + COALESCE(SUM(quantidade_matricula_infantil_pre_escola), 0) AS mat_infantil,
      COALESCE(SUM(quantidade_matricula_fundamental_anos_iniciais), 0) AS mat_fund_iniciais,
      COALESCE(SUM(quantidade_matricula_fundamental_anos_finais), 0)   AS mat_fund_finais,
      COALESCE(SUM(quantidade_matricula_fundamental_anos_iniciais), 0) + COALESCE(SUM(quantidade_matricula_fundamental_anos_finais), 0) AS mat_fundamental,
      COALESCE(SUM(quantidade_matricula_medio), 0) AS mat_medio
    FROM `basedosdados.br_inep_censo_escolar.escola`
    WHERE CAST(id_municipio AS STRING) = {id_municipio} AND ano IN ({anos_matricula*})
    GROUP BY ano
  ", .con = con)
  df_mkt <- DBI::dbGetQuery(con, q_mkt)
  
  # tabelas detalhadas (iniciais/finais)
  tabela_matriculas_escola   <- .matriculas_tabela_detalhada(df_matriculas %>% filter(id_escola == codinep))
  tabela_matriculas_mercado  <- .matriculas_tabela_detalhada(df_mkt)
  
  dados_enem <- processar_enem_local(codinep, ids_concorrentes, id_municipio)
  
  list(
    dados_propria_escola        = dados_propria_escola,
    dados_concorrentes_proximos = dados_concorrentes_proximos,
    dados_mercado_municipio     = .make_bloco_matriculas(df_mkt, anos_matricula),
    dados_infraestrutura        = df_infra,
    dados_enem_areas            = dados_enem$areas,
    dados_enem_redacao          = dados_enem$redacao,
    matriculas_detalhadas_escola  = tabela_matriculas_escola,
    matriculas_detalhadas_mercado = tabela_matriculas_mercado
  )
}

# -------------- Principal --------------
preprocessar_escola <- function(codinep, lat=NULL, lon=NULL){
  codinep <- as.character(codinep)
  cat("\n🔄 Iniciando processamento da escola:", codinep, "\n")
  
  nomes_df   <- .carregar_nomes_df()
  escolas_sf <- .carregar_escolas_sf()
  muni_lookup<- .carregar_muni_lookup()
  sf_fallback<- .carregar_escolas_sf_fallback()
  
  escola_principal <- escolas_sf %>% dplyr::filter(as.character(id_escola) == codinep) %>% dplyr::slice(1)
  if (nrow(escola_principal)==0) stop("Código INEP não encontrado no SF.")
  
  if (!is.null(lat) && !is.null(lon) && is.finite(lat) && is.finite(lon)) {
    sf::st_geometry(escola_principal) <- sf::st_sfc(sf::st_point(c(lon,lat)), crs = sf::st_crs(escolas_sf))
  }
  
  # atributos básicos
  nm_escola <- pick_nome_escola(escola_principal)
  id_muni <- coalesce_cols(sf::st_drop_geometry(escola_principal), c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU","code_muni"))[1] %>% as.character()
  nome_muni <- coalesce_cols(sf::st_drop_geometry(escola_principal), c("nome_municipio","NM_MUNICIPIO","municipio"))[1]
  uf <- coalesce_cols(sf::st_drop_geometry(escola_principal), c("uf","UF","sigla_uf"))[1]
  
  if (is.na(id_muni) || !nzchar(id_muni)) {
    aux <- muni_lookup %>%
      dplyr::filter(.data[[intersect(c("nome_municipio","NM_MUNICIPIO","municipio"), names(.))[1]]] == nome_muni) %>% dplyr::slice(1)
    id_muni <- as.character(coalesce_cols(aux, c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU")))[1]
  }
  
  # candidatos no município (sem filtrar geometry ainda)
  cand_muni <- escolas_sf %>%
    dplyr::filter(
      as.character(coalesce_cols(sf::st_drop_geometry(.), c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU"))) == id_muni,
      as.character(id_escola) != codinep
    ) %>%
    dplyr::mutate(nome_escola = pick_nome_escola(.)) %>%
    dplyr::select(id_escola, nome_escola, geometry)
  
  # backfill de geometry a partir do fallback quando faltar
  if (!is.null(sf_fallback)) {
    miss <- is.na(sf::st_is_empty(cand_muni)) | sf::st_is_empty(cand_muni)
    if (any(miss, na.rm = TRUE)) {
      geom_fb <- sf_fallback %>% dplyr::select(id_escola, geometry) %>% sf::st_drop_geometry() %>% as.data.frame()
      # pega WKT da geometria fallback e cria sfc (evita conflitos de CRS)
      if (!"geometry" %in% names(geom_fb)) {
        geom_fb <- sf_fallback %>% dplyr::select(id_escola, geometry)
      }
      cand_muni <- cand_muni %>%
        dplyr::left_join(sf_fallback %>% dplyr::select(id_escola, geometry), by="id_escola", suffix = c("", ".fb")) %>%
        dplyr::mutate(geometry = dplyr::coalesce(geometry, geometry.fb)) %>%
        dplyr::select(-dplyr::any_of("geometry.fb"))
    }
  }
  
  # mantém os que têm geometry para calcular distância
  with_geom <- cand_muni %>% dplyr::filter(!sf::st_is_empty(geometry))
  with_geom <- tryCatch(sf::st_transform(with_geom, sf::st_crs(escola_principal)), error = function(e) with_geom)
  
  # rank por distância
  if (nrow(with_geom)) {
    with_geom <- with_geom %>%
      dplyr::mutate(dist_metros = as.numeric(sf::st_distance(sf::st_geometry(.), sf::st_geometry(escola_principal)[1]))) %>%
      dplyr::arrange(dist_metros)
  }
  top_geom <- with_geom %>% dplyr::slice_head(n = 5)
  
  # se ainda não alcançou 5, completa com registros sem geometry (dist = NA)
  if (nrow(top_geom) < 5) {
    falta <- 5 - nrow(top_geom)
    sem_geom <- cand_muni %>%
      dplyr::filter(sf::st_is_empty(geometry)) %>%
      dplyr::mutate(dist_metros = NA_real_)
    if (nrow(sem_geom) > 0) {
      top_geom <- dplyr::bind_rows(top_geom, sem_geom %>% dplyr::slice_head(n=falta))
    }
  }
  
  concorrentes_finais <- top_geom %>% dplyr::select(id_escola, nome_escola, dist_metros, geometry)
  
  ids_concorrentes <- as.character(concorrentes_finais$id_escola)
  
  # Território
  setores_gpkg <- .get_setores_gpkg_uf(uf %||% "")
  dados_territorio <- .analisar_territorio(
    escolas_sf %>% dplyr::filter(id_escola %in% c(codinep, ids_concorrentes)) %>% dplyr::select(id_escola, geometry),
    setores_gpkg
  )
  dados_territorio_escola <- if (!is.null(dados_territorio)) dados_territorio %>% dplyr::filter(id_escola == codinep) else NULL
  dados_territorio_conc_media <- if (!is.null(dados_territorio)) dados_territorio %>% dplyr::filter(id_escola %in% ids_concorrentes) %>% dplyr::summarise(across(where(is.numeric), ~mean(.x,na.rm=TRUE))) else NULL
  
  # Blocos/ENEM/Infra
  basedosdados::set_billing_id(Sys.getenv("BD_BILLING_ID", unset = "able-study-331220"))
  out_proc <- reprocessar_dados_concorrentes(codinep, ids_concorrentes, id_muni)
  
  # lat/lon da escola
  escola_wgs <- tryCatch(sf::st_transform(escola_principal, 4326), error=function(e) escola_principal)
  ll <- tryCatch(sf::st_coordinates(escola_wgs), error=function(e) matrix(c(NA,NA), nrow=1))
  latitude  <- as.numeric(ll[,"Y"])[1]; longitude <- as.numeric(ll[,"X"])[1]
  
  resultado <- list(
    id_escola      = codinep,
    nome_escola    = (nm_escola %||% "")[1],
    id_municipio   = as.character(id_muni),
    nome_municipio = nome_muni %||% "",
    latitude       = latitude,
    longitude      = longitude,
    lista_concorrentes = concorrentes_finais,
    dados_propria_escola        = out_proc$dados_propria_escola,
    dados_concorrentes_proximos = out_proc$dados_concorrentes_proximos,
    dados_mercado_municipio     = out_proc$dados_mercado_municipio,
    dados_infraestrutura        = out_proc$dados_infraestrutura,
    dados_enem_areas            = out_proc$dados_enem_areas,
    dados_enem_redacao          = out_proc$dados_enem_redacao,
    matriculas_detalhadas_escola  = out_proc$matriculas_detalhadas_escola,
    matriculas_detalhadas_mercado = out_proc$matriculas_detalhadas_mercado,
    dados_territorio_escola             = dados_territorio_escola,
    dados_territorio_concorrentes_media = dados_territorio_conc_media,
    data_extracao = Sys.time()
  )
  
  dir.create(path_data("escolas"), recursive = TRUE, showWarnings = FALSE)
  out <- path_data("escolas", paste0(codinep, ".rds"))
  saveRDS(resultado, out)
  cat("✅ Dados processados e salvos em: ", out, "\n")
  TRUE
}

