## =========================================================
#   PREPROCESS UTILS — VERSÃO FINAL E DEFINITIVA
# =========================================================

suppressPackageStartupMessages({
  library(dplyr); library(sf); library(stringr); library(readr)
  library(DBI); library(arrow); library(glue); library(tidyr)
})

# ... (Funções helper como path_data, .carregar_nomes_df, etc., inalteradas)
path_data <- function(...) file.path(Sys.getenv("DATA_DIR", unset = "data"), ...)
`%||%` <- function(a,b) if (!is.null(a)) a else b
coalesce_cols <- function(df, cols){
  present <- intersect(cols, names(df)); if(!length(present)) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]; for(nm in present[-1]) out <- dplyr::coalesce(out, df[[nm]]); out
}
pick_nome_escola <- function(df) as.character(coalesce_cols(df, c("nome_escola","NO_ENTIDADE","NM_ENTIDADE","NO_NOME_ENTIDADE","id_escola")))
.carregar_nomes_df <- function(){ readRDS(path_data("escolas_privadas_nomelista.rds")) }
.carregar_escolas_sf <- function(){ readRDS(path_data("processed/escolas_enriquecidas.rds")) }
.carregar_muni_lookup <- function(){ readRDS(path_data("municipios_lookup.rds")) }
# ... (todas as outras funções auxiliares que já tínhamos)
.make_bloco_matriculas <- function(df, anos) {
  if (nrow(df) == 0) return(list())
  df_wide <- df %>%
    group_by(ano) %>%
    summarise(across(starts_with("mat_"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = ano, values_from = starts_with("mat_"))
  segs <- c("mat_infantil", "mat_fund_iniciais", "mat_fund_finais", "mat_fundamental", "mat_medio")
  rotulos <- c(mat_infantil = "Matrículas - Infantil", mat_fund_iniciais = "Matrículas - Fundamental — Iniciais", mat_fund_finais   = "Matrículas - Fundamental — Finais", mat_fundamental   = "Matrículas - Fundamental (Total)", mat_medio = "Matrículas - Médio")
  out <- list()
  for (seg in segs) {
    v1 <- df_wide[[paste0(seg, "_", anos[1])]] %||% 0; v2 <- df_wide[[paste0(seg, "_", anos[2])]] %||% 0
    v1 <- ifelse(is.na(v1), 0, v1); v2 <- ifelse(is.na(v2), 0, v2)
    var <- ifelse(v1 == 0, NA, round((v2 - v1) / v1 * 100, 1))
    out[[seg]] <- list(label = rotulos[[seg]], valor_ano_1 = as.numeric(v1), valor_ano_2 = as.numeric(v2), taxa_de_variacao = ifelse(is.na(var), "N/A", paste0(var, "%")))
  }
  out
}
.matriculas_tabela_detalhada <- function(df){
  if (nrow(df) == 0) return(data.frame())
  df %>%
    group_by(ano) %>%
    summarise(Infantil = sum(mat_infantil, na.rm = TRUE), `Fundamental – Iniciais` = sum(mat_fund_iniciais, na.rm = TRUE), `Fundamental – Finais`   = sum(mat_fund_finais,   na.rm = TRUE), Médio     = sum(mat_medio, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_longer(-ano, names_to = "segmento", values_to = "valor") %>%
    tidyr::pivot_wider(names_from = ano, values_from = valor) %>%
    mutate(`Var_%` = ifelse(`2023` %in% c(NA,0), NA_real_, round((`2024`-`2023`)/`2023`*100,1)))
}
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
      tipo = case_when(id_escola == codinep ~ "Sua Escola", id_escola %in% as.character(ids_concorrentes) ~ "Concorrente", id_escola == "Média Concorrentes" ~ "Média Concorrentes", id_escola == "Média Municipal" ~ "Média Municipal"),
      escola_label = case_when(tipo == "Sua Escola" ~ "Sua Escola", tipo == "Média Concorrentes" ~ paste0("Média Concorrentes (", n_found, " de ", length(ids_concorrentes), ")"), tipo == "Média Municipal" ~ "Média Municipal", TRUE ~ (nome_escola %||% id_escola))
    )
  df_areas <- dados_full %>%
    select(id_escola, tipo, escola_label, `Ciências da Natureza` = NU_NOTA_CN, `Ciências Humanas` = NU_NOTA_CH, `Linguagens e Códigos` = NU_NOTA_LC, `Matemática` = NU_NOTA_MT, `Redação` = NU_NOTA_REDACAO) %>%
    pivot_longer(cols = -c(id_escola, tipo, escola_label), names_to = "area", values_to = "nota")
  df_red <- dados_full %>%
    select(id_escola, tipo, escola_label, `Competência 1` = NU_NOTA_COMP1, `Competência 2` = NU_NOTA_COMP2, `Competência 3` = NU_NOTA_COMP3, `Competência 4` = NU_NOTA_COMP4, `Competência 5` = NU_NOTA_COMP5) %>%
    pivot_longer(cols = -c(id_escola, tipo, escola_label), names_to = "competencia", values_to = "nota")
  list(areas = df_areas, redacao = df_red)
}

buscar_dados_cache <- function(codinep, ids_concorrentes, id_municipio){
  censo_local <- arrow::read_parquet("data/processed/censo_local_sp_rj_mg.parquet")
  anos_matricula <- c(2023, 2024)
  ano_infra <- 2024
  todos_ids <- as.character(c(codinep, ids_concorrentes))
  df_matriculas_raw <- censo_local %>% filter(ano %in% anos_matricula, id_escola %in% todos_ids)
  df_matriculas <- df_matriculas_raw %>%
    transmute(ano, id_escola, mat_infantil = mat_inf_creche + mat_inf_pre, mat_fund_iniciais, mat_fund_finais, mat_fundamental = mat_fund_iniciais + mat_fund_finais, mat_medio)
  df_infra_raw <- censo_local %>% filter(ano == ano_infra)
  rename_map <- c(essencial_agua_potavel = "infra_agua_potavel", essencial_biblioteca = "infra_biblioteca", essencial_lab_informatica = "infra_lab_info", essencial_lab_ciencias = "infra_lab_ciencias", essencial_quadra_esportes = "infra_quadra", essencial_acessibilidade_pne = "infra_pne", essencial_refeitorio = "infra_refeitorio", lazer_area_verde = "infra_area_verde", lazer_parque_infantil = "infra_parque_infantil", lazer_quadra_coberta = "infra_quadra_coberta", tec_total_dispositivos_aluno = "tec_dispositivos_alunos")
  final_cols <- c("id_escola", "essencial_agua_potavel", "essencial_biblioteca", "essencial_lab_informatica", "essencial_lab_ciencias", "essencial_quadra_esportes", "essencial_acessibilidade_pne", "essencial_refeitorio", "lazer_area_verde", "lazer_parque_infantil", "lazer_quadra_coberta", "tec_internet_geral", "tec_banda_larga", "tec_internet_para_alunos", "tec_lousa_digital", "tec_total_dispositivos_aluno", "apoio_psicologo", "apoio_bibliotecario")
  df_infra_escolas <- df_infra_raw %>% filter(id_escola %in% todos_ids) %>% rename(any_of(rename_map)) %>% select(any_of(final_cols))
  media_municipio_infra <- df_infra_raw %>%
    filter(id_municipio == !!id_municipio) %>%
    summarise(across(starts_with("infra_") | starts_with("tec_") | starts_with("apoio_"), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(id_escola = "Média Municipal") %>%
    rename(any_of(rename_map)) %>%
    select(any_of(final_cols))
  df_infra <- bind_rows(df_infra_escolas, media_municipio_infra)
  dados_propria_escola <- .make_bloco_matriculas(df_matriculas %>% filter(id_escola == codinep), anos_matricula)
  dados_concorrentes_proximos <- .make_bloco_matriculas(df_matriculas %>% filter(id_escola != codinep), anos_matricula)
  dados_concorrentes_individualizados <- ids_concorrentes %>%
    lapply(function(id_conc) { .make_bloco_matriculas(df_matriculas %>% filter(id_escola == id_conc), anos_matricula) }) %>%
    setNames(ids_concorrentes)
  df_mkt <- censo_local %>%
    filter(ano %in% anos_matricula, id_municipio == !!id_municipio) %>%
    group_by(ano) %>%
    summarise(mat_infantil = sum(mat_inf_creche + mat_inf_pre, na.rm = TRUE), mat_fund_iniciais = sum(mat_fund_iniciais, na.rm = TRUE), mat_fund_finais = sum(mat_fund_finais, na.rm = TRUE), mat_fundamental = sum(mat_fund_iniciais + mat_fund_finais, na.rm = TRUE), mat_medio = sum(mat_medio, na.rm = TRUE))
  dados_mercado_municipio <- .make_bloco_matriculas(df_mkt, anos_matricula)
  tabela_matriculas_escola   <- .matriculas_tabela_detalhada(df_matriculas %>% filter(id_escola == codinep))
  tabela_matriculas_mercado  <- .matriculas_tabela_detalhada(df_mkt)
  dados_enem <- processar_enem_local(codinep, ids_concorrentes, id_municipio)
  
  list(
    dados_propria_escola = dados_propria_escola,
    dados_concorrentes_proximos = dados_concorrentes_proximos,
    dados_mercado_municipio = dados_mercado_municipio,
    dados_infraestrutura = df_infra,
    dados_enem_areas = dados_enem$areas,
    dados_enem_redacao = dados_enem$redacao,
    matriculas_detalhadas_escola = tabela_matriculas_escola,
    matriculas_detalhadas_mercado = tabela_matriculas_mercado,
    dados_concorrentes_individualizados = dados_concorrentes_individualizados
  )
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#      FUNÇÃO MESTRA 'preprocessar_escola' (cria e RECRIA o .rds)
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
preprocessar_escola <- function(codinep, lat=NULL, lon=NULL, concorrentes_custom = NULL){
  codinep <- as.character(codinep)
  cat("\n🔄 Iniciando/Atualizando processamento para a escola:", codinep, "\n")
  
  escolas_sf <- .carregar_escolas_sf() %>% rename(nome_escola = NO_ENTIDADE)
  muni_lookup<- .carregar_muni_lookup()
  
  escola_principal <- escolas_sf %>% filter(as.character(id_escola) == codinep) %>% slice(1)
  if (nrow(escola_principal)==0) stop("Código INEP não encontrado no SF.")
  
  if (!is.null(lat) && !is.null(lon) && is.finite(lat) && is.finite(lon)) {
    sf::st_geometry(escola_principal) <- sf::st_sfc(sf::st_point(c(lon,lat)), crs = sf::st_crs(escolas_sf))
  }
  
  nm_escola <- pick_nome_escola(escola_principal)
  id_muni <- coalesce_cols(sf::st_drop_geometry(escola_principal), c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU","code_muni"))[1] %>% as.character()
  nome_muni <- coalesce_cols(sf::st_drop_geometry(escola_principal), c("nome_municipio","NM_MUNICIPIO","municipio"))[1]
  
  if (is.null(concorrentes_custom)) {
    # Lógica padrão de cadastro: encontra os 5 mais próximos
    cat("  > Modo: Cadastro. Encontrando os 5 concorrentes mais próximos.\n")
    cand_muni <- escolas_sf %>%
      filter(
        as.character(coalesce_cols(sf::st_drop_geometry(.), c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU"))) == id_muni,
        as.character(id_escola) != codinep
      )
    
    with_geom <- cand_muni %>% filter(!sf::st_is_empty(geometry))
    with_geom <- tryCatch(sf::st_transform(with_geom, sf::st_crs(escola_principal)), error = function(e) with_geom)
    
    if (nrow(with_geom)) {
      with_geom <- with_geom %>%
        mutate(dist_metros = as.numeric(sf::st_distance(sf::st_geometry(.), sf::st_geometry(escola_principal)[1]))) %>%
        arrange(dist_metros)
    }
    concorrentes_finais <- with_geom %>% slice_head(n = 5)
    ids_concorrentes <- as.character(concorrentes_finais$id_escola)
    
  } else {
    # Lógica de atualização: usa os concorrentes fornecidos
    cat("  > Modo: Atualização. Usando a lista de concorrentes fornecida:", paste(concorrentes_custom, collapse=", "), "\n")
    ids_concorrentes <- concorrentes_custom
    concorrentes_finais <- escolas_sf %>% filter(id_escola %in% ids_concorrentes)
  }
  
  out_proc <- buscar_dados_cache(codinep, ids_concorrentes, id_muni)
  
  escola_wgs <- tryCatch(sf::st_transform(escola_principal, 4326), error=function(e) escola_principal)
  ll <- tryCatch(sf::st_coordinates(escola_wgs), error=function(e) matrix(c(NA,NA), nrow=1))
  latitude  <- as.numeric(ll[,"Y"])[1]; longitude <- as.numeric(ll[,"X"])[1]
  
  resultado <- list(
    id_escola = codinep, nome_escola = nm_escola, id_municipio = id_muni,
    nome_municipio = nome_muni, latitude = latitude, longitude = longitude,
    lista_concorrentes = concorrentes_finais,
    # Dados processados
    dados_propria_escola = out_proc$dados_propria_escola,
    dados_concorrentes_proximos = out_proc$dados_concorrentes_proximos,
    dados_mercado_municipio = out_proc$dados_mercado_municipio,
    dados_infraestrutura = out_proc$dados_infraestrutura,
    dados_enem_areas = out_proc$dados_enem_areas,
    dados_enem_redacao = out_proc$dados_enem_redacao,
    matriculas_detalhadas_escola = out_proc$matriculas_detalhadas_escola,
    matriculas_detalhadas_mercado = out_proc$matriculas_detalhadas_mercado,
    dados_concorrentes_individualizados = out_proc$dados_concorrentes_individualizados
  )
  
  dir.create(path_data("escolas"), recursive = TRUE, showWarnings = FALSE)
  out_path <- path_data("escolas", paste0(codinep, ".rds"))
  saveRDS(resultado, out_path)
  cat("✅ Dados para", codinep, "processados e salvos em:", out_path, "\n")
  return(resultado)
}