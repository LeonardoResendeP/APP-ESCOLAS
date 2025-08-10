# scripts/diagnostico_app_escolas.R
suppressPackageStartupMessages({
  library(fs); library(glue); library(dplyr); library(readr); library(purrr); library(stringr)
  library(sf); library(DBI); library(RSQLite); library(tidyr)
})

run_diagnostico <- function(bq_test = FALSE) {
  dir_create("logs")
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  log_path <- glue("logs/diagnostico_{stamp}.txt")
  cat(glue("📄 Salvando diagnóstico em {log_path}\n"))
  sink(log_path, split = TRUE); on.exit(sink(), add = TRUE)
  
  cat("========== DIAGNÓSTICO APP ESCOLAS ==========\n")
  cat("Data/Hora: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n")
  cat("\n--- Session/Versões ---\n")
  print(R.version.string)
  pkgs <- c("sf","dplyr","DBI","RSQLite","readr","stringr","tidyr")
  v <- sapply(pkgs, function(p) as.character(utils::packageVersion(p)))
  print(v)
  
  # ------------------ Helpers ------------------
  ok <- local({
    issues <- c()
    
    add_issue <- function(msg) {
      cat("!! ", msg, "\n")
      assign("issues", c(issues, msg), inherits = TRUE)
    }
    
    check_exists <- function(path) {
      e <- file_exists(path)
      cat(if (e) "✔ " else "✖ ", path, if (e) glue(" ({file_size(path)} / {as.character(file_info(path)$modification_time)})"), "\n", sep = "")
      e
    }
    
    safe_readRDS <- function(path) {
      tryCatch(readRDS(path), error = function(e) { add_issue(glue("Falha ao ler RDS: {path} -> {e$message}")); NULL })
    }
    
    safe_st_read <- function(path) {
      tryCatch(suppressWarnings(sf::st_read(path, quiet = TRUE)), error = function(e) { add_issue(glue("Falha ao ler SF: {path} -> {e$message}")); NULL })
    }
    
    check_cols <- function(df, needed, name = deparse(substitute(df))) {
      miss <- setdiff(needed, names(df))
      if (length(miss)) add_issue(glue("Colunas faltando em {name}: {paste(miss, collapse=', ')}"))
      length(miss) == 0
    }
    
    # ------------------ Estrutura de pastas ------------------
    cat("\n--- Estrutura de pastas (nível 2) ---\n")
    print(dir_tree(".", recurse = 2, file_info = FALSE))
    
    # ------------------ Arquivos esperados ------------------
    cat("\n--- Arquivos esperados ---\n")
    expected <- c(
      "data/escolas_privadas_nomelista.rds",
      "data/processed/escolas_privadas_unificadas_sf.rds",
      "data/municipios_lookup.rds",
      "data/processed/censo2022_universo_setor_joined_clean.gpkg",
      "data/processed/censo2022_universo_municipio.rds",
      "data/enem_consolidados.RData"
    )
    exists_vec <- vapply(expected, check_exists, logical(1))
    
    # ------------------ NOMES: escolas_privadas_nomelista.rds ------------------
    if (exists_vec[1]) {
      cat("\n[1] nomes_df\n")
      nomes_df <- safe_readRDS(expected[1])
      if (!is.null(nomes_df)) {
        cat("nrow: ", nrow(nomes_df), " | cols: ", paste(head(names(nomes_df), 20), collapse=", "), "\n")
        # Colunas usuais
        cols_ok <- check_cols(nomes_df, c("CO_ENTIDADE"))
        if (!("CO_MUNICIPIO" %in% names(nomes_df)) && !("NO_MUNICIPIO" %in% names(nomes_df))) {
          add_issue("nomes_df sem CO_MUNICIPIO/NO_MUNICIPIO — labels do select e filtro por município podem falhar.")
        }
        # coerção
        if ("CO_ENTIDADE" %in% names(nomes_df) && !is.character(nomes_df$CO_ENTIDADE)) {
          add_issue("CO_ENTIDADE não é character — padronize como string (com zero-padding se necessário).")
        }
      }
    }
    
    # ------------------ GEO: escolas_privadas_unificadas_sf.rds ------------------
    if (exists_vec[2]) {
      cat("\n[2] geo_sf (unificado)\n")
      geo_sf <- safe_readRDS(expected[2])
      if (!is.null(geo_sf)) {
        print(class(geo_sf)); print(sf::st_geometry_type(geo_sf, by_geometry = FALSE))
        cat("CRS: "); print(sf::st_crs(geo_sf))
        check_cols(geo_sf, c("id_escola","geometry"), "geo_sf")
        # métricas
        cat("nrow:", nrow(geo_sf), " | NAs geometry:", sum(sf::st_is_empty(geo_sf)), "\n")
        # tipo
        if (any(sf::st_geometry_type(geo_sf) != "POINT")) add_issue("geo_sf não é POINT — confira criação (st_as_sf coords=...).")
      }
    }
    
    # ------------------ MUNIS lookup ------------------
    if (exists_vec[3]) {
      cat("\n[3] municipios_lookup.rds\n")
      muni <- safe_readRDS(expected[3])
      if (!is.null(muni)) {
        cat("nrow:", nrow(muni), " | cols:", paste(names(muni), collapse=", "), "\n")
        if (!check_cols(muni, c("id_municipio","nome_municipio"), "municipios_lookup")) {
          add_issue("municipios_lookup sem id_municipio/nome_municipio.")
        } else {
          bad_len <- sum(nchar(as.character(muni$id_municipio)) != 7, na.rm = TRUE)
          if (bad_len > 0) add_issue(glue("municipios_lookup: {bad_len} ids não têm 7 dígitos."))
        }
      }
    }
    
    # ------------------ GPKG (setores unidos) ------------------
    if (exists_vec[4]) {
      cat("\n[4] censo2022_universo_setor_joined_clean.gpkg\n")
      gpkg <- safe_st_read(expected[4])
      if (!is.null(gpkg)) {
        cat("nrow:", nrow(gpkg), " | geom:", as.character(sf::st_geometry_type(gpkg, by_geometry = FALSE)), "\n")
        print(sf::st_crs(gpkg))
        need_num <- c("tam_total_0_17","pop_0_4","pop_5_9","pop_10_14","pop_15_17","alfabet_15mais","dpp_total")
        check_cols(gpkg, c("cd_setor","cod_municipio_ibge", need_num), "gpkg")
        na_rate <- sapply(gpkg[, intersect(names(gpkg), need_num), drop=FALSE], function(x) mean(is.na(x)))
        print(round(na_rate, 4))
      }
    }
    
    # ------------------ MUNICÍPIO .rds ------------------
    if (exists_vec[5]) {
      cat("\n[5] censo2022_universo_municipio.rds\n")
      muni_rds <- safe_readRDS(expected[5])
      if (!is.null(muni_rds)) {
        cat("nrow:", nrow(muni_rds), " | cols:", paste(names(muni_rds), collapse=", "), "\n")
        if (!("cod_municipio_ibge" %in% names(muni_rds))) add_issue("muni.rds sem cod_municipio_ibge.")
      }
    }
    
    # ------------------ ENEM .RData ------------------
    if (exists_vec[6]) {
      cat("\n[6] ENEM data/enem_consolidados.RData\n")
      e <- new.env(parent = emptyenv())
      tryCatch({
        load(expected[6], envir = e)
        cat("Objetos carregados:", paste(ls(e), collapse=", "), "\n")
        # checar typo frequente
        if ("enem_consoliado_municipio" %in% ls(e) && !("enem_consolidado_municipio" %in% ls(e))) {
          add_issue("Possível typo: objeto 'enem_consoliado_municipio' (sem 'd'). Funções esperam 'enem_consolidado_municipio'?")
        }
        if (!("enem_consolidado_escola" %in% ls(e))) {
          add_issue("Falta 'enem_consolidado_escola' no .RData (usado em processar_enem_local).")
        }
      }, error = function(err) add_issue(glue("Falha ao carregar ENEM .RData: {err$message}")))
    }
    
    # ------------------ Banco SQLite ------------------
    cat("\n--- Banco de dados (config/database.sqlite) ---\n")
    db_path <- "config/database.sqlite"
    if (file_exists(db_path)) {
      con <- dbConnect(RSQLite::SQLite(), db_path); on.exit(dbDisconnect(con), add = TRUE)
      tabs <- dbListTables(con); cat("Tabelas:", paste(tabs, collapse=", "), "\n")
      # users
      if ("users" %in% tabs) {
        u <- dbReadTable(con, "users")
        cat("users: ", nrow(u), " linhas | cols: ", paste(names(u), collapse=", "), "\n", sep="")
        need_users <- c("user","pass","role","codinep")
        if (!all(need_users %in% names(u))) add_issue("Tabela 'users' com colunas faltantes.")
      } else add_issue("Tabela 'users' ausente.")
      # escolas
      if ("escolas" %in% tabs) {
        e <- dbReadTable(con, "escolas")
        cat("escolas: ", nrow(e), " linhas | cols: ", paste(names(e), collapse=", "), "\n", sep="")
        need_escolas <- c("codinep","nome","user","pass")
        if (!all(need_escolas %in% names(e))) add_issue("Tabela 'escolas' com colunas faltantes.")
      } else add_issue("Tabela 'escolas' ausente.")
      # escola_concorrentes
      if ("escola_concorrentes" %in% tabs) {
        ec <- dbReadTable(con, "escola_concorrentes")
        cat("escola_concorrentes: ", nrow(ec), " linhas | cols: ", paste(names(ec), collapse=", "), "\n", sep="")
      } else add_issue("Tabela 'escola_concorrentes' ausente (será criada por db_init?).")
    } else {
      add_issue("Banco de dados não encontrado em config/database.sqlite (rode db_init no app).")
    }
    
    # ------------------ RDS por escola ------------------
    cat("\n--- Amostragem de data/escolas/*.rds ---\n")
    es_rds <- dir_ls("data/escolas", glob = "*.rds", fail = FALSE)
    print(es_rds)
    if (length(es_rds)) {
      sample_file <- es_rds[1]
      cat("Lendo amostra: ", sample_file, "\n")
      S <- safe_readRDS(sample_file)
      if (!is.null(S)) {
        need_top <- c("id_escola","nome_escola","id_municipio","nome_municipio","latitude","longitude",
                      "lista_concorrentes","dados_propria_escola","dados_concorrentes_proximos","dados_mercado_municipio")
        miss <- setdiff(need_top, names(S))
        if (length(miss)) add_issue(glue("Estrutura do .rds de escola sem campos: {paste(miss,collapse=', ')}"))
        # lista_concorrentes sf?
        if ("lista_concorrentes" %in% names(S)) {
          lc <- S$lista_concorrentes
          cat("lista_concorrentes class: ", paste(class(lc), collapse=", "), "\n")
          if (!inherits(lc, "sf")) add_issue("lista_concorrentes não é 'sf' — o mapa cairá. Ajustar preprocessar_escola para manter geometry.")
        }
      }
    } else {
      cat("Nenhum RDS de escola encontrado (cadastrar uma escola pelo admin vai gerar um). \n")
    }
    
    # ------------------ modules/chat e openai_utils ------------------
    cat("\n--- Chat/IA ---\n")
    chat_path <- "modules/mod_chat.R"
    if (file_exists(chat_path)) {
      cat("OK: modules/mod_chat.R presente.\n")
    } else {
      add_issue("modules/mod_chat.R ausente (o app referencia esse módulo).")
    }
    
    openai_path <- "utils/openai_utils.R"
    if (file_exists(openai_path)) {
      cat("OK: utils/openai_utils.R presente.\n")
      # tentar checar se função existe sem executar
      # (só parse)
      src <- tryCatch(read_file(openai_path), error = function(e) "")
      if (!grepl("get_openai_analysis\\s*<-\\s*function", src)) {
        add_issue("utils/openai_utils.R não define get_openai_analysis(). O chat mostrará erro.")
      }
    } else {
      add_issue("utils/openai_utils.R ausente — o módulo de chat chamará uma função inexistente.")
    }
    
    # ------------------ BigQuery (opcional) ------------------
    if (isTRUE(bq_test)) {
      cat("\n--- Teste BigQuery (opcional) ---\n")
      ok_bq <- FALSE
      try({
        library(bigrquery); library(basedosdados)
        # Tenta só listar datasets do projeto 'basedosdados' (sem cobrar)
        ds <- bigrquery::list_datasets("basedosdados")
        cat("Datasets BQ (basedosdados): ", nrow(ds), "\n")
        ok_bq <- TRUE
      }, silent = TRUE)
      if (!ok_bq) add_issue("Falha no teste BigQuery — verifique credenciais/billing.")
    }
    
    # ------------------ Resumo ------------------
    cat("\n========== RESUMO ==========\n")
    if (length(issues)) {
      cat("⚠️ Encontramos ", length(issues), " ponto(s) de atenção:\n", sep="")
      for (i in seq_along(issues)) cat(sprintf("%2d) %s\n", i, issues[[i]]))
    } else {
      cat("✅ Nenhum problema crítico detectado nos artefatos verificados.\n")
    }
    
    invisible(length(issues) == 0)
  })
  
  cat("\nFim. Log salvo em: ", log_path, "\n")
  invisible(ok)
}
