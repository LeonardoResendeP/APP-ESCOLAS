# scripts/diagnostico_mapa.R
suppressPackageStartupMessages({
  library(sf); library(dplyr); library(readr); library(glue); library(lubridate)
  library(htmlwidgets); library(leaflet); library(purrr); library(stringr)
})

diag_mapa <- function(codinep = NULL) {
  dir.create("logs", showWarnings = FALSE, recursive = TRUE)
  stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  log_path <- file.path("logs", glue("diag_mapa_{stamp}.txt"))
  con_log  <- file(log_path, open = "wt"); sink(con_log); on.exit({ sink(); close(con_log) }, add = TRUE)
  cat("========== DIAGNÓSTICO MAPA ==========\n")
  cat("Data/Hora:", as.character(Sys.time()), "\n\n")
  
  # 0) Escolher .rds da escola
  rds_files <- list.files("data/escolas", pattern = "\\.rds$", full.names = TRUE)
  stopifnot(length(rds_files) > 0)
  
  if (is.null(codinep)) {
    info <- file.info(rds_files)
    rds_target <- rownames(info)[which.max(info$mtime)]
    cat("Sem codinep informado: usando o .rds mais recente:\n  ", rds_target, "\n\n")
  } else {
    rds_target <- file.path("data/escolas", paste0(as.character(codinep), ".rds"))
    if (!file.exists(rds_target)) {
      cat("⚠️ .rds da escola não encontrado em:", rds_target, "\n")
      cat("Escolhendo o mais recente por fallback.\n")
      info <- file.info(rds_files)
      rds_target <- rownames(info)[which.max(info$mtime)]
    } else {
      cat("Usando .rds informado:\n  ", rds_target, "\n\n")
    }
  }
  
  dados <- readRDS(rds_target)
  req_names <- c("id_escola","nome_escola","id_municipio","nome_municipio","latitude","longitude","lista_concorrentes")
  faltam <- setdiff(req_names, names(dados))
  if (length(faltam)) {
    cat("❌ Campos faltando no .rds:", paste(faltam, collapse=", "), "\n")
  } else {
    cat("✔ Campos essenciais presentes no .rds\n")
  }
  
  # 1) Validar lat/lon
  lat <- suppressWarnings(as.numeric(dados$latitude))
  lon <- suppressWarnings(as.numeric(dados$longitude))
  cat("\n--- Coordenadas escola principal ---\n")
  cat("lat:", lat, " | lon:", lon, "\n")
  cat("lat ok? ", is.finite(lat) && lat >= -90 && lat <= 90, "\n")
  cat("lon ok? ", is.finite(lon) && lon >= -180 && lon <= 180, "\n")
  
  # 2) Concorrentes -> garantir sf
  conc <- dados$lista_concorrentes
  cat("\n--- lista_concorrentes ---\n")
  cat("classe:", paste(class(conc), collapse=", "), " | nrow:", ifelse(is.null(conc), 0, nrow(conc)), "\n")
  
  if (is.null(conc) || nrow(conc) == 0) {
    cat("⚠️ Sem concorrentes no .rds. O mapa deverá mostrar apenas a escola.\n")
    conc_sf <- NULL
  } else {
    if (!inherits(conc, "sf")) {
      cat("ℹ️ Convertendo concorrentes para sf via unificadas...\n")
      uni_sf <- readRDS("data/processed/escolas_privadas_unificadas_sf.rds") %>%
        mutate(id_escola = as.character(id_escola)) %>% select(id_escola, geometry)
      conc_sf <- conc %>%
        mutate(id_escola = as.character(id_escola)) %>%
        left_join(uni_sf, by = "id_escola") %>%
        sf::st_as_sf()
    } else {
      conc_sf <- conc
    }
    # limpar
    if (!is.na(sf::st_crs(conc_sf))) conc_sf <- suppressWarnings(st_transform(conc_sf, 4326))
    conc_sf <- conc_sf[!sf::st_is_empty(conc_sf), , drop = FALSE]
    if ("id_escola" %in% names(conc_sf)) conc_sf <- distinct(conc_sf, id_escola, .keep_all = TRUE)
    cat("conc_sf nrow após limpeza:", nrow(conc_sf), "\n")
    n_na_geom <- sum(is.na(sf::st_is_empty(conc_sf)))
    cat("st_is_empty NA count:", n_na_geom, "\n")
  }
  
  # 3) Escola principal sf
  escola_sf <- st_as_sf(
    tibble::tibble(lon = lon, lat = lat, nome = dados$nome_escola),
    coords = c("lon","lat"), crs = 4326
  )
  
  # 4) BBOX
  cat("\n--- BBOX ---\n")
  sfc_list <- list(st_geometry(escola_sf))
  if (!is.null(conc_sf) && nrow(conc_sf) > 0) sfc_list <- c(sfc_list, list(st_geometry(conc_sf)))
  sfc_all <- tryCatch(st_sfc(unlist(sfc_list, recursive = FALSE), crs = 4326), error = function(e) NULL)
  
  if (is.null(sfc_all) || length(sfc_all) == 0) {
    cat("❌ Não foi possível montar sfc_all para bbox.\n")
  } else {
    bb <- st_bbox(sfc_all)
    cat("bbox:", paste(names(bb), as.numeric(bb), collapse=" | "), "\n")
    cat("bbox finito? ", all(is.finite(as.numeric(bb))), "\n")
  }
  
  # 5) Teste de tiles (internet/proxy)
  cat("\n--- Teste de tiles ---\n")
  ok_tiles <- FALSE
  try({
    url <- "https://a.tile.openstreetmap.org/0/0/0.png"
    # usar curl simples para não depender de httr
    con <- url(url, "rb"); suppressWarnings(readBin(con, what="raw", n=50)); close(con)
    ok_tiles <- TRUE
  }, silent = TRUE)
  cat("Consegue baixar tile OSM? ", ok_tiles, "\n")
  if (!ok_tiles) cat("⚠️ Se false → rede bloqueando tiles → mapa cinza mesmo com dados OK.\n")
  
  # 6) Preview fora do Shiny
  cat("\n--- Preview htmlwidgets ---\n")
  map_out <- file.path("logs", glue("diag_mapa_preview_{stamp}.html"))
  m <- leaflet(options = leafletOptions(minZoom = 3)) %>% addTiles()
  m <- m %>% addAwesomeMarkers(
    data = escola_sf,
    icon = awesomeIcons(icon = 'school', library = 'fa', markerColor = "blue", iconColor = '#FFFFFF'),
    popup = ~htmltools::htmlEscape(nome)
  )
  if (!is.null(conc_sf) && nrow(conc_sf) > 0) {
    # garantir coluna para popup
    if (!"nome_escola" %in% names(conc_sf)) conc_sf$nome_escola <- conc_sf$id_escola
    m <- m %>% addAwesomeMarkers(
      data = conc_sf,
      icon = awesomeIcons(icon = 'school', library = 'fa', markerColor = "red", iconColor = '#FFFFFF'),
      popup = ~htmltools::htmlEscape(as.character(nome_escola))
    )
  }
  if (!is.null(sfc_all) && length(sfc_all) > 0) {
    bb <- st_bbox(sfc_all)
    if (all(is.finite(as.numeric(bb)))) {
      m <- m %>% fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
    } else {
      coords <- st_coordinates(escola_sf)
      m <- m %>% setView(lng = coords[, "X"], lat = coords[, "Y"], zoom = 14)
    }
  }
  saveWidget(m, map_out, selfcontained = TRUE)
  cat("Preview salvo em:", map_out, "\n")
  
  # 7) Checagem rápida de CSS local (opcional)
  css_path <- "www/styles.css"
  if (file.exists(css_path)) {
    css_txt <- readLines(css_path, warn = FALSE)
    has_leaflet_css <- any(str_detect(css_txt, "(?i)leaflet"))
    cat("\n--- CSS ---\n")
    cat("styles.css tem regras 'leaflet'? ", has_leaflet_css, "\n")
  } else {
    cat("\n--- CSS ---\n")
    cat("styles.css não encontrado (ok, não é obrigatório)\n")
  }
  
  cat("\n========== FIM DIAGNÓSTICO ==========\n")
  invisible(list(
    log = log_path,
    preview_html = map_out
  ))
}
