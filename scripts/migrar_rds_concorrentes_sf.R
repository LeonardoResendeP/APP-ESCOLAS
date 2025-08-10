suppressPackageStartupMessages({ library(dplyr); library(sf); library(readr); library(stringr); })

dir.create("logs", showWarnings = FALSE)
logfile <- file.path("logs", paste0("migracao_concorrentes_", format(Sys.time(), "%Y%m%d-%H%M%S"), ".txt"))
sink(logfile, split = TRUE); on.exit(sink(), add = TRUE)

cat("== MIGRAÇÃO lista_concorrentes -> sf ==\n")

uni_sf <- readRDS("data/processed/escolas_privadas_unificadas_sf.rds") %>%
  dplyr::select(id_escola, geometry)

files <- list.files("data/escolas", pattern = "\\.rds$", full.names = TRUE)
cat("Arquivos encontrados:", length(files), "\n")

for (f in files) {
  cat("\n--", basename(f), "--\n")
  x <- tryCatch(readRDS(f), error = function(e) { cat("Falha ao ler:", e$message, "\n"); return(NULL) })
  if (is.null(x)) next
  
  if (!("lista_concorrentes" %in% names(x))) { cat("Sem lista_concorrentes.\n"); next }
  
  lc <- x$lista_concorrentes
  if (inherits(lc, "sf")) { cat("Já é sf. OK.\n"); next }
  
  if (!all(c("id_escola","nome_escola") %in% names(lc))) {
    cat("Estrutura inesperada em lista_concorrentes. Pulando.\n"); next
  }
  
  lc_sf <- dplyr::left_join(lc, uni_sf, by = "id_escola")
  if (!("geometry" %in% names(lc_sf))) {
    cat("Não foi possível achar geometria pros concorrentes.\n"); next
  }
  lc_sf <- sf::st_as_sf(lc_sf)
  x$lista_concorrentes <- lc_sf
  
  # recalcula lat/lon a partir da geometria da escola (se existir geometry guardada em outro campo, ignore)
  if (!is.null(x$longitude) && !is.null(x$latitude)) {
    # já tem
  } else {
    # (opcional) tentar obter do uni_sf
    escola_geom <- uni_sf %>% filter(id_escola == x$id_escola)
    if (nrow(escola_geom)) {
      escola_wgs84 <- tryCatch(sf::st_transform(escola_geom, 4326), error = function(e) escola_geom)
      coords <- sf::st_coordinates(escola_wgs84)
      x$longitude <- coords[, "X"]; x$latitude <- coords[, "Y"]
    }
  }
  
  saveRDS(x, f)
  cat("Atualizado:", basename(f), "\n")
}
cat("\n✅ Migração concluída. Log:", logfile, "\n")
