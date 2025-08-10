# --- Pacotes (instala binários para evitar compilação) ---
pkgs <- c("data.table","readr","dplyr","stringr","sf","glue")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) {
  install.packages(to_install, type = "binary")
}
invisible(lapply(pkgs, library, character.only = TRUE))

# --- Paths ---
dir.create("data/_debug", showWarnings = FALSE, recursive = TRUE)
csv_path <- "data/raw/Agregados_preliminares_por_setores_censitarios_BR.csv"
shp_path <- "data/raw/BR_Malha_Preliminar_2022.shp"
stopifnot(file.exists(csv_path), file.exists(shp_path))

# --- 1) Ler CSV (auto-detecção com fread) ---
agregados <- data.table::fread(csv_path, encoding = "UTF-8", showProgress = TRUE)

# Normalizar nomes sem depender do janitor
names(agregados) <- gsub("[^A-Za-z0-9]+", "_", tolower(names(agregados)))
names(agregados) <- sub("_+$", "", names(agregados))

# Detectar coluna-chave do setor no CSV (case-insensitive via (?i))
cand_csv <- names(agregados)[
  stringr::str_detect(names(agregados), "(?i)((cd|co|cod).*setor|setor_censit|setor$)")
]
# Fallback com grepl ignore.case
if (length(cand_csv) == 0) {
  cand_csv <- names(agregados)[
    grepl("(cd|co|cod).*setor|setor_censit|setor$", names(agregados), ignore.case = TRUE)
  ]
}
key_csv <- cand_csv[1]

# --- 2) Ler SHP ---
setores <- suppressWarnings(sf::st_read(shp_path, quiet = TRUE, options = "ENCODING=UTF-8"))
names(setores) <- tolower(names(setores))
names(setores) <- gsub("[^a-z0-9]+", "_", names(setores))
names(setores) <- sub("_+$", "", names(setores))

# Detectar coluna-chave do setor no SHP
cand_shp <- names(setores)[
  stringr::str_detect(names(setores), "(?i)((cd|co|cod).*setor|setor_censit|setor$)")
]
if (length(cand_shp) == 0) {
  cand_shp <- names(setores)[
    grepl("(cd|co|cod).*setor|setor_censit|setor$", names(setores), ignore.case = TRUE)
  ]
}
key_shp <- cand_shp[1]

if (is.na(key_csv) || is.na(key_shp)) {
  message("Não encontrei as chaves automaticamente. Veja os nomes e me diga qual é a coluna do setor no CSV e no SHP:")
  print(names(agregados))
  print(names(setores))
  stop("Informe as colunas manualmente, ex.: key_csv <- 'cod_setor'; key_shp <- 'cd_setor'")
}

# Forçar as chaves como character e aparar espaços
agregados[[key_csv]] <- trimws(as.character(agregados[[key_csv]]))
setores[[key_shp]]   <- trimws(as.character(setores[[key_shp]]))

# --- 3) Diagnósticos rápidos ---
cat(glue("\n>> CSV: {nrow(agregados)} linhas | {ncol(agregados)} colunas | chave: {key_csv}\n"))
cat(glue("   amostra chave CSV: {paste(unique(agregados[[key_csv]])[1:3], collapse=', ')}\n"))
cat(glue("   nchar chave CSV (amostra 10): {paste(utils::head(nchar(agregados[[key_csv]]), 10), collapse=', ')}\n"))
cat(glue("   NAs na chave CSV: {sum(is.na(agregados[[key_csv]]) | agregados[[key_csv]]=='')}\n"))
cat(glue("   Duplicados na chave CSV: {nrow(agregados) - nrow(dplyr::distinct(agregados, .data[[key_csv]]))}\n"))

cat(glue("\n>> SHP: {nrow(setores)} polígonos | {ncol(setores)} colunas | chave: {key_shp}\n"))
cat(glue("   CRS: {sf::st_crs(setores)$input}\n"))
cat(glue("   amostra chave SHP: {paste(unique(setores[[key_shp]])[1:3], collapse=', ')}\n"))
cat(glue("   nchar chave SHP (amostra 10): {paste(utils::head(nchar(setores[[key_shp]]), 10), collapse=', ')}\n"))
cat(glue("   NAs na chave SHP: {sum(is.na(setores[[key_shp]]) | setores[[key_shp]]=='')}\n"))
cat(glue("   Duplicados na chave SHP: {nrow(setores) - nrow(dplyr::distinct(as.data.frame(setores)[, key_shp, drop=FALSE]))}\n"))

# Salvar amostras p/ inspeção
data.table::fwrite(utils::head(agregados, 1000), "data/_debug/agregados_head.csv")
sf::st_write(setores[1:100, ], "data/_debug/setores_head.gpkg", delete_dsn = TRUE, quiet = TRUE)

# --- 4) Verificar compatibilidade das chaves ---
mode_nchar <- function(x) {
  x <- na.omit(as.integer(nchar(x)))
  if (length(x) == 0) return(NA_integer_)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
mode_nchar_csv <- mode_nchar(agregados[[key_csv]])
mode_nchar_shp <- mode_nchar(setores[[key_shp]])
cat(glue("\nComprimento modal da chave - CSV: {mode_nchar_csv} | SHP: {mode_nchar_shp}\n"))

# --- 5) Join de teste (amostra) ---
# Selecionar 10 primeiras colunas além da chave para acelerar
cols_keep <- c(key_csv, setdiff(names(agregados), key_csv)[1:min(10, ncol(agregados)-1)])
agreg_mini <- agregados[, ..cols_keep]

set.seed(1)
amostra_ids <- sample(setores[[key_shp]], min(5000, nrow(setores)))
setores_sample <- setores[setores[[key_shp]] %in% amostra_ids, c(key_shp)]

joined <- dplyr::left_join(setores_sample, agreg_mini, by = setNames(key_csv, key_shp))
match_rate <- mean(!is.na(joined[[key_csv]]))
cat(glue("\nTaxa de match na amostra: {scales::percent(match_rate, 0.1)}\n"))

# --- 6) Salvar resultados de teste ---
sf::st_write(joined, "data/_debug/join_teste_setores_amostra.gpkg", delete_dsn = TRUE, quiet = TRUE)
saveRDS(list(
  info = list(
    key_csv = key_csv,
    key_shp = key_shp,
    n_csv = nrow(agregados),
    n_shp = nrow(setores),
    mode_nchar_csv = mode_nchar_csv,
    mode_nchar_shp = mode_nchar_shp,
    match_rate_sample = match_rate,
    crs = sf::st_crs(setores)$input
  ),
  joined_preview = joined[1:50, ]
), "data/_debug/diagnostico_join_censo2022.rds")

cat("\nArquivos de debug salvos em data/_debug/:\n - agregados_head.csv\n - setores_head.gpkg\n - join_teste_setores_amostra.gpkg\n - diagnostico_join_censo2022.rds\n\n")
