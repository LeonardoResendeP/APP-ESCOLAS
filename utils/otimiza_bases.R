# 01_setup_data_sp_rj_mg.R
# Organiza bases no DATA_DIR, e cria recortes enxutos (SP/RJ/MG)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sf)
  library(stringr)
  library(tools)
})

# ======= CONFIG =======
DATA_DIR <- Sys.getenv("DATA_DIR", unset = "data")     # defina em ~/.Renviron no seu PC/servidor
UF_TARGET <- c("SP","RJ","MG")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "processed"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(DATA_DIR, "cache_setores"), recursive = TRUE, showWarnings = FALSE)

# Utilitários -------------
path_proj <- function(...) file.path(...)
path_data <- function(...) file.path(DATA_DIR, ...)

move_if_exists <- function(from, to) {
  if (!file.exists(from)) return(invisible(FALSE))
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  ok <- file.rename(from, to)
  if (!ok) {
    ok <- file.copy(from, to, overwrite = TRUE, recursive = TRUE)
    if (ok) unlink(from, recursive = TRUE, force = TRUE)
  }
  ok
}

copy_if_exists <- function(from, to) {
  if (!file.exists(from)) return(invisible(FALSE))
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  file.copy(from, to, overwrite = TRUE, recursive = TRUE)
}

# ======= MOVER/CENTRALIZAR BASES GRANDES PARA DATA_DIR =======
cand <- c(
  "data/processed/censo2022_universo_setor_joined_clean.gpkg",
  "data/processed/escolas_privadas_unificadas_sf.rds",
  "data/escolas_privadas_nomelista.rds",
  "data/municipios_lookup.rds",
  "data/enem_consolidados.RData"
)

for (f in cand) {
  move_if_exists(path_proj(f), path_data(basename(f)))
}

# Se bases ainda estão no lugar antigo, apenas copie (não derruba seu workflow)
for (f in cand) {
  if (!file.exists(path_data(basename(f))) && file.exists(path_proj(f))) {
    copy_if_exists(path_proj(f), path_data(basename(f)))
  }
}

# ======= CARREGAR LOOKUP MUNICÍPIOS E PEGAR IDs de SP/RJ/MG =======
stopifnot(file.exists(path_data("municipios_lookup.rds")))
muni_lookup <- readRDS(path_data("municipios_lookup.rds"))

col_uf <- c("sigla_uf","UF","uf")
uf_col  <- col_uf[col_uf %in% names(muni_lookup)][1]
if (is.na(uf_col)) stop("Não encontrei coluna de UF em municipios_lookup.rds")

col_idm <- c("id_municipio","CO_MUNICIPIO","id_muni","code_muni","cod_municipio")
idm_col <- col_idm[col_idm %in% names(muni_lookup)][1]
if (is.na(idm_col)) stop("Não encontrei coluna de id_municipio em municipios_lookup.rds")

munis_target <- muni_lookup %>%
  filter(.data[[uf_col]] %in% UF_TARGET) %>%
  transmute(id_municipio = as.character(.data[[idm_col]]),
            sigla_uf = .data[[uf_col]])

ids_target <- unique(munis_target$id_municipio)

message("UF alvo: ", paste(UF_TARGET, collapse=", "))
message("Municípios alvo: ", length(ids_target))

# ======= SETORES (GPKG): RECORTE SP/RJ/MG =======
gpkg_full <- path_data("censo2022_universo_setor_joined_clean.gpkg")
if (!file.exists(gpkg_full)) stop("GPKG de setores não encontrado em DATA_DIR")

layers <- sf::st_layers(gpkg_full)$name
layer  <- layers[1]  # geralmente a primeira
message("Camada GPKG detectada: ", layer)

# Tenta recortar via query (mais rápido para GPKG grande)
# --- PATCH: detecção robusta de colunas ---

# --- PATCH: detecção robusta de colunas ---

# 1) Detecta o nome da coluna de geometria a partir do st_layers()
# --- PATCH: detecção robusta de colunas e recorte por UF ---

suppressPackageStartupMessages({
  library(sf); library(DBI); library(RSQLite); library(dplyr)
})

# Descobre a coluna de geometria (st_layers -> gpkg_geometry_columns)
detect_geom_col <- function(gpkg, layer) {
  li <- try(sf::st_layers(gpkg), silent = TRUE)
  if (!inherits(li, "try-error")) {
    i <- match(layer, li$name)
    if (!is.na(i)) {
      gf <- try(li$geomfields[[i]], silent = TRUE)
      if (!inherits(gf, "try-error") && length(gf) && !is.na(gf[1]) && nzchar(gf[1])) {
        return(gf[1])
      }
    }
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg); on.exit(DBI::dbDisconnect(con), add = TRUE)
  res <- try(DBI::dbGetQuery(con,
                             "SELECT column_name FROM gpkg_geometry_columns WHERE table_name = ?",
                             params = list(layer)), silent = TRUE)
  if (!inherits(res, "try-error") && nrow(res)) res$column_name[1] else NA_character_
}

# Heurística para detectar coluna de município
detect_muni_col_gpkg <- function(gpkg, layer, verbose = TRUE) {
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg); on.exit(DBI::dbDisconnect(con), add = TRUE)
  
  # 1) lista de colunas
  cols <- try(DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", layer))$name, silent = TRUE)
  if (inherits(cols, "try-error") || is.null(cols)) return(NA_character_)
  
  # 2) candidatos clássicos
  prefer <- c("id_municipio","ID_MUNICIPIO","CO_MUNICIPIO","co_municipio",
              "CD_MUN","cd_mun","CD_GEOCMU","cd_geocmu","cod_municipio","COD_MUNICIPIO",
              "code_muni","CODE_MUNI","cod_muni","COD_MUN","id_muni","ID_MUNI")
  inter <- prefer[prefer %in% cols]
  if (length(inter)) return(inter[1])
  
  # 3) candidatos por regex (qualquer coisa com 'mun' no nome)
  rx_cands <- grep("mun|muni|municip", cols, ignore.case = TRUE, value = TRUE)
  if (length(rx_cands) == 0L) {
    if (verbose) message("Nenhuma coluna com padrão 'mun|muni|municip' encontrada.\nColunas: ", paste(cols, collapse=", "))
    return(NA_character_)
  }
  
  # 4) escolhe a melhor candidata pela cara dos dados (6-7 dígitos, repetição alta)
  score_col <- function(col) {
    q <- sprintf("SELECT %s as v, COUNT(*) n FROM '%s' GROUP BY 1 ORDER BY n DESC LIMIT 100",
                 DBI::dbQuoteIdentifier(con, col), layer)
    tab <- try(DBI::dbGetQuery(con, q), silent = TRUE)
    if (inherits(tab, "try-error") || nrow(tab) == 0) return(-Inf)
    
    v <- as.character(tab$v)
    only_digits <- sum(grepl("^[0-9]+$", v, perl = TRUE))
    nchar_6_7   <- sum(nchar(v) %in% c(6,7))
    top_rep     <- if ("n" %in% names(tab)) max(tab$n, na.rm = TRUE) else 0
    # pontuação: prefere dígitos 6/7 e repetição alta
    return(only_digits + 2*nchar_6_7 + log1p(top_rep))
  }
  
  scores <- sapply(rx_cands, score_col)
  best   <- rx_cands[which.max(scores)]
  if (is.finite(max(scores)) && max(scores) > 0) {
    if (verbose) message("Candidatas: ", paste(rx_cands, collapse=", "),
                         " | escolhida: ", best, " (score=", round(max(scores),2), ")")
    return(best)
  }
  
  if (verbose) message("Falha na heurística. Colunas: ", paste(cols, collapse=", "))
  NA_character_
}

# ----- USO -----
# Defina estes:
# gpkg_full <- "data/processed/censo2022_universo_setor_joined_clean.gpkg"
# layer     <- "censo2022_universo_setor_joined_clean"

geom_col <- detect_geom_col(gpkg_full, layer)
idm_gpkg <- detect_muni_col_gpkg(gpkg_full, layer)
message("geom_col = ", geom_col, " | muni_col = ", idm_gpkg)

if (is.na(idm_gpkg)) {
  # DEBUG: lista as colunas e uma amostra para você ver e, se preciso, setar manualmente
  con <- DBI::dbConnect(RSQLite::SQLite(), gpkg_full)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- DBI::dbGetQuery(con, sprintf("PRAGMA table_info('%s')", layer))$name
  print(cols)
  amostra <- DBI::dbGetQuery(con, sprintf("SELECT * FROM '%s' LIMIT 5", layer))
  str(amostra)
  stop("Não encontrei coluna de município automaticamente — veja as colunas acima e defina 'idm_gpkg' manualmente, ex.: idm_gpkg <- 'CD_GEOCMU'")
}
# --- Use as detecções (mantém suas variáveis gpkg_full/layer) ---
geom_col <- detect_geom_col(gpkg_full, layer)
idm_gpkg <- detect_muni_col_gpkg(gpkg_full, layer)
message("geom_col = ", geom_col, " | coluna município = ", idm_gpkg)
if (is.na(idm_gpkg)) stop("Não encontrei coluna de município no GPKG.")

# 3) Recorte por UF via SQL IN em blocos; sem 'layer=' quando usar 'query' (evita warning)
slice_by_uf <- function(uf) {
  out <- path_data("cache_setores", paste0("setores_", uf, ".gpkg"))
  if (file.exists(out)) return(out)
  
  ids_uf <- munis_target %>% filter(sigla_uf == uf) %>% pull(id_municipio)
  block_size <- 800L
  blocks <- split(ids_uf, ceiling(seq_along(ids_uf) / block_size))
  
  if (file.exists(out)) file.remove(out)
  
  for (b in blocks) {
    cond <- paste0("'", paste(unique(b), collapse = "','"), "'")
    qry  <- sprintf('SELECT * FROM "%s" WHERE CAST("%s" AS TEXT) IN (%s)', layer, idm_gpkg, cond)
    
    chunk <- try(suppressMessages(sf::st_read(gpkg_full, query = qry, quiet = TRUE)), silent = TRUE)
    
    if (inherits(chunk, "try-error") || !inherits(chunk, "sf")) {
      message("Fallback para UF ", uf, ": leitura completa + filtro em memória (pode demorar).")
      all <- suppressMessages(sf::st_read(gpkg_full, layer = layer, quiet = TRUE))
      chunk <- dplyr::filter(all, as.character(.data[[idm_gpkg]]) %in% b)
    }
    
    if (nrow(chunk)) suppressMessages(sf::st_write(chunk, out, append = file.exists(out), quiet = TRUE))
  }
  out
}




gpkg_sp <- slice_by_uf("SP")
gpkg_rj <- slice_by_uf("RJ")
gpkg_mg <- slice_by_uf("MG")
message("Recortes de setores prontos em: ", dirname(gpkg_sp))

# ======= ESCOLAS (SF): SLIM + SP/RJ/MG =======
suppressPackageStartupMessages({
  library(dplyr); library(sf); library(stringr)
})

# Helpers robustos para pegar colunas se existirem
col_if_exists <- function(df, candidates) {
  nm <- candidates[candidates %in% names(df)]
  if (length(nm)) df[[nm[1]]] else NA
}
as_char <- function(x) if (is.null(x)) NA_character_ else as.character(x)

# 1) Carrega o SF "slim" (id_escola + geometry)
escolas_full_rds <- path_data("escolas_privadas_unificadas_sf.rds")
stopifnot(file.exists(escolas_full_rds))
escolas_full <- readRDS(escolas_full_rds) %>%
  mutate(id_escola = as.character(id_escola))

# 2) Carrega metadados para enriquecer
nomes_df <- readRDS("data/escolas_privadas_nomelista.rds")
# garante nomes padronizados e pega o que existir
nomes_df_std <- nomes_df %>%
  mutate(
    id_escola        = as.character(col_if_exists(nomes_df, c("CO_ENTIDADE","id_escola"))),
    NO_ENTIDADE      = col_if_exists(nomes_df, c("NO_ENTIDADE","nome_escola","NO_NOME_ENTIDADE")),
    nome_municipio   = col_if_exists(nomes_df, c("nome_municipio","NM_MUNICIPIO","municipio")),
    TP_DEPENDENCIA_ADM = suppressWarnings(as.integer(col_if_exists(nomes_df, c("TP_DEPENDENCIA_ADM","tp_dependencia_adm"))))
  ) %>%
  select(id_escola, NO_ENTIDADE, nome_municipio, TP_DEPENDENCIA_ADM) %>%
  distinct() %>%
  filter(!is.na(id_escola))

mun_lookup <- readRDS("data/municipios_lookup.rds")
mun_lookup_std <- mun_lookup %>%
  mutate(
    nome_municipio = col_if_exists(mun_lookup, c("nome_municipio","NM_MUNICIPIO","municipio")),
    id_municipio   = as.character(col_if_exists(mun_lookup, c("id_municipio","CD_MUN","CD_GEOCMU","cod_municipio"))),
    uf             = col_if_exists(mun_lookup, c("uf","sigla_uf","UF"))
  ) %>%
  select(nome_municipio, id_municipio, uf) %>%
  distinct() %>%
  filter(!is.na(nome_municipio), !is.na(id_municipio))

# (opcional) Defina munis_target se ainda não existir:
if (!exists("munis_target")) {
  munis_target <- mun_lookup_std %>%
    filter(uf %in% c("SP","RJ","MG")) %>%
    select(id_municipio) %>%
    distinct()
}

suppressPackageStartupMessages({ library(dplyr); library(sf) })

# helper pra pegar o primeiro não-NA
first_non_na <- function(x) { i <- which(!is.na(x) & x != ""); if (length(i)) x[i[1]] else NA }

# 1) schools slim (id_escola + geometry)
escolas_full <- readRDS("data/escolas_privadas_unificadas_sf.rds") %>%
  mutate(id_escola = as.character(id_escola))

# 2) metadados deduplicados por id_escola
suppressPackageStartupMessages({ library(dplyr) })

# Pega o primeiro valor "válido" (não-NA e, se for character, não vazio)
first_non_na <- function(x) {
  ok <- !is.na(x)
  if (is.character(x)) ok <- ok & nzchar(x)
  i <- which(ok)
  if (length(i)) x[i[1]] else NA
}

# Coalesce "manual" entre várias colunas candidatas que podem nem existir
coalesce_cols <- function(df, candidates) {
  present <- intersect(candidates, names(df))
  if (length(present) == 0) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]
  if (length(present) > 1) {
    for (nm in present[-1]) out <- dplyr::coalesce(out, df[[nm]])
  }
  out
}

# =========================================================
#   ENRIQUECIMENTO DE ESCOLAS (SP/RJ/MG) — SCRIPT ÚNICO
# =========================================================
suppressPackageStartupMessages({
  library(dplyr); library(sf); library(stringr)
})

# ---------- Helpers robustos ----------
first_non_na <- function(x) {
  ok <- !is.na(x)
  if (is.character(x)) ok <- ok & nzchar(x)
  i <- which(ok)
  if (length(i)) x[i[1]] else NA
}

coalesce_cols <- function(df, candidates) {
  present <- intersect(candidates, names(df))
  if (!length(present)) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]
  if (length(present) > 1) {
    for (nm in present[-1]) out <- dplyr::coalesce(out, df[[nm]])
  }
  out
}

# tenta carregar shape de UF (SP/RJ/MG); se falhar, segue sem (UF só do nomes_df)
read_uf_sp_rj_mg <- function(target_crs) {
  uf <- try({
    if (!requireNamespace("geobr", quietly = TRUE)) stop("no geobr")
    geobr::read_state(code_state = c("SP","RJ","MG"), year = 2020, simplified = TRUE) |>
      sf::st_transform(target_crs) |>
      dplyr::select(uf = abbrev_state, geometry)
  }, silent = TRUE)
  if (inherits(uf, "try-error")) return(NULL)
  uf
}

# ---------- Carrega bases ----------
message("• Lendo escolas (sf slim) e nomes…")
escolas_full <- readRDS("data/escolas_privadas_unificadas_sf.rds") |>
  mutate(id_escola = as.character(id_escola))

# assume que você já definiu:
# first_non_na() e coalesce_cols()

# ---- CORREÇÃO: construir nomes_df_std sem usar "." no mutate ----
df <- tibble::as_tibble(nomes_df)  # referência explícita ao data frame

id_escola_vec  <- as.character(coalesce_cols(df, c("CO_ENTIDADE","id_escola","CO_ESCOLA")))
nome_escola_vec<- as.character(coalesce_cols(df, c("NO_ENTIDADE","nome_escola","NO_NOME_ENTIDADE")))
muni_vec       <- as.character(coalesce_cols(df, c("nome_municipio","NM_MUNICIPIO","municipio","NM_MUN")))
uf_vec         <- toupper(as.character(coalesce_cols(df, c("SG_UF","UF","uf","sigla_uf"))))
dep_vec        <- suppressWarnings(as.integer(coalesce_cols(df, c("TP_DEPENDENCIA_ADM","tp_dependencia_adm"))))

nomes_df_std <- tibble::tibble(
  id_escola          = id_escola_vec,
  NO_ENTIDADE        = nome_escola_vec,
  nome_municipio     = muni_vec,
  uf_raw             = uf_vec,
  TP_DEPENDENCIA_ADM = dep_vec
) %>%
  dplyr::filter(!is.na(id_escola), nzchar(id_escola)) %>%
  dplyr::group_by(id_escola) %>%
  dplyr::summarise(
    NO_ENTIDADE        = first_non_na(NO_ENTIDADE),
    nome_municipio     = first_non_na(nome_municipio),
    uf                 = first_non_na(uf_raw),
    TP_DEPENDENCIA_ADM = suppressWarnings(as.integer(first_non_na(TP_DEPENDENCIA_ADM))),
    .groups = "drop"
  )


# pressupõe que já existem first_non_na() e coalesce_cols(df, cols)

# --- LOOKUP DE MUNICÍPIOS (SP/RJ/MG) — VERSÃO ROBUSTA, SEM USAR "." ---
df_mun <- tibble::as_tibble(mun_lookup)

nome_municipio_vec <- as.character(coalesce_cols(df_mun, c("nome_municipio","NM_MUNICIPIO","municipio","NM_MUN")))
id_municipio_vec   <- as.character(coalesce_cols(df_mun, c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU","cod_municipio","code_muni")))
uf_vec             <- toupper(as.character(coalesce_cols(df_mun, c("uf","sigla_uf","UF","SIGLA_UF"))))

mun_lookup_std <- tibble::tibble(
  nome_municipio = nome_municipio_vec,
  id_municipio   = id_municipio_vec,
  uf             = uf_vec
) |>
  dplyr::filter(
    !is.na(nome_municipio), nzchar(nome_municipio),
    !is.na(id_municipio),   nzchar(id_municipio)
  ) |>
  dplyr::filter(uf %in% c("SP","RJ","MG")) |>
  dplyr::arrange(id_municipio, uf, nome_municipio) |>
  dplyr::group_by(id_municipio) |>
  dplyr::summarise(
    nome_municipio = first_non_na(nome_municipio),
    uf             = first_non_na(uf),
    .groups = "drop"
  )

# cheque rápido:
stopifnot(nrow(mun_lookup_std) > 0)


# helpers (garanta que estejam definidos uma vez)
first_non_na <- function(x) { i <- which(!is.na(x) & x != ""); if (length(i)) x[i[1]] else NA }
coalesce_cols <- function(df, cols) {
  out <- NULL
  for (nm in cols) {
    if (!is.null(df[[nm]]) && !all(is.na(df[[nm]]))) {
      v <- df[[nm]]
      if (is.null(out)) out <- v else out <- dplyr::coalesce(out, v)
    }
  }
  out
}

# Fallback por nome+UF — sem usar "." dentro do mutate
df_mun2 <- tibble::as_tibble(mun_lookup)

nome_municipi o_vec2 <- as.character(coalesce_cols(df_mun2, c("nome_municipio","NM_MUNICIPIO","municipio","NM_MUN")))
id_municipio_vec2   <- as.character(coalesce_cols(df_mun2, c("id_municipio","CO_MUNICIPIO","CD_MUN","CD_GEOCMU","cod_municipio","code_muni")))
uf_vec2             <- toupper(as.character(coalesce_cols(df_mun2, c("uf","sigla_uf","UF","SIGLA_UF"))))

mun_lookup_byname <- tibble::tibble(
  nome_municipio = nome_municipio_vec2,
  id_municipio   = id_municipio_vec2,
  uf             = uf_vec2
) %>%
  dplyr::filter(
    !is.na(nome_municipio), nzchar(nome_municipio),
    !is.na(id_municipio),   nzchar(id_municipio),
    uf %in% c("SP","RJ","MG")
  ) %>%
  dplyr::arrange(nome_municipio, uf, id_municipio) %>%
  dplyr::group_by(nome_municipio, uf) %>%
  dplyr::summarise(id_municipio = first_non_na(id_municipio), .groups = "drop")

# checagem rápida:
stopifnot(nrow(mun_lookup_byname) > 0)


# =========================
# 1) Pacotes e helpers
# =========================
suppressPackageStartupMessages({
  library(DBI)
  library(bigrquery)
  library(dplyr)
  library(sf)
  library(tibble)
})

coalesce_cols <- function(df, cols) {
  present <- cols[cols %in% names(df)]
  if (!length(present)) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]
  if (length(present) > 1) for (c in present[-1]) out <- dplyr::coalesce(out, df[[c]])
  out
}
first_non_na <- function(x) { i <- which(!is.na(x) & nzchar(as.character(x))); if (length(i)) x[i[1]] else NA }

# =========================
# 2) Sanidade das entradas
# =========================
stopifnot(exists("escolas_full"), inherits(escolas_full, "sf"), "geometry" %in% names(escolas_full))
stopifnot(exists("nomes_df_std"))

# Normaliza nomes_df_std (id_escola, nome_municipio/uf se houver)
ndf <- tibble::as_tibble(nomes_df_std)
ndf_norm <- tibble::tibble(
  id_escola      = as.character(coalesce_cols(ndf, c("id_escola","CO_ENTIDADE","CO_ESCOLA"))),
  NO_ENTIDADE    = as.character(coalesce_cols(ndf, c("NO_ENTIDADE","nome_escola","NO_NOME_ENTIDADE"))),
  nome_municipio = as.character(coalesce_cols(ndf, c("nome_municipio","NM_MUNICIPIO","municipio","NM_MUN"))),
  uf             = toupper(as.character(coalesce_cols(ndf, c("uf","UF","sigla_uf","SG_UF")))),
  TP_DEPENDENCIA_ADM = suppressWarnings(as.integer(coalesce_cols(ndf, c("TP_DEPENDENCIA_ADM","tp_dependencia_adm"))))
) |>
  filter(!is.na(id_escola), nzchar(id_escola)) |>
  group_by(id_escola) |>
  summarise(
    NO_ENTIDADE        = first_non_na(NO_ENTIDADE),
    nome_municipio     = first_non_na(nome_municipio),
    uf                 = first_non_na(uf),
    TP_DEPENDENCIA_ADM = suppressWarnings(as.integer(first_non_na(TP_DEPENDENCIA_ADM))),
    .groups = "drop"
  )

# Garante CRS nos pontos de escola
if (is.na(sf::st_crs(escolas_full))) {
  bb <- sf::st_bbox(escolas_full)
  looks_br_lonlat <- (bb["xmin"] > -75 && bb["xmin"] < -33 &&
                        bb["xmax"] > -75 && bb["xmax"] < -33 &&
                        bb["ymin"] > -34 && bb["ymin"] <   6 &&
                        bb["ymax"] > -34 && bb["ymax"] <   6)
  if (isTRUE(looks_br_lonlat)) {
    message("CRS ausente — atribuindo WGS84 (EPSG:4326).")
    escolas_full <- sf::st_set_crs(escolas_full, 4326)
  } else stop("Defina manualmente o CRS de 'escolas_full', ex.: st_set_crs(4326).")
}

# =========================
# 3) Puxa MUNICÍPIOS SP/RJ/MG do Base dos Dados
# =========================
message("• Conectando ao BigQuery (Base dos Dados)…")
bq_billing <- "able-study-331220"  # <- seu billing
con <- DBI::dbConnect(bigrquery::bigquery(), project = "basedosdados", billing = bq_billing)
on.exit(DBI::dbDisconnect(con), add = TRUE)

# Pegamos só o necessário (id_municipio, sigla_uf, geometry como WKT).
# Nota: coluna de nome costuma chamar 'nome_municipio' nessa tabela. Incluímos; se não existir, remova da SELECT.
qry_muni <- "
SELECT
  CAST(id_municipio AS STRING) AS id_municipio,
  sigla_uf,
  id_municipio,
  geometria AS wkt
FROM `basedosdados.br_geobr_mapas.municipio`
  WHERE sigla_uf IN ('SP','RJ','MG')
"
message("• Baixando geometrias municipais de SP/RJ/MG…")
muni_df <- DBI::dbGetQuery(con, qry_muni)

suppressPackageStartupMessages({
  library(sf); library(dplyr); library(tibble); library(stringr)
})

# ---- Helpers ----
first_non_na <- function(x){
  i <- which(!is.na(x) & nzchar(as.character(x))); if(length(i)) x[i[1]] else NA
}
coalesce_cols <- function(df, cols){
  present <- intersect(cols, names(df))
  if(!length(present)) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]
  for(nm in present[-1]) out <- dplyr::coalesce(out, df[[nm]])
  out
}

# ==== 0) Sanidade: objetos esperados ====
stopifnot(exists("muni_df"))
stopifnot(exists("escolas_full"), inherits(escolas_full, "sf"), "geometry" %in% names(escolas_full))
stopifnot(exists("nomes_df_std"))

suppressPackageStartupMessages({ library(sf); library(dplyr); library(tibble) })

# --- 1) muni_df -> muni_sf (WKT → sf) com CRS garantido ---
stopifnot(all(c("id_municipio","sigla_uf","wkt") %in% names(muni_df)))

muni_geom <- suppressWarnings(sf::st_as_sfc(muni_df$wkt, crs = 4326))  # define CRS aqui
muni_sf <- sf::st_sf(
  id_municipio = as.character(muni_df$id_municipio),
  uf           = toupper(muni_df$sigla_uf),
  geometry     = muni_geom
)

# Se por algum motivo ainda vier sem CRS, força 4326
if (is.na(sf::st_crs(muni_sf))) {
  muni_sf <- sf::st_set_crs(muni_sf, 4326)
}
# Dedup e geometrias válidas
suppressWarnings({
  muni_sf <- muni_sf |>
    group_by(id_municipio, uf) |>
    summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    sf::st_make_valid()
})

# --- 2) CRS nas escolas (4326) ---
if (is.na(sf::st_crs(escolas_full))) {
  bb <- sf::st_bbox(escolas_full)
  looks_br_lonlat <- (bb["xmin"] > -75 && bb["xmin"] < -33 &&
                        bb["xmax"] > -75 && bb["xmax"] < -33 &&
                        bb["ymin"] > -34 && bb["ymin"] <   6 &&
                        bb["ymax"] > -34 && bb["ymax"] <   6)
  if (isTRUE(looks_br_lonlat)) {
    message("CRS ausente em 'escolas_full' — atribuindo WGS84 (EPSG:4326).")
    escolas_full <- sf::st_set_crs(escolas_full, 4326)
  } else stop("Defina manualmente o CRS de 'escolas_full' (ex.: st_set_crs(4326)).")
}
escolas_ll <- sf::st_transform(escolas_full, 4326)
muni_ll    <- muni_sf                      # já está em 4326

# --- 3) Overlay ponto→município (within + fallback NN) ---
old_s2 <- sf::sf_use_s2(); on.exit(sf::sf_use_s2(old_s2), add = TRUE)
sf::sf_use_s2(TRUE)

y_min <- muni_ll[, c("id_municipio","uf", "geometry")]
escolas_geo <- suppressWarnings(sf::st_join(escolas_ll, y_min, left = TRUE, join = sf::st_within))

na_idx <- is.na(escolas_geo$id_municipio)
if (any(na_idx)) {
  message("Fallback NN para ", sum(na_idx), " escola(s) sem match exato…")
  nn <- sf::st_nearest_feature(escolas_ll[na_idx, ], muni_ll)
  escolas_geo$id_municipio[na_idx] <- muni_ll$id_municipio[nn]
  escolas_geo$uf[na_idx]           <- muni_ll$uf[nn]
}

message("Resumo pós-join  • NAs uf: ", sum(is.na(escolas_geo$uf)),
        " | NAs id_municipio: ", sum(is.na(escolas_geo$id_municipio)))

# --- 4) Enriquecer com nomes_df_std (ajuste nomes se precisarem) ---
first_non_na <- function(x){ i <- which(!is.na(x) & nzchar(as.character(x))); if(length(i)) x[i[1]] else NA }
coalesce_cols <- function(df, cols){
  present <- intersect(cols, names(df)); if(!length(present)) return(rep(NA, nrow(df)))
  out <- df[[present[1]]]; for(nm in present[-1]) out <- dplyr::coalesce(out, df[[nm]]); out
}

ndf <- tibble::as_tibble(nomes_df_std)
ndf_norm <- tibble::tibble(
  id_escola          = as.character(coalesce_cols(ndf, c("id_escola","CO_ENTIDADE","CO_ESCOLA"))),
  NO_ENTIDADE        = as.character(coalesce_cols(ndf, c("NO_ENTIDADE","nome_escola","NO_NOME_ENTIDADE"))),
  nome_municipio_ref = as.character(coalesce_cols(ndf, c("nome_municipio","NM_MUNICIPIO","municipio","NM_MUN"))),
  uf_ref             = toupper(as.character(coalesce_cols(ndf, c("uf","UF","sigla_uf","SG_UF")))),
  TP_DEPENDENCIA_ADM = suppressWarnings(as.integer(coalesce_cols(ndf, c("TP_DEPENDENCIA_ADM","tp_dependencia_adm"))))
) |>
  dplyr::filter(!is.na(id_escola), nzchar(id_escola)) |>
  dplyr::group_by(id_escola) |>
  dplyr::summarise(
    NO_ENTIDADE        = first_non_na(NO_ENTIDADE),
    nome_municipio_ref = first_non_na(nome_municipio_ref),
    uf_ref             = first_non_na(uf_ref),
    TP_DEPENDENCIA_ADM = suppressWarnings(as.integer(first_non_na(TP_DEPENDENCIA_ADM))),
    .groups = "drop"
  )

# garantir id_escola como char
if (!"id_escola" %in% names(escolas_geo) && "CO_ENTIDADE" %in% names(escolas_geo)) {
  escolas_geo$id_escola <- as.character(escolas_geo$CO_ENTIDADE)
} else {
  escolas_geo$id_escola <- as.character(escolas_geo$id_escola)
}

# ---- garantir que as colunas existem antes do mutate ----
if (!"id_escola" %in% names(escolas_geo)) {
  # tenta derivar a chave a partir de colunas alternativas
  tmp <- tibble::as_tibble(sf::st_drop_geometry(escolas_geo))
  key <- tmp[[intersect(c("id_escola","CO_ENTIDADE","CO_ESCOLA"), names(tmp))[1]]]
  escolas_geo$id_escola <- as.character(key)
}
if (!"uf" %in% names(escolas_geo)) escolas_geo$uf <- NA_character_
if (!"nome_municipio" %in% names(escolas_geo)) escolas_geo$nome_municipio <- NA_character_

# ---- join + coalesce correto (em vetores) ----
escolas_enriq <- escolas_geo |>
  dplyr::left_join(ndf_norm, by = "id_escola") |>
  dplyr::mutate(
    uf             = dplyr::coalesce(.data$uf, .data$uf_ref),
    nome_municipio = dplyr::coalesce(.data$nome_municipio, .data$nome_municipio_ref),
    rede           = dplyr::case_when(
      !is.na(.data$TP_DEPENDENCIA_ADM) ~ dplyr::if_else(.data$TP_DEPENDENCIA_ADM == 4L, "Privada", "Pública"),
      TRUE ~ "Privada"
    )
  ) |>
  dplyr::select(id_escola, NO_ENTIDADE, nome_municipio, id_municipio, uf, rede, geometry)

# ---- (opcional) completar id_municipio ainda NA via lookup por nome+UF ----
na_muni <- is.na(escolas_enriq$id_municipio) & !is.na(escolas_enriq$nome_municipio) & !is.na(escolas_enriq$uf)
if (any(na_muni)) {
  stopifnot(exists("mun_lookup_byname"))
  escolas_enriq <- escolas_enriq |>
    dplyr::left_join(mun_lookup_byname, by = c("nome_municipio","uf"), suffix = c("", ".lkp")) |>
    dplyr::mutate(id_municipio = dplyr::coalesce(.data$id_municipio, .data$id_municipio.lkp)) |>
    dplyr::select(-dplyr::any_of("id_municipio.lkp"))
}

# ---- filtrar SP/RJ/MG e salvar (se quiser) ----
escolas_min <- escolas_enriq |>
  dplyr::filter(.data$uf %in% c("SP","RJ","MG"))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(escolas_enriq, "data/processed/escolas_enriquecidas.rds")
saveRDS(escolas_min,  "data/processed/escolas_SP_RJ_MG.rds")

message("OK! Linhas totais: ", nrow(escolas_enriq),
        " | Em SP/RJ/MG: ", nrow(escolas_min),
        " | NAs id_municipio: ", sum(is.na(escolas_enriq$id_municipio)))






##########################

# Lista os N maiores arquivos (>= min_mb)
list_big <- function(root=".", n=50, min_mb=50) {
  x <- list.files(root, recursive=TRUE, all.files=TRUE, full.names=TRUE, no..=TRUE)
  info <- file.info(x); info$path <- rownames(info); rownames(info) <- NULL
  info <- subset(info, !info$isdir)
  info$MB <- round(info$size/1024^2, 1)
  info <- info[order(info$size, decreasing=TRUE), c("MB","path")]
  info[info$MB >= min_mb, ][seq_len(min(n, nrow(info[info$MB >= min_mb, ]))), ]
}

# Soma por diretório de topo
dir_tops <- function(root=".") {
  x <- list.files(root, recursive=TRUE, all.files=TRUE, full.names=TRUE, no..=TRUE)
  info <- file.info(x); info$path <- rownames(info); rownames(info) <- NULL
  info <- subset(info, !info$isdir)
  top <- sub("^\\./","", info$path)
  top <- sub("\\\\","/", top)
  top <- ifelse(grepl("/", top), sub("/.*","", top), top)
  agg <- aggregate(info$size, list(top=top), sum)
  agg$GB <- round(agg$x/1024^3, 2)
  agg[order(-agg$x), c("top","GB")]
}

cat("== Top arquivos ==\n"); print(list_big(".", n=40, min_mb=25))
cat("\n== Tamanho por diretório ==\n"); print(dir_tops("."))





##################

# ===============================
# 1) Helpers
# ===============================
safe_move <- function(src, dest_dir) {
  if (!file.exists(src)) return(invisible(FALSE))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(dest_dir, basename(src))
  ok <- FALSE
  if (dir.exists(src)) {
    ok <- file.copy(src, dest, recursive = TRUE, copy.mode = TRUE, copy.date = TRUE)
    if (ok) unlink(src, recursive = TRUE, force = TRUE)
  } else {
    ok <- file.rename(src, dest)
    if (!ok) {
      ok <- file.copy(src, dest, overwrite = TRUE, copy.mode = TRUE, copy.date = TRUE)
      if (ok) unlink(src, force = TRUE)
    }
  }
  message(if (ok) "→ movido: " else "x falhou mover: ", src, "  ->  ", dest)
  ok
}

size_gb <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (!length(paths)) return(0)
  round(sum(file.info(paths)$size, na.rm = TRUE) / 1024^3, 2)
}

# ===============================
# 2) Definições de origem/destino
# ===============================
no_deploy_root <- "data/_no_deploy"  # tudo pesado vem pra cá

# Diretórios inteiros muito pesados (se existirem)
heavy_dirs <- c(
  "data/microdados_enem_2024",
  "data/raw",
  "data/censo_ibge_2022",
  "data/cache_setores"
)

# Arquivos soltos pesados
heavy_files <- c(
  "data/censo2022_universo_setor_joined_clean.gpkg",
  "data/processed/censo2022_universo_setor.gpkg",
  "data/app/setor_2025_ready.rds",
  "data/censo.csv",
  "data/processed/setores_slim.gpkg",
  "data/processed/nowcast_setor_2025.csv"
)

# Itens de artefato/saída (se tiverem dentro do projeto)
artifact_dirs <- c("outputs", "report", "reports")

# ===============================
# 3) Relatório antes
# ===============================
cat("Tamanho antes (GB):\n")
cat(sprintf("  data/          : %.2f\n", size_gb("data")))
cat(sprintf("  renv/library/  : %.2f\n", size_gb("renv/library")))
cat(sprintf("  outputs/       : %.2f\n", size_gb("outputs")))
cat(sprintf("  report(s)/     : %.2f\n", size_gb(c("report","reports"))))

# ===============================
# 4) Mover os pesados para _no_deploy
# ===============================
for (d in heavy_dirs) if (dir.exists(d)) safe_move(d, file.path(no_deploy_root, basename(d)))
for (f in heavy_files) if (file.exists(f)) safe_move(f, file.path(no_deploy_root, "files"))

# Opcional: mover artefatos/saídas (se quiser tirar do caminho local)
for (d in artifact_dirs) if (dir.exists(d)) safe_move(d, file.path(no_deploy_root, "artifacts", basename(d)))

# ===============================
# 5) .rscignore para rsconnect
#    (excluir tudo pesado do bundle)
# ===============================
rscignore_txt <- '
# ===== EXCLUIR DO DEPLOY =====
data/_no_deploy/**
data/microdados_enem_2024/**
data/raw/**
data/censo_ibge_2022/**
data/cache_setores/**
data/**/*.gpkg
data/**/*.zip
data/**/*.dbf
data/**/*.shp
data/**/*.shx
data/**/*.qix
data/**/*.csv
data/censo.csv
data/app/setor_2025_ready.rds

# SAÍDAS / ARTEFATOS
outputs/**
report/**
reports/**
*.pdf
*.png
*.html

# renv local (servidor recompila)
renv/library/**

# TEMP / IDE
.Rproj.user/**
.Rhistory
.RData
.RDataTmp*
.DS_Store
Thumbs.db

# ===== PERMITIR EXCEÇÕES (dados mínimos do app) =====
!data/processed/escolas_SP_RJ_MG.rds
!data/escolas/**
!data/municipios_lookup.rds
!data/processed/enem_escola_min.rds
!data/processed/enem_muni_min.rds
'
writeLines(rscignore_txt, ".rscignore")
message("Arquivo .rscignore escrito.")

# ===============================
# 6) Garantir .gitignore (pra não versionar o pesado)
# ===============================
git_lines <- c(
  "data/_no_deploy/",
  "data/microdados_enem_2024/",
  "data/raw/",
  "data/censo_ibge_2022/",
  "data/cache_setores/",
  "outputs/",
  "report/",
  "reports/",
  "renv/library/"
)
if (file.exists(".gitignore")) {
  gi <- readLines(".gitignore", warn = FALSE)
  add <- setdiff(git_lines, gi)
  if (length(add)) write(append = TRUE, x = paste0(add, collapse = "\n"), file = ".gitignore")
} else {
  writeLines(git_lines, ".gitignore")
}
message("Git ignore atualizado.")

# ===============================
# 7) Relatório depois + estimativa do bundle
# ===============================
cat("\nTamanho depois (GB):\n")
cat(sprintf("  data/          : %.2f\n", size_gb("data")))
cat(sprintf("  renv/library/  : %.2f (será EXCLUÍDO do bundle)\n", size_gb("renv/library")))
cat(sprintf("  outputs/       : %.2f (se moveu p/ _no_deploy, já sai do bundle)\n", size_gb("outputs")))
cat(sprintf("  report(s)/     : %.2f (idem)\n", size_gb(c("report","reports"))))
    
    # Estimativa simples de bundle: tudo que não casa com .rscignore (difícil calcular 100%),
    # mas dá para listar o essencial e somar:
    essential <- c(
      "app.R", "modules", "utils", "www",
      "renv.lock",
      "data/processed/escolas_SP_RJ_MG.rds",
      "data/escolas",
      "data/municipios_lookup.rds",
      "data/processed/enem_escola_min.rds",
      "data/processed/enem_muni_min.rds"
    )
    exists_ess <- essential[file.exists(essential)]
    bundle_est <- size_gb(unlist(lapply(exists_ess, function(p) {
      if (dir.exists(p)) {
        list.files(p, recursive = TRUE, full.names = TRUE)
      } else p
    })))
    cat(sprintf("\nEstimativa simplificada do bundle (GB): ~ %.2f\n", bundle_est))
    

    
    
    
    ###########
    
    
    # ============== CONFIG ==============
    library(rsconnect); library(fs)
    
    # 1) MONTE UMA WHITELIST BEM ENXUTA --------------------------
    whitelist <- c(
      "app.R",
      "renv.lock",
      # código
      dir_ls("modules", recurse = TRUE, type = "file", glob = "*.R", fail = FALSE),
      dir_ls("utils",   recurse = TRUE, type = "file", fail = FALSE),
      dir_ls("www",     recurse = TRUE, type = "file", fail = FALSE),
      # dados mínimos (ajuste os nomes conforme você tiver aí)
      "data/municipios_lookup.rds",
      "data/processed/escolas_SP_RJ_MG.rds",
      "data/processed/enem_escola_min.rds",
      "data/processed/enem_muni_min.rds",
      # escolas já cadastradas
      dir_ls("data/escolas", recurse = TRUE, type = "file", glob = "*.rds", fail = FALSE)
    )
    whitelist <- unique(whitelist[file_exists(whitelist)])
    
    # 2) RELATÓRIO DE TAMANHO DA WHITELIST -----------------------
    tamanho_mb <- function(files) sum(file_info(files)$size, na.rm = TRUE)/1024^2
    cat(sprintf("Arquivos whitelisted: %d | Tamanho total ~ %.1f MB\n",
                length(whitelist), tamanho_mb(whitelist)))
    
    # 3) AUMENTE O LIMITE DE BUNDLE (só nesta sessão) ------------
    options(rsconnect.max.bundle.size = 10 * 1024^3)  # 10 GB temporário
    
    # mantém a whitelist que você já montou
    options(rsconnect.max.bundle.size = 10 * 1024^3)  # evita o bloqueio de tamanho da pasta
    
    
    
    # dentro do projeto
    renv::activate()
    options(repos = c(CRAN = "https://cran.rstudio.com"))
    renv::settings$snapshot.type("implicit")   # captura deps usadas no código
    renv::snapshot(prompt = FALSE)             # atualiza renv.lock a partir da sua library
    renv::status()                             # deve ficar "up to date"
    

    
    
    rsconnect::deployApp(
      appDir   = ".",
      appFiles = whitelist,
      appName  = "Explora_dashboard",     # >= 4 chars, só letras/números/hífens
      account  = "primeiraescolhadevolutivas",# o account que você escolheu (2)
      server   = "shinyapps.io",
      logLevel = "verbose"
    )
    
    
    
    # Liste para confirmar o ID e o nome exatos
    rsconnect::applications(account = "primeiraescolhadevolutivas")
    
    # Se aparecer "Explora_dashboard", anote o appId (ex.: 15243922)
    # Mostre logs em tempo real (melhor), forçando um hit na URL em seguida:
    rsconnect::showLogs(
      appName   = "Explora_dashboard",                # ou appName="Explora_dashboard"
      account   = "primeiraescolhadevolutivas",
      streaming = TRUE
    )
    
    # Se quiser “dump” completo sem streaming:
    rsconnect::showLogs(appId = 15243922, account = "primeiraescolhadevolutivas", entries = Inf)
    
    
    
    
    rsconnect::showLogs(
      appName   = "Explora_dashboard",
      account = "primeiraescolhadevolutivas",
      server  = "shinyapps.io",
      streaming = FALSE,
      entries   = 500   # pega as últimas 500 linhas
    )
    
    
    install.packages("usethis")          # se ainda não tiver
    usethis::edit_r_environ("project")
    
    getwd()
    file.edit(".Renviron")   # se o arquivo não existir, ele cria
    readRenviron(".Renviron")
    nzchar(Sys.getenv("OPENAI_API_KEY"))
    whitelist <- unique(c(whitelist, ".Renviron"))    
    
    sum(fs::file_info(whitelist)$size)/1024^2

    
    
    
    
    
    
    
    
    
    
    
    
    
    #################
    
    
    
    
    
    
    
    
    # scripts/deploy_whitelist.R
    suppressPackageStartupMessages({
      library(fs)
      library(rsconnect)
      library(dplyr)
    })
    
    # ---- helpers ----
    exists_file <- function(x) x[file_exists(x)]
    ls_r <- function(path, pattern = NULL) {
      if (!dir_exists(path)) return(character(0))
      dir_ls(path, recurse = TRUE, type = "file", regexp = pattern)
    }
    
    # 1) núcleo do app (sempre incluir se existir)
    core <- c(
      "app.R",
      "renv.lock",
      ".Rprofile",
      ".Renviron",                 # leva suas variáveis (OPENAI_API_KEY, APP_DB_PATH etc.)
      "config/database.sqlite",    # seu banco local de login (ajuste se for outro nome)
      "config/users.sqlite"        # opcional: caso seu projeto use esse nome
    ) |> exists_file()
    
    # 2) código R
    r_code <- c(
      ls_r("modules", "\\.R$"),
      ls_r("utils",   "\\.R$")
    ) |> exists_file()
    
    # 3) assets estáticos (www)
    assets <- c(
      ls_r("www")                  # imagens, css, js do app
    ) |> exists_file()
    
    # 4) relatórios (se renderiza Rmd no servidor, inclua o necessário)
    reports <- c(
      "report/relatorio_escola.Rmd",
      "report/relatorio.css",
      ls_r("report", "\\.(png|jpg|jpeg|svg)$")
    ) |> exists_file()
    
    # 5) dados leves necessários em runtime (NADA gigante!)
    #    -> per-school RDS, minificados, lookups pequenos
    data_light <- c(
      ls_r("data/escolas", "\\.rds$"),
      "data/enem_escola_min.rds",
      "data/enem_muni_min.rds",
      "data/municipios_lookup_sp_rj_mg.rds"  # se você criou esse lookup; senão ignore
    ) |> exists_file()
    
    # 6) EXCLUSÕES explícitas (garantia extra)
    #    Mesmo usando appFiles, vamos auditar para não entrar nada pesado.
    deny_globs <- c(
      "data/**/RESULTADOS_2024.csv",
      "data/**/PARTICIPANTES_2024.csv",
      "data/**/setor_2025_ready.rds",
      "data/**/censo*.gpkg",
      "data/**/setores_*.gpkg",
      "data/**/BR_Malha*.{shp,dbf,zip}",
      "data/**/Agregados_*",
      "renv/library/**"
    )
    
    # 7) montar whitelist e filtrar coisas perigosas por tamanho e denylist
    whitelist <- unique(c(core, r_code, assets, reports, data_light)) |> exists_file()
    
    # segurança extra: limite por arquivo (MB) — nada acima de 50 MB
    INFO <- file_info(whitelist)
    too_big <- which(INFO$size > 50 * 1024^2)
    if (length(too_big)) {
      message("🔎 Removendo por tamanho > 50MB:")
      print(select(INFO[too_big,], path, size))
      whitelist <- setdiff(whitelist, INFO$path[too_big])
    }
    
    # segurança extra: tirar explicitamente o que bater no denylist
    if (length(deny_globs)) {
      to_deny <- unique(unlist(lapply(deny_globs, \(g) glob2rx(g))) )
      # construir uma regex OR de todos os padrões
      rx <- paste0("(", paste(to_deny, collapse = ")|("), ")")
      bad <- grepl(rx, whitelist, ignore.case = TRUE)
      if (any(bad)) {
        message("🚫 Removendo itens por denylist:")
        print(whitelist[bad])
        whitelist <- whitelist[!bad]
      }
    }
    
    # relatório
    size_mb <- sum(file_info(whitelist)$size)/1024^2
    cat(sprintf("\n✅ Arquivos na whitelist: %d | Tamanho total ~ %.1f MB\n\n",
                length(whitelist), size_mb))
    
    # 8) checagens úteis antes do deploy
    stopifnot("app.R" %in% whitelist)
    if (!any(grepl("^modules/", whitelist))) warning("⚠️ Nenhum arquivo em 'modules/' entrou na whitelist.")
    if (!any(grepl("^utils/", whitelist)))   warning("⚠️ Nenhum arquivo em 'utils/' entrou na whitelist.")
    if (!any(grepl("^www/", whitelist)))     message("ℹ️ Sem pasta 'www' — ok se seu app não usa assets estáticos.")
    if (!any(grepl("^config/database\\.sqlite$", whitelist))) {
      message("ℹ️ 'config/database.sqlite' não está na whitelist — se seu login usa SQLite local, inclua.")
    }

    # --- adicionar esses dados leves esperados pelo app ---
    extra_data <- c(
      "data/escolas_geo_com_empty_flag.rds"
    )
    whitelist <- unique(c(whitelist, extra_data))
    whitelist <- whitelist[file.exists(whitelist)]
    
        
    # 9) deploy (ajuste o appName/account se quiser)
    options(rsconnect.max.bundle.size = 10 * 1024^3)  # só por garantia
    rsconnect::deployApp(
      appDir   = ".",
      appFiles = whitelist,
      appName  = "Explora_dashboard",
      account  = "primeiraescolhadevolutivas",
      server   = "shinyapps.io",
      logLevel = "verbose"
    )
    