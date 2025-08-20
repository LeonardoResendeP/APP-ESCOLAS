# diagnostics/coverage_exec_map.R
suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(leaflet)
  library(htmlwidgets)
  library(RColorBrewer)
})

# =========================
# Utilitários & Formatação
# =========================
arg_or_default <- function(flag, default = NA) {
  args <- commandArgs(trailingOnly = TRUE)
  hit  <- which(args == flag)
  if (!length(hit) || hit == length(args)) return(default)
  val <- args[hit + 1]
  if (is.na(val) || val == "" || startsWith(val, "--")) return(default)
  val
}
first_col <- function(df, candidates) {
  cands <- candidates[candidates %in% names(df)]
  if (length(cands)) cands[[1]] else NULL
}
norm_key <- function(x) gsub("\\D", "", as.character(x))

fmt_num <- function(x, digits = 0) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—",
         formatC(round(x, digits), big.mark=".", decimal.mark=",",
                 format="f", digits=digits))
}
fmt_pct <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), "—", paste0(formatC(x, format="f", digits=digits), "%"))
}
delta_pct_val <- function(old, new) {
  old <- as.numeric(old); new <- as.numeric(new)
  out <- rep(NA_real_, length(old))
  ok  <- !is.na(old) & old > 0 & !is.na(new)
  out[ok] <- 100 * (new[ok] - old[ok]) / old[ok]
  # quando ambos ~0, defina 0%
  both_small <- is.na(out) & !is.na(old) & !is.na(new) & (abs(old) <= 1e-9 & abs(new) <= 1e-9)
  out[both_small] <- 0
  out
}

mk_quantile_pal <- function(vals, n = 7, palette = "YlOrRd") {
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(colorBin("Greys", bins = 3, na.color = "#cccccc"))
  qs <- unique(unname(quantile(vals, probs = seq(0,1,length.out = n+1), na.rm = TRUE)))
  if (length(qs) < 3) qs <- pretty(range(vals, na.rm = TRUE), n = 7)
  colorBin(RColorBrewer::brewer.pal(max(3, min(9, length(qs)-1)), palette),
           domain = vals, bins = qs, na.color = "#cccccc")
}
mk_diverging_pal <- function(vals) {
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(colorBin(RColorBrewer::brewer.pal(11,"RdBu"), bins = 3, na.color="#cccccc"))
  rng  <- range(vals, na.rm = TRUE)
  lim  <- max(abs(rng[1]), abs(rng[2]))
  if (!is.finite(lim) || lim == 0) lim <- 1
  brks <- unique(c(-lim, -20, -10, -5, -2, 0, 2, 5, 10, 20, lim))
  colorBin(RColorBrewer::brewer.pal(11, "RdBu"), domain = vals, bins = brks, na.color = "#cccccc")
}
fmt_legenda <- function(type, cuts) {
  format(cuts, big.mark = ".", decimal.mark = ",")
}

# EPSG: UTM / SIRGAS 2000 zona automática (fallback 31983)
pick_utm_epsg <- function(lon, lat) {
  zone <- floor((lon + 180)/6) + 1
  epsg <- 31960 + zone
  if (is.na(epsg) || epsg < 31961 || epsg > 31985) epsg <- 31983
  list(epsg = epsg, zone = zone)
}
to_projected_point <- function(lon, lat, epsg) {
  p_ll <- st_sfc(st_point(c(lon, lat)), crs = 4674)
  st_transform(p_ll, epsg)
}
find_gpkg <- function() {
  cands <- c(
    "data/processed/censo2022_universo_setor_joined_clean.gpkg",
    "data/processed/setor_2022_features_sf.gpkg"
  )
  for (p in cands) if (file.exists(p)) return(p)
  NULL
}

# Normalização de features (2022 e 2025)
normalize_feat22 <- function(feat) {
  if (!nrow(feat)) return(tibble(cd_setor_norm = character(0)))
  f <- feat
  key <- first_col(f, c("cd_setor","CD_SETOR","cd_setor_2022","CD_SETOR_2022"))
  if (is.null(key)) stop("feat22 sem coluna de chave cd_setor.")
  f <- mutate(f, cd_setor_norm = norm_key(.data[[key]]))
  tibble(
    cd_setor_norm = f$cd_setor_norm,
    pop_0a4   = coalesce(f$pop_0a4,   f$pop_0_4),
    pop_5a9   = coalesce(f$pop_5a9,   f$pop_5_9),
    pop_10a14 = coalesce(f$pop_10a14, f$pop_10_14),
    pop_15a17 = coalesce(f$pop_15a17, f$pop_15_17),
    dpp_total = f$dpp_total,
    tam_total_0_17 = f$tam_total_0_17
  ) |> distinct(cd_setor_norm, .keep_all = TRUE)
}
normalize_feat25 <- function(feat) {
  if (!nrow(feat)) return(tibble(cd_setor_norm = character(0)))
  f <- feat
  key <- first_col(f, c("cd_setor","CD_SETOR","cd_setor_2022","CD_SETOR_2022"))
  if (is.null(key)) stop("feat25 sem coluna de chave cd_setor.")
  f <- mutate(f, cd_setor_norm = norm_key(.data[[key]]))
  tibble(
    cd_setor_norm = f$cd_setor_norm,
    pop_0a4_2025   = coalesce(f$pop_0a4_2025,   f$pop_0_4_2025),
    pop_5a9_2025   = coalesce(f$pop_5a9_2025,   f$pop_5_9_2025),
    pop_10a14_2025 = coalesce(f$pop_10a14_2025, f$pop_10_14_2025),
    pop_15a17_2025 = coalesce(f$pop_15a17_2025, f$pop_15_17_2025),
    tam_total_0_17_2025 = f$tam_total_0_17_2025
  ) |> distinct(cd_setor_norm, .keep_all = TRUE)
}

# =========================
# Parâmetros de execução
# =========================
codinep <- suppressWarnings(as.numeric(arg_or_default("--codinep", 35104991)))
lon     <- suppressWarnings(as.numeric(arg_or_default("--lon", NA)))
lat     <- suppressWarnings(as.numeric(arg_or_default("--lat", NA)))
radii_m <- c(1000, 2000, 5000)    # buffers (resumo por raio)
base_radius <- max(radii_m)       # raio usado no choropleth (executivo)

# Escola principal (RDS)
if (!is.na(codinep)) {
  fpe <- file.path("data", "escolas", sprintf("%s.rds", codinep))
  if (file.exists(fpe)) {
    es <- readRDS(fpe)
    if (is.na(lon) || is.na(lat)) {
      lon <- suppressWarnings(as.numeric(es$longitude))
      lat <- suppressWarnings(as.numeric(es$latitude))
    }
  } else {
    stop("RDS da escola não encontrado: ", fpe)
  }
}
if (is.na(lon) || is.na(lat)) stop("Passe --codinep válido OU --lon e --lat.")
dir.create("outputs/coverage", recursive = TRUE, showWarnings = FALSE)

# Extrair nome/município e um pctl do ENEM (se existir no RDS)
nome_escola_principal     <- if ("nome_escola"   %in% names(es)) es$nome_escola   else "Escola Principal"
municipio_escola_principal<- if ("nome_municipio"%in% names(es)) es$nome_municipio else "Município"
enem_pctl_from_es <- NA_real_
if ("dados_enem_areas" %in% names(es) && !is.null(es$dados_enem_areas)) {
  dea <- tryCatch(as.data.frame(es$dados_enem_areas), error = function(e) NULL)
  if (!is.null(dea) && nrow(dea)) {
    # tente achar alguma coluna de percentil
    pcols <- intersect(names(dea), c("percentil","percentil_brasil","pctl","pctl_enem","percentil_municipio"))
    if (length(pcols)) enem_pctl_from_es <- suppressWarnings(as.numeric(round(mean(as.numeric(dea[[pcols[1]]]), na.rm = TRUE))))
  }
}

# =========================
# Setores: leitura & join
# =========================
gpkg <- find_gpkg()
if (is.null(gpkg)) stop("GPKG de setores não encontrado em data/processed/.")
layer_name <- sf::st_layers(gpkg)$name[1]
message(sprintf("[GPKG] %s | layer: %s", gpkg, layer_name))

bbox_size_deg <- 0.06
bbox <- st_bbox(c(xmin = lon - bbox_size_deg, xmax = lon + bbox_size_deg,
                  ymin = lat - bbox_size_deg, ymax = lat + bbox_size_deg),
                crs = st_crs(4674))
wkt_filter <- st_as_text(st_as_sfc(bbox))
t0 <- proc.time()
setores_raw <- tryCatch(
  suppressWarnings(st_read(gpkg, wkt_filter = wkt_filter, quiet = TRUE)),
  error = function(e) {
    message("wkt_filter não suportado; lendo layer completo e recortando por bbox...")
    dd <- suppressWarnings(st_read(gpkg, quiet = TRUE))
    st_crop(dd, bbox)
  }
)
message(sprintf("[Leitura] %0.2fs", (proc.time() - t0)[3]))
if (!nrow(setores_raw)) stop("Nenhum setor lido na área do bbox.")

cds <- first_col(setores_raw, c("cd_setor","CD_SETOR","cd_setor_2022","CD_SETOR_2022"))
if (is.null(cds)) stop("Não encontrei coluna 'cd_setor' no GPKG.")
setores_raw <- setores_raw |>
  dplyr::rename(cd_setor = !!cds) |>
  dplyr::mutate(cd_setor_norm = norm_key(cd_setor))

mun_col <- first_col(setores_raw, c("cod_municipio_ibge","CD_MUN","CD_MUNICIPIO",
                                    "cod_municipio_ibge_22","cod_municipio_ibge_25"))
if (is.null(mun_col)) {
  setores_raw$cod_municipio_ibge <- NA_character_
} else if (mun_col != "cod_municipio_ibge") {
  setores_raw <- dplyr::mutate(setores_raw, cod_municipio_ibge = .data[[mun_col]])
}

old_s2 <- sf::sf_use_s2(); sf::sf_use_s2(FALSE)
on.exit({ try(sf::sf_use_s2(old_s2), silent = TRUE) }, add = TRUE)
setores_raw <- suppressWarnings(st_make_valid(setores_raw))
gt <- unique(as.character(st_geometry_type(setores_raw)))
if (any(grepl("GEOMETRYCOLLECTION|MULTISURFACE", gt))) {
  setores_raw <- suppressWarnings(st_collection_extract(setores_raw, "POLYGON"))
}
setores_raw <- suppressWarnings(st_buffer(setores_raw, 0))

# Features
feat22 <- if (file.exists("data/processed/censo2022_setor_features_min.rds")) {
  readRDS("data/processed/censo2022_setor_features_min.rds") |> normalize_feat22()
} else tibble(cd_setor_norm = character(0))
feat25 <- if (file.exists("data/processed/nowcast_setor_2025.rds")) {
  readRDS("data/processed/nowcast_setor_2025.rds") |> normalize_feat25()
} else tibble(cd_setor_norm = character(0))

geom_col <- attr(setores_raw, "sf_column")
setores_min <- setores_raw |>
  dplyr::select(cd_setor, cd_setor_norm, cod_municipio_ibge, !!geom_col) |>
  dplyr::left_join(feat22, by = "cd_setor_norm") |>
  dplyr::left_join(feat25, by = "cd_setor_norm")

# Projeção e buffers
epsg <- pick_utm_epsg(lon, lat)$epsg
setores_utm <- st_transform(setores_min, epsg) |> suppressWarnings(st_make_valid())
setores_utm$area_setor_m2 <- as.numeric(st_area(setores_utm))
pt_utm <- to_projected_point(lon, lat, epsg)

buffers_ll <- do.call(rbind, lapply(radii_m, function(rm) {
  b <- st_buffer(pt_utm, rm) |> st_transform(4326)
  st_sf(raio_m = rm, geometry = b)
}))

# =========================
# Agregação por raio
# =========================
aggr_buffer <- function(rad_m) {
  buf <- st_buffer(pt_utm, rad_m)
  inter <- suppressWarnings(st_intersection(setores_utm, buf))
  if (!nrow(inter)) {
    return(list(
      resumo        = tibble(raio_m = rad_m, n_setores = 0, tem_2022 = 0, tem_2025 = 0, tem_ambos = 0, nenhum = 0),
      detalhe       = st_sfc(crs = 4326),
      buffer_stats  = tibble(raio_m = rad_m, n_setores = 0, n_municipios = 0)
    ))
  }
  
  inter$area_int_m2 <- as.numeric(st_area(inter))
  inter_df <- inter |>
    st_drop_geometry() |>
    mutate(
      area_prop = pmin(1, area_int_m2 / area_setor_m2),
      has22 = rowSums(cbind(!is.na(pop_0a4), !is.na(pop_5a9), !is.na(pop_10a14),
                            !is.na(pop_15a17), !is.na(dpp_total), !is.na(tam_total_0_17)), na.rm=TRUE) > 0,
      has25 = rowSums(cbind(!is.na(pop_0a4_2025), !is.na(pop_5a9_2025), !is.na(pop_10a14_2025),
                            !is.na(pop_15a17_2025), !is.na(tam_total_0_17_2025)), na.rm=TRUE) > 0,
      cobertura = case_when(
        has22 & has25 ~ "ambos",
        has22 & !has25 ~ "apenas_2022",
        !has22 & has25 ~ "apenas_2025",
        TRUE ~ "nenhum"
      ),
      # estimativas dentro do buffer (proporção de área)
      est_p04_22   = area_prop * as.numeric(pop_0a4),
      est_p59_22   = area_prop * as.numeric(pop_5a9),
      est_p1014_22 = area_prop * as.numeric(pop_10a14),
      est_p1517_22 = area_prop * as.numeric(pop_15a17),
      est_dom_22   = area_prop * as.numeric(dpp_total),
      est_p04_25   = area_prop * as.numeric(pop_0a4_2025),
      est_p59_25   = area_prop * as.numeric(pop_5a9_2025),
      est_p1014_25 = area_prop * as.numeric(pop_10a14_2025),
      est_p1517_25 = area_prop * as.numeric(pop_15a17_2025)
    ) |>
    mutate(
      est_p017_22 = est_p04_22 + est_p59_22 + est_p1014_22 + est_p1517_22,
      est_p017_25 = est_p04_25 + est_p59_25 + est_p1014_25 + est_p1517_25,
      var_pct_mid = delta_pct_val(est_p017_22, est_p017_25)
    )
  
  by_setor <- inter_df |>
    group_by(cd_setor_norm) |>
    slice_max(order_by = area_int_m2, n = 1, with_ties = FALSE) |>
    ungroup()
  
  n_tot   <- nrow(by_setor)
  n_22    <- sum(by_setor$has22, na.rm = TRUE)
  n_25    <- sum(by_setor$has25, na.rm = TRUE)
  n_ambos <- sum(by_setor$cobertura == "ambos", na.rm = TRUE)
  n_none  <- sum(by_setor$cobertura == "nenhum", na.rm = TRUE)
  
  # saída por setor (geometria + campos) em WGS84
  inter_out <- inter |>
    left_join(by_setor |> select(
      cd_setor_norm, cobertura,
      est_p04_22, est_p59_22, est_p1014_22, est_p1517_22, est_p017_22, est_dom_22,
      est_p04_25, est_p59_25, est_p1014_25, est_p1517_25, est_p017_25, var_pct_mid
    ), by = "cd_setor_norm") |>
    mutate(raio_m = rad_m) |>
    st_transform(4326)
  
  # densidade setorial (25) baseada na área de interseção
  inter_out$dens_25 <- with(st_drop_geometry(inter_out),
                            ifelse(area_int_m2 > 0, est_p017_25 / (area_int_m2/1e6), NA_real_))
  
  st_write(inter_out,
           file.path("outputs","coverage", sprintf("buffer_%s_%dm.gpkg", ifelse(is.na(codinep), "custom", codinep), rad_m)),
           layer = "setores_intersectados", quiet = TRUE)
  
  buf_stats <- inter_df |>
    summarise(
      raio_m = rad_m,
      n_setores = n_distinct(cd_setor_norm),
      n_municipios = n_distinct(cod_municipio_ibge, na.rm = TRUE),
      p04_22   = sum(est_p04_22,   na.rm = TRUE),
      p59_22   = sum(est_p59_22,   na.rm = TRUE),
      p1014_22 = sum(est_p1014_22, na.rm = TRUE),
      p1517_22 = sum(est_p1517_22, na.rm = TRUE),
      p017_22  = sum(est_p017_22,  na.rm = TRUE),
      dom_22   = sum(est_dom_22,   na.rm = TRUE),
      p04_25   = sum(est_p04_25,   na.rm = TRUE),
      p59_25   = sum(est_p59_25,   na.rm = TRUE),
      p1014_25 = sum(est_p1014_25, na.rm = TRUE),
      p1517_25 = sum(est_p1517_25, na.rm = TRUE),
      p017_25  = sum(est_p017_25,  na.rm = TRUE)
    ) |>
    mutate(
      p04_var   = delta_pct_val(p04_22,   p04_25),
      p59_var   = delta_pct_val(p59_22,   p59_25),
      p1014_var = delta_pct_val(p1014_22, p1014_25),
      p1517_var = delta_pct_val(p1517_22, p1517_25),
      p017_var  = delta_pct_val(p017_22,  p017_25)
    )
  
  list(
    resumo        = tibble(raio_m = rad_m, n_setores = n_tot, tem_2022 = n_22, tem_2025 = n_25, tem_ambos = n_ambos, nenhum = n_none),
    detalhe       = inter_out,
    buffer_stats  = buf_stats
  )
}

res_list <- lapply(radii_m, aggr_buffer)
resumo <- bind_rows(lapply(res_list, `[[`, "resumo"))
detalhe_all <- do.call(rbind, lapply(res_list, `[[`, "detalhe"))
buffer_stats_df <- bind_rows(lapply(res_list, `[[`, "buffer_stats"))

# Camada base (raio executivo)
di_base <- detalhe_all %>% filter(raio_m == base_radius)

# =========================
# Concorrentes (top-5 por distância)
# =========================
# Ponto da escola (WGS84)
pt_escola <- st_sfc(st_point(c(lon, lat)), crs = 4326)

get_competitors_points <- function(es, pt_escola) {
  lc <- NULL
  if ("lista_concorrentes" %in% names(es)) lc <- es$lista_concorrentes
  if (is.null(lc)) return(NULL)
  if (!inherits(lc, "sf")) {
    # Tente cruzar IDs com base de pontos (RDS ou GPKG)
    epi <- NULL
    fp_epi_rds  <- "data/processed/escolas_privadas_unificadas_sf.rds"
    fp_epi_gpkg <- "data/processed/escolas_privadas_unificadas_sf.gpkg"
    if (file.exists(fp_epi_rds)) epi <- tryCatch(readRDS(fp_epi_rds), error = function(e) NULL)
    if (is.null(epi) && file.exists(fp_epi_gpkg)) epi <- tryCatch(sf::read_sf(fp_epi_gpkg), error = function(e) NULL)
    if (is.null(epi)) return(NULL)
    idcol <- first_col(lc, c("id_escola","ID_ESCOLA","codinep","CODINEP"))
    nmcol <- first_col(lc, c("nome_escola","NO_ENTIDADE","nome"))
    if (is.null(idcol)) return(NULL)
    lc_df <- as_tibble(lc) %>%
      transmute(id_escola = as.character(.data[[idcol]]),
                nome_escola = if (!is.null(nmcol)) as.character(.data[[nmcol]]) else as.character(.data[[idcol]]))
    if (inherits(epi, "sf")) {
      epi_sf <- epi %>% st_transform(4326)
      epi_sf$id_escola <- as.character(epi_sf$id_escola)
      cand <- lc_df %>% left_join(epi_sf %>% st_drop_geometry() %>% select(id_escola), by = "id_escola") %>%
        left_join(epi_sf %>% select(id_escola, geometry), by = "id_escola") %>% st_as_sf()
    } else return(NULL)
  } else {
    cand <- lc
    if (is.na(st_crs(cand))) st_crs(cand) <- 4326
    cand <- st_transform(cand, 4326)
    idcol <- first_col(cand, c("id_escola","ID_ESCOLA","codinep","CODINEP"))
    nmcol <- first_col(cand, c("nome_escola","NO_ENTIDADE","nome"))
    cand$id_escola  <- as.character(cand[[idcol]])
    cand$nome_escola<- if (!is.null(nmcol)) as.character(cand[[nmcol]]) else cand$id_escola
  }
  # distância para ponto da escola (vetor; y é único)
  cand$dist_km <- as.numeric(st_distance(st_geometry(cand), pt_escola)) / 1000
  cand <- cand %>% filter(id_escola != as.character(codinep)) %>% arrange(dist_km)
  # pctl (se houver algo)
  if (!"pctl" %in% names(cand)) cand$pctl <- NA_real_
  head(cand, 5) %>% st_transform(4326)
}

conc_points <- tryCatch(get_competitors_points(es, pt_escola), error = function(e) NULL)

# =========================
# Paletas e Cards
# =========================
pal_dens <- mk_quantile_pal(di_base$dens_25, n = 7, palette = "YlOrRd")
pal_var  <- mk_diverging_pal(di_base$var_pct_mid)

buffer_card_html <- function(bs) {
  v <- function(n) if (n %in% names(bs)) bs[[n]] else NA
  rows <- paste0(
    "<tr><td>Pop. 0–4</td><td>", fmt_num(v('p04_22'),0), "</td><td>", fmt_num(v('p04_25'),0),
    "</td><td>", fmt_pct(v('p04_var'),2), "</td></tr>",
    "<tr><td>Pop. 5–9</td><td>", fmt_num(v('p59_22'),0), "</td><td>", fmt_num(v('p59_25'),0),
    "</td><td>", fmt_pct(v('p59_var'),2), "</td></tr>",
    "<tr><td>Pop. 10–14</td><td>", fmt_num(v('p1014_22'),0), "</td><td>", fmt_num(v('p1014_25'),0),
    "</td><td>", fmt_pct(v('p1014_var'),2), "</td></tr>",
    "<tr><td>Pop. 15–17</td><td>", fmt_num(v('p1517_22'),0), "</td><td>", fmt_num(v('p1517_25'),0),
    "</td><td>", fmt_pct(v('p1517_var'),2), "</td></tr>",
    "<tr><td style='font-weight:600;'>Total 0–17</td><td style='font-weight:600;'>", fmt_num(v('p017_22'),0),
    "</td><td style='font-weight:600;'>", fmt_num(v('p017_25'),0), "</td><td style='font-weight:600;'>", fmt_pct(v('p017_var'),2), "</td></tr>",
    "<tr><td>Domicílios</td><td>", fmt_num(v('dom_22'),0), "</td><td>—</td><td>—</td></tr>"
  )
  paste0(
    "<div style='font-family:Inter,system-ui,Segoe UI,Arial; font-size:12.5px;",
    "background:#ffffff; border:1px solid #e0e0e0; border-radius:12px; padding:10px 12px; margin-bottom:10px; box-shadow:0 1px 12px rgba(0,0,0,0.06);'>",
    "<div style='font-weight:700; margin-bottom:4px;'>Resumo — Raio ", fmt_num(v('raio_m'),0), " m</div>",
    "<div style='color:#666; margin-bottom:6px;'>Setores: ", fmt_num(v('n_setores'),0),
    " &nbsp;&middot;&nbsp; Municípios: ", fmt_num(v('n_municipios'),0), "</div>",
    "<table style='width:100%; border-collapse:collapse; font-size:12.5px;'>",
    "<tr><th style='text-align:left;'>Indicador</th><th style='text-align:right;'>2022</th><th style='text-align:right;'>2025</th><th style='text-align:right;'>Var%</th></tr>",
    rows,
    "</table>",
    "</div>"
  )
}

popup_sector_card <- function(d) {
  # d é uma linha (data.frame) sem geometria
  # garante numéricos
  num <- function(x) suppressWarnings(as.numeric(x))
  est <- function(n) if (n %in% names(d)) num(d[[n]]) else NA_real_
  html <- paste0(
    "<div style='font-family:Inter,system-ui,Segoe UI,Arial; font-size:12.5px; line-height:1.25;'>",
    "<div style='font-weight:600; margin-bottom:2px;'>Setor: ",
    htmltools::htmlEscape(if ("cd_setor" %in% names(d)) d$cd_setor else if ("cd_setor_norm" %in% names(d)) d$cd_setor_norm else "—"), "</div>",
    "<div style='color:#666; margin-bottom:6px;'>Estimativas dentro do buffer</div>",
    "<table style='width:100%; border-collapse:collapse; font-size:12.5px;'>",
    "<tr><th style='text-align:left;'>Indicador</th><th style='text-align:right;'>2022</th><th style='text-align:right;'>2025</th><th style='text-align:right;'>Var%</th></tr>",
    "<tr><td>Pop. 0–4</td><td style='text-align:right;'>", fmt_num(est('est_p04_22'),0), "</td>",
    "<td style='text-align:right;'>", fmt_num(est('est_p04_25'),0), "</td>",
    "<td style='text-align:right;'>", fmt_pct(delta_pct_val(est('est_p04_22'), est('est_p04_25')),2), "</td></tr>",
    "<tr><td>Pop. 5–9</td><td style='text-align:right;'>", fmt_num(est('est_p59_22'),0), "</td>",
    "<td style='text-align:right;'>", fmt_num(est('est_p59_25'),0), "</td>",
    "<td style='text-align:right;'>", fmt_pct(delta_pct_val(est('est_p59_22'), est('est_p59_25')),2), "</td></tr>",
    "<tr><td>Pop. 10–14</td><td style='text-align:right;'>", fmt_num(est('est_p1014_22'),0), "</td>",
    "<td style='text-align:right;'>", fmt_num(est('est_p1014_25'),0), "</td>",
    "<td style='text-align:right;'>", fmt_pct(delta_pct_val(est('est_p1014_22'), est('est_p1014_25')),2), "</td></tr>",
    "<tr><td>Pop. 15–17</td><td style='text-align:right;'>", fmt_num(est('est_p1517_22'),0), "</td>",
    "<td style='text-align:right;'>", fmt_num(est('est_p1517_25'),0), "</td>",
    "<td style='text-align:right;'>", fmt_pct(delta_pct_val(est('est_p1517_22'), est('est_p1517_25')),2), "</td></tr>",
    "<tr><td style='font-weight:600;'>Total 0–17</td>",
    "<td style='text-align:right; font-weight:600;'>", fmt_num(est('est_p017_22'),0), "</td>",
    "<td style='text-align:right; font-weight:600;'>", fmt_num(est('est_p017_25'),0), "</td>",
    "<td style='text-align:right; font-weight:600;'>", fmt_pct(est('var_pct_mid'),2), "</td></tr>",
    "<tr><td>Domicílios</td><td style='text-align:right;'>", fmt_num(est('est_dom_22'),0), "</td>",
    "<td style='text-align:right;'>—</td><td style='text-align:right;'>—</td></tr>",
    "</table>",
    "<div style='margin-top:6px'>Densidade 0–17 (2025): <b>", fmt_num(est('dens_25'),1), " /km²</b>",
    " &nbsp;&middot;&nbsp; Cobertura: <span style='background:",
    ifelse(!is.null(d$cobertura) && d$cobertura=="ambos","#e8f5e9","#eeeeee"),
    "; border:1px solid #ccc; padding:1px 6px; border-radius:10px;'>", ifelse(!is.null(d$cobertura), d$cobertura, "—"), "</span></div>",
    "<div style='margin-top:2px; color:#666;'>Raio: ", base_radius, " m</div>",
    "</div>"
  )
  html
}

# Cards do topo-direito (um por raio)
cards_html <- paste0(
  lapply(split(buffer_stats_df, buffer_stats_df$raio_m), buffer_card_html),
  collapse = ""
)
panel_html <- paste0(
  "<div style='max-width:320px;'>",
  "<div style='font-family:Inter,system-ui,Segoe UI,Arial; font-weight:700; margin-bottom:6px;'>Resumo por Buffer</div>",
  cards_html,
  "</div>"
)

# =========================
# Escola: números para card
# =========================
# usar agregados do buffer-base
bs_row <- buffer_stats_df %>% filter(raio_m == base_radius)
if (nrow(bs_row) == 1) {
  populacao_total_escola <- bs_row$p017_25
  area_km2 <- (pi * (base_radius^2)) / 1e6
  densidade_escola <- if (is.finite(area_km2) && area_km2 > 0) bs_row$p017_25 / area_km2 else NA_real_
} else {
  populacao_total_escola <- NA_real_
  densidade_escola <- NA_real_
}
pctl_enem_escola <- enem_pctl_from_es

popup_school_card <- function(nome, municipio, dens, pop_total, pctl = NA) {
  pctl_lbl <- ifelse(is.na(pctl), "—", paste0(round(pctl), "º pctl ENEM"))
  paste0(
    "<div style='font-family:Inter,Segoe UI,sans-serif; font-size:13px; line-height:1.5;'>",
    "<div style='font-weight:600; font-size:14px;'>", htmltools::htmlEscape(nome), "</div>",
    "<div style='color:#555;'>", htmltools::htmlEscape(municipio), "</div>",
    "<b>População 0–17 (raio ", base_radius, "m):</b> ", fmt_num(pop_total), "<br>",
    "<b>Densidade 2025:</b> ", fmt_num(dens, 1), " pessoas/km²<br>",
    "<b>Posição no ENEM:</b> ", pctl_lbl,
    "</div>"
  )
}

# =========================
# MAPA EXECUTIVO
# =========================
m <- leaflet(options = leafletOptions(minZoom = 3)) %>%
  addProviderTiles(providers$CartoDB.Positron)

# Escola principal
m <- m %>% addCircleMarkers(
  lng = lon, lat = lat, radius = 7, weight = 2,
  fillColor = "#111111", color = "#ffffff",
  fillOpacity = 1, opacity = 1,
  label = "Sua Escola",
  popup = popup_school_card(
    nome = nome_escola_principal,
    municipio = municipio_escola_principal,
    dens = densidade_escola,
    pop_total = populacao_total_escola,
    pctl = pctl_enem_escola
  ),
  group = "Sua Escola"
)

# Concorrentes (5 mais próximas)
if (!is.null(conc_points) && nrow(conc_points) > 0) {
  if (!"pctl" %in% names(conc_points) || length(conc_points$pctl) != nrow(conc_points))
    conc_points$pctl <- rep(NA_real_, nrow(conc_points))
  popup_conc <- function(nome, dist_km, pctl = NA) {
    pctl_lbl <- ifelse(is.na(pctl), "—", paste0(round(pctl), "º pctl"))
    paste0(
      "<div style='font-family:Inter,system-ui,Segoe UI,Arial; font-size:12.5px;'>",
      "<div style='font-weight:600;'>", htmltools::htmlEscape(nome), "</div>",
      "<div style='color:#555;'>Distância: ", fmt_num(dist_km,1), " km</div>",
      "<div style='margin-top:4px'>ENEM (média das áreas): <b>", pctl_lbl, "</b></div>",
      "</div>"
    )
  }
  m <- m %>%
    addCircleMarkers(
      data = conc_points,
      radius = 6, weight = 1.5,
      fillColor = "#1769aa", color = "#0d47a1",
      fillOpacity = 0.9, opacity = 1,
      label = ~nome_escola,
      popup = purrr::pmap_chr(list(conc_points$nome_escola, conc_points$dist_km, conc_points$pctl), popup_conc),
      group = "Concorrentes"
    )
}

# Buffers (contornos discretos)
for (rm in radii_m) {
  bl <- buffers_ll[buffers_ll$raio_m == rm, ]
  bl_lines <- st_cast(st_boundary(bl), "MULTILINESTRING")
  m <- m %>% addPolylines(data = bl_lines, weight = 2, opacity = 1, color = "#2c3e50",
                          group = "Buffers")
}

# Popups por setor (estilo executivo)
sector_popups <- purrr::map_chr(
  1:nrow(di_base),
  ~ popup_sector_card(di_base[.x, ] %>% sf::st_drop_geometry())
)

# Camada 1 — Densidade 0–17 (2025) com quantis
m <- m %>%
  addPolygons(
    data = di_base,
    fillColor = ~pal_dens(dens_25), fillOpacity = 0.75,
    color = "#666666", weight = 0.3, opacity = 0.7,
    popup = sector_popups,
    group = "Densidade 0–17 (2025)"
  ) %>%
  addLegend(
    position = "bottomleft",
    pal = pal_dens, values = di_base$dens_25,
    title = paste0("Densidade 0–17 (", base_radius, "m)"),
    opacity = 0.9, labFormat = fmt_legenda
  )

# Camada 2 — Variação 22→25 (%), azul=positivo, vermelho=negativo
m <- m %>%
  addPolygons(
    data = di_base,
    fillColor = ~pal_var(var_pct_mid), fillOpacity = 0.65,
    color = "#666666", weight = 0.3, opacity = 0.7,
    popup = sector_popups,
    group = "Variação 22→25 (%)"
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_var, values = di_base$var_pct_mid,
    title = "Variação 22→25 (%)",
    opacity = 0.9, labFormat = fmt_legenda
  )

# Painel executivo (cards de resumo por buffer)
m <- m %>%
  addControl(
    html = panel_html,
    position = "topright"
  )

# Controle de layers (canto superior esquerdo, longe das legendas)
m <- m %>%
  addLayersControl(
    position = "topleft",
    overlayGroups = c("Sua Escola", "Concorrentes", "Buffers",
                      "Densidade 0–17 (2025)", "Variação 22→25 (%)"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Salva HTML
html_path <- file.path("outputs","coverage",
                       sprintf("coverage_map_executivo_%s.html", ifelse(is.na(codinep), "custom", codinep)))
htmlwidgets::saveWidget(m, html_path, selfcontained = TRUE)
message("Mapa executivo salvo em: ", html_path)
