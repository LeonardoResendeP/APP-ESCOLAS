# -*- coding: utf-8 -*-
# Pipeline Censo 2022 - Resultados do Universo por Setor Censitário
# Saídas:
#   - data/processed/censo2022_universo_setor.gpkg
#   - data/processed/censo2022_universo_setor.rds
#   - data/processed/censo2022_universo_municipio.rds
#   - data/_debug/censo2022_dic_variaveis.csv

# ---------- Pacotes ----------
pkgs <- c("data.table","dplyr","stringr","readr","readxl","janitor","sf","glue","purrr","tidyr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, type = "binary")
invisible(lapply(pkgs, library, character.only = TRUE))

# ---------- Dirs ----------
dir.create("data/raw/ibge_ftp", TRUE, TRUE)
dir.create("data/processed", TRUE, TRUE)
dir.create("data/_debug", TRUE, TRUE)

# ---------- URLs IBGE (FTP/HTTP) ----------
base_csv <- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/Agregados_por_Setor_csv/"
base_root<- "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios/"

urls <- c(
  alfabet     = paste0(base_csv, "Agregados_por_setores_alfabetizacao_BR.zip"),
  demografia  = paste0(base_csv, "Agregados_por_setores_demografia_BR.zip"),
  basico      = paste0(base_csv, "Agregados_por_setores_basico_BR_20250417.zip"),
  # extras (opcionais) — descomente se quiser trazer já:
  # cor_raca    = paste0(base_csv, "Agregados_por_setores_cor_ou_raca_BR.zip"),
  # dom1        = paste0(base_csv, "Agregados_por_setores_caracteristicas_domicilio1_BR.zip"),
  # dom2        = paste0(base_csv, "Agregados_por_setores_caracteristicas_domicilio2_BR_20250417.zip"),
  # dom3        = paste0(base_csv, "Agregados_por_setores_caracteristicas_domicilio3_BR_20250417.zip"),
  dic_agreg   = paste0(base_root,"dicionario_de_dados_agregados_por_setores_censitarios_20250417.xlsx")
)

# ---------- Download ----------
dest <- file.path("data/raw/ibge_ftp", basename(urls))
mapply(download.file, urls, dest, MoreArgs = list(mode="wb", quiet=FALSE))

# Extrai zips
zips <- dest[str_detect(dest, "\\.zip$", negate = FALSE)]
if (length(zips)) lapply(zips, unzip, exdir = "data/raw/ibge_ftp")

# ---------- Localiza CSVs por tema ----------
all_csv <- list.files("data/raw/ibge_ftp", pattern="\\.csv$", full.names = TRUE)
csv_alf  <- all_csv[str_detect(tolower(all_csv), "alfabet")]
csv_demo <- all_csv[str_detect(tolower(all_csv), "demograf")]
csv_basic<- all_csv[str_detect(tolower(all_csv), "basico")]

stopifnot(length(csv_alf)>0, length(csv_demo)>0, length(csv_basic)>0)

# ---------- Funções auxiliares ----------
clean_names2 <- function(df){
  df <- janitor::clean_names(df)
  names(df) <- gsub("[^a-z0-9_]", "", names(df))
  sub("_+$","", names(df)) |> `names<-`(df, value = _)
}

read_agregado <- function(path_csv){
  dt <- data.table::fread(path_csv, encoding="UTF-8", showProgress=TRUE)
  names(dt) <- tolower(names(dt))
  names(dt) <- gsub("[^a-z0-9_]+","_", names(dt))
  names(dt) <- sub("_+$","", names(dt))
  key <- names(dt)[str_detect(names(dt), "^cd_?setor$")]
  if (length(key)==0) stop(glue("Coluna cd_setor não encontrada em {basename(path_csv)}"))
  data.table::setnames(dt, key[1], "cd_setor")
  dt[, cd_setor := as.character(cd_setor)]
  as.data.frame(dt)
}

# -------- Minerador universal de dicionário IBGE --------
library(readxl); library(dplyr); library(stringr); library(purrr); library(tidyr); library(glue)

load_dic_universal <- function(path_xlsx) {
  if (!file.exists(path_xlsx)) {
    message("⚠️ Dicionário não encontrado: ", path_xlsx)
    return(NULL)
  }
  sheets <- readxl::excel_sheets(path_xlsx)
  
  grab_sheet <- function(sh) {
    # Lê como character pra não perder nada
    x <- suppressWarnings(readxl::read_xlsx(path_xlsx, sheet = sh, col_names = TRUE))
    if (!is.data.frame(x) || !ncol(x)) return(NULL)
    
    # guarda ordem real das colunas para sabermos "qual é a última"
    col_order <- names(x)
    names(x) <- make.names(names(x), unique = TRUE)
    
    long <- x %>%
      mutate(.row = dplyr::row_number()) %>%
      tidyr::pivot_longer(-.row, names_to = ".col", values_to = "val") %>%
      mutate(val = as.character(val)) %>%
      filter(!is.na(val), trimws(val) != "")
    
    # encontra células com Vxxxxx
    vs <- long %>%
      filter(str_detect(val, "^\\s*[Vv]\\d{3,6}\\s*$")) %>%
      mutate(variavel = str_to_lower(str_trim(val))) %>%
      select(.row, .col, variavel)
    
    if (!nrow(vs)) return(NULL)
    
    # candidata a descrição: última coluna preenchida da linha que NÃO é outro Vxxxxx
    desc_cand <- long %>%
      filter(.row %in% vs$.row) %>%
      filter(!str_detect(val, "^\\s*[Vv]\\d{3,6}\\s*$")) %>%
      mutate(col_index = match(.col, make.names(col_order))) %>%   # índice de ordem real
      group_by(.row) %>%
      slice(which.max(col_index)) %>%                              # pega a "mais à direita"
      ungroup() %>%
      transmute(.row, descricao = str_squish(val))
    
    dic <- vs %>%
      left_join(desc_cand, by = ".row") %>%
      mutate(
        sheet = sh,
        descricao = coalesce(descricao, ""),
        descricao_norm = descricao %>%
          stringi::stri_trans_general("Latin-ASCII") %>%
          tolower()
      ) %>%
      select(sheet, variavel, descricao, descricao_norm) %>%
      distinct()
    
    dic
  }
  
  dic_all <- purrr::map_dfr(sheets, grab_sheet)
  if (!nrow(dic_all)) {
    message("⚠️ Ainda não foi possível extrair pares Vxxxxx/descrição.")
    return(NULL)
  }
  dic_all
}

# (mantém sua find_var)
find_var <- function(dic, pattern_vec){
  if (is.null(dic) || !nrow(dic)) return(NA_character_)
  for (p in pattern_vec){
    hit <- dic %>% filter(str_detect(descricao_norm, p)) %>% slice(1)
    if (nrow(hit)) return(hit$variavel)
  }
  NA_character_
}

# ---- carregar o dicionário usando o minerador universal ----
dic_path <- file.path("data/raw/ibge_ftp", "dicionario_de_dados_agregados_por_setores_censitarios_20250417.xlsx")
dic <- load_dic_universal(dic_path)

if (!is.null(dic) && nrow(dic)) {
  readr::write_csv(dic, "data/_debug/censo2022_dic_variaveis.csv", na = "")
  cat(glue("✅ Dicionário minerado: {nrow(dic)} pares Vxxxxx/descrição extraídos.\n"))
} else {
  cat("⚠️ Dicionário segue vazio; vamos para o fallback de inspecionar os CSVs.\n")
}
Se ainda assim não vier nada do dicionário…
Sem drama. A gente consegue sem dicionário lendo só o cabeçalho dos CSVs e você me mostra os nomes das colunas — eu devolvo os códigos exatos para colar no vars_manual. Execute:
  
  r
Copy
Edit
csv_alf  <- list.files("data/raw/ibge_ftp", pattern="alfabet.*\\.csv$", full.names = TRUE)[1]
csv_demo <- list.files("data/raw/ibge_ftp", pattern="demograf.*\\.csv$", full.names = TRUE)[1]
csv_basic<- list.files("data/raw/ibge_ftp", pattern="basico.*\\.csv$", full.names = TRUE)[1]

cab_alf  <- names(data.table::fread(csv_alf,  nrows = 0))
cab_demo <- names(data.table::fread(csv_demo, nrows = 0))
cab_basic<- names(data.table::fread(csv_basic,nrows = 0))

cat("\nALFABETIZAÇÃO (primeiras 60 colunas):\n"); print(utils::head(cab_alf, 60))
cat("\nDEMOGRAFIA (primeiras 100 colunas):\n");   print(utils::head(cab_demo, 100))
cat("\nBÁSICO (todas):\n");                      print(cab_basic)

readr::write_csv(dic, "data/_debug/censo2022_dic_variaveis.csv", na = "")

alf  <- read_agregado(csv_alf[1])
demo <- read_agregado(csv_demo[1])
basi <- read_agregado(csv_basic[1])

# ========= 1) Coagir variáveis Vxxxxx para numéricas =========
force_vars_numeric <- function(df) {
  vcols <- grep("^v\\d+", names(df), value = TRUE, ignore.case = TRUE)
  if (length(vcols)) {
    df[vcols] <- lapply(df[vcols], function(x) {
      # remove tudo que não for dígito, sinal ou ponto e converte
      as.numeric(gsub("[^0-9\\-\\.]", "", x))
    })
  }
  df
}

alf  <- force_vars_numeric(alf)
demo <- force_vars_numeric(demo)
basi <- force_vars_numeric(basi)

stopifnot(is.numeric(demo[["v01011"]]))  # sanity: 10–14 agora é numérico

# ========= 2) Confirmar/validar 15–17 =========
v_0_4   <- "v01009"
v_5_9   <- "v01010"
v_10_14 <- "v01011"

# Heurística: sequência etária -> 15–17 costuma ser o próximo código
v_15_17 <- "v01012"

# Checagem: média(15–17) deve ser < média(10–14) (faixa menor)
chk_1517 <- mean(demo[[v_15_17]], na.rm = TRUE) < mean(demo[[v_10_14]], na.rm = TRUE)
cat("Validação 15–17 < 10–14: ", ifelse(isTRUE(chk_1517), "OK", "VERIFICAR"), "\n")

# ========= 3) Construir total 15+ a partir da DEMOGRAFIA =========
# Pega todas as colunas do bloco V010** a partir de v01012 (inclusive)
v_demo <- grep("^v010\\d\\d$", names(demo), value = TRUE)
idx_start <- match(v_15_17, v_demo)
stopifnot(!is.na(idx_start))

v_15mais_bloc <- v_demo[idx_start:length(v_demo)]
total_15mais_demo <- rowSums(demo[v_15mais_bloc], na.rm = TRUE)

# Guardar para uso posterior no join/feature
demo$tot_15mais <- total_15mais_demo

# ========= 4) Descobrir automaticamente “alfabetizados 15+” no arquivo de alfabetização =========
# Estratégia: escolher a coluna de ALF com:
#  - alta correlação com total_15mais_demo
#  - média <= média(total_15mais_demo)
#  - e razão média ~ 0.6–1.0 (alfabetizados não pode superar total 15+)
cand_alf <- grep("^v00(6|7)\\d\\d$", names(alf), value = TRUE)

# calcula métricas por coluna candidata
score_alf <- lapply(cand_alf, function(v) {
  x <- alf[[v]]
  if (!is.numeric(x)) return(NULL)
  if (all(is.na(x))) return(NULL)
  # correlação (protege contra variância zero)
  corv <- suppressWarnings(cor(x, total_15mais_demo, use = "pairwise.complete.obs"))
  m_x  <- mean(x, na.rm = TRUE)
  m_t  <- mean(total_15mais_demo, na.rm = TRUE)
  ratio <- m_x / m_t
  data.frame(var = v, cor = corv, mean = m_x, ratio = ratio)
})
score_alf <- do.call(rbind, score_alf)
score_alf <- subset(score_alf, is.finite(cor) & !is.na(cor) & ratio > 0 & ratio <= 1.05)

# Prioriza maior correlação; desempata pelo quão perto a média fica do total (ratio mais próximo de 1)
if (nrow(score_alf)) {
  score_alf <- score_alf[order(-score_alf$cor, -pmin(score_alf$ratio, 1/score_alf$ratio)), ]
  v_alf15   <- score_alf$var[1]
  cat("Variável candidata para 'alfabetizados 15+': ", v_alf15,
      " | correlação=", round(score_alf$cor[1], 4),
      " | razão média=", round(score_alf$ratio[1], 3), "\n", sep = "")
} else {
  stop("Não foi possível identificar automaticamente a coluna de 'alfabetizados 15+'. Verifique o dicionário.")
}

# Validação simples: máximo de alfabetizados 15+ não deve exceder máximo do total 15+
ok_bounds <- max(alf[[v_alf15]], na.rm = TRUE) <= max(total_15mais_demo, na.rm = TRUE)
cat("Bound check alfabetizados15+ <= total15+: ", ifelse(ok_bounds, "OK", "VERIFICAR"), "\n")

# ========= 5) Montar features-chave por setor (apenas colunas essenciais) =========
# Chaves
stopifnot("cd_setor" %in% names(alf), "cd_setor" %in% names(demo), "cd_setor" %in% names(basi))
key <- "cd_setor"

feat_demo <- demo[, c(key, v_0_4, v_5_9, v_10_14, v_15_17, "tot_15mais")]
names(feat_demo) <- c("cd_setor", "pop_0_4", "pop_5_9", "pop_10_14", "pop_15_17", "pop_15mais_total")

feat_alf  <- alf[, c(key, v_alf15)]
names(feat_alf) <- c("cd_setor", "alf_15mais")

# DPP (domicílios particulares permanentes) – v0001 no arquivo básico
if (!"v0001" %in% names(basi)) {
  stop("No arquivo básico não encontrei v0001 (DPP total) – confira se está atualizado.")
}
feat_dpp <- basi[, c(key, "v0001")]
names(feat_dpp) <- c("cd_setor", "dpp_total")

# ========= 6) Consolidar e salvar =========
feat <- feat_demo |>
  dplyr::left_join(feat_alf, by = "cd_setor") |>
  dplyr::left_join(feat_dpp, by = "cd_setor")

# sanity final: alfabetizados 15+ não pode ultrapassar pop 15+
bad <- sum(feat$alf_15mais > feat$pop_15mais_total, na.rm = TRUE)
cat("Setores com alf_15+ > pop_15+: ", bad, "\n")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
readr::write_csv(feat, "data/processed/censo2022_setor_features_min.csv")
saveRDS(feat, "data/processed/censo2022_setor_features_min.rds")
cat("OK! Salvos:\n - data/processed/censo2022_setor_features_min.csv\n - data/processed/censo2022_setor_features_min.rds\n")


# ---------- Construção dos indicadores (agora com todos os códigos) ----------
vars <- list(
  idade_0_4   = v_0_4,
  idade_5_9   = v_5_9,
  idade_10_14 = v_10_14,
  idade_15_17 = v_15_17,
  alfabet_15p = v_alf15,
  dpp_total   = v_dpp
)
print(vars)

# Exemplo de join final e amostra (ajuste 'setores' se já estiver lido como sf da malha 2022)
agreg_core <- demo[, c("cd_setor", v_0_4, v_5_9, v_10_14, v_15_17)] |>
  dplyr::left_join(alf[, c("cd_setor", v_alf15)], by = "cd_setor") |>
  dplyr::left_join(basi[, c("cd_setor", v_dpp, "v0003")], by = "cd_setor") |>
  dplyr::rename(
    pop_0_4   = !!v_0_4,
    pop_5_9   = !!v_5_9,
    pop_10_14 = !!v_10_14,
    pop_15_17 = !!v_15_17,
    alfabet_15p = !!v_alf15,
    dpp = !!v_dpp,
    total_pessoas = v0003
  )

readr::write_csv(utils::head(agreg_core, 200), "data/_debug/amostra_agreg_core.csv")
cat("\n✅ Salvamos uma amostra em data/_debug/amostra_agreg_core.csv\n")

# Se quiser o GPKG com tudo unido nos polígonos (supondo 'setores' já lido):
# joined_full <- dplyr::left_join(setores, agreg_core, by = c("cd_setor" = "cd_setor"))
# sf::st_write(joined_full, "data/processed/censo2022_setor_core.gpkg", delete_dsn = TRUE)
# ---------- Selecionar e renomear ----------
sel_demo <- demo |> select(cd_setor,
                           !!v_0_4, !!v_5_9, !!v_10_14, !!v_15_17)
names(sel_demo) <- c("cd_setor","pop_0_4","pop_5_9","pop_10_14","pop_15_17")

sel_alf  <- alf |> select(cd_setor, !!v_alf15)
names(sel_alf) <- c("cd_setor","alfabet_15mais")

sel_basi <- basi |> select(cd_setor, !!v_dpp)
names(sel_basi) <- c("cd_setor","domic_pp_total")

# ---------- Consolidar por setor ----------
agreg_setor <- sel_basi |>
  left_join(sel_demo, by="cd_setor") |>
  left_join(sel_alf,  by="cd_setor") |>
  mutate(
    across(where(is.character), ~ .x),
    across(-cd_setor, ~ suppressWarnings(as.numeric(.x))),
    tam_infantil = pop_0_4,
    tam_fund_I   = pop_5_9,
    tam_fund_II  = pop_10_14,
    tam_medio    = pop_15_17,
    tam_total_0_17 = rowSums(across(c(pop_0_4,pop_5_9,pop_10_14,pop_15_17)), na.rm=TRUE),
    cod_municipio_ibge = substr(cd_setor, 1, 7)
  )

# ---------- Malha Setores 2022 (a que você já baixou) ----------
# espere junto ao .shp os arquivos .dbf/.shx/.prj
shp_path <- "data/raw/BR_Malha_Preliminar_2022.shp"
stopifnot(file.exists(shp_path))

setores <- suppressWarnings(sf::st_read(shp_path, quiet = TRUE, options = "ENCODING=UTF-8"))
names(setores) <- tolower(names(setores))
names(setores) <- gsub("[^a-z0-9_]+","_", names(setores))
names(setores) <- sub("_+$","", names(setores))
key_shp <- names(setores)[str_detect(names(setores), "(?i)^cd_?setor$")][1]
stopifnot(length(key_shp)==1)
setores[[key_shp]] <- as.character(setores[[key_shp]])

# Join espacial
setores_kpi <- setores |>
  select(cd_setor = all_of(key_shp), geometry) |>
  left_join(agreg_setor, by="cd_setor")

# ---------- Agregado Municipal ----------
muni_agg <- setores_kpi |>
  st_drop_geometry() |>
  group_by(cod_municipio_ibge) |>
  summarise(
    tam_infantil     = sum(tam_infantil, na.rm=TRUE),
    tam_fund_I       = sum(tam_fund_I,   na.rm=TRUE),
    tam_fund_II      = sum(tam_fund_II,  na.rm=TRUE),
    tam_medio        = sum(tam_medio,    na.rm=TRUE),
    tam_total_0_17   = sum(tam_total_0_17, na.rm=TRUE),
    alfabet_15mais   = sum(alfabet_15mais, na.rm=TRUE),
    domic_pp_total   = sum(domic_pp_total, na.rm=TRUE),
    .groups = "drop"
  )

# ---------- Salvar ----------
gpkg_out <- "data/processed/censo2022_universo_setor.gpkg"
if (file.exists(gpkg_out)) unlink(gpkg_out)
sf::st_write(setores_kpi, gpkg_out, quiet = TRUE)

saveRDS(st_drop_geometry(setores_kpi), "data/processed/censo2022_universo_setor.rds")
saveRDS(muni_agg, "data/processed/censo2022_universo_municipio.rds")

cat(glue("\n✅ Pronto!\n- {gpkg_out}\n- data/processed/censo2022_universo_setor.rds\n- data/processed/censo2022_universo_municipio.rds\n"))
cat(glue("CRS: {sf::st_crs(setores_kpi)$input}\n"))
