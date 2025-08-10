# ===========================================
#   PIPELINE ÚNICO — Censo 2022 → Setor → 2025
# ===========================================
suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(stringr)
  library(readr)
  library(readxl)
  library(tidyr)
  library(glue)
  library(sf)
})

dir.create("data/_debug",    recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/external",  recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 0) PATHS
# ------------------------------------------------------------
csv_alf   <- list.files("data/raw/ibge_ftp", pattern = "alfabet.*\\.csv$",  full.names = TRUE)[1]
csv_demo  <- list.files("data/raw/ibge_ftp", pattern = "demograf.*\\.csv$", full.names = TRUE)[1]
csv_basic <- list.files("data/raw/ibge_ftp", pattern = "basico.*\\.csv$",   full.names = TRUE)[1]
stopifnot(length(csv_alf) > 0, length(csv_demo) > 0, length(csv_basic) > 0)

proj_xlsx <- "data/external/projecoes_2024_tab1_idade_simples.xlsx"  # começa na linha 6 (skip=5)
stopifnot(file.exists(proj_xlsx))

# ------------------------------------------------------------
# Utils
# ------------------------------------------------------------
.norm_names <- function(x){
  x <- gsub("^\ufeff", "", x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(trimws(x))
  x <- gsub("[^a-z0-9_]+","_", x)
  x <- gsub("_+","_", x)
  sub("^_+|_+$","", x)
}
.looks_like_setor <- function(v){
  v <- as.character(v)
  m <- grepl("^\\d{15,16}[A-Z]?$", v)
  num <- sum(m, na.rm = TRUE); den <- sum(!is.na(v))
  if (den == 0) return(0)
  num/den
}
.as_num_br <- function(x){
  x2 <- suppressWarnings(readr::parse_number(as.character(x),
                                             locale = readr::locale(decimal_mark=",", grouping_mark=".")))
  nas <- is.na(x2) & !is.na(x)
  if (any(nas)) x2[nas] <- suppressWarnings(as.numeric(gsub("[^0-9\\-\\.]", "", x[nas])))
  x2
}

read_agregado_safe <- function(path_csv, debug_dir = "data/_debug"){
  dir.create(debug_dir, recursive = TRUE, showWarnings = FALSE)
  hdr_raw  <- names(data.table::fread(path_csv, nrows = 0, encoding="UTF-8"))
  hdr_norm <- .norm_names(hdr_raw)
  
  dt <- data.table::fread(path_csv, encoding="UTF-8", showProgress=TRUE)
  data.table::setnames(dt, hdr_norm)
  
  # 1) tenta achar a chave por nome
  key_candidates <- names(dt)[stringr::str_detect(names(dt),
                                                  "^cd_?setor$|^id_?setor$|^cod_?setor$|^cdsetor$|^idsetor$|^codsetor$")]
  # 2) se não achou, tenta por conteúdo
  if (!length(key_candidates)) {
    rates <- sapply(names(dt), function(nm) .looks_like_setor(dt[[nm]]))
    key_candidates <- names(dt)[which(rates >= 0.85)]
  }
  if (!length(key_candidates)) {
    # dump p/ debug
    data.table::fwrite(data.table::as.data.table(utils::head(dt, 3)),
                       file.path(debug_dir, paste0("peek_", basename(path_csv), ".csv")))
    writeLines(c("NOMES BRUTOS:", paste0(" - ", hdr_raw), "",
                 "NOMES NORMALIZADOS:", paste0(" - ", names(dt))),
               con=file.path(debug_dir, paste0("header_", basename(path_csv), ".txt")))
    stop(paste0("❌ Não encontrei coluna de setor em ", basename(path_csv),
                ". Veja diagnósticos em ", normalizePath(debug_dir)))
  }
  
  data.table::setnames(dt, key_candidates[1], "cd_setor")
  dt[, cd_setor := as.character(cd_setor)]
  
  # Vxxxx → numérico
  vcols <- grep("^v\\d{3,6}$", names(dt), value = TRUE, ignore.case = TRUE)
  if (length(vcols)) dt[, (vcols) := lapply(.SD, .as_num_br), .SDcols = vcols]
  
  as.data.frame(dt)
}

advance_cohorts_3y <- function(df, factor_births = 1.00){
  df %>%
    mutate(
      pop_0_4   = coalesce(as.numeric(pop_0_4),   0),
      pop_5_9   = coalesce(as.numeric(pop_5_9),   0),
      pop_10_14 = coalesce(as.numeric(pop_10_14), 0),
      pop_15_17 = coalesce(as.numeric(pop_15_17), 0)
    ) %>%
    mutate(
      pop_0_4_2025   = pop_0_4 * factor_births,
      pop_5_9_2025   = (3/5) * pop_0_4   + (2/5) * pop_5_9,
      pop_10_14_2025 = (3/5) * pop_5_9   + (2/5) * pop_10_14,
      pop_15_17_2025 = (3/5) * pop_10_14
    )
}

# ------------------------------------------------------------
# 1) Ler agregados (alfabetização, demografia, básico)
# ------------------------------------------------------------
message("📥 Lendo agregados CSV (alfabetização/demografia/básico)...")
alf  <- read_agregado_safe(csv_alf)
demo <- read_agregado_safe(csv_demo)
basi <- read_agregado_safe(csv_basic)

stopifnot("cd_setor" %in% names(alf),
          "cd_setor" %in% names(demo),
          "cd_setor" %in% names(basi))
stopifnot("v0001"   %in% names(basi))  # DPP total

# ------------------------------------------------------------
# 2) Selecionar faixas etárias + detectar alfabetizados 15+
# ------------------------------------------------------------
v_0_4   <- "v01009"
v_5_9   <- "v01010"
v_10_14 <- "v01011"
v_15_17 <- "v01012"

# total 15+ (somando V010** a partir de 15–17)
v_demo_seq <- grep("^v010\\d\\d$", names(demo), value = TRUE)
idx_1517   <- match(v_15_17, v_demo_seq); stopifnot(!is.na(idx_1517))
total_15mais_demo <- rowSums(demo[, v_demo_seq[idx_1517:length(v_demo_seq)], drop = FALSE], na.rm = TRUE)

# achar automaticamente a coluna de alfabetizados 15+
cand_alf <- grep("^v00(6|7)\\d\\d$", names(alf), value = TRUE)
score_alf <- lapply(cand_alf, function(v) {
  x <- alf[[v]]
  if (!is.numeric(x) || all(is.na(x))) return(NULL)
  data.frame(
    var   = v,
    cor   = suppressWarnings(cor(x, total_15mais_demo, use = "pairwise.complete.obs")),
    ratio = mean(x, na.rm = TRUE) / mean(total_15mais_demo, na.rm = TRUE)
  )
}) |> (\(L) do.call(rbind, L))()
score_alf <- subset(score_alf, is.finite(cor) & !is.na(cor) & ratio > 0 & ratio <= 1.05)
stopifnot(nrow(score_alf) > 0)
score_alf <- score_alf[order(-score_alf$cor, -pmin(score_alf$ratio, 1/score_alf$ratio)), ]
v_alf15   <- score_alf$var[1]
message(glue("✅ 'alfabetizados 15+' candidato: {v_alf15} | cor={round(score_alf$cor[1],4)} | razão média={round(score_alf$ratio[1],3)}"))
stopifnot(max(alf[[v_alf15]], na.rm = TRUE) <= max(total_15mais_demo, na.rm = TRUE))

# ------------------------------------------------------------
# 3) FEATURES 2022 por setor
# ------------------------------------------------------------
feat_demo <- demo[, c("cd_setor", v_0_4, v_5_9, v_10_14, v_15_17)]
names(feat_demo) <- c("cd_setor","pop_0_4","pop_5_9","pop_10_14","pop_15_17")

feat_alf  <- alf[, c("cd_setor", v_alf15)]
names(feat_alf) <- c("cd_setor","alf_15mais")

feat_dpp  <- basi[, c("cd_setor", "v0001")]
names(feat_dpp) <- c("cd_setor","dpp_total")

features_2022 <- feat_demo %>%
  left_join(feat_alf, by = "cd_setor") %>%
  left_join(feat_dpp, by = "cd_setor") %>%
  mutate(
    tam_total_0_17   = rowSums(across(c(pop_0_4, pop_5_9, pop_10_14, pop_15_17)), na.rm = TRUE),
    cod_municipio_ibge = substr(cd_setor, 1, 7),
    cd_uf = substr(cd_setor, 1, 2)
  )

stopifnot(sum(features_2022$pop_0_4, na.rm = TRUE) > 0)

write_csv(features_2022, "data/processed/censo2022_setor_features_min.csv")
saveRDS(features_2022, "data/processed/censo2022_setor_features_min.rds")
message("💾 Salvo 2022 features em data/processed/")

# ------------------------------------------------------------
# 4) FATORES 2022→2025 (projeções UF por idade simples)
# ------------------------------------------------------------
message("📈 Lendo projeções UF (idade simples) e calculando fatores 2022→2025...")
proj_raw <- readxl::read_xlsx(proj_xlsx, skip = 5)
names(proj_raw) <- names(proj_raw) |>
  stringi::stri_trans_general("Latin-ASCII") |>
  tolower() |>
  gsub("[^a-z0-9_]+","_", x = _)

col_idade <- "idade"
col_sexo  <- names(proj_raw)[str_detect(names(proj_raw), "^sexo$")][1]
col_cod   <- names(proj_raw)[str_detect(names(proj_raw), "^cod_?$")][1]
col_sigla <- names(proj_raw)[str_detect(names(proj_raw), "^sigla$")][1]
stopifnot(!is.na(col_idade), !is.na(col_sexo), !is.na(col_cod), !is.na(col_sigla))

proj <- proj_raw %>%
  mutate(sexo_norm = tolower(as.character(.data[[col_sexo]]))) %>%
  filter(sexo_norm %in% c("total","ambos os sexos","ambos")) %>%
  mutate(
    cd_uf    = sprintf("%02d", suppressWarnings(as.integer(.data[[col_cod]]))),
    uf_sigla = toupper(as.character(.data[[col_sigla]]))
  ) %>%
  filter(!is.na(cd_uf), nchar(cd_uf) == 2)

year_cols <- names(proj)[str_detect(names(proj), "^[0-9]{4}$")]
stopifnot(all(c("2022","2025") %in% year_cols))

proj_long <- proj %>%
  select(cd_uf, uf_sigla, idade = all_of(col_idade), all_of(year_cols)) %>%
  mutate(idade = as.integer(gsub("\\D","", as.character(idade)))) %>%
  pivot_longer(cols = all_of(year_cols), names_to = "ano", values_to = "pop") %>%
  mutate(ano = as.integer(ano)) %>%
  filter(!is.na(idade), idade >= 0, idade <= 120, ano %in% c(2022, 2025))

to_grp <- function(a){
  case_when(
    a >= 0  & a <= 4  ~ "0-4",
    a >= 5  & a <= 9  ~ "5-9",
    a >= 10 & a <= 14 ~ "10-14",
    a >= 15 & a <= 17 ~ "15-17",
    TRUE ~ NA_character_
  )
}
proj_grp <- proj_long %>%
  mutate(grp = to_grp(idade)) %>%
  filter(!is.na(grp)) %>%
  group_by(cd_uf, uf_sigla, ano, grp) %>%
  summarise(pop = sum(as.numeric(pop), na.rm = TRUE), .groups="drop")

fatores <- proj_grp %>%
  tidyr::pivot_wider(names_from = ano, values_from = pop) %>%
  mutate(fator_2025 = `2025`/`2022`) %>%
  select(cd_uf, uf_sigla, grp, fator_2025)

# sanity e dedup (1 linha por UF)
fw <- fatores %>%
  mutate(
    cd_uf = sprintf("%02s", cd_uf),
    grp2  = stringr::str_replace_all(grp, "-", "_")
  ) %>%
  distinct(cd_uf, grp2, .keep_all = TRUE) %>%
  tidyr::pivot_wider(
    names_from  = grp2,
    values_from = fator_2025,
    names_prefix = "fx_",
    values_fn   = list(fator_2025 = ~ mean(.x, na.rm = TRUE)),
    values_fill = 1
  ) %>%
  group_by(cd_uf) %>%
  summarise(
    across(starts_with("fx_"), ~ first(.)), # <- Parêntese extra removido daqui
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("fx_"), ~ ifelse(is.na(.), 1, .)))

stopifnot(!any(duplicated(fw$cd_uf)))

# cobertura de UF
ufs_setor <- sort(unique(features_2022$cd_uf))
ufs_fator <- sort(unique(fw$cd_uf))
faltando  <- setdiff(ufs_setor, ufs_fator)
if (length(faltando)){
  warning("⚠️ Faltam fatores para UF(s): ", paste(faltando, collapse=", "), ". Vou preencher fator=1.")
  add_rows <- data.frame(cd_uf = faltando)
  for(nm in setdiff(paste0("fx_", c("0_4","5_9","10_14","15_17")), names(add_rows))) add_rows[[nm]] <- 1
  fw <- bind_rows(fw, add_rows)
}

# ------------------------------------------------------------
# 5) Nowcast 2025 por setor  (coortes + calibração UF)
# ------------------------------------------------------------
base22   <- features_2022 %>% mutate(cd_uf = sprintf("%02s", cd_uf))
coorte25 <- advance_cohorts_3y(base22, factor_births = 1.00)

# join many-to-one (um fator por UF), e NA→1 nos fatores se sobrar
setor_2025 <- coorte25 %>%
  left_join(fw, by = "cd_uf", relationship = "many-to-one") %>%
  mutate(across(starts_with("fx_"), ~ replace_na(., 1))) %>%
  mutate(
    pop_0_4_2025   = coalesce(pop_0_4_2025,   0) * fx_0_4,
    pop_5_9_2025   = coalesce(pop_5_9_2025,   0) * fx_5_9,
    pop_10_14_2025 = coalesce(pop_10_14_2025, 0) * fx_10_14,
    pop_15_17_2025 = coalesce(pop_15_17_2025, 0) * fx_15_17,
    tam_total_0_17_2025 = rowSums(across(c(pop_0_4_2025, pop_5_9_2025, pop_10_14_2025, pop_15_17_2025)), na.rm = TRUE)
  ) %>%
  select(-starts_with("fx_"))

# diagnósticos finais
diag_na <- colSums(sapply(setor_2025[,c("pop_0_4_2025","pop_5_9_2025","pop_10_14_2025","pop_15_17_2025")], is.na))
if (any(diag_na > 0)) warning("⚠️ Ainda há NAs nas colunas 2025: ", paste(names(diag_na[diag_na>0]), collapse=", "))
stopifnot(all(is.finite(unlist(setor_2025[,c("pop_0_4_2025","pop_5_9_2025","pop_10_14_2025","pop_15_17_2025")]))))

# ------------------------------------------------------------
# 6) Salvar outputs
# ------------------------------------------------------------
write_csv(setor_2025, "data/processed/nowcast_setor_2025.csv")
saveRDS(setor_2025,  "data/processed/nowcast_setor_2025.rds")
message("✅ Nowcast 2025 salvo em data/processed/")

