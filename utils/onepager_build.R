# utils/onepager_build.R
# Requisitos:
# install.packages(c("chromote","ggplot2","dplyr","tidyr","scales"))

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

`%||%` <- function(a,b) if (!is.null(a)) a else b
nz <- function(x) !is.null(x) && length(x) > 0

# -------- helpers --------
clamp_chars <- function(s, n) {
  if (is.null(s) || length(s) == 0 || anyNA(s)) return("")
  s <- as.character(s[1]); if (!nzchar(s)) return("")
  if (nchar(s) <= n) return(s)
  paste0(substr(s, 1, n - 1), "…")
}

# -------- KPIs --------
kpi_var_matriculas_total <- function(dados) {
  segs <- c("mat_infantil","mat_fundamental","mat_medio")
  v23 <- 0; v24 <- 0
  for (s in segs) {
    l <- dados$dados_propria_escola[[s]] %||% list()
    v23 <- v23 + as.numeric(l$valor_ano_1 %||% 0)
    v24 <- v24 + as.numeric(l$valor_ano_2 %||% 0)
  }
  if (is.na(v23) || v23 == 0) return("—")
  pct <- round((v24 - v23) / v23 * 100, 1)
  paste0(ifelse(is.finite(pct), sprintf("%.1f", pct), "—"), "%")
}

# -------- Matrículas por segmento (com Var% Conc. e Var% Mun.) --------
table_matriculas <- function(dados) {
  seg_row <- function(seg, label) {
    es <- dados$dados_propria_escola[[seg]]        %||% list(valor_ano_1 = NA, valor_ano_2 = NA, taxa_de_variacao = "—")
    co <- dados$dados_concorrentes_proximos[[seg]] %||% list(taxa_de_variacao = "—")
    mu <- dados$dados_mercado_municipio[[seg]]     %||% list(taxa_de_variacao = "—")
    tibble::tibble(
      Segmento     = label,
      `2023`       = as.numeric(es$valor_ano_1 %||% NA),
      `2024`       = as.numeric(es$valor_ano_2 %||% NA),
      `Var% Esc.`  = es$taxa_de_variacao %||% "—",
      `Var% Conc.` = co$taxa_de_variacao %||% "—",
      `Var% Mun.`  = mu$taxa_de_variacao %||% "—"
    )
  }
  dplyr::bind_rows(
    seg_row("mat_infantil","Infantil"),
    seg_row("mat_fundamental","Fundamental"),
    seg_row("mat_medio","Médio")
  )
}

# -------- Concorrentes (todos) --------
concorrentes_line <- function(dados) {
  if (!nz(dados$lista_concorrentes)) return("")
  dados$lista_concorrentes %>%
    dplyr::mutate(nome = dplyr::coalesce(nome_escola, id_escola),
                  nome = paste0(nome, " (", id_escola, ")")) %>%
    dplyr::pull(nome) %>%
    paste(collapse = ", ")
}

# -------- ENEM --------
enem_media_e_delta <- function(dados) {
  df <- dados$dados_enem_areas
  esc  <- df %>% dplyr::filter(tipo == "Sua Escola")         %>% dplyr::summarise(v = mean(as.numeric(nota), na.rm = TRUE)) %>% dplyr::pull(v)
  conc <- df %>% dplyr::filter(tipo == "Média Concorrentes") %>% dplyr::summarise(v = mean(as.numeric(nota), na.rm = TRUE)) %>% dplyr::pull(v)
  list(media = ifelse(is.finite(esc), round(esc,1), NA),
       delta = ifelse(is.finite(esc) & is.finite(conc), round(esc - conc,1), NA))
}

enem_table_areas <- function(dados) {
  df <- dados$dados_enem_areas %>%
    dplyr::filter(tipo %in% c("Sua Escola","Média Concorrentes","Média Municipal")) %>%
    dplyr::group_by(tipo, area) %>%
    dplyr::summarise(nota = mean(as.numeric(nota), na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = tipo, values_from = nota) %>%
    dplyr::rename(Área = area,
                  `Sua Escola` = `Sua Escola`,
                  `Média Concorrentes` = `Média Concorrentes`,
                  `Média Municipal` = `Média Municipal`)
  # garante 3 colunas
  missing <- setdiff(c("Sua Escola","Média Concorrentes","Média Municipal"), names(df))
  for (m in missing) df[[m]] <- NA_real_
  df
}

# gráfico MUITO menor
save_enem_plot <- function(dados, path) {
  df <- dados$dados_enem_areas %>%
    dplyr::filter(tipo %in% c("Sua Escola","Média Concorrentes","Média Municipal")) %>%
    dplyr::group_by(tipo) %>% dplyr::summarise(nota = mean(as.numeric(nota), na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(escola_label = dplyr::recode(tipo,
                                               "Sua Escola"="Sua Escola",
                                               "Média Concorrentes"="Média Concorrentes",
                                               "Média Municipal"="Média Municipal"))
  if (nrow(df) == 0 || all(is.na(df$nota))) {
    df <- tibble::tibble(escola_label=c("Sua Escola","Média Concorrentes","Média Municipal"), nota=c(NA,NA,NA))
  }
  pal <- c("Sua Escola"="#6A4CFF","Média Concorrentes"="#00B3A4","Média Municipal"="#FF7A59")
  ymax <- max(df$nota, na.rm = TRUE); if (!is.finite(ymax)) ymax <- 600
  p <- ggplot(df, aes(x = escola_label, y = nota, fill = escola_label)) +
    geom_col(na.rm = TRUE, width = 0.68) +
    geom_text(aes(label = ifelse(is.na(nota), "—", round(nota,1))), vjust = -0.35, size = 2.2) +
    scale_fill_manual(values = pal, guide = "none") +
    coord_cartesian(ylim = c(0, ymax * 1.12)) +
    labs(x = NULL, y = "Nota média ENEM (todas as áreas)") +
    theme_minimal(base_size = 8) +
    theme(plot.margin = margin(2,2,2,2))
  ggsave(path, p, width = 3.4, height = 2.0, dpi = 300)
}

# -------- BUILD PRINCIPAL --------
build_onepager <- function(dados, out_pdf) {
  stopifnot(is.list(dados), length(out_pdf)==1)
  
  out_html  <- file.path(tempdir(), "relatorio_escola.html")
  graf_path <- file.path(tempdir(), "grafico_enem.png")
  logo_abs  <- normalizePath(file.path("report","logo_rabbit.png"), winslash = "/", mustWork = FALSE)
  qr_abs    <- normalizePath(file.path("report","qr_demo.png"),    winslash = "/", mustWork = FALSE)
  
  kpi_mat_tot <- kpi_var_matriculas_total(dados)
  mat_tab     <- table_matriculas(dados)
  conc_line   <- concorrentes_line(dados)
  
  emd         <- enem_media_e_delta(dados)
  enem_md     <- emd$media
  enem_delta  <- emd$delta
  enem_tab    <- enem_table_areas(dados)
  if (!nrow(enem_tab)) enem_tab <- tibble::tibble(Área=character(0))
  save_enem_plot(dados, graf_path)
  
  rmarkdown::render(
    "report/relatorio_escola.Rmd",
    params = list(
      meta = list(
        nome_escola = dados$nome_escola,
        municipio   = dados$nome_municipio,
        data_ref    = format(Sys.Date(), "%d/%m/%Y"),
        logo_path   = if (file.exists(logo_abs)) logo_abs else "",
        qr_path     = if (file.exists(qr_abs))   qr_abs   else "",
        concorrentes_line = conc_line
      ),
      kpis = list(
        matriculas_var_total = kpi_mat_tot,
        enem_media      = ifelse(is.na(enem_md), "—", format(round(enem_md,1), nsmall=1, decimal.mark=",")),
        enem_delta_conc = ifelse(is.na(enem_delta), "",  sprintf("%+.1f", enem_delta))
      ),
      tabelas = list(
        matriculas   = mat_tab,
        enem_areas   = enem_tab
      ),
      figuras = list(
        grafico_path = graf_path
      )
    ),
    output_file = out_html,
    quiet = TRUE
  )
  
  # Chromote -> PDF (escala menor para garantir 1 página)
  if (requireNamespace("chromote", quietly = TRUE)) {
    b <- chromote::ChromoteSession$new()
    on.exit(try(b$close(), silent = TRUE), add = TRUE)
    b$Page$navigate(paste0("file:///", normalizePath(out_html, winslash = "/")))
    b$Page$loadEventFired(wait_ = TRUE)
    b$Emulation$setEmulatedMedia(media = "print")
    pdf <- b$Page$printToPDF(
      paperWidth  = 8.27, paperHeight = 11.69,
      marginTop   = 0.39, marginBottom = 0.39,
      marginLeft  = 0.39, marginRight  = 0.39,
      printBackground = TRUE,
      preferCSSPageSize = TRUE,
      scale = 0.82
    )
    raw_pdf <- jsonlite::base64_dec(pdf$data)
    con <- file(out_pdf, open = "wb")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    writeBin(raw_pdf, con)
  } else {
    stop("Pacote 'chromote' não encontrado. Instale com install.packages('chromote').")
  }
  
  invisible(out_pdf)
}

