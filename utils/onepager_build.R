# utils/onepager_build.R
# Requisitos: install.packages(c("here","rmarkdown","jsonlite","dplyr","tidyr"))
# Opcional p/ PDF: pagedown (preferido) ou chromote

suppressPackageStartupMessages({
  library(here)
  library(rmarkdown)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
})

`%||%` <- function(a,b) if (!is.null(a)) a else b
nz <- function(x) !is.null(x) && length(x) > 0

# ----------------- HELPERS DE DADOS -----------------
kpi_var_matriculas_total <- function(dados) {
  segs <- c("mat_infantil","mat_fundamental","mat_medio")
  v23 <- 0; v24 <- 0
  for (s in segs) {
    l <- dados$dados_propria_escola[[s]] %||% list()
    v23 <- v23 + suppressWarnings(as.numeric(l$valor_ano_1 %||% 0))
    v24 <- v24 + suppressWarnings(as.numeric(l$valor_ano_2 %||% 0))
  }
  if (is.na(v23) || v23 == 0) return("—")
  pct <- round((v24 - v23) / v23 * 100, 1)
  if (is.finite(pct)) sprintf("%.1f%%", pct) else "—"
}

table_matriculas <- function(dados) {
  seg_row <- function(seg, label) {
    es <- dados$dados_propria_escola[[seg]]        %||% list(valor_ano_1 = NA, valor_ano_2 = NA, taxa_de_variacao = "—")
    co <- dados$dados_concorrentes_proximos[[seg]] %||% list(taxa_de_variacao = "—")
    mu <- dados$dados_mercado_municipio[[seg]]     %||% list(taxa_de_variacao = "—")
    tibble::tibble(
      Segmento     = label,
      `2023`       = suppressWarnings(as.numeric(es$valor_ano_1 %||% NA)),
      `2024`       = suppressWarnings(as.numeric(es$valor_ano_2 %||% NA)),
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

concorrentes_line <- function(dados) {
  if (!nz(dados$lista_concorrentes)) return("")
  dados$lista_concorrentes %>%
    mutate(nome = dplyr::coalesce(nome_escola, id_escola),
           nome = paste0(nome, " (", id_escola, ")")) %>%
    pull(nome) %>%
    paste(collapse = ", ")
}

enem_media_delta <- function(dados) {
  df <- dados$dados_enem_areas %||% data.frame()
  if (!nrow(df)) return(list(media = NA_real_, delta = NA_real_))
  num <- function(x) suppressWarnings(as.numeric(x))
  esc  <- df %>% filter(tipo == "Sua Escola")         %>% summarise(v = mean(num(nota), na.rm = TRUE)) %>% pull(v)
  conc <- df %>% filter(tipo == "Média Concorrentes") %>% summarise(v = mean(num(nota), na.rm = TRUE)) %>% pull(v)
  list(
    media = ifelse(is.finite(esc), round(esc,1), NA_real_),
    delta = ifelse(is.finite(esc) & is.finite(conc), round(esc - conc,1), NA_real_)
  )
}

enem_table_areas <- function(dados) {
  # Espera dados$dados_enem_areas com colunas: tipo (Sua Escola / Média Concorrentes / Média Municipal),
  # area (nome da área), nota (numérico)
  df <- as.data.frame(dados$dados_enem_areas %||% data.frame())
  
  req_cols <- c("tipo","area","nota")
  if (!all(req_cols %in% names(df))) return(data.frame())
  
  # agrega média por (tipo, área) e abre em colunas
  out <- df |>
    dplyr::filter(tipo %in% c("Sua Escola","Média Concorrentes","Média Municipal")) |>
    dplyr::mutate(nota = suppressWarnings(as.numeric(nota))) |>
    dplyr::group_by(tipo, area) |>
    dplyr::summarise(nota = mean(nota, na.rm = TRUE), .groups = "drop") |>
    tidyr::pivot_wider(names_from = tipo, values_from = nota) |>
    dplyr::rename(Área = area)
  
  # garante as colunas mesmo se faltarem
  for (m in c("Sua Escola","Média Concorrentes","Média Municipal")) {
    if (!m %in% names(out)) out[[m]] <- NA_real_
  }
  
  out
}

# =========================
# ANÁLISE EXECUTIVA RICA (IA + fallback)
# =========================

`%||%` <- function(a,b) if (!is.null(a)) a else b

numify <- function(x) suppressWarnings(as.numeric(x))
fmt_pct1 <- function(x) if (!is.finite(x)) "—" else paste0(sprintf("%.1f", x), "%")
fmt_num1 <- function(x) if (!is.finite(x)) "—" else format(round(x,1), nsmall=1, decimal.mark=",")

# Calcula deltas por área e fatos adicionais
collect_facts_rich <- function(dados) {
  # Matrículas: total e por segmento
  segs <- c("mat_infantil","mat_fundamental","mat_medio")
  v23 <- 0; v24 <- 0; seg_var_str <- list(); seg_var_num <- list()
  for (s in segs) {
    l <- dados$dados_propria_escola[[s]] %||% list()
    a1 <- numify(l$valor_ano_1 %||% NA); a2 <- numify(l$valor_ano_2 %||% NA)
    v23 <- v23 + (a1 %||% 0); v24 <- v24 + (a2 %||% 0)
    seg_var_str[[s]] <- l$taxa_de_variacao %||% NA
    seg_var_num[[s]] <- {
      z <- gsub("%","", seg_var_str[[s]]); z <- gsub(",", ".", z)
      numify(z)
    }
  }
  var_total <- if (v23 > 0) (v24 - v23)/v23*100 else NA_real_
  
  # ENEM geral e por área
  df_enem <- as.data.frame(dados$dados_enem_areas %||% data.frame())
  enem_media <- NA_real_; enem_conc <- NA_real_; enem_mun <- NA_real_
  if (nrow(df_enem)) {
    enem_media <- mean(numify(df_enem$nota[df_enem$tipo=="Sua Escola"]), na.rm=TRUE)
    enem_conc  <- mean(numify(df_enem$nota[df_enem$tipo=="Média Concorrentes"]), na.rm=TRUE)
    enem_mun   <- mean(numify(df_enem$nota[df_enem$tipo=="Média Municipal"]), na.rm=TRUE)
    if (!is.finite(enem_media)) enem_media <- NA_real_
    if (!is.finite(enem_conc))  enem_conc  <- NA_real_
    if (!is.finite(enem_mun))   enem_mun   <- NA_real_
  }
  enem_delta_conc <- if (is.finite(enem_media) && is.finite(enem_conc)) enem_media - enem_conc else NA_real_
  
  enem_by_area <- data.frame()
  if (nrow(df_enem)) {
    enem_by_area <- df_enem |>
      dplyr::filter(tipo %in% c("Sua Escola","Média Concorrentes","Média Municipal")) |>
      dplyr::mutate(nota = numify(nota)) |>
      dplyr::group_by(tipo, area) |>
      dplyr::summarise(nota = mean(nota, na.rm=TRUE), .groups="drop") |>
      tidyr::pivot_wider(names_from = tipo, values_from = nota) |>
      dplyr::rename(Área = area)
    # deltas por área
    if (nrow(enem_by_area)) {
      enem_by_area$DeltaConc <- with(enem_by_area, `Sua Escola` - `Média Concorrentes`)
      enem_by_area$DeltaMun  <- with(enem_by_area, `Sua Escola` - `Média Municipal`)
    }
  }
  
  # Melhor/pior área vs concorrentes
  best_area <- worst_area <- list(nome="—", delta=NA_real_)
  if (nrow(enem_by_area)) {
    ord <- order(enem_by_area$DeltaConc, decreasing=TRUE, na.last=NA)
    if (length(ord)) {
      best_area  <- list(nome = enem_by_area$Área[ord[1]],  delta = enem_by_area$DeltaConc[ord[1]])
      worst_area <- list(nome = enem_by_area$Área[rev(ord)[1]], delta = enem_by_area$DeltaConc[rev(ord)[1]])
    }
  }
  
  # Concorrentes (top 6)
  conc <- dados$lista_concorrentes %||% data.frame()
  conc_names <- character(0)
  if (nrow(conc)) {
    conc_names <- conc |>
      dplyr::mutate(nome = dplyr::coalesce(.data$nome_escola, .data$id_escola)) |>
      dplyr::pull(nome) |> unique() |> head(6)
  }
  
  list(
    escola     = dados$nome_escola %||% "—",
    municipio  = dados$nome_municipio %||% "—",
    var_total  = var_total,
    seg_var    = seg_var_num,     # numérico
    seg_var_s  = seg_var_str,     # string original
    enem_media = enem_media,
    enem_conc  = enem_conc,
    enem_mun   = enem_mun,
    enem_delta_conc = enem_delta_conc,
    enem_by_area = enem_by_area,
    best_area  = best_area,
    worst_area = worst_area,
    concorrentes = conc_names
  )
}

build_prompt_rich <- function(F) {
  seg_line <- {
    labs <- c(mat_infantil="Infantil", mat_fundamental="Fundamental", mat_medio="Médio")
    paste(
      sprintf("%s: %s", labs[names(F$seg_var)], vapply(F$seg_var_s, \(z) z %||% "—", "")),
      collapse = " · "
    )
  }
  conc_line <- if (length(F$concorrentes)) paste(F$concorrentes, collapse=", ") else "—"
  best_str  <- if (is.finite(F$best_area$delta)) sprintf("%s (%+.1f)", F$best_area$nome, F$best_area$delta) else "—"
  worst_str <- if (is.finite(F$worst_area$delta)) sprintf("%s (%+.1f)", F$worst_area$nome, F$worst_area$delta) else "—"
  
  paste0(
    "Você é consultor educacional escrevendo para direção escolar (PT-BR, tom executivo).
Entregue um texto de 100–150 palavras, claro e acionável. NÃO invente números.
Estrutura obrigatória (HTML simples):
<p><strong>Síntese:</strong> ...</p>
<p><strong>Sinais do período:</strong></p>
<ul><li>...</li><li>...</li><li>...</li></ul>
Guidelines:
- NÃO use blocos de código Markdown (```). Responda apenas com HTML simples.
- Use dados abaixo; quando faltar, descreva qualitativamente (use “—”).
- Cite segmentos/áreas de forma específica (ex.: “Médio recuou”, “Matemática acima do mercado”).
- Recomendações devem atrair o usuario acessar o app através do qr code, intigando desejo por analises mais detalhadas, mencione que todos os meses terão analises novas.

DADOS (não repita em lista bruta; use para embasar a análise):
- Escola/Município: ", F$escola, " — ", F$municipio, "
- Matrículas 23→24 var% total: ", fmt_pct1(F$var_total), "
- Por segmento: ", seg_line, "
- ENEM médio: escola=", fmt_num1(F$enem_media), " | conc=", fmt_num1(F$enem_conc), " | mun=", fmt_num1(F$enem_mun), " | Δ vs conc=", ifelse(is.finite(F$enem_delta_conc), sprintf("%+.1f", F$enem_delta_conc), "—"), "
- ENEM por área (melhor vs conc): ", best_str, " | pior: ", worst_str, "
- Concorrentes (amostra): ", conc_line, "
"
  )
}

fallback_exec_text_rich <- function(F) {
  seg_msg <- {
    labs <- c(mat_infantil="Infantil", mat_fundamental="Fundamental", mat_medio="Médio")
    # rank pelo maior módulo de variação disponível
    ord <- names(sort(unlist(F$seg_var), decreasing=TRUE))
    main <- if (length(ord)) labs[ord[1]] else "—"
    sprintf("Variação total de matrículas em %s; destaque para %s.", fmt_pct1(F$var_total), main)
  }
  best_str  <- if (is.finite(F$best_area$delta)) sprintf("%s (%+.1f)", F$best_area$nome, F$best_area$delta) else "—"
  worst_str <- if (is.finite(F$worst_area$delta)) sprintf("%s (%+.1f)", F$worst_area$nome, F$worst_area$delta) else "—"
  conc_line <- if (length(F$concorrentes)) paste(F$concorrentes, collapse=", ") else "—"
  
  sprintf(
    '<p><strong>Síntese:</strong> %s No ENEM, a escola registra média %s (Δ vs conc.: %s).</p>
<p><strong>Sinais do período:</strong></p>
<ul>
  <li>Matrículas 23→24: %s</li>
  <li>Melhor área vs conc.: %s</li>
  <li>Pior área vs conc.: %s</li>
</ul>
<p><strong>Riscos & Oportunidades:</strong></p>
<ul>
  <li>Risco de pressão competitiva nas áreas abaixo do mercado.</li>
  <li>Oportunidade de comunicação e captação nos segmentos com tração.</li>
</ul>
<p><strong>Plano 30–60 dias:</strong></p>
<ul>
  <li>Campanha de captação/retomada focada no segmento com melhor tração.</li>
  <li>Plano de reforço acadêmico nas 1–2 áreas com pior delta.</li>
  <li>Rotina de acompanhamento quinzenal com metas simples (captação e ENEM).</li>
</ul>
<p><small><em>KPIs a monitorar:</em> leads por canal, taxa de conversão por série, frequência a reforço nas áreas críticas.</small></p>',
    if (is.finite(F$enem_media)) "há dados acadêmicos consistentes." else "faltam dados acadêmicos comparáveis.",
    fmt_num1(F$enem_media), if (is.finite(F$enem_delta_conc)) sprintf("%+.1f", F$enem_delta_conc) else "—",
    fmt_pct1(F$var_total),
    best_str, worst_str
  )
}

generate_exec_analysis_rich <- function(dados, model = "gpt-4o-mini", timeout = 25) {
  F <- collect_facts_rich(dados)
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key) || !requireNamespace("httr2", quietly = TRUE)) {
    return(fallback_exec_text_rich(F))
  }
  
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0.7,
      max_tokens = 600,
      messages = list(
        list(role="system", content="Você é consultor educacional sênior. Responda em HTML simples, PT-BR, tom executivo. -**não** deve usar blocos de código. Limpe cercas se mesmo assim vierem."),
        list(role="user",   content=build_prompt_rich(F))
      )
    )) |>
    httr2::req_timeout(timeout)
  
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) return(fallback_exec_text_rich(F))
  
  js <- try(httr2::resp_body_json(resp), silent = TRUE)
  if (inherits(js, "try-error")) return(fallback_exec_text_rich(F))
  
  out <- try(js$choices[[1]]$message$content, silent = TRUE)
  if (inherits(out, "try-error") || is.null(out) || !nzchar(out)) return(fallback_exec_text_rich(F))
  
  # higiene básica
  out <- gsub("\\s+\n", "\n", out)
  out <- gsub("\n{2,}", "\n", out)
  if (nchar(out) > 2400) out <- substr(out, 1, 2400)
  # ...depois de 'out <- try(js$choices[[1]]$message$content, silent = TRUE)'
if (inherits(out, "try-error") || is.null(out) || !nzchar(out)) return(fallback_exec_text_rich(F))

# Remove cercas de código caso o modelo insista em devolver ```html ... ```
out <- sub("^\\s*```[a-zA-Z]*\\s*\\n?", "", out)
out <- sub("\\n?\\s*```\\s*$", "", out)

# Higiene geral
out <- gsub("\\s+\n", "\n", out)
out <- gsub("\n{2,}", "\n", out)
if (nchar(out) > 2400) out <- substr(out, 1, 2400)

out

  out
}


try_openai_summary <- function(dados, max_tokens = 200) {
  key <- Sys.getenv("OPENAI_API_KEY", "")
  if (!nzchar(key)) return("")
  # evite dependências extras: use httr2/jsonlite se quiser chamar de verdade
  # Aqui mantemos um placeholder simples para não quebrar em produção:
  ""
}

# ----------------- LOGO HELPERS -----------------
find_logo_uri <- function(basenames, dir = here::here("report","assets")) {
  exts <- c("png","jpg","jpeg","svg")
  for (bn in basenames) {
    for (ex in exts) {
      p <- file.path(dir, paste0(bn, ".", ex))
      if (file.exists(p)) return(knitr::image_uri(p))
    }
  }
  ""
}






# ----------------- BUILDER (com filtro de params) -----------------
build_onepager <- function(dados, out_pdf) {
  stopifnot(is.list(dados), length(out_pdf) == 1)
  
  # Caminho do Rmd
  rmd_path <- here::here("report", "relatorio_escola.Rmd")
  
  # Logos (qualquer extensão comum)
  logo_explora_uri <- find_logo_uri(c("logo_explora","explora_logo","explora"))
  logo_pe_uri      <- find_logo_uri(c("logo_primeira_escolha","logo_pe","primeira_escolha"))
  logo_rabbit_uri  <- find_logo_uri(c("logo_rabbit","rabbit_logo","rabbit"))
  qr_uri  <- find_logo_uri(c("qr_demo","qr_code","qr"))
  # Paleta
  COL_PRIM  <- "#147AD6"  # azul Explora
  COL_TEXTO <- "#111827"  # grafite
  COL_LINHA <- "#E5E7EB"  # linha clara
  
  # KPIs / tabelas / texto
  kpi_mat_tot <- kpi_var_matriculas_total(dados)
  mat_tab     <- table_matriculas(dados)
  emd         <- enem_media_delta(dados)
  conc_line   <- concorrentes_line(dados)
  texto_ai <- generate_exec_analysis_rich(dados)
  if (!nzchar(texto_ai)) texto_ai <- compose_insight_fallback(dados)
  enem_tab <- tryCatch(enem_table_areas(dados), error = function(e) data.frame())
  # Monta a lista completa de params (podem existir extras…)
  params_full <- list(
    meta = list(
      nome_escola       = dados$nome_escola,
      municipio         = dados$nome_municipio,
      data_ref          = format(Sys.Date(), "%d/%m/%Y"),
      logo_explora_uri  = logo_explora_uri,
      logo_pe_uri       = logo_pe_uri,
      logo_rabbit_uri   = logo_rabbit_uri,
      qr_uri = qr_uri,
      concorrentes_line = conc_line,
      col_prim  = COL_PRIM,
      col_texto = COL_TEXTO,
      col_linha = COL_LINHA
    ),
    kpis = list(
      matriculas_var_total = kpi_mat_tot,
      enem_media           = ifelse(is.finite(emd$media), format(round(emd$media,1), nsmall=1, decimal.mark=","), "—"),
      enem_delta_conc      = ifelse(is.finite(emd$delta), sprintf("%+.1f", emd$delta), "")
    ),
    tabelas = list(
      matriculas = mat_tab,
      enem_areas = enem_tab
    ),
    analise = list(
      texto = texto_ai
    )
    # <- **NÃO** incluímos 'figuras' aqui
  )
  
  # >>> HOTFIX: filtra quaisquer params NÃO declarados no YAML <<<
  fm <- rmarkdown::yaml_front_matter(rmd_path)
  declared <- names(fm$params %||% list())
  params_filtered <- params_full[intersect(names(params_full), declared)]
  
  # Render para HTML temporário
  out_html <- file.path(tempdir(), "relatorio_escola_explora.html")
  rmarkdown::render(
    rmd_path,
    params = params_filtered,
    output_file = out_html,
    quiet = TRUE,
    envir = new.env(parent = globalenv())
  )
  
  # HTML -> PDF: pagedown (preferido) ou chromote
  ok <- FALSE
  if (requireNamespace("pagedown", quietly = TRUE)) {
    try({
      pagedown::chrome_print(input = out_html, output = out_pdf, timeout = 120)
      ok <- file.exists(out_pdf)
    }, silent = TRUE)
  }
  if (!ok && requireNamespace("chromote", quietly = TRUE)) {
    try({
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
        scale = 0.78
      )
      raw_pdf <- jsonlite::base64_dec(pdf$data)
      con <- file(out_pdf, "wb"); writeBin(raw_pdf, con); close(con)
      ok <- file.exists(out_pdf)
    }, silent = TRUE)
  }
  if (!ok) stop("Não foi possível gerar PDF (instale 'pagedown' ou 'chromote').")
  invisible(out_pdf)
}

