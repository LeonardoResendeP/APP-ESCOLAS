# =========================================================
#   UTILS/ONEPAGER_BUILD.R (VERSÃO FINAL E DEFINITIVA)
# =========================================================

suppressPackageStartupMessages({
  library(here)
  library(rmarkdown)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(commonmark)
  library(webshot)# Necessário para converter Markdown em HTML
})

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#      CORREÇÃO DEFINITIVA PARA DEPLOY: Carrega o .Renviron
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
if (file.exists(here::here(".Renviron"))) {
  readRenviron(here::here(".Renviron"))
}


# --- Helpers Globais ---
`%||%` <- function(a,b) if (!is.null(a)) a else b
nz <- function(x) !is.null(x) && length(x) > 0
numify <- function(x) suppressWarnings(as.numeric(x))
fmt_pct1 <- function(x) if (is.na(x) || !is.finite(x)) "—" else paste0(sprintf("%.1f", x), "%")
fmt_num1 <- function(x) if (is.na(x) || !is.finite(x)) "—" else format(round(x,1), nsmall=1, decimal.mark=",")

# ----------------- HELPERS DE DADOS -----------------

kpi_var_matriculas_total <- function(dados) {
  segs <- c("mat_infantil","mat_fundamental","mat_medio")
  v23 <- 0; v24 <- 0
  for (s in segs) {
    l <- dados$dados_propria_escola[[s]] %||% list()
    v23 <- v23 + numify(l$valor_ano_1 %||% 0)
    v24 <- v24 + numify(l$valor_ano_2 %||% 0)
  }
  if (is.na(v23) || v23 == 0) return("—")
  pct <- round((v24 - v23) / v23 * 100, 1)
  if (is.finite(pct)) sprintf("%.1f%%", pct) else "—"
}

table_matriculas <- function(dados) {
  seg_row <- function(seg, label) {
    es <- dados$dados_propria_escola[[seg]] %||% list()
    co <- dados$dados_concorrentes_proximos[[seg]] %||% list()
    mu <- dados$dados_mercado_municipio[[seg]] %||% list()
    tibble::tibble(
      Segmento = label,
      `2023` = numify(es$valor_ano_1 %||% NA),
      `2024` = numify(es$valor_ano_2 %||% NA),
      `Var% Esc.` = es$taxa_de_variacao %||% "—",
      `Var% Conc.` = co$taxa_de_variacao %||% "—",
      `Var% Mun.` = mu$taxa_de_variacao %||% "—"
    )
  }
  bind_rows(seg_row("mat_infantil","Infantil"), seg_row("mat_fundamental","Fundamental"), seg_row("mat_medio","Médio"))
}

concorrentes_line <- function(dados) {
  if (!nz(dados$lista_concorrentes) || nrow(dados$lista_concorrentes) == 0) return("Nenhum concorrente selecionado.")
  df_conc <- as.data.frame(dados$lista_concorrentes)
  if (!"nome_escola" %in% names(df_conc) && "NO_ENTIDADE" %in% names(df_conc)) {
    df_conc <- rename(df_conc, nome_escola = NO_ENTIDADE)
  }
  df_conc %>%
    mutate(nome = dplyr::coalesce(nome_escola, as.character(id_escola))) %>%
    pull(nome) %>%
    paste(collapse = ", ")
}

# --- LÓGICA DO ENEM CENTRALIZADA E CORRIGIDA ---

# 1. Gera a tabela do ENEM (Fonte da Verdade para as áreas)
enem_table_areas <- function(dados) {
  df <- as.data.frame(dados$dados_enem_areas %||% data.frame())
  req_cols <- c("tipo", "area", "nota")
  if (!all(req_cols %in% names(df))) return(data.frame())
  
  out <- df |>
    filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) |>
    mutate(nota = numify(nota)) |>
    group_by(tipo, area) |>
    summarise(nota = mean(nota, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(names_from = tipo, values_from = nota) |>
    rename(Área = area)
  
  colunas_esperadas <- c("Sua Escola", "Média Concorrentes", "Média Municipal")
  for (m in colunas_esperadas) { if (!m %in% names(out)) out[[m]] <- NA_real_ }
  
  out %>% select(Área, all_of(colunas_esperadas))
}

# 2. Calcula o KPI de Média Geral e Delta CORRETAMENTE (sem Redação)
enem_media_delta <- function(enem_tab) {
  if (!is.data.frame(enem_tab) || nrow(enem_tab) == 0) return(list(media = NA, delta = NA))
  
  # Filtra para excluir a Redação do cálculo da média geral
  tab_sem_redacao <- enem_tab %>% filter(Área != "Redação")
  
  esc <- mean(numify(tab_sem_redacao$`Sua Escola`), na.rm = TRUE)
  conc <- mean(numify(tab_sem_redacao$`Média Concorrentes`), na.rm = TRUE)
  
  list(
    media = ifelse(is.finite(esc), esc, NA),
    delta = ifelse(is.finite(esc) && is.finite(conc), esc - conc, NA)
  )
}

# --- FUNÇÃO `collect_facts_rich` CORRIGIDA E ENRIQUECIDA ---
collect_facts_rich <- function(dados, enem_tab) {
  # Matrículas
  segs <- c("mat_infantil", "mat_fundamental", "mat_medio")
  v23 <- 0; v24 <- 0; seg_var_s <- list()
  for (s in segs) {
    l <- dados$dados_propria_escola[[s]] %||% list()
    v23 <- v23 + numify(l$valor_ano_1 %||% 0)
    v24 <- v24 + numify(l$valor_ano_2 %||% 0)
    seg_var_s[[s]] <- l$taxa_de_variacao %||% "N/A"
  }
  var_total <- if (v23 > 0) (v24 - v23) / v23 * 100 else NA_real_
  
  # ENEM (usa a tabela e o KPI já corrigidos)
  enem_medias <- enem_media_delta(enem_tab)
  
  enem_by_area <- if (is.data.frame(enem_tab) && nrow(enem_tab) > 0) {
    enem_tab %>% mutate(across(where(is.numeric), ~round(.x, 1)))
  } else { data.frame() }
  
  enem_redacao_tab <- as.data.frame(dados$dados_enem_redacao %||% data.frame()) %>%
    filter(tipo %in% c("Sua Escola", "Média Concorrentes")) %>%
    select(competencia, tipo, nota) %>%
    mutate(nota = round(numify(nota), 1)) %>%
    pivot_wider(names_from = tipo, values_from = nota)
  
  # Concorrentes
  conc_names <- concorrentes_line(dados)
  
  # Retorna a lista COMPLETA de fatos
  list(
    escola = dados$nome_escola %||% "—",
    municipio = dados$nome_municipio %||% "—",
    variacao_matriculas_total_pct = round(kpi_var_matriculas_total(dados) %>% sub("%", "", .) %>% as.numeric(), 1),
    tabela_matriculas = table_matriculas(dados),
    enem_media_geral_escola = round(enem_medias$media, 1),
    enem_delta_vs_concorrentes = round(enem_medias$delta, 1),
    enem_desempenho_por_area = enem_by_area,
    enem_desempenho_redacao = enem_redacao_tab,
    concorrentes_nomes = conc_names
  )
}

generate_exec_analysis_rich <- function(dados, enem_tab, model = "gpt-5-chat-latest", timeout = 45) {
  
  # 1. Coleta os fatos ricos e corretos para a análise
  F_rich <- collect_facts_rich(dados, enem_tab)
  facts_json <- jsonlite::toJSON(F_rich, auto_unbox = TRUE, pretty = TRUE, na = "string")
  
  # 2. Usa o prompt estruturado para garantir uma resposta concisa
  prompt_final <- paste(
    "Gere um relatório executivo de alto nível para o setor educacional. Siga estas instruções à risca:

  **FORMATAÇÃO E ESTILO (OBRIGATÓRIO):**
  - FORMATO DE SAÍDA: Texto puro e contínuo. NÃO USE MARKDOWN, HTML, EMOJIS, ou quaisquer caracteres de formatação.
  - NÃO USE títulos entre colchetes como [PARÁGRAFO 1]. A transição entre ideias deve ser feita naturalmente pela redação.
  - ESTRUTURA: Três parágrafos densos e contínuos, seguidos por uma única frase de call to action.
  - TOM: Profissional, analítico e slightly provocativo. Escreva para um diretor de escola sofisticado.

  **CONTEÚDO E ESTRUTURA (OBRIGATÓRIO):**
  
  **Parágrafo 1 (O Contraste Estratégico):**
  Comece imediatamente com os dados de matrícula, destacando os contrastes mais impactantes entre a performance da escola, dos concorrentes e do município. Use a estrutura 'enquanto X, Y' para criar tensão narrativa. Finalize sugerindo que estes movimentos revelam uma reconfiguração do mercado.

  **Parágrafo 2 (A Lacuna Competitiva):**
  Transfira a análise para o desempenho no ENEM. Destaque a posição competitiva geral e depois os pontos de maior força e a(s) área(s) de menor vantagem relativa. Termine com a pergunta estratégica central: 'Esta combinação de [força] e [oportunidade] levanta uma questão crucial:'

  **Parágrafo 3 (A Jornada de Descoberta):**
  Comece com 'Estes números, porém, são apenas o ponto de partida.' Apresente a plataforma Explora como o sistema que responde à pergunta do parágrafo anterior. Cite de forma integrada ao texto as três capacidades: (1) personalização da concorrência, (2) acompanhamento mensal, e (3) suporte das gestoras Rabbit. Finalize o parágrafo com: 'Esta não é uma análise estática; é um sistema de inteligência contínua.'

  **Call to Action (FINAL):**
  Frase única, em uma linha separada: 'Escaneie o QR code para desbloquear a análise completa e transformar esta visão inicial em uma vantagem competitiva sustentável.'

  **PROIBIÇÕES ABSOLUTAS:**
  - NÃO USE marcadores de lista (1., 2., 3., -, *).
  - NÃO USE emojis, símbolos ou caracteres especiais (→, ★, 📊).
  - NÃO USE negrito, itálico ou qualquer outra formatação.
  - NÃO USE colchetes para separar seções ([...]).
  - NÃO INVENTE dados. Use APENAS os fornecidos.

  O texto final deve ser um bloco de texto limpo, pronto para ser inserido em um sistema de markup sem requerer edição.

  DADOS:",
    facts_json
  )
  
  # 3. Verifica a chave da API
  key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(key) || !requireNamespace("httr2", quietly = TRUE)) {
    return("*(Análise da IA não disponível: Chave de API não configurada)*")
  }
  
  # 4. Realiza a chamada à API usando a sua lógica de conexão original
  req <- httr2::request("https://api.openai.com/v1/chat/completions") |>
    httr2::req_headers(
      "Authorization" = paste("Bearer", key),
      "Content-Type"  = "application/json"
    ) |>
    httr2::req_body_json(list(
      model = model,
      temperature = 0.7,
      max_tokens = 600, # Limite ajustado
      messages = list(
        list(role = "system", content = "Você é um consultor educacional sênior da Explora, especialista em análise de dados para escolas."),
        list(role = "user", content = prompt_final)
      )
    )) |>
    httr2::req_timeout(timeout)
  
  # 5. Trata a resposta e os possíveis erros
  resp <- try(httr2::req_perform(req), silent = TRUE)
  if (inherits(resp, "try-error")) {
    msg <- attr(resp, "condition")$message
    warning("API Call Failed: ", msg)
    return("*(Falha na comunicação com a API da OpenAI. Verifique sua conexão ou a validade da chave.)*")
  }
  
  if (httr2::resp_status(resp) >= 300) {
    error_body <- try(httr2::resp_body_string(resp), silent = TRUE)
    warning("API Response Error: ", error_body)
    return(paste0("*(A API da OpenAI retornou um erro: ", httr2::resp_status_desc(resp), ")*"))
  }
  
  js <- try(httr2::resp_body_json(resp), silent = TRUE)
  if (inherits(js, "try-error")) return("*(Resposta da API inválida.)*")
  
  out <- try(js$choices[[1]]$message$content, silent = TRUE)
  if (inherits(out, "try-error") || is.null(out) || !nzchar(out)) {
    return("*(Análise da IA não pôde ser gerada a partir da resposta)*")
  }
  
  # 6. LIMPEZA E CONVERSÃO PARA HTML (A CORREÇÃO)
  
  # Remove as cercas de código que a IA às vezes adiciona
  out <- sub("^\\s*```[a-zA-Z]*\\s*\\n?", "", out)
  out <- sub("\\n?\\s*```\\s*$", "", out)
  
  # Converte a resposta em Markdown (com **negrito**, listas, etc.) para HTML
  # para que o relatório renderize corretamente.
  html_output <- commonmark::markdown_html(out)
  
  return(html_output)
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






# =========================================================
#      FUNÇÃO PRINCIPAL `build_onepager` (VERSÃO FINAL)
# =========================================================
.build_html_onepager <- function(rmd_path, params_list, out_html) {
  rmarkdown::render(
    rmd_path,
    params = params_list,
    output_file = out_html,
    quiet = TRUE,
    envir = new.env(parent = globalenv()),
    output_options = list(self_contained = TRUE)  # << essencial p/ shiny
  )
  stopifnot(file.exists(out_html))
  out_html
}

.html_to_pdf_webshot <- function(in_html, out_pdf, vwidth = 1240, vheight = 1754, zoom = 1, delay = 0.5) {
  if (!requireNamespace("webshot", quietly = TRUE))
    stop("Pacote 'webshot' não está instalado.")
  # Garante PhantomJS no shinyapps.io
  if (!isTRUE(webshot::is_phantomjs_installed())) {
    try(webshot::install_phantomjs(), silent = TRUE)
  }
  webshot::webshot(
    url     = paste0("file:///", normalizePath(in_html, winslash = "/")),
    file    = out_pdf,
    vwidth  = vwidth,   # ~A4 portrait em pixels (proporção)
    vheight = vheight,
    zoom    = zoom,
    delay   = delay
  )
  if (!file.exists(out_pdf) || is.na(file.size(out_pdf)) || file.size(out_pdf) < 1024)
    stop("Falha ao gerar PDF com webshot/PhantomJS.")
  out_pdf
}

# Fallbacks opcionais (usados só se webshot falhar)
.try_pagedown <- function(in_html, out_pdf, timeout = 120) {
  if (!requireNamespace("pagedown", quietly = TRUE)) stop("pagedown indisponível.")
  pagedown::chrome_print(input = in_html, output = out_pdf, timeout = timeout)
  if (!file.exists(out_pdf) || file.size(out_pdf) < 1024) stop("chrome_print não gerou PDF.")
  out_pdf
}
.try_chromote <- function(in_html, out_pdf, scale = 0.82) {
  if (!requireNamespace("chromote", quietly = TRUE)) stop("chromote indisponível.")
  b <- chromote::ChromoteSession$new()
  on.exit(try(b$close(), silent = TRUE), add = TRUE)
  b$Page$navigate(paste0("file:///", normalizePath(in_html, winslash = "/")))
  b$Page$loadEventFired(wait_ = TRUE)
  b$Emulation$setEmulatedMedia(media = "print")
  pdf <- b$Page$printToPDF(
    paperWidth  = 8.27, paperHeight = 11.69,
    marginTop   = 0.39, marginBottom = 0.39,
    marginLeft  = 0.39, marginRight  = 0.39,
    printBackground = TRUE,
    preferCSSPageSize = TRUE,
    scale = scale
  )
  raw_pdf <- jsonlite::base64_dec(pdf$data)
  con <- file(out_pdf, open = "wb"); on.exit(try(close(con), silent = TRUE), add = TRUE)
  writeBin(raw_pdf, con)
  if (!file.exists(out_pdf) || file.size(out_pdf) < 1024) stop("chromote não gerou PDF.")
  out_pdf
}

# =========================
# VERSÃO FINAL: build_onepager()
# =========================
build_onepager <- function(dados, out_pdf) {
  stopifnot(is.list(dados), length(out_pdf) == 1)
  
  # --- Caminho do Rmd ---
  rmd_path <- file.path("report", "relatorio_escola.Rmd")
  if (!file.exists(rmd_path)) stop("Arquivo Rmd não encontrado em: ", rmd_path)
  
  # --- Montagem dos dados (usa suas funções novas) ---
  enem_tab  <- tryCatch(enem_table_areas(dados), error = function(e) data.frame())
  kpi_mat   <- tryCatch(kpi_var_matriculas_total(dados), error = function(e) "—")
  mat_tab   <- tryCatch(table_matriculas(dados), error = function(e) data.frame())
  emd       <- tryCatch(enem_media_delta(enem_tab), error = function(e) list(media = NA, delta = NA))
  conc_line <- tryCatch(concorrentes_line(dados), error = function(e) "—")
  texto_ai  <- tryCatch(generate_exec_analysis_rich(dados, enem_tab), error = function(e) "<p><em>Sem análise automática.</em></p>")
  
  params_full <- list(
    meta = list(
      nome_escola      = dados$nome_escola %||% "—",
      municipio        = dados$nome_municipio %||% "—",
      data_ref         = format(Sys.Date(), "%d/%m/%Y"),
      logo_explora_uri = find_logo_uri("logo_explora"),
      logo_pe_uri      = find_logo_uri("logo_pe"),
      logo_rabbit_uri  = find_logo_uri("logo_rabbit"),
      qr_uri           = find_logo_uri("qr_demo"),
      concorrentes_line= conc_line
    ),
    kpis = list(
      matriculas_var_total = kpi_mat,
      enem_media      = if (is.finite(emd$media)) format(round(emd$media, 1), nsmall = 1, decimal.mark = ",") else "—",
      enem_delta_conc = if (is.finite(emd$delta)) sprintf("%+.1f", emd$delta) else ""
    ),
    tabelas = list(
      matriculas = mat_tab,
      enem_areas = enem_tab
    ),
    analise = list(
      texto = texto_ai  # já vem em HTML “limpo” do generate_exec_analysis_rich()
    )
  )
  
  # --- Render para HTML self-contained (chave p/ shiny) ---
  out_html <- file.path(tempdir(), "relatorio_escola.html")
  .build_html_onepager(rmd_path, params_full, out_html)
  
  # --- HTML -> PDF (prioriza webshot/PhantomJS no shinyapps) ---
  ok <- FALSE
  try({
    .html_to_pdf_webshot(out_html, out_pdf, vwidth = 1240, vheight = 1754, zoom = 1, delay = 0.5)
    ok <- TRUE
  }, silent = TRUE)
  
  if (!ok) {
    # fallback 1: pagedown (se por acaso houver Chrome/Chromium)
    try({
      .try_pagedown(out_html, out_pdf, timeout = 180)
      ok <- TRUE
    }, silent = TRUE)
  }
  if (!ok) {
    # fallback 2: chromote (último recurso)
    .try_chromote(out_html, out_pdf, scale = 0.82)
    ok <- TRUE
  }
  
  invisible(out_pdf)
}