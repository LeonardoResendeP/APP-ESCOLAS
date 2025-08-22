# VERSÃO FINAL - O CÉREBRO
suppressPackageStartupMessages({
  library(glue)
  library(base64enc)
  library(knitr)
  library(kableExtra)
  library(dplyr)
  library(purrr)
  library(here)
})

`%||%` <- function(a, b) if (!is.null(a)) a else b

image_to_base_64 <- function(path) {
  full_path <- here::here(path)
  if (!file.exists(full_path)) {
    warning(paste("Arquivo de imagem não encontrado:", full_path)); return("")
  }
  ext <- tolower(tools::file_ext(full_path))
  mime_type <- switch(ext,
                      "png" = "image/png", "jpg" = "image/jpeg", "jpeg" = "image/jpeg",
                      "svg" = "image/svg+xml", "image/unknown"
  )
  return(base64enc::dataURI(file = full_path, mime = mime_type))
}

compose_insight_fallback <- function(d) {
  v23 <- sum(sapply(d$dados_propria_escola, function(x) x$valor_ano_1 %||% 0), na.rm = TRUE)
  v24 <- sum(sapply(d$dados_propria_escola, function(x) x$valor_ano_2 %||% 0), na.rm = TRUE)
  var <- if (is.finite(v23) && v23 > 0) round((v24 - v23) / v23 * 100, 1) else NA
  em <- tryCatch(mean(as.numeric(d$dados_enem_areas$nota[d$dados_enem_areas$tipo == "Sua Escola"]), na.rm = TRUE), error = \(e) NA)
  mc <- tryCatch(mean(as.numeric(d$dados_enem_areas$nota[d$dados_enem_areas$tipo == "Média Concorrentes"]), na.rm = TRUE), error = \(e) NA)
  delta <- if (is.finite(em) && is.finite(mc)) round(em - mc, 1) else NA
  glue(
    "Entre 2023 e 2024, o total de matrículas da escola variou {ifelse(is.finite(var), sprintf('**%s%%**', format(var, decimal.mark=',')), 'de forma inconclusiva')}. ",
    "No desempenho do ENEM, a média geral da instituição foi de {ifelse(is.finite(em), glue('**{format(round(em,1), decimal.mark=',')} pontos**'), 'não informada')}",
    "{ifelse(is.finite(delta), sprintf(', posicionando-se %s em relação aos concorrentes diretos', ifelse(delta>=0, paste0('**',format(delta, decimal.mark=','), ' pontos acima**'), paste0(format(abs(delta), decimal.mark=','), ' pontos abaixo'))), '')}. ",
    "**Recomendação:** Priorizar a análise dos segmentos com maior potencial de crescimento e alavancar os resultados positivos do ENEM nas campanhas de captação."
  )
}

summarize_matriculas <- function(data_source, label) {
  if (is.null(data_source) || length(data_source) == 0) {
    return(tibble(Categoria = label, `2023` = NA_real_, `2024` = NA_real_, `Var %` = "N/A"))
  }
  total_v1 <- sum(sapply(data_source, function(x) x$valor_ano_1 %||% 0), na.rm = TRUE)
  total_v2 <- sum(sapply(data_source, function(x) x$valor_ano_2 %||% 0), na.rm = TRUE)
  var_pct <- if (total_v1 > 0) { paste0(round((total_v2 - total_v1) / total_v1 * 100, 1), "%") } else { "N/A" }
  tibble(Categoria = label, `2023` = total_v1, `2024` = total_v2, `Var %` = var_pct)
}

create_matriculas_table_html <- function(dados) {
  escola_summary <- summarize_matriculas(dados$dados_propria_escola, "Sua Escola")
  concorrentes_summary <- summarize_matriculas(dados$dados_concorrentes_proximos, "Média Concorrentes")
  mercado_summary <- summarize_matriculas(dados$dados_mercado_municipio, "Mercado (Município)")
  df_final <- bind_rows(escola_summary, concorrentes_summary, mercado_summary)
  if (nrow(df_final) == 0) return("<p><em>Dados de matrículas indisponíveis.</em></p>")
  df_final %>%
    kbl(format = "html", escape = FALSE, align = "lrrr") %>%
    kable_styling(bootstrap_options = c("striped", "condensed"), full_width = TRUE, font_size = 10) %>%
    column_spec(1, bold = TRUE) %>%
    as.character()
}

build_report_params <- function(dados) {
  css_path <- here::here("report", "style.css")
  css_content <- if (file.exists(css_path)) {
    paste(readLines(css_path, warn = FALSE), collapse = "\n")
  } else {
    warning("Arquivo style.css não encontrado!"); ""
  }
  concorrentes_texto <- if (!is.null(dados$lista_concorrentes) && nrow(dados$lista_concorrentes) > 0) {
    paste(dados$lista_concorrentes$nome_escola, collapse = ", ")
  } else { "Não informado" }
  list(
    css = css_content,
    meta = list(
      nome_escola = dados$nome_escola %||% "Nome da Escola",
      municipio = dados$nome_municipio %||% "Município",
      data_ref = format(Sys.Date(), "%d/%m/%Y"),
      concorrentes_line = concorrentes_texto
    ),
    logos = list(
      explora = image_to_base_64("report/logo_explora.png"),
      primeira_escolha = image_to_base_64("report/logo_primeira_escolha.png"),
      rabbit = image_to_base_64("report/logo_rabbit.jpg")
    ),
    analysis_text = compose_insight_fallback(dados),
    tabela_matriculas_html = create_matriculas_table_html(dados)
  )
}