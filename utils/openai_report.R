# utils/openai_report.R
library(httr); library(jsonlite)

openai_generate_report_text <- function(context_json,
                                        api_key   = Sys.getenv("OPENAI_API_KEY"),
                                        model     = ifelse(nzchar(Sys.getenv("OPENAI_MODEL")), Sys.getenv("OPENAI_MODEL"), "gpt-5"),
                                        base_url  = Sys.getenv("OPENAI_BASE", "https://api.openai.com")) {
  stopifnot(nzchar(api_key))
  base <- sub("/+$","", base_url)
  
  sys <- "Você é um consultor educacional da Rabbit. Escreva para mantenedores em linguagem simples, direta e executiva. Use apenas os dados fornecidos."
  usr <- paste(
    "Gere, com base nos dados abaixo:",
    "1) Resumo (70-120 palavras).",
    "2) Oportunidades (3 bullets, começar com verbo).",
    "3) Riscos (até 2 bullets).",
    "4) Plano (30 dias) (1 bullet).",
    "Regras: não inventar números; se um dado faltar, escreva 'dados não disponíveis'.",
    "DADOS (JSON):\n", context_json
  )
  
  # ===== Tentativa 1: Responses API =====
  url_resp <- paste0(base, "/v1/responses")
  body_resp <- list(
    model = model,
    instructions = sys,
    input = usr,
    verbosity = "medium",
    reasoning = list(effort = "minimal")
  )
  
  r <- POST(
    url_resp,
    add_headers(Authorization = paste("Bearer", api_key),
                `Content-Type` = "application/json"),
    body = toJSON(body_resp, auto_unbox = TRUE),
    encode = "json",
    timeout(60)
  )
  
  if (status_code(r) < 300) {
    out <- content(r, "parsed")
    txt <- out$output_text %||%
      tryCatch(paste(vapply(out$output, function(o) o$content %||% "", ""), collapse = "\n"), error = function(e) "")
    if (nzchar(txt)) return(txt)
  }
  
  # ===== Tentativa 2: Chat Completions (fallback) =====
  url_chat <- paste0(base, "/v1/chat/completions")
  for (m in c(model, "gpt-4o")) {
    body_chat <- list(
      model = m,
      messages = list(
        list(role = "system", content = sys),
        list(role = "user", content = usr)
      ),
      temperature = 0.3,
      max_tokens = 800
    )
    r2 <- POST(
      url_chat,
      add_headers(Authorization = paste("Bearer", api_key),
                  `Content-Type` = "application/json"),
      body = toJSON(body_chat, auto_unbox = TRUE),
      encode = "json",
      timeout(60)
    )
    if (status_code(r2) < 300) {
      return(content(r2, "parsed")$choices[[1]]$message$content)
    }
  }
  
  # Se tudo falhar:
  stop("Falha ao gerar texto com a OpenAI: ", status_code(r), " => ", content(r, "text", encoding = "UTF-8"))
}

`%||%` <- function(a,b) if (!is.null(a)) a else b
