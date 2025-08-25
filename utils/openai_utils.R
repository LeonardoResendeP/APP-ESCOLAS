# utils/openai_utils.R
library(httr)
library(jsonlite)

.rtrim_slash <- function(x) sub("/+$", "", x)

get_openai_analysis <- function(
    prompt_text,
    api_key      = Sys.getenv("OPENAI_API_KEY"),
    model        = Sys.getenv("OPENAI_MODEL", unset = "gpt-4o"),
    base_url     = Sys.getenv("OPENAI_BASE",  unset = "https://api.openai.com"),
    project_id   = Sys.getenv("OPENAI_PROJECT", unset = ""),
    organization = Sys.getenv("OPENAI_ORG",     unset = "")
) {
  api_key <- trimws(api_key)
  if (!nzchar(api_key)) {
    stop("Chave de API da OpenAI não encontrada. Configure a variável de ambiente OPENAI_API_KEY.")
  }
  
  url <- paste0(.rtrim_slash(base_url), "/v1/chat/completions")
  
  # Cabeçalhos
  headers <- add_headers(
    Authorization = paste("Bearer", api_key),
    `Content-Type` = "application/json"
  )
  if (nzchar(project_id))     headers <- c(headers, add_headers(`OpenAI-Project` = project_id))
  if (nzchar(organization))   headers <- c(headers, add_headers(`OpenAI-Organization` = organization))
  
  body <- list(
    model = model,  # gpt-4o é suportado no Chat Completions
    messages = list(
      list(
        role    = "system",
        content = "Você é um assistente especializado em análise de dados educacionais. Sua função é ajudar diretores de escolas a entenderem seus dados. Responda de forma clara e objetiva. Baseie-se exclusivamente nos dados fornecidos. O contexto inclui dados detalhados para a escola principal e para cada um de seus concorrentes. Quando solicitado a detalhar ou comparar, use os dados individuais de cada escola. Não mencione o formato dos dados (como JSON). Se a informação não estiver disponível, informe que não possui os dados para essa análise."
      ),
      list(role = "user", content = prompt_text)
    ),
    temperature = 0.3,
    max_tokens  = 2000
  )
  
  resp <- httr::RETRY(
    "POST", url, headers,
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    encode = "json",
    times = 2, terminate_on = c(400, 401, 403, 404, 422), pause_base = 1
  )
  
  sc <- status_code(resp)
  if (sc >= 300) {
    raw <- content(resp, "text", encoding = "UTF-8")
    stop(sprintf("Falha na chamada da API (%s): %s", sc, raw))
  }
  
  payload <- content(resp, "parsed", type = "application/json")
  if (is.null(payload$choices) || length(payload$choices) == 0) {
    stop("Resposta da API sem 'choices'.")
  }
  payload$choices[[1]]$message$content
}

# (Opcional) health check para usar no console
openai_health_check <- function() {
  base_url <- Sys.getenv("OPENAI_BASE",  unset = "https://api.openai.com")
  key      <- trimws(Sys.getenv("OPENAI_API_KEY"))
  url      <- paste0(.rtrim_slash(base_url), "/v1/me")
  resp <- GET(url, add_headers(Authorization = paste("Bearer", key)))
  list(status = status_code(resp), body = content(resp, "parsed"))
}
