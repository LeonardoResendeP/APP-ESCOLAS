# utils/openai_utils.R

library(httr)
library(jsonlite)

# Esta função encapsula a lógica para enviar um prompt e receber uma resposta.
get_openai_analysis <- function(prompt_text, api_key = Sys.getenv("OPENAI_API_KEY")) {
  
  # Verifica se a chave de API foi configurada
  if (api_key == "") {
    stop("Chave de API da OpenAI não encontrada. Configure a variável de ambiente OPENAI_API_KEY.")
  }
  
  cat("🤖 Enviando prompt para a OpenAI...\n")
  
  # Corpo da requisição em formato JSON
  body <- list(
    model = "gpt-3.5-turbo", # Modelo mais rápido e econômico
    messages = list(
      list(
        role = "system",
        # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< INÍCIO DA CORREÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
        content = "Você é um assistente especializado em análise de dados educacionais. Sua função é ajudar diretores de escolas a entenderem seus dados. Responda de forma clara e objetiva. Baseie-se exclusivamente nos dados fornecidos. O contexto inclui dados detalhados para a escola principal e para cada um de seus concorrentes. Quando solicitado a detalhar ou comparar, use os dados individuais de cada escola. Não mencione o formato dos dados (como JSON). Se a informação não estiver disponível, informe que não possui os dados para essa análise."
        # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      ),
      list(
        role = "user",
        content = prompt_text
      )
    ),
    temperature = 0.3, # Respostas mais diretas e factuais
    max_tokens = 1000   # Limita o tamanho da resposta
  )
  
  # Realiza a chamada POST para a API
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", api_key)),
    content_type_json(),
    encode = "json",
    body = body
  )
  
  # Verifica se a requisição foi bem-sucedida
  if (status_code(response) == 200) {
    cat("✅ Resposta recebida com sucesso!\n")
    # Extrai o conteúdo da resposta
    content <- content(response, "parsed")
    return(content$choices[[1]]$message$content)
  } else {
    # Mostra uma mensagem de erro detalhada
    error_details <- content(response, "text", encoding = "UTF-8")
    stop("Falha na chamada da API: ", status_code(response), "\nDetalhes: ", error_details)
  }
}