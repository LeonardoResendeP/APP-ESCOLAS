# modules/mod_chat.R

mod_chat_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Estilos CSS para as bolhas de chat
    tags$head(
      tags$style(HTML("
        .chat-container { height: 60vh; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px; margin-bottom: 10px; }
        .chat-bubble { max-width: 70%; padding: 10px; border-radius: 10px; margin-bottom: 10px; }
        .user-bubble { background-color: #007bff; color: white; float: right; clear: both; }
        .assistant-bubble { background-color: #f1f1f1; color: black; float: left; clear: both; }
      "))
    ),
    
    # Interface do chat
    fluidRow(
      column(12,
             h4("Chat com Assistente de Análise"),
             p("Faça perguntas sobre os dados desta escola. A IA usará os dados carregados para gerar insights."),
             
             div(class = "chat-container", uiOutput(ns("chat_history"))),
             
             div(style = "display: flex;",
                 textInput(ns("user_message"), label = NULL, placeholder = "Digite sua pergunta aqui...", width = "85%"),
                 actionButton(ns("send_message"), "Enviar", icon = icon("paper-plane"), class = "btn-primary", style="margin-left: 10px; height: 38px;")
             )
      )
    )
  )
}

mod_chat_server <- function(id, dados_escola) {
  moduleServer(id, function(input, output, session) {
    
    source("utils/openai_utils.R", local = TRUE)
    
    # Armazena o histórico da conversa
    chat_log <- reactiveVal(list(
      list(role = "assistant", content = "Olá! Como posso ajudar a analisar os dados desta escola?")
    ))
    
    # Renderiza o histórico do chat na UI
    output$chat_history <- renderUI({
      history <- chat_log()
      lapply(history, function(msg) {
        bubble_class <- if (msg$role == "user") "user-bubble" else "assistant-bubble"
        div(class = paste("chat-bubble", bubble_class), shiny::markdown(msg$content))
      })
    })
    
    # Observador para o botão de enviar
    observeEvent(input$send_message, {
      req(input$user_message)
      
      # Adiciona a mensagem do usuário ao log
      current_log <- chat_log()
      current_log <- c(current_log, list(list(role = "user", content = input$user_message)))
      chat_log(current_log)
      
      # Limpa o campo de texto
      updateTextInput(session, "user_message", value = "")
      
      # Mostra uma mensagem de "pensando"
      showNotification("O assistente está analisando os dados...", type = "message", duration = 4)
      
      # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< INÍCIO DA CORREÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Prepara o contexto com todos os dados da escola para enviar à IA
      dados <- dados_escola()
      
      # Pega os nomes e IDs dos concorrentes atuais para usar como legenda
      concorrentes_info <- dados$lista_concorrentes %>% select(id_escola, nome_escola)
      
      # Filtra e formata os dados detalhados do ENEM para a escola principal e cada concorrente
      enem_detalhado_areas <- dados$dados_enem_areas %>%
        left_join(concorrentes_info, by = "id_escola") %>%
        mutate(
          escola = case_when(
            tipo == "Sua Escola" ~ dados$nome_escola,
            tipo == "Concorrente" ~ nome_escola, # Usa o nome real da escola concorrente
            TRUE ~ tipo # Mantém "Média Concorrentes" e "Média Municipal"
          )
        ) %>%
        select(escola, area, nota) %>%
        filter(!is.na(nota)) %>%
        pivot_wider(names_from = area, values_from = nota)
      
      enem_detalhado_redacao <- dados$dados_enem_redacao %>%
        left_join(concorrentes_info, by = "id_escola") %>%
        mutate(
          escola = case_when(
            tipo == "Sua Escola" ~ dados$nome_escola,
            tipo == "Concorrente" ~ nome_escola,
            TRUE ~ tipo
          )
        ) %>%
        select(escola, competencia, nota) %>%
        filter(!is.na(nota)) %>%
        pivot_wider(names_from = competencia, values_from = nota)
      
      # 1. Prepara uma lista R com todos os dados relevantes
      contexto_lista <- list(
        escola_principal = list(
          nome = dados$nome_escola,
          cod_inep = dados$id_escola,
          municipio = dados$nome_municipio
        ),
        resumo_matriculas = list(
          escola_principal = dados$dados_propria_escola,
          media_concorrentes = dados$dados_concorrentes_proximos
        ),
        # A IA agora recebe uma tabela completa com dados individuais e médias
        desempenho_enem_completo = list(
          areas_conhecimento = enem_detalhado_areas,
          competencias_redacao = enem_detalhado_redacao
        )
      )
      
      # 2. Converte a lista para uma string JSON formatada
      contexto_json <- jsonlite::toJSON(contexto_lista, auto_unbox = TRUE, pretty = TRUE)
      
      # 3. Cria o prompt final, instruindo a IA a usar o JSON
      prompt_final <- paste(
        "Aqui estão os dados da escola para análise:\n\n```json\n",
        contexto_json,
        "\n```\n\n",
        "Com base **apenas e estritamente** nos dados acima, responda à seguinte pergunta do usuário:\n\n",
        tail(current_log, 1)[[1]]$content
      )
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      # Chama a API da OpenAI
      tryCatch({
        resposta_ia <- get_openai_analysis(prompt_final)
        
        # Adiciona a resposta da IA ao log
        final_log <- chat_log()
        final_log <- c(final_log, list(list(role = "assistant", content = resposta_ia)))
        chat_log(final_log)
        
      }, error = function(e) {
        # Em caso de erro, adiciona uma mensagem de erro ao chat
        error_log <- chat_log()
        error_log <- c(error_log, list(list(role = "assistant", content = paste("Desculpe, ocorreu um erro ao contatar o assistente:", e$message))))
        chat_log(error_log)
      })
    })
  })
}
