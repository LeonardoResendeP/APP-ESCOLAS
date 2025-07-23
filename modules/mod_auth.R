mod_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("user"), "Usuário"),
    passwordInput(ns("pass"), "Senha"),
    actionButton(ns("login_btn"), "Entrar"),
    verbatimTextOutput(ns("login_status"))
  )
}

mod_auth_server <- function(id, users_df) {
  moduleServer(id, function(input, output, session) {
    # Estado de autenticação
    user_data <- reactiveValues(
      authenticated = FALSE,
      user = NULL,
      role = NULL,
      codinep = NULL
    )
    
    # Quando clica no botão "Entrar"
    observeEvent(input$login_btn, {
      req(input$user, input$pass)
      
      # Busca credenciais válidas
      match_row <- users_df %>%
        filter(user == input$user, pass == input$pass)
      
      if (nrow(match_row) == 1) {
        # Sucesso no login
        user_data$authenticated <- TRUE
        user_data$user <- match_row$user[[1]]
        user_data$role <- match_row$role[[1]]
        user_data$codinep <- match_row$codinep[[1]]
        
        output$login_status <- renderText("")  # limpa mensagem anterior
      } else {
        # Falha de autenticação
        user_data$authenticated <- FALSE
        user_data$user <- NULL
        user_data$role <- NULL
        user_data$codinep <- NULL
        output$login_status <- renderText("❌ Usuário ou senha incorretos.")
      }
    })
    
    # Retorna as informações de autenticação
    return(user_data)
  })
}
