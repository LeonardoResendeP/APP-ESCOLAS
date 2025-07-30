mod_auth_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Adicionando um pouco de estilo para centralizar a tela de login
    tags$head(
      tags$style(HTML("
        .auth-container {
          display: flex;
          justify-content: center;
          align-items: center;
          height: 80vh;
        }
        .auth-box {
          width: 300px;
          padding: 20px;
          border: 1px solid #ddd;
          border-radius: 5px;
          background-color: #f9f9f9;
        }
      "))
    ),
    div(class = "auth-container",
        div(class = "auth-box",
            h3("Login", align = "center"),
            textInput(ns("user"), "Usuário"),
            passwordInput(ns("pass"), "Senha"),
            actionButton(ns("login_btn"), "Entrar", class = "btn-primary", width = "100%"),
            br(),br(),
            verbatimTextOutput(ns("login_status"))
        )
    )
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
      
      # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< LÓGICA DE LOGIN ATUALIZADA <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # 1. Encontra o usuário pelo nome de usuário
      user_row <- users_df %>%
        filter(user == input$user)
      
      # 2. Verifica se o usuário existe
      if (nrow(user_row) == 1) {
        
        # 3. Pega o hash salvo no arquivo
        stored_hash <- user_row$pass[[1]]
        
        # 4. Verifica se a senha digitada corresponde ao hash salvo
        is_password_correct <- scrypt::verifyPassword(stored_hash, input$pass)
        
        if (isTRUE(is_password_correct)) {
          # Sucesso no login
          user_data$authenticated <- TRUE
          user_data$user <- user_row$user[[1]]
          user_data$role <- user_row$role[[1]]
          user_data$codinep <- user_row$codinep[[1]]
          
          output$login_status <- renderText("")
        } else {
          # Senha incorreta
          output$login_status <- renderText("❌ Usuário ou senha incorretos.")
        }
        
      } else {
        # Usuário não encontrado
        output$login_status <- renderText("❌ Usuário ou senha incorretos.")
      }
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA ATUALIZAÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    })
    
    # Retorna as informações de autenticação
    return(user_data)
  })
}