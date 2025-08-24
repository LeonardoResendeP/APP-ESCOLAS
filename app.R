library(shiny)
library(tidyverse)
library(bslib)

# --- Variáveis de ambiente ---
if (file.exists(".Renviron")) readRenviron(".Renviron")
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
  stop("OPENAI_API_KEY não está definida. Verifique o seu .Renviron.")
}

# Carrega utilitários
source("utils/db_utils.R")
source("utils/onepager_build.R")

# Carrega os módulos da aplicação
source("modules/mod_auth.R")
source("modules/mod_admin.R")
source("modules/mod_escola.R")
source("modules/mod_chat.R")

# --- Inicialização do Banco de Dados ---
db_init()

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"), 
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),
  
  # Header da EXPLORA
  div(class = "app-header",
      div(class = "header-content",
          div(class = "logo-container",
              div(class = "logo",
                  tags$img(src = "logo_explora.png", height = "50")
              ),
              div(class = "logo-brand",
                  div(class = "logo-text", "EXPLORA"),
                  div(class = "logo-subtitle", "Pesquisas, métricas e tendências educacionais")
              )
          ),
          
          # No UI, substitua as condicionais por:
          conditionalPanel(
            condition = "output.auth_status == 'logged_in'",
            div(class = "header-separator")
          ),
          
          conditionalPanel(
            condition = "output.auth_status == 'logged_in'", 
            div(class = "school-title-container",
                h2(class = "school-title", textOutput("nome_escola_header"))
            )
          )
      )
  ),
  
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  # Carrega os usuários do banco de dados
  users <- get_users_from_db()
  auth <- mod_auth_server("login", users)
  
  # Reactive value para armazenar o nome da escola
  escola_nome <- reactiveVal("")
  
  # Status de autenticação para o conditionalPanel
  output$auth_status <- reactive({
    if (isTRUE(auth$authenticated)) "logged_in" else "logged_out"
  })
  outputOptions(output, "auth_status", suspendWhenHidden = FALSE)
  
  output$user_role <- reactive({
    if (isTRUE(auth$authenticated)) auth$role else ""
  })
  outputOptions(output, "user_role", suspendWhenHidden = FALSE)
  
  # UI condicional baseada na autenticação
  output$main_ui <- renderUI({
    if (!isTRUE(auth$authenticated)) {
      mod_auth_ui("login")
    } else if (identical(auth$role, "admin")) {
      mod_admin_ui("admin")
    } else {
      mod_escola_ui("escola")
    }
  })
  
  # Lógica de backend reativa após login - SIMPLIFICADA
  observeEvent(auth$authenticated, {
    req(auth$authenticated)
    
    if (auth$role == "admin") {
      mod_admin_server("admin")
      escola_nome("Painel Administrativo")
    } else {
      req(auth$codinep)
      mod_escola_server("escola", user = auth$user, codinep = auth$codinep)
      
      # SOLUÇÃO DIRETA: Use um valor fixo para teste
      escola_nome("AGOSTINIANO MENDEL COLEGIO") # ← NOME DIRETO
    }
  })
  
  # Output para o título da escola
  output$nome_escola_header <- renderText({
    escola_nome()
  })
  
  # Debug para ver o que está acontecendo - CORRIGIDO
  observe({
    req(auth$authenticated)  # ← ADICIONEI req() AQUI
    cat("=== DEBUG APP.R ===\n")
    cat("Autenticado:", auth$authenticated, "\n")
    cat("Role:", auth$role, "\n")
    cat("Código INEP:", auth$codinep, "\n")
    cat("Nome atual:", escola_nome(), "\n")
    cat("===================\n")
  })
}

shinyApp(ui, server)