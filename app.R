library(shiny)
library(tidyverse)

# Carrega o novo utilitário de banco de dados
source("utils/db_utils.R")

# Carrega os módulos da aplicação
source("modules/mod_auth.R")
source("modules/mod_admin.R")
source("modules/mod_escola.R")

# --- Inicialização do Banco de Dados ---
# Esta função será executada uma vez quando o app iniciar.
# Ela cria o arquivo do banco de dados e as tabelas se não existirem.
db_init()
# ------------------------------------

ui <- fluidPage(
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  
  # Carrega os usuários do banco de dados
  users <- get_users_from_db()
  auth <- mod_auth_server("login", users)
  
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
  
  # Lógica de backend reativa após login
  observeEvent(auth$authenticated, {
    req(auth$authenticated, auth$role)
    
    if (auth$role == "admin") {
      mod_admin_server("admin")
    } else {
      req(auth$codinep)
      mod_escola_server("escola", user = auth$user, codinep = auth$codinep)
    }
  })
}

shinyApp(ui, server)