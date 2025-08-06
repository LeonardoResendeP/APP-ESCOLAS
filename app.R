library(shiny)
library(tidyverse)
library(bslib) # Carrega a biblioteca de temas

# Carrega o novo utilitário de banco de dados
source("utils/db_utils.R")

# Carrega os módulos da aplicação
source("modules/mod_auth.R")
source("modules/mod_admin.R")
source("modules/mod_escola.R")
# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ADICIONAR ESTA LINHA <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
source("modules/mod_chat.R") # Carrega o módulo de chat globalmente
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA ADIÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# --- Inicialização do Banco de Dados ---
db_init()
# ------------------------------------

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"), 
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),
  
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
