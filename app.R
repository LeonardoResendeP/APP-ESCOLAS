library(shiny)
library(tidyverse)

source("utils/auth_utils.R")
source("modules/mod_auth.R")
source("modules/mod_admin.R")
source("modules/mod_escola.R")

ui <- fluidPage(
  uiOutput("main_ui")
)

server <- function(input, output, session) {
  
  # 🔄 Carrega os usuários no momento do login (evita cache)
  users <- read_users()
  auth <- mod_auth_server("login", users)
  
  # ✅ UI condicional baseada na autenticação
  output$main_ui <- renderUI({
    if (!isTRUE(auth$authenticated)) {
      mod_auth_ui("login")
    } else if (identical(auth$role, "admin")) {
      mod_admin_ui("admin")
    } else {
      mod_escola_ui("escola")
    }
  })
  
  # ✅ Lógica de backend reativa após login
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
