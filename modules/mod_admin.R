mod_admin_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    navbarPage(
      title = "Painel Admin",
      
      tabPanel("Dashboard Geral",
               h3("Bem-vindo ao painel administrativo")
      ),
      
      tabPanel("Cadastrar Escola",
               fluidRow(
                 column(6,
                        h4("Cadastrar Nova Escola"),
                        selectizeInput(ns("nome_escola_busca"), "Buscar escola por nome", choices = NULL, options = list(placeholder = 'Digite para buscar...')),
                        verbatimTextOutput(ns("aviso_geo")),
                        textInput(ns("codinep"), "Código INEP (preenchido automaticamente)", placeholder = "Selecionar escola acima"),
                        
                        conditionalPanel(
                          condition = sprintf("input['%s'] == 'TRUE'", ns("mostrar_geo_manual")),
                          tagList(
                            numericInput(ns("lat_manual"), "Latitude (caso não exista)", value = NA, step = 0.0001),
                            numericInput(ns("lon_manual"), "Longitude (caso não exista)", value = NA, step = 0.0001)
                          )
                        ),
                        
                        shinyjs::hidden(
                          textInput(ns("mostrar_geo_manual"), label = NULL, value = "FALSE")
                        ),
                        
                        textInput(ns("user_escola"), "Usuário para Login"),
                        passwordInput(ns("senha_escola"), "Senha para Login"),
                        actionButton(ns("btn_add_escola"), "Cadastrar Escola"),
                        br(), 
                        verbatimTextOutput(ns("status_cadastro"))
                 )
               )
      ),
      
      # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVA ABA <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      tabPanel("Gestão de Escolas",
               fluidRow(
                 column(6,
                        h4("Escolas Atualmente Cadastradas"),
                        DT::dataTableOutput(ns("tabela_escolas"))
                 ),
                 column(6,
                        h4("Excluir Escola Cadastrada"),
                        uiOutput(ns("seletor_escola_excluir_ui")),
                        actionButton(ns("btn_delete_escola"), "Excluir Escola Selecionada", class = "btn-danger"),
                        br(),
                        verbatimTextOutput(ns("status_exclusao"))
                 )
               )
      )
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA NOVA ABA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    )
  )
}


mod_admin_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("utils/admin_utils.R", local = TRUE)
    source("utils/preprocess_utils.R", local = TRUE)
    
    # --- Lógica de Cadastro ---
    
    escolas_base <- reactive({
      nomes <- readRDS("data/escolas_privadas_nomelista.rds") %>%
        mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
      
      geo <- readRDS("data/escolas_geo_com_empty_flag.rds")
      
      if ("sf" %in% class(geo)) {
        if (!require(sf)) install.packages("sf"); library(sf)
        geo <- sf::st_drop_geometry(geo)
      }
      
      geo <- geo %>%
        mutate(code_school = as.character(code_school))
      
      nomes %>%
        left_join(geo, by = c("CO_ENTIDADE" = "code_school")) %>%
        mutate(empty_geo = ifelse(is.na(empty_geo), TRUE, empty_geo)) %>%
        distinct(CO_ENTIDADE, NO_ENTIDADE, empty_geo, latitude, longitude, name_muni) %>%
        arrange(NO_ENTIDADE)
    })
    
    observe({
      updateSelectizeInput(
        session, "nome_escola_busca",
        choices = escolas_base()$NO_ENTIDADE,
        server = TRUE
      )
    })
    
    observeEvent(input$nome_escola_busca, {
      req(input$nome_escola_busca)
      
      escola <- escolas_base() %>%
        filter(NO_ENTIDADE == input$nome_escola_busca) %>%
        slice(1)
      
      updateTextInput(session, "codinep", value = escola$CO_ENTIDADE)
      updateTextInput(session, "mostrar_geo_manual", value = ifelse(isTRUE(escola$empty_geo), "TRUE", "FALSE"))
      
      output$aviso_geo <- renderText({
        if (isTRUE(escola$empty_geo)) {
          "⚠️ Esta escola não possui localização na base. Insira a latitude e longitude manualmente."
        } else {
          "✅ Localização geográfica encontrada."
        }
      })
    })
    
    observeEvent(input$btn_add_escola, {
      req(input$codinep, input$nome_escola_busca, input$user_escola, input$senha_escola)
      
      tryCatch({
        save_escola(
          codinep = input$codinep,
          nome = input$nome_escola_busca,
          user = input$user_escola,
          pass = input$senha_escola
        )
        
        dados_cadastrados(get_escolas()) # Atualiza a reatividade
        
        usar_geo_manual <- input$mostrar_geo_manual == "TRUE" &&
          !is.na(input$lat_manual) &&
          !is.na(input$lon_manual)
        
        tryCatch({
          if (usar_geo_manual) {
            preprocessar_escola(input$codinep, lat = input$lat_manual, lon = input$lon_manual)
          } else {
            preprocessar_escola(input$codinep)
          }
          output$status_cadastro <- renderText("✅ Escola cadastrada e dados gerados com sucesso!")
        }, error = function(e_proc) {
          output$status_cadastro <- renderText(paste("⚠️ Escola cadastrada, mas houve erro ao gerar os dados:\n", e_proc$message))
        })
      }, error = function(e_main) {
        output$status_cadastro <- renderText(paste("❌ Erro ao cadastrar escola:\n", e_main$message))
      })
    })
    
    # --- Lógica de Gestão e Exclusão ---
    
    dados_cadastrados <- reactiveVal(get_escolas())
    
    output$tabela_escolas <- DT::renderDataTable({
      DT::datatable(dados_cadastrados(), options = list(pageLength = 5))
    })
    
    output$seletor_escola_excluir_ui <- renderUI({
      escolas <- dados_cadastrados()
      # Cria uma lista nomeada: o texto é o nome da escola, o valor é o codinep
      choices_list <- setNames(escolas$codinep, escolas$nome)
      selectInput(ns("escola_a_excluir"), "Selecione a escola para excluir", choices = choices_list)
    })
    
    observeEvent(input$btn_delete_escola, {
      req(input$escola_a_excluir)
      
      cod_a_excluir <- input$escola_a_excluir
      
      tryCatch({
        delete_escola(cod_a_excluir)
        dados_cadastrados(get_escolas()) # Atualiza a reatividade após a exclusão
        output$status_exclusao <- renderText(paste("✅ Escola com Cód. INEP", cod_a_excluir, "foi excluída com sucesso."))
      }, error = function(e) {
        output$status_exclusao <- renderText(paste("❌ Erro ao excluir escola:\n", e$message))
      })
    })
    
  })
}

