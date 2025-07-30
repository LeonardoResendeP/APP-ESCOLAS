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
                        selectizeInput(ns("nome_escola_busca"), "Buscar escola (Nome - Município - INEP)", choices = NULL, options = list(placeholder = 'Digite para buscar...')),
                        
                        shinyjs::hidden(
                          div(id = ns("manual_muni_panel"),
                              selectizeInput(ns("muni_manual_busca"), "Município não encontrado. Por favor, selecione o município correto:", choices = NULL)
                          )
                        ),
                        
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
    )
  )
}


mod_admin_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("utils/preprocess_utils.R", local = TRUE)
    
    # --- Lógica de Cadastro ---
    
    geo_data <- reactive({
      geo <- readRDS("data/escolas_geo_com_empty_flag.rds")
      if ("sf" %in% class(geo)) {
        geo <- sf::st_drop_geometry(geo)
      }
      geo %>% mutate(code_school = as.character(code_school))
    })
    
    lista_municipios <- reactive({
      municipios_df <- readRDS("data/municipios_lookup.rds")
      setNames(municipios_df$nome_municipio, municipios_df$display_name)
    })
    
    observe({
      updateSelectizeInput(session, "muni_manual_busca", choices = lista_municipios(), server = TRUE)
    })
    
    escolas_base <- reactive({
      # Carrega a nova lista de escolas ENRIQUECIDA
      nomes <- readRDS("data/escolas_privadas_nomelista.rds")
      
      geo <- geo_data()
      
      nomes_limpos <- nomes %>%
        mutate(
          NO_ENTIDADE = iconv(NO_ENTIDADE, to = "latin1", sub = " "),
          NO_ENTIDADE = stringr::str_squish(NO_ENTIDADE)
        )
      
      nomes_limpos %>%
        left_join(geo, by = c("CO_ENTIDADE" = "code_school")) %>%
        mutate(
          # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< LÓGICA ATUALIZADA <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
          # Usa coalesce para pegar o primeiro nome de município não-nulo:
          # 1. Tenta usar o nome do ficheiro geográfico (name_muni)
          # 2. Se for nulo, usa o nome do ficheiro do censo (nome_municipio)
          # 3. Se ambos forem nulos, define como "Município Desconhecido"
          final_muni_name = coalesce(name_muni, nome_municipio, "Município Desconhecido"),
          final_muni_name = iconv(final_muni_name, to = "latin1", sub = " "),
          # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA ATUALIZAÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
          
          display_name = paste0(NO_ENTIDADE, " - ", final_muni_name, " (", CO_ENTIDADE, ")"),
          empty_geo = ifelse(is.na(empty_geo), TRUE, empty_geo)
        ) %>%
        filter(!is.na(NO_ENTIDADE)) %>%
        arrange(NO_ENTIDADE)
    })
    
    observe({
      updateSelectizeInput(
        session, "nome_escola_busca",
        choices = escolas_base()$display_name,
        server = TRUE 
      )
    })
    
    observeEvent(input$nome_escola_busca, {
      req(input$nome_escola_busca)
      
      escola <- escolas_base() %>%
        filter(display_name == input$nome_escola_busca) %>%
        slice(1)
      
      updateTextInput(session, "codinep", value = escola$CO_ENTIDADE)
      updateTextInput(session, "mostrar_geo_manual", value = ifelse(isTRUE(escola$empty_geo), "TRUE", "FALSE"))
      
      if (escola$final_muni_name == "Município Desconhecido") {
        shinyjs::show("manual_muni_panel")
      } else {
        shinyjs::hide("manual_muni_panel")
      }
      
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
      
      escola_selecionada <- escolas_base() %>%
        filter(display_name == input$nome_escola_busca) %>%
        slice(1)
      
      nome_real_escola <- escola_selecionada$NO_ENTIDADE
      
      muni_manual <- NULL
      if (escola_selecionada$final_muni_name == "Município Desconhecido") {
        req(input$muni_manual_busca, message = "Por favor, selecione um município para a escola.")
        muni_manual <- input$muni_manual_busca
      }
      
      tryCatch({
        save_escola_to_db(
          codinep = input$codinep,
          nome = nome_real_escola,
          user = input$user_escola,
          pass = input$senha_escola
        )
        
        dados_cadastrados(get_escolas_from_db())
        
        usar_geo_manual <- input$mostrar_geo_manual == "TRUE" &&
          !is.na(input$lat_manual) &&
          !is.na(input$lon_manual)
        
        tryCatch({
          if (usar_geo_manual) {
            preprocessar_escola(input$codinep, lat = input$lat_manual, lon = input$lon_manual, nome_muni_manual = muni_manual)
          } else {
            preprocessar_escola(input$codinep, nome_muni_manual = muni_manual)
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
    
    dados_cadastrados <- reactiveVal(get_escolas_from_db())
    
    output$tabela_escolas <- DT::renderDataTable({
      DT::datatable(dados_cadastrados(), options = list(pageLength = 5))
    })
    
    output$seletor_escola_excluir_ui <- renderUI({
      escolas <- dados_cadastrados()
      choices_list <- setNames(escolas$codinep, escolas$nome)
      selectInput(ns("escola_a_excluir"), "Selecione a escola para excluir", choices = choices_list)
    })
    
    observeEvent(input$btn_delete_escola, {
      req(input$escola_a_excluir)
      
      cod_a_excluir <- input$escola_a_excluir
      
      tryCatch({
        delete_escola_from_db(cod_a_excluir)
        dados_cadastrados(get_escolas_from_db())
        output$status_exclusao <- renderText(paste("✅ Escola com Cód. INEP", cod_a_excluir, "foi excluída com sucesso."))
      }, error = function(e) {
        output$status_exclusao <- renderText(paste("❌ Erro ao excluir escola:\n", e$message))
      })
    })
    
  })
}
