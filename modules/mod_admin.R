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
                        
                        # Painel que só aparece se a escola não tiver município definido
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
    
    # Carrega a base de geolocalização uma vez
    geo_data <- reactive({
      geo <- readRDS("data/escolas_geo_com_empty_flag.rds")
      if ("sf" %in% class(geo)) {
        geo <- sf::st_drop_geometry(geo)
      }
      geo %>% mutate(code_school = as.character(code_school))
    })
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< MUDANÇA IMPORTANTE <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Prepara a lista de municípios a partir do ficheiro definitivo
    lista_municipios <- reactive({
      # Lê o dataframe completo que criámos
      municipios_df <- readRDS("data/municipios_br.rds")
      # Cria uma lista nomeada: o que o utilizador vê é "Alecrim - RS", 
      # o valor que o R recebe é "Alecrim"
      setNames(municipios_df$name_muni, municipios_df$display_name)
    })
    
    # Atualiza o seletor de município manual com a lista completa
    observe({
      updateSelectizeInput(session, "muni_manual_busca", choices = lista_municipios(), server = TRUE)
    })
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA MUDANÇA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    escolas_base <- reactive({
      nomes <- readRDS("data/escolas_privadas_nomelista.rds") %>%
        mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
      
      geo <- geo_data()
      
      nomes_limpos <- nomes %>%
        mutate(
          NO_ENTIDADE = iconv(NO_ENTIDADE, to = "latin1", sub = " "),
          NO_ENTIDADE = stringr::str_squish(NO_ENTIDADE)
        )
      
      nomes_limpos %>%
        left_join(geo, by = c("CO_ENTIDADE" = "code_school")) %>%
        mutate(
          name_muni = ifelse(is.na(name_muni), "Município Desconhecido", name_muni),
          name_muni = iconv(name_muni, to = "latin1", sub = " "),
          display_name = paste0(NO_ENTIDADE, " - ", name_muni, " (", CO_ENTIDADE, ")"),
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
      
      if (escola$name_muni == "Município Desconhecido") {
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
      if (escola_selecionada$name_muni == "Município Desconhecido") {
        req(input$muni_manual_busca, message = "Por favor, selecione um município para a escola.")
        # O valor recebido já é apenas o nome do município, sem o estado
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
