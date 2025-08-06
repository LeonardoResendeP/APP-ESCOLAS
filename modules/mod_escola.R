# --- Bibliotecas necessárias para as novas abas ---
# Certifique-se de ter instalado: install.packages(c("plotly", "knitr", "kableExtra", "scales", "leaflet", "shinyjs"))

mod_escola_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(), # Habilita o uso do shinyjs para feedback
    div(class = "page-header", h2(textOutput(ns("nome_escola_titulo")))),
    
    navbarPage(
      title = NULL,
      
      # --- Aba Visão Geral ---
      tabPanel("Visão Geral", 
               fluidRow(
                 column(12,
                        h3("Resumo de Matrículas (Variação 2023 vs 2024)"),
                        p("Comparativo do desempenho da sua escola em relação aos concorrentes selecionados e ao mercado total do seu município.")
                 )
               ),
               br(),
               fluidRow(
                 column(4, h4(icon("school"), "Sua Escola"), hr(), uiOutput(ns("kpis_escola"))),
                 column(4, h4(icon("users"), "Média dos Concorrentes"), hr(), uiOutput(ns("kpis_concorrentes"))),
                 column(4, h4(icon("chart-pie"), "Mercado (Município)"), hr(), uiOutput(ns("kpis_mercado")))
               )
      ),
      
      # --- Aba Benchmark Concorrentes ---
      tabPanel("Benchmark Concorrentes", 
               sidebarLayout(
                 sidebarPanel(
                   h4("Seleção de Concorrentes"),
                   p("Escolha até 5 escolas para uma análise personalizada. A análise padrão usa os 5 concorrentes mais próximos."),
                   
                   selectizeInput(ns("selecao_concorrentes"), 
                                  label = "Busque e selecione as escolas:",
                                  choices = NULL,
                                  multiple = TRUE,
                                  options = list(maxItems = 5, placeholder = 'Digite para buscar...')),
                   
                   actionButton(ns("btn_atualizar_analise"), "Atualizar e Salvar Seleção", icon = icon("sync"), class = "btn-primary btn-block"),
                   br(),
                   actionButton(ns("btn_restaurar_padrao"), "Restaurar Padrão", icon = icon("undo"), class = "btn-secondary btn-block")
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     type = "tabs",
                     tabPanel("Mapa de Concorrentes", leaflet::leafletOutput(ns("mapa_concorrentes"), height = "600px")),
                     tabPanel("Lista de Concorrentes Atuais", DT::dataTableOutput(ns("tabela_concorrentes")))
                   )
                 )
               )
      ),
      
      # --- Aba Análise de Infraestrutura ---
      tabPanel("Análise de Infraestrutura",
               fluidRow(
                 column(12,
                        h3("Comparativo de Infraestrutura (Censo 2024)"),
                        p("Análise comparativa dos principais indicadores de infraestrutura da sua escola em relação aos concorrentes selecionados e à média do município.")
                 )
               ),
               hr(),
               fluidRow(
                 column(12,
                        h4("Análise Detalhada"),
                        tabsetPanel(
                          type = "tabs",
                          tabPanel("Resumo Comparativo", 
                                   br(),
                                   uiOutput(ns("cards_infra_detalhada"))),
                          tabPanel("Detalhe por Concorrente", 
                                   br(),
                                   uiOutput(ns("tabela_infra_detalhada_ui")))
                        )
                 )
               )
      ),
      
      # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< INÍCIO DA CORREÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # --- Aba Desempenho Acadêmico com Sub-abas ---
      tabPanel("Desempenho Acadêmico",
               h3("Análise de Desempenho - ENEM 2024"),
               p("Comparativo das médias do ENEM por área de conhecimento e detalhamento das competências da redação."),
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Visão Consolidada",
                          br(),
                          uiOutput(ns("ui_desempenho_areas_consolidado")),
                          hr(),
                          uiOutput(ns("ui_desempenho_redacao_consolidado"))
                 ),
                 tabPanel("Detalhe por Concorrente",
                          br(),
                          uiOutput(ns("ui_desempenho_areas_detalhado")),
                          hr(),
                          uiOutput(ns("ui_desempenho_redacao_detalhado"))
                 )
               )
      ),
      # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
      
      tabPanel("Chat com IA",
               icon = icon("robot"),
               mod_chat_ui(ns("chat_ia"))
      )
    )
  )
}


mod_escola_server <- function(id, user, codinep) {
  moduleServer(id, function(input, output, session) {
    
    source("utils/preprocess_utils.R", local = TRUE)
    source("utils/db_utils.R", local = TRUE)
    
    dados_escola_reativo <- reactiveVal(NULL)
    dados_escola_padrao <- reactiveVal(NULL)
    
    observe({
      req(codinep)
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep, ".rds"))
      if (file.exists(caminho_arquivo)) {
        dados_carregados <- readRDS(caminho_arquivo)
        dados_escola_reativo(dados_carregados)
        
        if (is.null(dados_escola_padrao())) {
          dados_escola_padrao(dados_carregados)
        }
      }
    })
    
    mod_chat_server("chat_ia", dados_escola = dados_escola_reativo)
    
    output$nome_escola_titulo <- renderText({
      dados <- dados_escola_reativo()
      if (!is.null(dados)) {
        paste("Painel da Escola:", dados$nome_escola)
      } else {
        "Painel da Escola"
      }
    })
    
    concorrentes_disponiveis <- reactive({
      dados <- dados_escola_reativo()
      req(dados)
      
      nomes_all <- readRDS("data/escolas_privadas_nomelista.rds")
      geo_all_df <- readRDS("data/escolas_geo_com_empty_flag.rds") %>% sf::st_drop_geometry()
      
      escolas_municipio <- nomes_all %>%
        filter(nome_municipio == dados$nome_municipio, CO_ENTIDADE != codinep) %>%
        left_join(geo_all_df, by = c("CO_ENTIDADE" = "code_school")) %>%
        mutate(
          final_muni_name = coalesce(name_muni, nome_municipio, "Município Desconhecido"),
          display_name = paste0(NO_ENTIDADE, " - ", final_muni_name, " (", CO_ENTIDADE, ")")
        ) %>%
        select(CO_ENTIDADE, display_name)
      
      return(escolas_municipio)
    })
    
    observe({
      choices_df <- concorrentes_disponiveis()
      req(nrow(choices_df) > 0)
      
      choices_list <- setNames(choices_df$CO_ENTIDADE, choices_df$display_name)
      
      dados <- dados_escola_reativo()
      concorrentes_atuais <- dados$lista_concorrentes$id_escola
      
      updateSelectizeInput(session, 
                           "selecao_concorrentes", 
                           choices = choices_list,
                           selected = concorrentes_atuais,
                           server = TRUE)
    })
    
    observeEvent(input$btn_atualizar_analise, {
      req(input$selecao_concorrentes)
      
      showNotification("Atualizando e salvando nova seleção de concorrentes...", type = "message", duration = 5)
      
      dados_atuais <- dados_escola_reativo()
      
      save_concorrentes_selecionados(codinep, input$selecao_concorrentes)
      
      novos_dados_processados <- reprocessar_dados_concorrentes(codinep, input$selecao_concorrentes, dados_atuais$id_municipio)
      
      nomes_all <- readRDS("data/escolas_privadas_nomelista.rds")
      novos_concorrentes_info <- nomes_all %>% filter(CO_ENTIDADE %in% input$selecao_concorrentes)
      geo_all_df <- readRDS("data/escolas_geo_com_empty_flag.rds") %>% sf::st_drop_geometry()
      
      dados_atuais$lista_concorrentes <- novos_concorrentes_info %>%
        left_join(geo_all_df, by = c("CO_ENTIDADE" = "code_school")) %>%
        select(id_escola = CO_ENTIDADE, nome_escola = NO_ENTIDADE, latitude, longitude) %>%
        mutate(dist_metros = NA)
      
      dados_finais <- c(dados_atuais[c("id_escola", "nome_escola", "id_municipio", "nome_municipio", "latitude", "longitude", "lista_concorrentes", "data_extracao")], novos_dados_processados)
      
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep, ".rds"))
      saveRDS(dados_finais, caminho_arquivo)
      
      dados_escola_reativo(dados_finais)
      
      showNotification("Análise atualizada com sucesso!", type = "message", duration = 3)
    })
    
    observeEvent(input$btn_restaurar_padrao, {
      showNotification("Restaurando análise para os 5 concorrentes mais próximos...", type = "message", duration = 3)
      
      save_concorrentes_selecionados(codinep, character(0))
      
      dados_padrao_originais <- dados_escola_padrao()
      ids_concorrentes_padrao <- dados_padrao_originais$lista_concorrentes$id_escola
      
      dados_reprocessados_padrao <- reprocessar_dados_concorrentes(codinep, ids_concorrentes_padrao, dados_padrao_originais$id_municipio)
      
      dados_finais_padrao <- c(dados_padrao_originais[c("id_escola", "nome_escola", "id_municipio", "nome_municipio", "latitude", "longitude", "lista_concorrentes", "data_extracao")], dados_reprocessados_padrao)
      
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep, ".rds"))
      saveRDS(dados_finais_padrao, caminho_arquivo)
      
      dados_escola_reativo(dados_finais_padrao)
    })
    
    criar_caixa_kpi <- function(dados_segmento) {
      valor1 <- dados_segmento$valor_ano_1
      valor2 <- dados_segmento$valor_ano_2
      taxa <- dados_segmento$taxa_de_variacao
      
      cor_taxa <- if (is.na(taxa) || taxa == "N/A") {
        "#6c757d"
      } else if (as.numeric(sub("%", "", taxa)) > 0) {
        "#198754"
      } else {
        "#dc3545"
      }
      
      div(class = "stat-card",
          tagList(
            h5(dados_segmento$label),
            p(paste("2023:", valor1, "| 2024:", valor2)),
            h4(style = paste0("color:", cor_taxa, ";"), taxa)
          )
      )
    }
    
    output$kpis_escola <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_propria_escola, criar_caixa_kpi)
    })
    
    output$kpis_concorrentes <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_concorrentes_proximos, criar_caixa_kpi)
    })
    
    output$kpis_mercado <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_mercado_municipio, criar_caixa_kpi)
    })
    
    output$tabela_concorrentes <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$lista_concorrentes)
      tabela <- dados$lista_concorrentes %>%
        rename(`Cód. INEP` = id_escola, `Nome da Escola` = nome_escola)
      DT::datatable(tabela, options = list(pageLength = 10, scrollY = "450px"))
    })
    
    output$mapa_concorrentes <- leaflet::renderLeaflet({
      dados <- dados_escola_reativo()
      req(dados, dados$lista_concorrentes, !is.na(dados$latitude), !is.na(dados$longitude))
      escola_principal <- tibble(nome_escola = dados$nome_escola, latitude = dados$latitude, longitude = dados$longitude, tipo = "Sua Escola")
      concorrentes <- dados$lista_concorrentes %>% mutate(tipo = "Concorrente") %>% select(nome_escola, latitude, longitude, tipo)
      df_mapa <- bind_rows(escola_principal, concorrentes) %>% filter(!is.na(latitude), !is.na(longitude))
      icons <- leaflet::awesomeIcons(icon = 'school', library = 'fa', markerColor = ifelse(df_mapa$tipo == "Sua Escola", "blue", "red"), iconColor = '#FFFFFF')
      leaflet::leaflet(data = df_mapa) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addAwesomeMarkers(lng = ~longitude, lat = ~latitude, icon = icons, popup = ~htmltools::htmlEscape(nome_escola))
    })
    
    dados_infra_resumidos <- reactive({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_infraestrutura)
      df_infra <- dados$dados_infraestrutura
      escola_principal <- df_infra %>% filter(id_escola == codinep)
      media_municipal <- df_infra %>% filter(id_escola == "Média Municipal")
      media_concorrentes <- df_infra %>%
        filter(id_escola != codinep, id_escola != "Média Municipal") %>%
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
        mutate(id_escola = "Média Concorrentes")
      df_comparativo <- bind_rows(escola_principal, media_concorrentes, media_municipal)
      df_long <- df_comparativo %>%
        pivot_longer(cols = -id_escola, names_to = "indicador", values_to = "valor") %>%
        mutate(
          categoria = str_extract(indicador, "^[^_]+"),
          indicador_label = str_replace_all(indicador, "_", " ") %>% str_remove(categoria) %>% str_trim() %>% str_to_title()
        ) %>%
        pivot_wider(names_from = id_escola, values_from = valor) %>%
        rename(`Sua Escola` = !!sym(codinep))
      return(df_long)
    })
    
    output$cards_infra_detalhada <- renderUI({
      df_cards <- dados_infra_resumidos()
      criar_cartao_indicador <- function(row) {
        indicador_nome <- row[["indicador_label"]]
        valor_escola <- row[["Sua Escola"]]
        valor_concorrentes <- row[["Média Concorrentes"]]
        valor_municipal <- row[["Média Municipal"]]
        is_count <- str_detect(row[["indicador"]], "total_dispositivos")
        formatar_linha <- function(label, valor) {
          valor_num <- as.numeric(valor)
          valor_formatado <- if(is_count) round(valor_num, 1) else scales::percent(valor_num, accuracy = 0.1)
          tagList(div(class = "indicator-row", shiny::span(class = "indicator-label", label), shiny::span(class = "indicator-value", valor_formatado)), if(!is_count) { div(class = "progress-bar-container", div(class = "progress-bar-fill", style = paste0("width: ", valor_num * 100, "%;"))) })
        }
        column(4, div(class = "indicator-card", div(class = "indicator-card-title", indicador_nome), formatar_linha("Sua Escola", valor_escola), formatar_linha("Média Concorrentes", valor_concorrentes), formatar_linha("Média Municipal", valor_municipal)))
      }
      categorias <- unique(df_cards$categoria)
      lapply(categorias, function(cat) {
        df_cat <- df_cards %>% filter(categoria == cat)
        tagList(h5(str_to_title(cat)), fluidRow(apply(df_cat, 1, criar_cartao_indicador)), br())
      })
    })
    
    output$tabela_infra_detalhada_ui <- renderUI({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_infraestrutura, dados$lista_concorrentes)
      
      df_infra_raw <- dados$dados_infraestrutura
      nomes_concorrentes <- dados$lista_concorrentes %>% select(id_escola, nome_escola)
      
      df_detalhada <- df_infra_raw %>%
        filter(id_escola %in% c(codinep, dados$lista_concorrentes$id_escola)) %>%
        left_join(nomes_concorrentes, by = "id_escola") %>%
        mutate(label = ifelse(id_escola == codinep, "Sua Escola", nome_escola)) %>%
        select(-id_escola, -nome_escola) %>%
        pivot_longer(cols = -label, names_to = "indicador", values_to = "valor") %>%
        mutate(
          indicador_label = str_replace_all(indicador, "_", " ") %>% 
            str_remove("^(essencial|lazer|tec|apoio)") %>% 
            str_trim() %>% 
            str_to_title()
        ) %>%
        select(Indicador = indicador_label, Escola = label, Valor = valor) %>%
        pivot_wider(names_from = Escola, values_from = Valor) %>%
        relocate(Indicador, `Sua Escola`)
      
      df_formatada <- df_detalhada %>%
        mutate(across(-Indicador, ~ {
          if_else(
            Indicador == "Total Dispositivos Aluno",
            as.character(round(as.numeric(.), 1)),
            if_else(as.numeric(.) == 1, 
                    as.character(icon("check", class = "table-icon", style = "color: green;")), 
                    as.character(icon("times", class = "table-icon", style = "color: red;")))
          )
        }))
      
      tabela_html <- knitr::kable(df_formatada, "html", escape = FALSE, align = c('l', rep('c', ncol(df_formatada) - 1))) %>%
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = T) %>%
        kableExtra::scroll_box(width = "100%")
      
      HTML(tabela_html)
    })
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< INÍCIO DA CORREÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # --- Lógica para Visão Consolidada ---
    output$ui_desempenho_areas_consolidado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Comparativo de Médias por Área"), plotly::plotlyOutput(ns("plot_enem_areas_consolidado"))),
        column(5, h4("Tabela de Médias"), DT::dataTableOutput(ns("tabela_enem_areas_consolidado")))
      )
    })
    
    output$plot_enem_areas_consolidado <- plotly::renderPlotly({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      df_enem <- dados$dados_enem_areas %>%
        filter(escola_label %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      dados$dados_enem_areas %>%
        filter(escola_label %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        pivot_wider(names_from = escola_label, values_from = nota) %>%
        rename(Área = area) %>%
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    output$ui_desempenho_redacao_consolidado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Detalhamento da Nota de Redação"), plotly::plotlyOutput(ns("plot_enem_redacao_consolidado"))),
        column(5, h4("Tabela de Competências"), DT::dataTableOutput(ns("tabela_enem_redacao_consolidado")))
      )
    })
    
    output$plot_enem_redacao_consolidado <- plotly::renderPlotly({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      df_redacao <- dados$dados_enem_redacao %>%
        filter(escola_label %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      dados$dados_enem_redacao %>%
        filter(escola_label %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        pivot_wider(names_from = escola_label, values_from = nota) %>%
        rename(Competência = competencia) %>%
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    # --- Lógica para Visão Detalhada ---
    output$ui_desempenho_areas_detalhado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Comparativo de Médias por Área"), plotly::plotlyOutput(ns("plot_enem_areas_detalhado"))),
        column(5, h4("Tabela de Médias"), DT::dataTableOutput(ns("tabela_enem_areas_detalhado")))
      )
    })
    
    output$plot_enem_areas_detalhado <- plotly::renderPlotly({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      df_enem <- dados$dados_enem_areas %>%
        filter(!escola_label %in% c("Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      dados$dados_enem_areas %>%
        filter(!escola_label %in% c("Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        pivot_wider(names_from = escola_label, values_from = nota) %>%
        rename(Área = area) %>%
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    output$ui_desempenho_redacao_detalhado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Detalhamento da Nota de Redação"), plotly::plotlyOutput(ns("plot_enem_redacao_detalhado"))),
        column(5, h4("Tabela de Competências"), DT::dataTableOutput(ns("tabela_enem_redacao_detalhado")))
      )
    })
    
    output$plot_enem_redacao_detalhado <- plotly::renderPlotly({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      df_redacao <- dados$dados_enem_redacao %>%
        filter(!escola_label %in% c("Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      dados$dados_enem_redacao %>%
        filter(!escola_label %in% c("Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        pivot_wider(names_from = escola_label, values_from = nota) %>%
        rename(Competência = competencia) %>%
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
  })
}