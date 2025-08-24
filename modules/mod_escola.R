# --- Bibliotecas necessárias ---
library(plotly)
library(DT)
library(htmlwidgets)

mod_escola_ui <- function(id) {
  ns <- NS(id)
  
  tagList(# No UI, adicione este código no tagList principal
    tags$script(HTML("
$(document).on('shiny:connected', function() {
  // Melhora a aparência do Selectize
  $('.selectize-input input').attr('placeholder', 'Digite para buscar escolas...');
  
  // Ajusta o dropdown para ficar acima se não couber embaixo
  $(document).on('click', '.selectize-control', function() {
    var $dropdown = $(this).find('.selectize-dropdown');
    var windowHeight = $(window).height();
    var dropdownHeight = $dropdown.outerHeight();
    var inputOffset = $(this).offset().top;
    var inputHeight = $(this).outerHeight();
    
    if (inputOffset + inputHeight + dropdownHeight > windowHeight - 50) {
      $dropdown.addClass('dropdown-up');
    } else {
      $dropdown.removeClass('dropdown-up');
    }
  });
});
")),
    shinyjs::useShinyjs(),
    
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
               ),
               # Botão de download na Visão Geral
               div(class = "benchmark-download text-center",
                   style = "margin: 2rem 0;",
                   downloadButton(ns("dl_onepager"), "📄 Baixar Relatório Completo (PDF)", 
                                  class = "btn-success btn-lg",
                                  style = "font-size: 1.1rem; padding: 12px 24px;")
               )
      ),
      
      # --- Aba Benchmark Concorrentes ---
      tabPanel("Benchmark Concorrentes", 
               div(class = "benchmark-container",

                   
                   
                   # Card de Análise Detalhada
                   div(class = "benchmark-card",
                       div(class = "benchmark-card-title",
                           icon("chart-bar", style = "color: var(--primary);"),
                           "Análise Detalhada por Concorrente"
                       ),
                       p("Comparativo detalhado das métricas de matrícula entre sua escola e os concorrentes selecionados. Os dados mostram a variação 2023 vs 2024."),
                       
                       uiOutput(ns("kpis_concorrentes_detalhados"))
                   ),
                   div(class = "benchmark-card",
                       div(class = "benchmark-card-title",
                           icon("users", style = "color: var(--primary);"),
                           "Seleção de Concorrentes"
                       ),
                       p("Selecione até 5 escolas concorrentes para análise comparativa detalhada. A seleção personalizada permite focar nos competidores mais relevantes para sua estratégia."),
                       
                       div(class = "selectize-container",
                           selectizeInput(ns("selecao_concorrentes"), 
                                          label = NULL,
                                          choices = NULL,
                                          multiple = TRUE,
                                          width = "100%",
                                          options = list(
                                            maxItems = 5, 
                                            placeholder = 'Digite o nome da escola...',
                                            dropdownParent = 'body',
                                            highlight = TRUE,
                                            render = I("{
                                        item: function(item, escape) {
                                          return '<div class=\"item\" style=\"padding: 8px; border-bottom: 1px solid #f0f0f0;\">' + escape(item.value) + '</div>';
                                        },
                                        option: function(item, escape) {
                                          return '<div style=\"padding: 8px; border-bottom: 1px solid #f0f0f0;\">' + escape(item.value) + '</div>';
                                        }
                                      }")
                                          ))
                       ),
                       
                       div(class = "benchmark-actions",
                           actionButton(ns("btn_atualizar_analise"), 
                                        label = tagList(icon("sync"), "Atualizar Análise"), 
                                        class = "btn-primary btn-icon"),
                           actionButton(ns("btn_restaurar_padrao"), 
                                        label = tagList(icon("undo"), "Restaurar Padrão"), 
                                        class = "btn-secondary btn-icon")
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
      
      # --- Aba Desempenho Acadêmico com Sub-abas ---
      tabPanel("Desempenho Acadêmico",
               div(class = "academic-performance",
                   div(class = "performance-card",
                       h3(class = "academic-section-title", "Análise de Desempenho - ENEM 2024"),
                       p("Comparativo das médias do ENEM por área de conhecimento e detalhamento das competências da redação.")
                   ),
                   
                   tabsetPanel(
                     type = "tabs",
                     
                     tabPanel("Visão Consolidada",
                              br(),
                              div(class = "performance-card",
                                  h4("Comparativo de Médias por Área"),
                                  uiOutput(ns("ui_desempenho_areas_consolidado"))
                              ),
                              
                              div(class = "performance-card",
                                  h4("Detalhamento da Nota de Redação"),
                                  uiOutput(ns("ui_desempenho_redacao_consolidado"))
                              )
                     ),
                     
                     tabPanel("Detalhe por Concorrente",
                              br(),
                              div(class = "performance-card",
                                  h4("Comparativo de Médias por Área"),
                                  uiOutput(ns("ui_desempenho_areas_detalhado"))
                              ),
                              
                              div(class = "performance-card",
                                  h4("Detalhamento da Nota de Redação"),
                                  uiOutput(ns("ui_desempenho_redacao_detalhado"))
                              )
                     )
                   )
               )
      ),
      
      tabPanel("Chat com IA",
               icon = icon("robot"),
               mod_chat_ui(ns("chat_ia"))
      )
    )
  )
}

mod_escola_server <- function(id, user, codinep) {
  moduleServer(id, function(input, output, session) {
    
    # Carregar utilitários
    source("utils/preprocess_utils.R", local = TRUE)
    source("utils/db_utils.R", local = TRUE)
    
    dados_escola_reativo <- reactiveVal(NULL)
    dados_escola_padrao <- reactiveVal(NULL)
    dados_concorrentes_individuais <- reactiveVal(NULL)
    
    # Observar mudanças na seleção de concorrentes
    observeEvent(input$selecao_concorrentes, {
      req(input$selecao_concorrentes, length(input$selecao_concorrentes) > 0)
      
      showNotification("Carregando dados individuais dos concorrentes...")
      
      tryCatch({
        dados_individuais <- obter_dados_individuais_concorrentes(
          codinep, 
          input$selecao_concorrentes,
          dados_escola_reativo()$id_municipio
        )
        dados_concorrentes_individuais(dados_individuais)
      }, error = function(e) {
        showNotification(paste("Erro ao carregar dados individuais:", e$message), type = "error")
      })
    })
    
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
    # Função para obter nome do concorrente pelo ID
    get_nome_concorrente <- function(id_conc) {
      dados <- dados_escola_reativo()
      if (!is.null(dados$lista_concorrentes)) {
        conc <- dados$lista_concorrentes %>%
          filter(id_escola == id_conc) %>%
          pull(nome_escola)
        
        if (length(conc) > 0 && !is.na(conc)) {
          return(conc)
        }
      }
      
      # Fallback: buscar da base completa
      tryCatch({
        nomes_df <- readRDS("data/escolas_privadas_nomelista.rds")
        nome <- nomes_df %>%
          filter(CO_ENTIDADE == id_conc) %>%
          pull(NO_ENTIDADE)
        
        if (length(nome) > 0) return(nome)
        return(paste("Concorrente", id_conc))
      }, error = function(e) {
        return(paste("Concorrente", id_conc))
      })
    }
    mod_chat_server("chat_ia", dados_escola = dados_escola_reativo)
    
    output$nome_escola_titulo <- renderText({
      dados <- dados_escola_reativo()
      nm <- if (!is.null(dados)) (dados$nome_escola %||% "") else ""
      if (!nzchar(nm)) nm <- "Sua Escola"
      nm
    })
    
    concorrentes_disponiveis <- reactive({
      dados <- dados_escola_reativo(); req(dados)
      nomes_all <- readRDS("data/escolas_privadas_nomelista.rds")
      escolas_muni <- nomes_all %>%
        dplyr::filter(
          .data[[intersect(c("nome_municipio","NM_MUNICIPIO","municipio"), names(.))[1]]] == dados$nome_municipio,
          CO_ENTIDADE != codinep
        ) %>%
        dplyr::mutate(
          display_name = paste0(NO_ENTIDADE, " - ", dados$nome_municipio, " (", CO_ENTIDADE, ")")
        ) %>%
        dplyr::select(CO_ENTIDADE, display_name) %>%
        dplyr::distinct()
      escolas_muni
    })
    
    observe({
      df <- concorrentes_disponiveis()
      req(nrow(df) > 0)
      
      # A abordagem original que funcionava
      choices <- df$display_name
      names(choices) <- df$CO_ENTIDADE
      
      dados <- dados_escola_reativo()
      sel <- tryCatch({
        # Para cada concorrente selecionado, encontrar o display_name correspondente
        concorrentes_atuais <- dados$lista_concorrentes$id_escola
        matches <- df$display_name[df$CO_ENTIDADE %in% concorrentes_atuais]
        as.character(matches)
      }, error = function(e) NULL)
      
      updateSelectizeInput(session,
                           "selecao_concorrentes",
                           choices = choices,
                           selected = sel,
                           server = TRUE
      )
    })
    
    observeEvent(input$btn_atualizar_analise, {
      req(input$selecao_concorrentes)
      showNotification("Atualizando e salvando nova seleção de concorrentes...", type = "message", duration = 5)
      
      dados_atuais <- dados_escola_reativo(); req(dados_atuais)
      codinep_ch <- as.character(codinep)
      
      # input$selecao_concorrentes já contém os CO_ENTIDADE (valores)
      # porque usamos setNames(df$CO_ENTIDADE, df$display_name)
      selected_ids <- input$selecao_concorrentes
      
      save_concorrentes_selecionados(codinep, selected_ids)
      
      novos_proc <- reprocessar_dados_concorrentes(
        codinep = codinep_ch,
        ids_concorrentes = as.character(selected_ids),
        id_municipio = dados_atuais$id_municipio
      )
      
      build_concorrentes_sf <- function(ids_sel, codinep_ch, escola_lon = NA_real_, escola_lat = NA_real_) {
        # ... (mantenha esta função como estava) ...
      }
      
      conc_sf <- build_concorrentes_sf(
        ids_sel = as.character(selected_ids),
        codinep_ch = codinep_ch,
        escola_lon = suppressWarnings(as.numeric(dados_atuais$longitude)),
        escola_lat = suppressWarnings(as.numeric(dados_atuais$latitude))
      )
      
      dados_atuais$lista_concorrentes <- conc_sf
      dados_finais <- c(
        dados_atuais[c("id_escola","nome_escola","id_municipio","nome_municipio","latitude","longitude","lista_concorrentes","data_extracao")],
        novos_proc
      )
      
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep_ch, ".rds"))
      saveRDS(dados_finais, caminho_arquivo)
      dados_escola_reativo(dados_finais)
      
      showNotification("Análise atualizada com sucesso!", type = "message", duration = 3)
    })
    
    observeEvent(input$btn_restaurar_padrao, {
      showNotification("Restaurando análise para os 5 concorrentes mais próximos...", type = "message", duration = 3)
      
      # Para restaurar padrão, passamos character(0) para limpar a seleção personalizada
      save_concorrentes_selecionados(codinep, character(0))
      
      dados_padrao_originais <- dados_escola_padrao(); req(dados_padrao_originais)
      codinep_ch <- as.character(codinep)
      ids_concorrentes_padrao <- as.character(dados_padrao_originais$lista_concorrentes$id_escola)
      
      # Atualiza o selectize para mostrar os concorrentes padrão
      updateSelectizeInput(session, "selecao_concorrentes", selected = ids_concorrentes_padrao)
      
      dados_reprocessados_padrao <- reprocessar_dados_concorrentes(
        codinep = codinep_ch,
        ids_concorrentes = ids_concorrentes_padrao,
        id_municipio = dados_padrao_originais$id_municipio
      )
      
      conc_sf <- build_concorrentes_sf(ids_concorrentes_padrao, codinep_ch)
      
      dados_finais_padrao <- c(
        list(
          id_escola = dados_padrao_originais$id_escola,
          nome_escola = dados_padrao_originais$nome_escola,
          id_municipio = dados_padrao_originais$id_municipio,
          nome_municipio = dados_padrao_originais$nome_municipio,
          latitude = dados_padrao_originais$latitude,
          longitude = dados_padrao_originais$longitude,
          lista_concorrentes = conc_sf,
          data_extracao = Sys.time()
        ),
        dados_reprocessados_padrao
      )
      
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep_ch, ".rds"))
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
    
    output$kpis_concorrentes_detalhados <- renderUI({
      dados_indiv <- dados_concorrentes_individuais()
      req(dados_indiv, length(dados_indiv) > 0)
      
      concorrentes_ids <- names(dados_indiv)
      
      div(class = "concorrentes-grid",
          lapply(concorrentes_ids, function(id_conc) {
            concorrente_info <- dados_escola_reativo()$lista_concorrentes %>%
              filter(id_escola == id_conc)
            
            nome_concorrente <- get_nome_concorrente(id_conc)
            distancia <- ifelse(!is.na(concorrente_info$dist_metros), 
                                paste(round(concorrente_info$dist_metros), "m de distância"), 
                                "Distância não disponível")
            
            div(class = "concorrente-card",
                div(class = "concorrente-header",
                    h5(class = "concorrente-nome", nome_concorrente),
                    p(class = "concorrente-distancia",
                      icon("map-marker-alt"), distancia)
                ),
                
                div(class = "concorrente-content",
                    div(class = "concorrente-stats",
                        lapply(dados_indiv[[id_conc]], function(segmento) {
                          if (!is.null(segmento$valor_ano_1) && !is.null(segmento$valor_ano_2)) {
                            div(class = "stat-item",
                                div(class = "stat-label", segmento$label),
                                div(class = "stat-values",
                                    p(class = "stat-value", 
                                      paste("2023:", format(segmento$valor_ano_1, big.mark = ".", decimal.mark = ","))),
                                    p(class = "stat-value", 
                                      paste("2024:", format(segmento$valor_ano_2, big.mark = ".", decimal.mark = ",")))
                                ),
                                p(class = "stat-variation",
                                  style = paste0("background-color:", 
                                                 ifelse(grepl("-", segmento$taxa_de_variacao), 
                                                        "rgba(220, 53, 69, 0.1)", 
                                                        "rgba(25, 135, 84, 0.1)"),
                                                 "; color:",
                                                 ifelse(grepl("-", segmento$taxa_de_variacao), 
                                                        "#dc3545", 
                                                        "#198754"),
                                                 ";"),
                                  segmento$taxa_de_variacao)
                            )
                          }
                        })
                    )
                )
            )
          })
      )
    })
    
    output$kpis_escola <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_propria_escola, criar_caixa_kpi)
    })
    
    output$kpis_mercado <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_mercado_municipio, criar_caixa_kpi)
    })
    
    output$kpis_concorrentes <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_concorrentes_proximos, criar_caixa_kpi)
    })
    
    output$tabela_concorrentes <- DT::renderDataTable({
      dados <- dados_escola_reativo(); req(dados, dados$lista_concorrentes)
      df <- dados$lista_concorrentes
      if (inherits(df, "sf")) df <- sf::st_drop_geometry(df)
      tabela <- df %>%
        dplyr::select(`Cód. INEP` = id_escola, `Nome da Escola` = nome_escola,
                      `Distância (m)` = dplyr::any_of("dist_metros"))
      DT::datatable(tabela, options = list(pageLength = 10, scrollY = "450px"))
    })
    
    output$mapa_concorrentes <- leaflet::renderLeaflet({
      req(dados_escola_reativo)
      dados <- dados_escola_reativo(); req(dados)
      
      # ... (mantenha a função do mapa como estava) ...
    })
    
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
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      
      df <- dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Área = area, Categoria = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Categoria, values_from = Nota)
      
      DT::datatable(
        df,
        options = list(
          dom = 't',
          pageLength = 10
        ),
        rownames = FALSE
      )
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
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      
      df <- dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Competência = competencia, Categoria = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Categoria, values_from = Nota)
      
      DT::datatable(
        df,
        options = list(
          dom = 't',
          pageLength = 10
        ),
        rownames = FALSE
      )
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
        filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      
      df <- dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Concorrente")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Área = area, Escola = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Escola, values_from = Nota)
      
      DT::datatable(
        df,
        options = list(
          dom = 't',
          pageLength = 10
        ),
        rownames = FALSE
      )
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
        filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      
      df <- dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Concorrente")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Competência = competencia, Escola = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Escola, values_from = Nota)
      
      DT::datatable(
        df,
        options = list(
          dom = 't',
          pageLength = 10
        ),
        rownames = FALSE
      )
    })
    

    
    
    output$dl_onepager <- downloadHandler(
      filename = function() {
        d <- dados_escola_reativo()
        paste0("Relatorio_", d$id_escola, "_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
      },
      content = function(file) {
        d <- dados_escola_reativo()
        shiny::validate(shiny::need(!is.null(d), "Dados não carregados."))
        
        tryCatch({
          build_onepager(dados = d, out_pdf = file)
          if (!file.exists(file)) stop("Falha ao criar o PDF.")
        }, error = function(e) {
          showNotification(
            paste("Erro ao gerar o relatório PDF:", conditionMessage(e)),
            type = "error", duration = 8
          )
          stop(e)
        })
      }
    )
    # --- INFRAESTRUTURA: Diagnóstico e Correção ---
    
    # Debug para verificar os dados
    observe({
      dados <- dados_escola_reativo()
      if (!is.null(dados)) {
        cat("=== DEBUG INFRAESTRUTURA ===\n")
        cat("Dados carregados: ", !is.null(dados), "\n")
        cat("Dados infraestrutura existe: ", !is.null(dados$dados_infraestrutura), "\n")
        
        if (!is.null(dados$dados_infraestrutura)) {
          cat("Tipo: ", class(dados$dados_infraestrutura), "\n")
          cat("Estrutura: \n")
          print(str(dados$dados_infraestrutura))
          cat("Colunas: ", names(dados$dados_infraestrutura), "\n")
        }
        cat("=============================\n")
      }
    })
    
    # --- INFRAESTRUTURA: Versão Corrigida com Identidade Visual ---
    # --- INFRAESTRUTURA: Versão Corrigida (SEM ERROS) ---
    
    output$cards_infra_detalhada <- renderUI({
      dados <- dados_escola_reativo()
      
      if (is.null(dados) || is.null(dados$dados_infraestrutura)) {
        return(
          div(class = "alert alert-info",
              icon("sync", class = "spinning"),
              h4("Carregando dados de infraestrutura..."),
              p("Aguarde enquanto os dados são processados.")
          )
        )
      }
      
      tryCatch({
        df_infra <- dados$dados_infraestrutura
        
        # Encontra nossa escola
        nossa_escola <- df_infra %>% 
          filter(id_escola == as.character(codinep))
        
        # Calcula média dos concorrentes
        concorrentes <- df_infra %>%
          filter(!id_escola %in% c(as.character(codinep), "Média Municipal")) 
        
        if (nrow(concorrentes) > 0) {
          media_concorrentes <- concorrentes %>%
            summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>%
            mutate(id_escola = "Média Concorrentes")
        } else {
          media_concorrentes <- data.frame(id_escola = "Média Concorrentes")
          numeric_cols <- names(df_infra)[sapply(df_infra, is.numeric)]
          for (col in numeric_cols) {
            media_concorrentes[[col]] <- NA
          }
        }
        
        # Média municipal
        media_municipal <- df_infra %>% 
          filter(id_escola == "Média Municipal")
        
        if (nrow(media_municipal) == 0) {
          media_municipal <- data.frame(id_escola = "Média Municipal")
          for (col in names(df_infra)[-1]) {
            media_municipal[[col]] <- NA
          }
        }
        
        # Combina os dados
        df_comparativo <- bind_rows(nossa_escola, media_concorrentes, media_municipal)
        
        # Transforma para formato longo
        df_long <- df_comparativo %>%
          pivot_longer(cols = -id_escola, names_to = "indicador", values_to = "valor") %>%
          mutate(
            categoria = str_extract(indicador, "^[^_]+"),
            indicador_label = case_when(
              indicador == "essencial_agua_potavel" ~ "Água Potável",
              indicador == "essencial_biblioteca" ~ "Biblioteca",
              indicador == "essencial_lab_informatica" ~ "Lab. Informática",
              indicador == "essencial_lab_ciencias" ~ "Lab. Ciências",
              indicador == "essencial_quadra_esportes" ~ "Quadra Esportes",
              indicador == "essencial_acessibilidade_pne" ~ "Acessibilidade PNE",
              indicador == "essencial_refeitorio" ~ "Refeitório",
              indicador == "lazer_area_verde" ~ "Área Verde",
              indicador == "lazer_parque_infantil" ~ "Parque Infantil",
              indicador == "lazer_quadra_coberta" ~ "Quadra Coberta",
              indicador == "tec_internet_geral" ~ "Internet Geral",
              indicador == "tec_banda_larga" ~ "Banda Larga",
              indicador == "tec_internet_para_alunos" ~ "Internet Alunos",
              indicador == "tec_lousa_digital" ~ "Lousa Digital",
              indicador == "tec_total_dispositivos_aluno" ~ "Dispositivos por Aluno",
              indicador == "apoio_psicologo" ~ "Psicólogo",
              indicador == "apoio_bibliotecario" ~ "Bibliotecário",
              TRUE ~ indicador  # Fallback seguro
            )
          ) %>%
          pivot_wider(names_from = id_escola, values_from = valor)
        
        # Função para criar cards - RESUMO COMPARATIVO (porcentagem)
        criar_cartao_indicador <- function(row) {
          indicador_nome <- row[["indicador_label"]]
          codinep_str <- as.character(codinep)
          
          # Valores
          valor_escola <- if (!is.null(row[[codinep_str]])) row[[codinep_str]] else NA
          valor_concorrentes <- if (!is.null(row[["Média Concorrentes"]])) row[["Média Concorrentes"]] else NA
          valor_municipal <- if (!is.null(row[["Média Municipal"]])) row[["Média Municipal"]] else NA
          
          is_boolean <- indicador_nome != "Dispositivos por Aluno"
          
          # Formatação para RESUMO (porcentagem)
          formatar_valor_resumo <- function(valor, is_boolean, tipo = "default") {
            if (is.na(valor)) return(tags$span("N/A", class = "text-muted"))
            
            if (tipo == "municipal") {
              # Média municipal em porcentagem
              return(tags$span(scales::percent(valor, accuracy = 1), 
                               class = "text-primary"))
            } else if (is_boolean) {
              # Booleanos em porcentagem
              return(tags$span(scales::percent(valor, accuracy = 1),
                               class = ifelse(valor >= 0.5, "text-success", "text-warning")))
            } else {
              # Dispositivos por aluno - NÚMERO INTEIRO
              return(tags$span(round(valor), class = "text-info"))  # ← round() sem decimal
            }
          }
          
          formatar_linha <- function(label, valor, emoji, tipo = "default") {
            tags$div(class = "indicator-row",
                     tags$span(class = "indicator-label", 
                               tags$span(style = "font-size: 1.2em; margin-right: 8px;", emoji),
                               label),
                     tags$span(class = "indicator-value", 
                               formatar_valor_resumo(valor, is_boolean, tipo))
            )
          }
          
          # Emoji para cada categoria
          emoji_categoria <- case_when(
            grepl("essencial", row[["categoria"]]) ~ "🏗️",
            grepl("lazer", row[["categoria"]]) ~ "🎯",
            grepl("tec", row[["categoria"]]) ~ "💻",
            grepl("apoio", row[["categoria"]]) ~ "👥",
            TRUE ~ "📊"
          )
          
          div(class = "col-md-6 col-lg-4 mb-3",
              div(class = "indicator-card",
                  tags$div(class = "indicator-header",
                           tags$h5(class = "indicator-card-title", 
                                   tags$span(style = "font-size: 1.2em; margin-right: 8px;", emoji_categoria),
                                   indicador_nome)
                  ),
                  
                  formatar_linha("Sua Escola", valor_escola, "🏫"),
                  formatar_linha("Concorrentes", valor_concorrentes, "👥"),
                  formatar_linha("Município", valor_municipal, "🏙️", "municipal")
              )
          )
        }
        
        # Agrupa por categoria
        categorias <- unique(df_long$categoria)
        tagList(
          lapply(categorias, function(cat) {
            df_cat <- df_long %>% filter(categoria == cat)
            if (nrow(df_cat) > 0) {
              tagList(
                tags$div(class = "category-header-resumo",
                         tags$h4(class = "category-title-resumo",
                                 case_when(
                                   cat == "essencial" ~ "🏗️ Infraestrutura Essencial",
                                   cat == "lazer" ~ "🎯 Áreas de Lazer",
                                   cat == "tec" ~ "💻 Tecnologia",
                                   cat == "apoio" ~ "👥 Apoio Profissional",
                                   TRUE ~ cat
                                 )
                         )
                ),
                
                fluidRow(
                  lapply(1:nrow(df_cat), function(i) {
                    criar_cartao_indicador(df_cat[i, ])
                  })
                ),
                tags$hr()
              )
            }
          })
        )
        
      }, error = function(e) {
        div(class = "alert alert-danger",
            icon("exclamation-triangle"),
            h4("Erro ao carregar dados"),
            p("Detalhes do erro:", tags$code(e$message))
        )
      })
    })
    
    output$tabela_infra_detalhada_ui <- renderUI({
      dados <- dados_escola_reativo()
      
      if (is.null(dados) || is.null(dados$dados_infraestrutura)) {
        return(
          div(class = "alert alert-info",
              icon("sync", class = "spinning"),
              h4("Carregando dados de infraestrutura..."),
              p("Aguarde enquanto os dados são processados.")
          )
        )
      }
      
      tryCatch({
        df_infra <- dados$dados_infraestrutura
        concorrentes <- dados$lista_concorrentes
        
        # Mapeia ID para nome da escola - CORREÇÃO PARA SF
        mapa_nomes <- data.frame(
          id_escola = character(),
          nome_escola = character(),
          stringsAsFactors = FALSE
        )
        
        # Adiciona concorrentes se existirem - TRATAMENTO PARA OBJETOS SF
        if (!is.null(concorrentes) && nrow(concorrentes) > 0) {
          # Converte objeto sf para dataframe se necessário
          if (inherits(concorrentes, "sf")) {
            concorrentes_df <- sf::st_drop_geometry(concorrentes)
          } else {
            concorrentes_df <- as.data.frame(concorrentes)
          }
          
          if ("id_escola" %in% names(concorrentes_df) && "nome_escola" %in% names(concorrentes_df)) {
            mapa_nomes <- concorrentes_df %>%
              select(id_escola, nome_escola) %>%
              mutate_all(as.character) %>%
              distinct()
          }
        }
        
        # Adiciona escola principal e média municipal
        mapa_nomes <- bind_rows(
          mapa_nomes,
          data.frame(
            id_escola = c(as.character(codinep), "Média Municipal"),
            nome_escola = c("Sua Escola", "Média Municipal"),
            stringsAsFactors = FALSE
          )
        )
        
        # Transformação dos dados
        df_long <- df_infra %>%
          mutate(id_escola = as.character(id_escola)) %>%
          left_join(mapa_nomes, by = "id_escola") %>%
          mutate(Escola = coalesce(nome_escola, id_escola)) %>%
          select(-id_escola, -nome_escola) %>%
          pivot_longer(
            cols = -Escola, 
            names_to = "Item", 
            values_to = "Valor"
          ) %>%
          pivot_wider(
            names_from = Escola, 
            values_from = Valor
          )
        
        # Renomeia os itens para nomes amigáveis
        itens_amigaveis <- c(
          "essencial_agua_potavel" = "Água Potável",
          "essencial_biblioteca" = "Biblioteca",
          "essencial_lab_informatica" = "Lab. Informática",
          "essencial_lab_ciencias" = "Lab. Ciências",
          "essencial_quadra_esportes" = "Quadra Esportes",
          "essencial_acessibilidade_pne" = "Acessibilidade PNE",
          "essencial_refeitorio" = "Refeitório",
          "lazer_area_verde" = "Área Verde",
          "lazer_parque_infantil" = "Parque Infantil",
          "lazer_quadra_coberta" = "Quadra Coberta",
          "tec_internet_geral" = "Internet Geral",
          "tec_banda_larga" = "Banda Larga",
          "tec_internet_para_alunos" = "Internet Alunos",
          "tec_lousa_digital" = "Lousa Digital",
          "tec_total_dispositivos_aluno" = "Dispositivos por Aluno",
          "apoio_psicologo" = "Psicólogo",
          "apoio_bibliotecario" = "Bibliotecário"
        )
        
        df_long$Item <- itens_amigaveis[df_long$Item]
        df_long$Item <- as.character(df_long$Item)
        
        # Aplica formatação
        escolas_colunas <- names(df_long)[names(df_long) != "Item"]
        
        for (col in escolas_colunas) {
          df_long[[col]] <- sapply(seq_len(nrow(df_long)), function(i) {
            valor <- df_long[[col]][i]
            item <- df_long$Item[i]
            
            if (is.na(valor)) return("❓")
            
            if (item == "Dispositivos por Aluno") {
              return(as.character(round(as.numeric(valor))))
            } else if (col == "Média Municipal") {
              return(scales::percent(as.numeric(valor), accuracy = 1))
            } else {
              return(ifelse(as.numeric(valor) == 1, "✅", "❌"))
            }
          })
        }
        
        # Ordenar itens
        ordem_itens <- c(
          "Água Potável", "Biblioteca", "Lab. Informática", "Lab. Ciências",
          "Quadra Esportes", "Acessibilidade PNE", "Refeitório",
          "Área Verde", "Parque Infantil", "Quadra Coberta",
          "Internet Geral", "Banda Larga", "Internet Alunos", "Lousa Digital", "Dispositivos por Aluno",
          "Psicólogo", "Bibliotecário"
        )
        
        ordem_itens <- ordem_itens[ordem_itens %in% df_long$Item]
        
        if (length(ordem_itens) > 0) {
          df_long <- df_long %>%
            mutate(Item = factor(Item, levels = ordem_itens)) %>%
            arrange(Item) %>%
            mutate(Item = as.character(Item))
        }
        
        div(
          h4("Visão Detalhada por Escola", class = "mb-3"),
          div(class = "table-responsive",
              DT::renderDataTable({
                DT::datatable(
                  df_long,
                  class = "performance-table",
                  options = list(
                    dom = 'Bfrtip',
                    pageLength = 20,
                    scrollX = TRUE,
                    buttons = c('copy', 'csv', 'excel'),
                    language = list(
                      url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json'
                    )
                  ),
                  rownames = FALSE,
                  escape = FALSE
                ) %>%
                  DT::formatStyle(
                    columns = escolas_colunas,
                    textAlign = 'center',
                    fontSize = '16px'
                  ) %>%
                  DT::formatStyle(
                    "Item",
                    fontWeight = 'bold'
                  )
              })
          )
        )
        
      }, error = function(e) {
        div(class = "alert alert-danger",
            icon("exclamation-triangle"),
            h4("Erro ao carregar tabela"),
            p("Detalhes do erro:", tags$code(e$message)),
            p("Por favor, contate o suporte técnico.")
        )
      })
    })

    # Reactive value para o nome da escola
    nome_exportado <- reactiveVal(NULL)
    
    # Quando os dados carregarem, captura o nome
    observe({
      dados <- dados_escola_reativo()
      if (!is.null(dados) && !is.null(dados$nome_escola)) {
        nome_exportado(dados$nome_escola)
      }
    })
    
    # Exporta o nome para o app principal
    return(
      list(
        nome_escola = reactive({ nome_exportado() })
      )
    )

    
  })
}
    
    
