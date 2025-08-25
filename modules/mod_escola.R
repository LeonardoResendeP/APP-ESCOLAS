# --- Bibliotecas necessárias ---
library(plotly)
library(DT)
library(htmlwidgets)
library(shinyjs)
library(dplyr)
library(tidyr)
library(stringr)

# ===================================================================
#      MOD_ESCOLA_UI (VERSÃO FINAL)
# ===================================================================
mod_escola_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    navbarPage(
      title = NULL,
      tabPanel("Visão Geral", 
               fluidRow(column(12, h3("Resumo de Matrículas (Variação 2023 vs 2024)"), p("Comparativo do desempenho da sua escola em relação aos concorrentes selecionados e ao mercado total do seu município."))),
               br(),
               fluidRow(
                 column(4, h4(icon("school"), "Sua Escola"), hr(), uiOutput(ns("kpis_escola"))),
                 column(4, h4(icon("users"), "Média dos Concorrentes"), hr(), uiOutput(ns("kpis_concorrentes"))),
                 column(4, h4(icon("chart-pie"), "Mercado (Município)"), hr(), uiOutput(ns("kpis_mercado")))
               ),
               div(class = "benchmark-download text-center", style = "margin: 2rem 0;",
                   downloadButton(ns("dl_onepager"), "📄 Baixar Relatório Completo (PDF)", class = "btn-success btn-lg", style = "font-size: 1.1rem; padding: 12px 24px;"))
      ),
      tabPanel("Benchmark Concorrentes", 
               div(class = "benchmark-container",
                   div(class = "benchmark-card",
                       div(class = "benchmark-card-title", icon("chart-bar", style = "color: var(--primary);"), "Análise Detalhada por Concorrente"),
                       p("Comparativo detalhado das métricas de matrícula entre sua escola e os concorrentes selecionados. Os dados mostram a variação 2023 vs 2024."),
                       uiOutput(ns("kpis_concorrentes_detalhados"))
                   ),
                   div(class = "benchmark-card",
                       div(class = "benchmark-card-title", icon("users", style = "color: var(--primary);"), "Seleção de Concorrentes"),
                       p("Selecione até 5 escolas concorrentes do seu município para análise comparativa detalhada."),
                       div(class = "selectize-container",
                           selectizeInput(ns("selecao_concorrentes"), 
                                          label = NULL, choices = NULL, multiple = TRUE, width = "100%",
                                          options = list(
                                            maxItems = 5, placeholder = 'Digite para buscar na base de escolas...',
                                            valueField = 'value', labelField = 'label', searchField = 'label',
                                            render = I("{
                                              option: function(item, escape) {
                                                return '<div style=\"padding: 8px; border-bottom: 1px solid #f0f0f0;\">' +
                                                         '<strong>' + escape(item.label.split(' - ')[0]) + '</strong>' +
                                                         '<br><small style=\"color: #888;\">' + escape(item.label.split(' - ').slice(1).join(' - ')) + '</small>' +
                                                       '</div>';
                                              },
                                              item: function(item, escape) { return '<div>' + escape(item.label) + '</div>'; }
                                            }")
                                          ))
                       ),
                       div(class = "benchmark-actions",
                           actionButton(ns("btn_atualizar_analise"), label = tagList(icon("sync"), "Atualizar Análise"), class = "btn-primary btn-icon"),
                           actionButton(ns("btn_restaurar_padrao"), label = tagList(icon("undo"), "Restaurar Padrão"), class = "btn-secondary btn-icon")
                       )
                   )
               )
      ),
      tabPanel("Análise de Infraestrutura",
               fluidRow(column(12, h3("Comparativo de Infraestrutura (Censo 2024)"), p("Análise comparativa dos principais indicadores de infraestrutura da sua escola em relação aos concorrentes selecionados e à média do município."))),
               hr(),
               fluidRow(
                 column(12, h4("Análise Detalhada"),
                        tabsetPanel(type = "tabs",
                                    tabPanel("Resumo Comparativo", br(), uiOutput(ns("cards_infra_detalhada"))),
                                    tabPanel("Detalhe por Concorrente", br(), uiOutput(ns("tabela_infra_detalhada_ui")))
                        )
                 )
               )
      ),
      tabPanel("Desempenho Acadêmico",
               div(class = "academic-performance",
                   div(class = "performance-card", h3(class = "academic-section-title", "Análise de Desempenho - ENEM 2024"), p("Comparativo das médias do ENEM por área de conhecimento e detalhamento das competências da redação.")),
                   tabsetPanel(
                     type = "tabs",
                     tabPanel("Visão Consolidada", br(),
                              div(class = "performance-card", h4("Comparativo de Médias por Área"),
                                  fluidRow(column(7, plotly::plotlyOutput(ns("plot_enem_areas_consolidado"))), column(5, DT::dataTableOutput(ns("tabela_enem_areas_consolidado"))))
                              ),
                              div(class = "performance-card", h4("Detalhamento da Nota de Redação"),
                                  fluidRow(column(7, plotly::plotlyOutput(ns("plot_enem_redacao_consolidado"))), column(5, DT::dataTableOutput(ns("tabela_enem_redacao_consolidado"))))
                              )
                     ),
                     tabPanel("Detalhe por Concorrente", br(),
                              div(class = "performance-card", h4("Comparativo de Médias por Área"),
                                  fluidRow(column(7, plotly::plotlyOutput(ns("plot_enem_areas_detalhado"))), column(5, DT::dataTableOutput(ns("tabela_enem_areas_detalhado"))))
                              ),
                              div(class = "performance-card", h4("Detalhamento da Nota de Redação"),
                                  fluidRow(column(7, plotly::plotlyOutput(ns("plot_enem_redacao_detalhado"))), column(5, DT::dataTableOutput(ns("tabela_enem_redacao_detalhado"))))
                              )
                     )
                   )
               )
      ),
      tabPanel("Chat com IA", icon = icon("robot"), mod_chat_ui(ns("chat_ia")))
    )
  )
}

# ===================================================================
#      MOD_ESCOLA_SERVER (VERSÃO FINAL E DEFINITIVA)
# ===================================================================
mod_escola_server <- function(id, user, codinep) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    source("utils/db_utils.R", local = TRUE)
    
    dados_escola_reativo <- reactiveVal(NULL)
    dados_escola_padrao <- reactiveVal(NULL)
    dados_concorrentes_individuais <- reactiveVal(NULL)
    
    observe({
      req(codinep)
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep, ".rds"))
      if (file.exists(caminho_arquivo)) {
        dados_carregados <- readRDS(caminho_arquivo)
        dados_escola_reativo(dados_carregados)
        if ("dados_concorrentes_individualizados" %in% names(dados_carregados)) {
          dados_concorrentes_individuais(dados_carregados$dados_concorrentes_individualizados)
        }
        if (is.null(dados_escola_padrao())) {
          dados_escola_padrao(dados_carregados)
        }
      }
    })
    
    get_nome_concorrente <- function(id_conc) {
      dados <- dados_escola_reativo()
      # Verifica na lista de concorrentes atual, que agora tem a coluna correta
      if (!is.null(dados$lista_concorrentes)) {
        df_conc <- as.data.frame(dados$lista_concorrentes) # Converte de sf para df
        if ("nome_escola" %in% names(df_conc)) {
          nome <- df_conc %>% filter(id_escola == id_conc) %>% pull(nome_escola)
          if (length(nome) > 0) return(nome)
        }
      }
      # Fallback para a base de dados completa
      tryCatch({
        readRDS("data/escolas_privadas_nomelista.rds") %>%
          filter(CO_ENTIDADE == id_conc) %>%
          pull(NO_ENTIDADE) %>%
          .[1] %||% paste("Concorrente", id_conc)
      }, error = function(e) paste("Concorrente", id_conc))
    }
    
    mod_chat_server("chat_ia", dados_escola = dados_escola_reativo)
    
    concorrentes_disponiveis <- reactive({
      dados <- dados_escola_reativo(); req(dados)
      readRDS("data/escolas_privadas_nomelista.rds") %>%
        filter(nome_municipio == dados$nome_municipio, CO_ENTIDADE != codinep) %>%
        mutate(display_name = paste0(NO_ENTIDADE, " - ", nome_municipio, " (", CO_ENTIDADE, ")")) %>%
        { setNames(.$CO_ENTIDADE, .$display_name) }
    })
    
    # LÓGICA DE BUSCA DO MOD_ADMIN APLICADA AQUI
    observe({
      req(dados_escola_reativo())
      opcoes_completas <- concorrentes_disponiveis()
      selecionados_atuais <- dados_escola_reativo()$lista_concorrentes$id_escola
      
      updateSelectizeInput(
        session, "selecao_concorrentes",
        choices = opcoes_completas,
        selected = selecionados_atuais,
        server = TRUE
      )
    })
    
    observeEvent(input$btn_atualizar_analise, {
      req(input$selecao_concorrentes)
      showNotification("Recriando análise completa com novos concorrentes...", type = "message", duration = 8)
      
      selected_ids <- as.character(input$selecao_concorrentes)
      save_concorrentes_selecionados(codinep, selected_ids)
      
      # CHAMA A FUNÇÃO MESTRA PARA RECRIAR O OBJETO .RDS DO ZERO
      novos_dados_completos <- preprocessar_escola(
        codinep = codinep,
        concorrentes_custom = selected_ids
      )
      
      dados_escola_reativo(novos_dados_completos)
      dados_concorrentes_individuais(novos_dados_completos$dados_concorrentes_individualizados)
      
      showNotification("Análise atualizada com sucesso!", type = "message", duration = 3)
    })
    
    observeEvent(input$btn_restaurar_padrao, {
      showNotification("Restaurando e reprocessando para os 5 concorrentes padrão...", type = "message", duration = 8)
      save_concorrentes_selecionados(codinep, character(0))
      
      dados_padrao_reprocessados <- preprocessar_escola(codinep = codinep)
      
      dados_escola_reativo(dados_padrao_reprocessados)
      dados_concorrentes_individuais(dados_padrao_reprocessados$dados_concorrentes_individualizados)
      dados_escola_padrao(dados_padrao_reprocessados)
    })
    
    # --- RENDERIZAÇÃO DOS ELEMENTOS DA UI ---
    
    criar_caixa_kpi <- function(dados_segmento) {
      valor1 <- dados_segmento$valor_ano_1
      valor2 <- dados_segmento$valor_ano_2
      taxa <- dados_segmento$taxa_de_variacao
      cor_taxa <- if (is.na(taxa) || taxa == "N/A") "#6c757d" else if (as.numeric(sub("%", "", taxa)) > 0) "#198754" else "#dc3545"
      div(class = "stat-card",
          tagList(h5(dados_segmento$label), p(paste("2023:", valor1, "| 2024:", valor2)), h4(style = paste0("color:", cor_taxa, ";"), taxa)))
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
            distancia <- ifelse("dist_metros" %in% names(concorrente_info) && !is.na(concorrente_info$dist_metros), 
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
                                    p(class = "stat-value", paste("2023:", format(segmento$valor_ano_1, big.mark = ".", decimal.mark = ","))),
                                    p(class = "stat-value", paste("2024:", format(segmento$valor_ano_2, big.mark = ".", decimal.mark = ",")))
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
      dados <- dados_escola_reativo(); req(dados, dados$dados_propria_escola)
      lapply(dados$dados_propria_escola, criar_caixa_kpi)
    })
    
    output$kpis_mercado <- renderUI({
      dados <- dados_escola_reativo(); req(dados, dados$dados_mercado_municipio)
      lapply(dados$dados_mercado_municipio, criar_caixa_kpi)
    })
    
    output$kpis_concorrentes <- renderUI({
      dados <- dados_escola_reativo(); req(dados, dados$dados_concorrentes_proximos)
      lapply(dados$dados_concorrentes_proximos, criar_caixa_kpi)
    })
    
    output$ui_desempenho_areas_consolidado <- renderUI({
      fluidRow(
        column(7, plotly::plotlyOutput(ns("plot_enem_areas_consolidado"))),
        column(5, DT::dataTableOutput(ns("tabela_enem_areas_consolidado")))
      )
    })
    
    output$plot_enem_areas_consolidado <- plotly::renderPlotly({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_areas)
      df_enem <- dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_areas)
      df <- dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Área = area, Categoria = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Categoria, values_from = Nota)
      
      DT::datatable(df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })
    
    output$ui_desempenho_redacao_consolidado <- renderUI({
      fluidRow(
        column(7, plotly::plotlyOutput(ns("plot_enem_redacao_consolidado"))),
        column(5, DT::dataTableOutput(ns("tabela_enem_redacao_consolidado")))
      )
    })
    
    output$plot_enem_redacao_consolidado <- plotly::renderPlotly({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_redacao)
      df_redacao <- dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_redacao)
      df <- dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Competência = competencia, Categoria = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Categoria, values_from = Nota)
      
      DT::datatable(df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })
    
    output$ui_desempenho_areas_detalhado <- renderUI({
      fluidRow(
        column(7, plotly::plotlyOutput(ns("plot_enem_areas_detalhado"))),
        column(5, DT::dataTableOutput(ns("tabela_enem_areas_detalhado")))
      )
    })
    
    output$plot_enem_areas_detalhado <- plotly::renderPlotly({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_areas)
      df_enem <- dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_areas)
      df <- dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Concorrente")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Área = area, Escola = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Escola, values_from = Nota)
      
      DT::datatable(df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })
    
    output$ui_desempenho_redacao_detalhado <- renderUI({
      fluidRow(
        column(7, plotly::plotlyOutput(ns("plot_enem_redacao_detalhado"))),
        column(5, DT::dataTableOutput(ns("tabela_enem_redacao_detalhado")))
      )
    })
    
    output$plot_enem_redacao_detalhado <- plotly::renderPlotly({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_redacao)
      df_redacao <- dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo(); req(dados, dados$dados_enem_redacao)
      df <- dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Concorrente")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Competência = competencia, Escola = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Escola, values_from = Nota)
      
      DT::datatable(df, options = list(dom = 't', pageLength = 10), rownames = FALSE)
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
    
    # Utilitário para garantir que o builder esteja carregado
    load_onepager_builder <- function() {
      src <- here::here("utils", "onepager_build.R")
      if (!file.exists(src)) stop("Arquivo utils/onepager_build.R não encontrado.")
      source(src, local = TRUE) # expõe build_onepager() no ambiente atual
      if (!exists("build_onepager")) stop("Função build_onepager() não disponível após source().")
    }
    
    output$dl_onepager <- downloadHandler(
      filename = function() {
        d <- dados_escola_reativo()
        paste0("Relatorio_Explora_", d$id_escola, "_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
      },
      content = function(file) {
        # Garante que o script do builder esteja disponível
        source(here::here("utils", "onepager_build.R"), local = TRUE)
        
        id <- showNotification("Gerando relatório em PDF, por favor aguarde...", duration = NULL, closeButton = FALSE)
        on.exit(removeNotification(id), add = TRUE)
        
        dados_para_relatorio <- dados_escola_reativo()
        shiny::validate(shiny::need(!is.null(dados_para_relatorio), "Dados da escola ainda não foram carregados."))
        
        # Chama a função principal do builder, que faz todo o trabalho
        tryCatch({
          build_onepager(dados = dados_para_relatorio, out_pdf = file)
        }, error = function(e) {
          showNotification(paste("Erro ao gerar PDF:", e$message), type = "error", duration = 10)
          # Cria um arquivo vazio para evitar que o navegador fique esperando
          file.create(file)
        })
      }
    )
    
    
    # Exporta o nome da escola para o cabeçalho principal
    return(
      list(
        nome_escola = reactive({ 
          d <- dados_escola_reativo()
          d$nome_escola %||% ""
        })
      )
    )
    
  })
}