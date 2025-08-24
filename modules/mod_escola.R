# --- Bibliotecas necessárias para as novas abas ---
# Certifique-se de ter instalado: install.packages(c("plotly", "knitr", "kableExtra", "scales", "leaflet", "shinyjs"))

mod_escola_ui <- function(id) {
  ns <- NS(id)
  
  tagList(tags$style(HTML("
  .concorrente-col {
    flex: 1;
    min-width: 220px;
    padding: 0 5px;
  }
  
  .concorrente-column {
    height: 100%;
    display: flex;
    flex-direction: column;
    background: white;
    border-radius: 8px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.1);
    overflow: hidden;
  }
  
  .concorrente-header {
    background: linear-gradient(135deg, #5a36a9 0%, #6f42c1 100%);
    padding: 12px 10px;
    text-align: center;
    margin-bottom: 0;
  }
  
  .concorrente-header h5 {
    margin: 0;
    font-weight: bold;
    color: white;
    font-size: 0.95em;
    line-height: 1.2;
    text-shadow: 0 1px 2px rgba(0,0,0,0.2);
  }
  
  .concorrente-header p {
    margin: 5px 0 0 0;
    font-size: 0.75em;
    color: rgba(255,255,255,0.95);
  }
  
  .concorrente-cards {
    padding: 12px;
    display: flex;
    flex-direction: column;
    gap: 8px;
    flex-grow: 1;
    overflow: visible !important;
    max-height: none !important;
  }
  
  .stat-card {
    background: #f8f9fa;
    padding: 10px;
    border-radius: 6px;
    border-left: 3px solid #6f42c1;
    flex-shrink: 0;
  }
  
  .stat-card h6 {
    color: #495057;
    font-weight: 600;
    margin: 0 0 6px 0;
    font-size: 0.8em;
    border-bottom: 1px solid #dee2e6;
    padding-bottom: 4px;
  }
  
  .stat-card p {
    margin: 3px 0;
    font-size: 0.75em;
    color: #6c757d;
  }
  
  /* Layout responsivo */
  @media (max-width: 1200px) {
    .concorrente-col {
      min-width: 200px;
    }
  }
  
  @media (max-width: 992px) {
    .concorrente-col {
      min-width: 180px;
    }
    
    .concorrente-header {
      padding: 10px 8px;
    }
    
    .concorrente-cards {
      padding: 10px;
    }
  }
  
  /* Container principal sem scroll */
  .concorrentes-detailed-view {
    overflow: visible !important;
    max-height: none !important;
  }
  
  /* Garantir que tudo seja visível */
  .benchmark-analysis {
    overflow: visible !important;
  }
  
  /* Scroll horizontal apenas se necessário para muitas colunas */
  .benchmark-analysis .fluid-row {
    flex-wrap: nowrap;
    overflow-x: auto;
    padding-bottom: 10px;
  }
  
  /* Esconder scrollbar mas manter funcionalidade */
  .benchmark-analysis .fluid-row::-webkit-scrollbar {
    height: 6px;
  }
  
  .benchmark-analysis .fluid-row::-webkit-scrollbar-track {
    background: #f1f1f1;
    border-radius: 3px;
  }
  
  .benchmark-analysis .fluid-row::-webkit-scrollbar-thumb {
    background: #c1c1c1;
    border-radius: 3px;
  }
  
  .benchmark-analysis .fluid-row::-webkit-scrollbar-thumb:hover {
    background: #a8a8a8;
  }
")),
          shinyjs::useShinyjs(), # Habilita o uso do shinyjs para feedback
          #div(class = "page-header", h2(textOutput(ns("nome_escola_titulo")))),
          
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
                     div(class = "benchmark-container",
                         
                         # Seleção de concorrentes - Layout vertical
                         div(class = "benchmark-selection",
                             h4("Seleção de Concorrentes", icon("users")),
                             p("Selecione até 5 escolas concorrentes para análise detalhada."),
                             
                             selectizeInput(ns("selecao_concorrentes"), 
                                            label = NULL,
                                            choices = NULL,
                                            multiple = TRUE,
                                            width = "100%",
                                            options = list(maxItems = 5, 
                                                           placeholder = 'Digite o nome da escola...',
                                                           dropdownParent = 'body')),
                             
                             div(class = "selection-buttons",
                                 actionButton(ns("btn_atualizar_analise"), "Atualizar Análise", 
                                              icon = icon("sync"), class = "btn-primary"),
                                 actionButton(ns("btn_restaurar_padrao"), "Restaurar Padrão", 
                                              icon = icon("undo"), class = "btn-secondary")
                             )
                         ),
                         
                         # Análise detalhada - Cards padronizados
                         div(class = "benchmark-analysis",
                             h4("Análise Detalhada por Concorrente", icon("chart-bar")),
                             uiOutput(ns("kpis_concorrentes_detalhados"))
                         ),
                         
                         # Mapa - Agora fica por último
                         div(class = "benchmark-map",
                             h4("Localização Geográfica", icon("map-marked-alt")),
                             p("Visualização da localização da sua escola e dos concorrentes selecionados."),
                             leaflet::leafletOutput(ns("mapa_concorrentes"), height = "400px")
                         ),
                         
                         # Botão de download
                         div(class = "benchmark-download",
                             downloadButton(ns("dl_onepager"), "Baixar Relatório Completo (PDF)", 
                                            class = "btn-success")
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
            
            tabPanel("Chat com IA",
                     icon = icon("robot"),
                     mod_chat_ui(ns("chat_ia"))
            )
          )
  )
}


mod_escola_server <- function(id, user, codinep) {
  moduleServer(id, function(input, output, session) {
    source("utils/global_reactives.R")
    source("utils/preprocess_utils.R", local = TRUE)
    source("utils/db_utils.R", local = TRUE)
    
    dados_escola_reativo <- reactiveVal(NULL)
    dados_escola_padrao <- reactiveVal(NULL)
    
    # No server do módulo, após dados_escola_reativo
    dados_concorrentes_individuais <- reactiveVal(NULL)
    
    # Observar mudanças na seleção de concorrentes
    observeEvent(input$selecao_concorrentes, {
      req(input$selecao_concorrentes, length(input$selecao_concorrentes) > 0)
      
      showNotification("Carregando dados individuais dos concorrentes...")
      
      tryCatch({
        source("utils/preprocess_utils.R", local = TRUE)
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
    
    mod_chat_server("chat_ia", dados_escola = dados_escola_reativo)
    
    output$nome_escola_titulo <- renderText({
      dados <- dados_escola_reativo()
      nm <- if (!is.null(dados)) (dados$nome_escola %||% "") else ""
      if (!nzchar(nm)) nm <- "Sua Escola"
      nm  # Retorna apenas o nome, sem "Painel da Escola:"
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
      df <- concorrentes_disponiveis(); req(nrow(df) > 0)
      choices <- stats::setNames(df$CO_ENTIDADE, df$display_name)
      
      dados <- dados_escola_reativo()
      sel <- tryCatch(as.character(dados$lista_concorrentes$id_escola), error = function(e) NULL)
      
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
      
      # ---- estado atual + ids
      dados_atuais <- dados_escola_reativo(); req(dados_atuais)
      codinep_ch   <- as.character(codinep)
      
      # ---- persiste escolha do usuário
      save_concorrentes_selecionados(codinep, input$selecao_concorrentes)
      
      # ---- reprocessa blocos (KPIs/ENEM/Infra/etc.)
      novos_proc <- reprocessar_dados_concorrentes(
        codinep          = codinep_ch,
        ids_concorrentes = as.character(input$selecao_concorrentes),
        id_municipio     = dados_atuais$id_municipio
      )
      
      # === Helper: construir SF de concorrentes com nome + geometria (robusto) =====
      build_concorrentes_sf <- function(ids_sel, codinep_ch, escola_lon = NA_real_, escola_lat = NA_real_) {
        ids_sel <- as.character(ids_sel)
        
        # 1) bases
        escolas_sf  <- tryCatch(readRDS("data/processed/escolas_enriquecidas.rds"), error = function(e) NULL)
        if (!is.null(escolas_sf) && is.na(sf::st_crs(escolas_sf))) escolas_sf <- sf::st_set_crs(escolas_sf, 4326)
        
        sf_fallback <- tryCatch(readRDS("data/processed/escolas_privadas_unificadas_sf.rds"), error = function(e) NULL)
        if (!is.null(sf_fallback) && is.na(sf::st_crs(sf_fallback))) sf_fallback <- sf::st_set_crs(sf_fallback, 4326)
        
        geo_ll <- tryCatch(readRDS("data/escolas_geo_com_empty_flag.rds"), error = function(e) NULL)
        if (!is.null(geo_ll)) {
          geo_ll <- sf::st_drop_geometry(geo_ll)
          nm_id <- intersect(c("code_school","id_escola","CO_ENTIDADE"), names(geo_ll))[1]
          if (length(nm_id)) names(geo_ll)[names(geo_ll)==nm_id] <- "id_escola"
          geo_ll$id_escola <- as.character(geo_ll$id_escola)
        }
        
        nomes_all <- readRDS("data/escolas_privadas_nomelista.rds") |>
          dplyr::mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
        
        # 2) ponto da escola alvo
        escola_pt <- NULL
        if (!is.null(escolas_sf)) {
          escola_pt <- escolas_sf |>
            dplyr::filter(as.character(id_escola) == codinep_ch) |>
            dplyr::slice(1)
          if (nrow(escola_pt) == 0) escola_pt <- NULL
        }
        if (is.null(escola_pt) && !is.null(sf_fallback)) {
          escola_pt <- sf_fallback |>
            dplyr::filter(as.character(id_escola) == codinep_ch) |>
            dplyr::slice(1)
          if (nrow(escola_pt) == 0) escola_pt <- NULL
        }
        if (is.null(escola_pt) && is.finite(escola_lon) && is.finite(escola_lat)) {
          escola_pt <- sf::st_sf(
            id_escola = codinep_ch,
            geometry  = sf::st_sfc(sf::st_point(c(escola_lon, escola_lat)), crs = 4326)
          )
        }
        
        # 3) concorrentes a partir das SF
        conc_sf <- NULL
        if (!is.null(escolas_sf)) {
          conc_sf <- escolas_sf |>
            dplyr::filter(as.character(id_escola) %in% ids_sel) |>
            dplyr::select(id_escola, geometry)
        }
        if (is.null(conc_sf) || nrow(conc_sf) < length(ids_sel)) {
          if (!is.null(sf_fallback)) {
            add_fb <- sf_fallback |>
              dplyr::filter(as.character(id_escola) %in% ids_sel) |>
              dplyr::select(id_escola, geometry)
            if (is.null(conc_sf)) {
              conc_sf <- add_fb
            } else {
              conc_sf <- conc_sf |>
                dplyr::full_join(add_fb, by = "id_escola", suffix = c("", ".fb"))
              # se houver geometry.fb, usa onde faltar
              if ("geometry.fb" %in% names(conc_sf)) {
                idx <- is.na(sf::st_is_empty(conc_sf$geometry)) | sf::st_is_empty(conc_sf$geometry)
                idx[is.na(idx)] <- TRUE
                conc_sf$geometry[idx] <- conc_sf$geometry.fb[idx]
                conc_sf$geometry.fb <- NULL
              }
            }
          }
        }
        
        # 3b) se ainda vazio, cria esqueleto
        if (is.null(conc_sf)) {
          geoms <- sf::st_sfc(replicate(length(ids_sel), sf::st_geometrycollection(), simplify = FALSE), crs = 4326)
          conc_sf <- sf::st_sf(id_escola = ids_sel, geometry = geoms)
        }
        conc_sf$id_escola <- as.character(conc_sf$id_escola)
        
        # 4) preencher com lat/lon tabular por ID (atribuição por índice)
        if (!is.null(geo_ll) && nrow(conc_sf)) {
          # quem ainda não tem ponto?
          empt <- is.na(sf::st_is_empty(conc_sf$geometry)) | sf::st_is_empty(conc_sf$geometry)
          empt[is.na(empt)] <- TRUE
          ids_need <- conc_sf$id_escola[empt]
          
          if (length(ids_need)) {
            lon_nm <- intersect(c("longitude","lon","LONGITUDE","x"), names(geo_ll))[1]
            lat_nm <- intersect(c("latitude","lat","LATITUDE","y"), names(geo_ll))[1]
            if (length(lon_nm) && length(lat_nm)) {
              gg <- geo_ll |>
                dplyr::filter(id_escola %in% ids_need) |>
                dplyr::mutate(
                  longitude = suppressWarnings(as.numeric(.data[[lon_nm]])),
                  latitude  = suppressWarnings(as.numeric(.data[[lat_nm]]))
                ) |>
                dplyr::filter(is.finite(longitude), is.finite(latitude))
              if (nrow(gg)) {
                gg_sf <- sf::st_as_sf(gg, coords = c("longitude","latitude"), crs = 4326)
                # atribuição por ID (tamanho compatível)
                pos <- match(conc_sf$id_escola, gg_sf$id_escola)
                take <- !is.na(pos) & empt
                conc_sf$geometry[take] <- gg_sf$geometry[pos[take]]
              }
            }
          }
        }
        
        # 5) acrescenta NOME (sem depender de 'nm' em mutate)
        conc_sf <- conc_sf |>
          dplyr::left_join(nomes_all, by = c("id_escola" = "CO_ENTIDADE"))
        nm_col <- intersect(c("NO_ENTIDADE","nome_escola","NM_ENTIDADE","NO_NOME_ENTIDADE"), names(conc_sf))[1]
        conc_sf$nome_escola <- if (length(nm_col)) as.character(conc_sf[[nm_col]]) else as.character(conc_sf$id_escola)
        
        # 6) distância
        if (!is.null(escola_pt) && nrow(conc_sf)) {
          conc_sf$dist_metros <- suppressWarnings(as.numeric(sf::st_distance(conc_sf, escola_pt[1, ])))
        } else {
          conc_sf$dist_metros <- NA_real_
        }
        
        conc_sf |>
          dplyr::arrange(dplyr::desc(!sf::st_is_empty(geometry)), dist_metros) |>
          dplyr::distinct(id_escola, .keep_all = TRUE) |>
          dplyr::slice_head(n = length(ids_sel)) |>
          dplyr::select(id_escola, nome_escola, dist_metros, geometry)
      }
      
      # ---- reconstruir lista_concorrentes
      conc_sf <- build_concorrentes_sf(
        ids_sel    = as.character(input$selecao_concorrentes),
        codinep_ch = codinep_ch,
        escola_lon = suppressWarnings(as.numeric(dados_atuais$longitude)),
        escola_lat = suppressWarnings(as.numeric(dados_atuais$latitude))
      )
      
      # ---- salvar e atualizar estado
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
      
      save_concorrentes_selecionados(codinep, character(0))
      
      dados_padrao_originais <- dados_escola_padrao(); req(dados_padrao_originais)
      codinep_ch <- as.character(codinep)
      ids_concorrentes_padrao <- as.character(dados_padrao_originais$lista_concorrentes$id_escola)
      
      dados_reprocessados_padrao <- reprocessar_dados_concorrentes(
        codinep          = codinep_ch,
        ids_concorrentes = ids_concorrentes_padrao,
        id_municipio     = dados_padrao_originais$id_municipio
      )
      
      # usa o mesmo helper da atualização (definido no bloco acima)
      conc_sf <- (function(ids_sel, codinep_ch) {
        escola_lon <- suppressWarnings(as.numeric(dados_padrao_originais$longitude))
        escola_lat <- suppressWarnings(as.numeric(dados_padrao_originais$latitude))
        # reaproveita a função definida no bloco anterior:
        build_concorrentes_sf(ids_sel, codinep_ch, escola_lon, escola_lat)
      })(ids_concorrentes_padrao, codinep_ch)
      
      dados_finais_padrao <- c(
        list(
          id_escola       = dados_padrao_originais$id_escola,
          nome_escola     = dados_padrao_originais$nome_escola,
          id_municipio    = dados_padrao_originais$id_municipio,
          nome_municipio  = dados_padrao_originais$nome_municipio,
          latitude        = dados_padrao_originais$latitude,
          longitude       = dados_padrao_originais$longitude,
          lista_concorrentes = conc_sf,
          data_extracao   = Sys.time()
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
    
    # Função para criar cards detalhados de matrículas por concorrente
    criar_card_concorrente_detalhado <- function(dados_concorrente, nome_concorrente) {
      div(class = "concorrente-card",
          h5(nome_concorrente),
          lapply(dados_concorrente, function(segmento) {
            if (!is.null(segmento$valor_ano_1) && !is.null(segmento$valor_ano_2)) {
              div(class = "segmento-card",
                  h6(segmento$label),
                  p(paste("2023:", segmento$valor_ano_1, "| 2024:", segmento$valor_ano_2)),
                  p(style = paste0("color:", ifelse(grepl("-", segmento$taxa_de_variacao), "#dc3545", "#198754"), ";"),
                    segmento$taxa_de_variacao)
              )
            }
          })
      )
    }
    
    # Função para extrair dados de um concorrente específico
    extrair_dados_concorrente <- function(dados_escola, id_concorrente) {
      # Buscar dados do concorrente específico
      concorrente_info <- dados_escola$lista_concorrentes %>%
        filter(id_escola == id_concorrente)
      
      # Buscar dados de matrículas deste concorrente
      # (Esta função precisará ser adaptada dependendo da estrutura dos seus dados)
      dados_matriculas <- NULL
      
      # Se os dados individuais por concorrente não estiverem disponíveis,
      # podemos calcular a partir dos dados agregados ou buscar de outra fonte
      return(list(
        info = concorrente_info,
        matriculas = dados_matriculas
      ))
    }
    
    output$kpis_escola <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_propria_escola, criar_caixa_kpi)
    })
    
    # Substituir o output kpis_concorrentes_detalhados
    output$kpis_concorrentes_detalhados <- renderUI({
      dados_indiv <- dados_concorrentes_individuais()
      req(dados_indiv, length(dados_indiv) > 0)
      
      # Garantir até 5 concorrentes
      concorrentes_ids <- names(dados_indiv)[1:min(5, length(dados_indiv))]
      
      fluidRow(
        style = "margin: 0 -5px; display: flex; flex-wrap: nowrap; overflow-x: auto;",
        lapply(concorrentes_ids, function(id_conc) {
          concorrente_info <- dados_escola_reativo()$lista_concorrentes %>%
            filter(id_escola == id_conc)
          
          nome_concorrente <- concorrente_info$nome_escola %||% paste("Concorrente", id_conc)
          distancia <- ifelse(!is.na(concorrente_info$dist_metros), 
                              paste(round(concorrente_info$dist_metros), "m"), 
                              "N/A")
          
          div(class = "concorrente-col",
              style = "flex: 1; min-width: 220px; padding: 0 5px;",
              
              div(class = "concorrente-column",
                  div(class = "concorrente-header",
                      h5(nome_concorrente, style = "margin: 0; font-weight: bold; color: white; font-size: 0.95em; line-height: 1.2;"),
                      p(style = "margin: 3px 0 0 0; font-size: 0.75em; color: rgba(255,255,255,0.9);", 
                        icon("map-marker-alt"), " ", distancia)
                  ),
                  
                  div(class = "concorrente-cards",
                      lapply(dados_indiv[[id_conc]], function(segmento) {
                        if (!is.null(segmento$valor_ano_1) && !is.null(segmento$valor_ano_2)) {
                          div(class = "stat-card",
                              h6(segmento$label, style = "font-size: 0.8em; margin: 0 0 5px 0; color: #495057; font-weight: 600;"),
                              p(style = "margin: 2px 0; font-size: 0.75em; color: #6c757d;", 
                                "23: ", segmento$valor_ano_1),
                              p(style = "margin: 2px 0; font-size: 0.75em; color: #6c757d;", 
                                "24: ", segmento$valor_ano_2),
                              p(style = "margin: 3px 0 0 0; font-weight: bold; font-size: 0.8em;",
                                style = paste0("color:", ifelse(grepl("-", segmento$taxa_de_variacao), "#dc3545", "#198754"), ";"),
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
    
    # Nova tabela com informações dos concorrentes
    output$tabela_info_concorrentes <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$lista_concorrentes)
      
      tabela <- dados$lista_concorrentes %>%
        select(`Cód. INEP` = id_escola, 
               `Nome da Escola` = nome_escola,
               `Distância (metros)` = dist_metros) %>%
        mutate(`Distância (metros)` = ifelse(is.na(`Distância (metros)`), 
                                             "N/A", 
                                             round(`Distância (metros)`)))
      
      DT::datatable(tabela, 
                    options = list(pageLength = 5, dom = 't'),
                    rownames = FALSE)
    })
    
    output$kpis_mercado <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_mercado_municipio, criar_caixa_kpi)
    })
    output$kpis_concorrentes <- renderUI({
      dados <- dados_escola_reativo()
      req(dados)
      lapply(dados$dados_concorrentes_proximos, criar_caixa_kpi)  # ← DEVE usar dados_concorrentes_proximos
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
    
    
    # --- MAPA: escola + concorrentes (limpo e robusto) ---------------------------
    output$mapa_concorrentes <- leaflet::renderLeaflet({
      req(dados_escola_reativo)
      dados <- dados_escola_reativo(); req(dados)
      
      `%||%` <- function(a,b) if (!is.null(a)) a else b
      first_col_name <- function(df, candidates) {
        nm <- candidates[candidates %in% names(df)]
        if (length(nm)) nm[1] else NULL
      }
      # --- formatador de distância (substitui label_number_si) ---
      fmt_dist <- function(m) {
        m <- suppressWarnings(as.numeric(m))
        ifelse(!is.finite(m), "—",
               ifelse(m < 1000, paste0(round(m), " m"),
                      paste0(scales::number(m/1000, accuracy = 0.1), " km")))
      }
      
      # 1) Escola
      escola_pt <- tibble::tibble(
        nome_plot = as.character(dados$nome_escola %||% "Sua escola"),
        longitude = suppressWarnings(as.numeric(dados$longitude))[1],
        latitude  = suppressWarnings(as.numeric(dados$latitude))[1]
      ) |> dplyr::filter(is.finite(longitude), is.finite(latitude))
      
      # 2) Concorrentes -> long/lat
      conc <- dados$lista_concorrentes; req(!is.null(conc), nrow(conc) > 0)
      if ("geometry" %in% names(conc)) {
        conc_wgs <- tryCatch(sf::st_transform(conc, 4326), error = function(e) conc)
        coords   <- suppressWarnings(sf::st_coordinates(conc_wgs))
        conc_df  <- dplyr::bind_cols(sf::st_drop_geometry(conc_wgs),
                                     tibble::tibble(longitude = coords[,1], latitude = coords[,2]))
      } else {
        conc_df <- conc
        lng_nm <- first_col_name(conc_df, c("longitude","lon","LONGITUDE","x"))
        lat_nm <- first_col_name(conc_df, c("latitude","lat","LATITUDE","y"))
        conc_df$longitude <- if (!is.null(lng_nm)) suppressWarnings(as.numeric(conc_df[[lng_nm]])) else NA_real_
        conc_df$latitude  <- if (!is.null(lat_nm)) suppressWarnings(as.numeric(conc_df[[lat_nm]])) else NA_real_
      }
      nome_nm <- first_col_name(conc_df, c("nome_escola","NO_ENTIDADE","NO_NOME_ENTIDADE","NM_ENTIDADE","nome"))
      conc_df$nome_plot <- if (!is.null(nome_nm)) as.character(conc_df[[nome_nm]]) else as.character(conc_df$id_escola)
      conc_df <- conc_df |> dplyr::filter(is.finite(longitude), is.finite(latitude))
      
      # 3) Fallback centro
      if (!nrow(escola_pt)) {
        validate(need(nrow(conc_df) > 0, "Sem coordenadas para escola/concorrentes."))
        escola_pt <- tibble::tibble(
          nome_plot = dados$nome_escola %||% "Sua escola",
          longitude = mean(conc_df$longitude, na.rm = TRUE),
          latitude  = mean(conc_df$latitude,  na.rm = TRUE)
        )
      }
      
      # 4) Ícones
      ic_escola <- leaflet::awesomeIcons(icon = "graduation-cap", library = "fa",
                                         markerColor = "blue", iconColor = "white")
      ic_conc   <- leaflet::awesomeIcons(icon = "building", library = "fa",
                                         markerColor = "red", iconColor = "white")
      
      # 5) Base
      m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE, minZoom = 3)) |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron,
                                  options = leaflet::providerTileOptions(noWrap = TRUE))
      
      # Escola
      m <- m |> leaflet::addAwesomeMarkers(
        data = escola_pt, lng = ~longitude, lat = ~latitude,
        icon = ic_escola,
        label = ~nome_plot, popup = ~nome_plot,
        options = leaflet::markerOptions(zIndexOffset = 1000)
      )
      
      # Concorrentes
      if (nrow(conc_df)) {
        popup_txt <- if ("dist_metros" %in% names(conc_df)) {
          paste0("<b>", htmltools::htmlEscape(conc_df$nome_plot), "</b>",
                 "<br/>Dist.: ", fmt_dist(conc_df$dist_metros))
        } else {
          htmltools::htmlEscape(conc_df$nome_plot)
        }
        
        m <- m |> leaflet::addAwesomeMarkers(
          data = conc_df, lng = ~longitude, lat = ~latitude,
          icon = ic_conc,
          label = ~nome_plot, popup = popup_txt
        )
      }
      
      # 6) Enquadrar
      all_pts <- dplyr::bind_rows(
        dplyr::mutate(escola_pt, tipo = "escola"),
        dplyr::mutate(dplyr::select(conc_df, nome_plot, longitude, latitude), tipo = "conc")
      )
      if (nrow(all_pts)) {
        m <- m |>
          leaflet::fitBounds(lng1 = min(all_pts$longitude), lat1 = min(all_pts$latitude),
                             lng2 = max(all_pts$longitude), lat2 = max(all_pts$latitude))
      }
      
      # 7) Legenda
      m |> leaflet::addLegend(
        position = "bottomleft",
        colors = c("#2C7BE5", "#E63757"),
        labels = c("Sua escola", "Concorrentes"),
        opacity = 1
      )
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
    
    # --- INFRA: tabela detalhada com formatação por célula ----------------------
    output$tabela_infra_detalhada_ui <- renderUI({
      req(dados_escola_reativo)
      dados <- dados_escola_reativo()
      req(dados, dados$dados_infraestrutura, dados$lista_concorrentes)
      
      codinep <- as.character(dados$id_escola %||% NA_character_)
      df_infra_raw <- dados$dados_infraestrutura
      
      # 1) nomes dos concorrentes (para rótulos)
      nomes_conc <- dados$lista_concorrentes |>
        dplyr::select(
          id_escola,
          nome_escola = dplyr::any_of(c("NO_ENTIDADE","nome_escola"))
        )
      
      # 2) mantém apenas colunas numéricas/lógicas (evita 'geometry')
      num_log_cols <- names(df_infra_raw)[vapply(df_infra_raw, \(x) is.numeric(x) || is.logical(x), logical(1))]
      num_log_cols <- setdiff(num_log_cols, "id_escola")  # não pivotar chave
      
      # 3) monta base longa: escola alvo + concorrentes + média municipal
      df_long <- df_infra_raw |>
        dplyr::filter(id_escola %in% c(codinep, nomes_conc$id_escola, "Média Municipal")) |>
        dplyr::left_join(nomes_conc, by = "id_escola") |>
        dplyr::mutate(Escola = dplyr::case_when(
          id_escola == codinep ~ "Sua Escola",
          id_escola == "Média Municipal" ~ "Média Municipal",
          TRUE ~ dplyr::coalesce(nome_escola, id_escola)
        )) |>
        dplyr::select(Escola, dplyr::any_of(num_log_cols)) |>
        tidyr::pivot_longer(cols = -Escola, names_to = "indicador", values_to = "valor") |>
        dplyr::mutate(
          Indicador = indicador |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_remove("^(essencial|lazer|tec|apoio)\\s*") |>
            stringr::str_trim() |>
            stringr::str_to_title()
        ) |>
        dplyr::select(Indicador, Escola, valor)
      
      # 4) formatação célula-a-célula (vectorizada)
      safe_num <- function(x) suppressWarnings(as.numeric(x))
      df_long <- df_long |>
        dplyr::mutate(Valor_fmt = dplyr::case_when(
          Indicador == "Total Dispositivos Aluno" ~ as.character(round(safe_num(valor), 1)),
          safe_num(valor) == 1 ~ as.character(shiny::icon("check", class = "table-icon", style = "color:#16a34a;")),
          TRUE ~ as.character(shiny::icon("times", class = "table-icon", style = "color:#dc2626;"))
        ))
      
      # 5) volta para wide, garantindo ordem de colunas
      ordem_cols <- c("Sua Escola", setdiff(unique(df_long$Escola), c("Sua Escola")))
      df_fmt <- df_long |>
        dplyr::select(-valor) |>
        tidyr::pivot_wider(names_from = Escola, values_from = Valor_fmt) |>
        dplyr::relocate(Indicador, dplyr::any_of(ordem_cols))
      
      # 6) tabela HTML
      tbl <- knitr::kable(df_fmt, "html", escape = FALSE,
                          align = c("l", rep("c", ncol(df_fmt) - 1))) |>
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                                  full_width = TRUE) |>
        kableExtra::scroll_box(width = "100%")
      
      # 7) um CSSzinho pros ícones, se ainda não tiver
      shiny::tagList(
        tags$style(HTML(".table-icon{font-size:14px; line-height:1;}")),
        HTML(tbl)
      )
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
      dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Área = area, Categoria = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Categoria, values_from = Nota) %>%
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
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_consolidado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Competência = competencia, Categoria = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Categoria, values_from = Nota) %>%
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
        filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df_enem, x = ~area, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média'), xaxis = list(title = 'Área de Conhecimento'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_areas)
      dados$dados_enem_areas %>%
        filter(tipo %in% c("Sua Escola", "Concorrente")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Área = area, Escola = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Escola, values_from = Nota) %>%
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
        filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df_redacao, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar', text = ~round(nota, 1), textposition = 'outside') %>%
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)), xaxis = list(title = 'Competência da Redação'), barmode = 'group', legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_detalhado <- DT::renderDataTable({
      dados <- dados_escola_reativo()
      req(dados, dados$dados_enem_redacao)
      dados$dados_enem_redacao %>%
        filter(tipo %in% c("Sua Escola", "Concorrente")) %>%
        mutate(nota = round(nota, 1)) %>%
        select(Competência = competencia, Escola = escola_label, Nota = nota) %>%
        pivot_wider(names_from = Escola, values_from = Nota) %>%
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    # ---- Download do One-Pager ----
    # modules/mod_escola.R — trecho do server com downloadHandler
    
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
        paste0("Relatorio_", d$id_escola, "_", format(Sys.Date(), "%Y-%m-%d"), ".pdf")
      },
      content = function(file) {
        # captura dados
        d <- dados_escola_reativo()
        shiny::validate(shiny::need(!is.null(d), "Dados não carregados."))
        
        # carrega o builder
        load_onepager_builder()
        
        # gera PDF diretamente no 'file' fornecido pelo Shiny
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
    # Reactive value para o nome da escola
    nome_exportado <- reactiveVal(NULL)
    
    # Quando os dados carregarem, captura o nome
    observe({
      dados <- dados_escola_reativo()
      if (!is.null(dados) && !is.null(dados$nome_escola)) {
        nome_exportado(dados$nome_escola)
        cat("Nome exportado do módulo:", dados$nome_escola, "\n")
      }
    })
    
    # Exporta o nome para o app principal
    return(
      list(
        nome_escola = reactive({ nome_exportado() })
      )
    )
    # No final do mod_escola_server.R
    observe({
      dados <- dados_escola_reativo()
      if (!is.null(dados) && !is.null(dados$nome_escola)) {
        cat("=== DEBUG MOD_ESCOLA ===\n")
        cat("Nome da escola no módulo:", dados$nome_escola, "\n")
        cat("=========================\n")
      }
    })
  })
}  # Quando os dados da escola carregarem, atualiza globalmente

    
    
