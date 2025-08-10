# modules/mod_escola.R

mod_escola_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    shinyjs::useShinyjs(),
    
    # Handler para invalidar tamanho de widgets (Leaflet) e ping ao abrir modal
    tags$script(HTML(sprintf("
      Shiny.addCustomMessageHandler('%s', function(x){
        try{
          var w = HTMLWidgets.find('#' + x.id);
          if (w && w.getMap){ var m = w.getMap(); if (m) m.invalidateSize(); }
        } catch(e){}
      });
      $(document).on('shown.bs.modal', '#shiny-modal', function(){
        Shiny.setInputValue('%s', Date.now(), {priority: 'event'});
      });
    ", ns('ivsz'), ns('modal_shown')))),
    
    # Handler para abrir URLs (mapa salvo / Google Maps)
    tags$script(HTML("
      Shiny.addCustomMessageHandler('openURL', function(m){
        try { window.open(m.url, '_blank', 'noopener'); }
        catch(e){ window.location.href = m.url; }
      });
    ")),
    
    div(class = "page-header", h2(textOutput(ns("nome_escola_titulo")))),
    
    navbarPage(
      title = NULL,
      id = ns("main_tabs"),
      
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
      
      tabPanel("Benchmark Concorrentes",
               sidebarLayout(
                 sidebarPanel(
                   h4("Seleção de Concorrentes"),
                   p("Escolha até 5 escolas para uma análise personalizada. A análise padrão usa os 5 concorrentes mais próximos."),
                   
                   selectizeInput(ns("selecao_concorrentes"),
                                  label = "Busque e selecione as escolas:",
                                  choices = NULL, multiple = TRUE,
                                  options = list(maxItems = 5, placeholder = 'Digite para buscar...')
                   ),
                   
                   actionButton(ns("btn_atualizar_analise"), "Atualizar e Salvar Seleção",
                                icon = icon("sync"), class = "btn-primary btn-block"),
                   br(),
                   actionButton(ns("btn_restaurar_padrao"), "Restaurar Padrão",
                                icon = icon("undo"), class = "btn-secondary btn-block"),
                   br(), tags$hr(),
                   
                   actionButton(ns("abrir_mapa"),  "Abrir mapa (tela cheia)",
                                icon = icon("expand"), class = "btn-success btn-block"),
                   actionButton(ns("abrir_gmaps"), "Abrir no Google Maps (backup)",
                                icon = icon("external-link"))
                 ),
                 
                 mainPanel(
                   tabsetPanel(
                     id = ns("tabs_bench"), type = "tabs",
                     tabPanel("Lista de Concorrentes Atuais",
                              DT::dataTableOutput(ns("tabela_concorrentes"))
                     )
                   )
                 )
               )
      ),
      
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
                          tabPanel("Resumo Comparativo", br(), uiOutput(ns("cards_infra_detalhada"))),
                          tabPanel("Detalhe por Concorrente", br(), uiOutput(ns("tabela_infra_detalhada_ui")))
                        )
                 )
               )
      ),
      
      tabPanel("Desempenho Acadêmico",
               h3("Análise de Desempenho - ENEM 2024"),
               p("Comparativo das médias do ENEM por área de conhecimento e detalhamento das competências da redação."),
               tabsetPanel(
                 type = "tabs",
                 tabPanel("Visão Consolidada", br(),
                          uiOutput(ns("ui_desempenho_areas_consolidado")), hr(),
                          uiOutput(ns("ui_desempenho_redacao_consolidado"))
                 ),
                 tabPanel("Detalhe por Concorrente", br(),
                          uiOutput(ns("ui_desempenho_areas_detalhado")), hr(),
                          uiOutput(ns("ui_desempenho_redacao_detalhado"))
                 )
               )
      ),
      
      tabPanel("Chat com IA", icon = icon("robot"),
               mod_chat_ui(ns("chat_ia"))
      )
    )
  )
}


mod_escola_server <- function(id, user, codinep) {
  moduleServer(id, function(input, output, session) {
    
    source("utils/preprocess_utils.R", local = TRUE)
    source("utils/db_utils.R",        local = TRUE)
    
    dados_escola_reativo <- reactiveVal(NULL)
    dados_escola_padrao  <- reactiveVal(NULL)
    
    # Carrega .rds da escola após login
    observe({
      req(codinep)
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep, ".rds"))
      if (file.exists(caminho_arquivo)) {
        dados_carregados <- readRDS(caminho_arquivo)
        dados_escola_reativo(dados_carregados)
        if (is.null(dados_escola_padrao())) dados_escola_padrao(dados_carregados)
      }
    })
    
    # Chat IA
    mod_chat_server("chat_ia", dados_escola = dados_escola_reativo)
    
    # Título
    output$nome_escola_titulo <- renderText({
      d <- dados_escola_reativo()
      if (is.null(d)) "Painel da Escola" else paste("Painel da Escola:", d$nome_escola)
    })
    
    # ----- Base de concorrentes disponível para seleção -----
    concorrentes_disponiveis <- reactive({
      nomes_df <- readRDS("data/escolas_privadas_nomelista.rds") |>
        dplyr::mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
      
      geo_sf <- readRDS("data/processed/escolas_privadas_unificadas_sf.rds") |>
        dplyr::mutate(id_escola = as.character(id_escola))
      
      geo_sf |>
        dplyr::left_join(nomes_df, by = c("id_escola" = "CO_ENTIDADE")) |>
        dplyr::mutate(
          display_name = paste0(
            NO_ENTIDADE, " - ",
            dplyr::coalesce(nome_municipio, "(município desconhecido)"),
            " (", id_escola, ")"
          )
        ) |>
        dplyr::filter(!is.na(NO_ENTIDADE)) |>
        dplyr::arrange(NO_ENTIDADE)
    })
    
    # Preenche selectize com escolas do mesmo município (fallback: todas)
    observe({
      choices_df <- concorrentes_disponiveis()
      d <- dados_escola_reativo()
      req(d)
      
      alvo_muni <- as.character(d$nome_municipio)
      escolas_municipio <- choices_df |>
        dplyr::filter(nome_municipio == alvo_muni, id_escola != codinep)
      
      if (nrow(escolas_municipio) == 0) {
        escolas_municipio <- choices_df |>
          dplyr::filter(id_escola != codinep)
      }
      
      updateSelectizeInput(
        session, "selecao_concorrentes",
        choices  = stats::setNames(escolas_municipio$id_escola, escolas_municipio$display_name),
        selected = d$lista_concorrentes$id_escola,
        server   = TRUE
      )
    })
    
    # Salva nova seleção e reprocessa
    observeEvent(input$btn_atualizar_analise, {
      req(input$selecao_concorrentes)
      showNotification("Atualizando e salvando nova seleção de concorrentes...", type = "message", duration = 5)
      
      d_atual <- dados_escola_reativo()
      save_concorrentes_selecionados(codinep, input$selecao_concorrentes)
      
      novos <- reprocessar_dados_concorrentes(codinep, input$selecao_concorrentes, d_atual$id_municipio)
      
      escolas_full <- concorrentes_disponiveis()
      info_conc   <- escolas_full |>
        dplyr::filter(id_escola %in% input$selecao_concorrentes)
      
      d_atual$lista_concorrentes <- info_conc |>
        dplyr::select(id_escola, nome_escola = NO_ENTIDADE, geometry)
      
      dados_finais <- c(
        d_atual[c("id_escola","nome_escola","id_municipio","nome_municipio","latitude","longitude","lista_concorrentes","data_extracao")],
        novos
      )
      
      saveRDS(dados_finais, file.path("data", "escolas", paste0(codinep, ".rds")))
      dados_escola_reativo(dados_finais)
      showNotification("Análise atualizada com sucesso!", type = "message", duration = 3)
    })
    
    # Restaura 5 mais próximos (padrão)
    observeEvent(input$btn_restaurar_padrao, {
      showNotification("Restaurando análise para os 5 concorrentes mais próximos...", type = "message", duration = 3)
      save_concorrentes_selecionados(codinep, character(0))
      
      d_padrao <- dados_escola_padrao()
      ids_padrao <- d_padrao$lista_concorrentes$id_escola
      
      repro <- reprocessar_dados_concorrentes(codinep, ids_padrao, d_padrao$id_municipio)
      
      dados_final <- c(
        d_padrao[c("id_escola","nome_escola","id_municipio","nome_municipio","latitude","longitude","lista_concorrentes","data_extracao")],
        repro
      )
      
      saveRDS(dados_final, file.path("data", "escolas", paste0(codinep, ".rds")))
      dados_escola_reativo(dados_final)
    })
    
    # ----- KPIs -----
    criar_caixa_kpi <- function(seg) {
      valor1 <- seg$valor_ano_1
      valor2 <- seg$valor_ano_2
      taxa   <- seg$taxa_de_variacao
      
      cor_taxa <- if (is.na(taxa) || taxa == "N/A") "#6c757d"
      else if (as.numeric(sub("%", "", taxa)) > 0) "#198754"
      else "#dc3545"
      
      div(class = "stat-card",
          tagList(
            h5(seg$label),
            p(paste("2023:", valor1, "| 2024:", valor2)),
            h4(style = paste0("color:", cor_taxa, ";"), taxa)
          )
      )
    }
    
    output$kpis_escola <- renderUI({
      d <- dados_escola_reativo(); req(d)
      lapply(d$dados_propria_escola, criar_caixa_kpi)
    })
    output$kpis_concorrentes <- renderUI({
      d <- dados_escola_reativo(); req(d)
      lapply(d$dados_concorrentes_proximos, criar_caixa_kpi)
    })
    output$kpis_mercado <- renderUI({
      d <- dados_escola_reativo(); req(d)
      lapply(d$dados_mercado_municipio, criar_caixa_kpi)
    })
    
    # ----- Tabela de concorrentes -----
    output$tabela_concorrentes <- DT::renderDataTable({
      d <- dados_escola_reativo(); req(d, d$lista_concorrentes)
      tab <- d$lista_concorrentes |>
        sf::st_drop_geometry() |>
        dplyr::rename(`Cód. INEP` = id_escola, `Nome da Escola` = nome_escola)
      DT::datatable(tab, options = list(pageLength = 10, scrollY = "450px"))
    })
    
    # =================== MAPA – abrir em nova aba (HTML estático) ===================
    observeEvent(input$abrir_mapa, {
      d <- dados_escola_reativo()
      req(d, !is.na(d$latitude), !is.na(d$longitude))
      
      # Escola principal
      escola_sf <- sf::st_as_sf(
        tibble::tibble(
          lon = as.numeric(d$longitude),
          lat = as.numeric(d$latitude),
          nome_escola = d$nome_escola
        ),
        coords = c("lon","lat"), crs = 4326
      )
      
      # Concorrentes -> garantir sf + CRS
      conc_sf <- d$lista_concorrentes
      if (!inherits(conc_sf, "sf")) {
        if (!is.null(conc_sf) && nrow(conc_sf) > 0) {
          uni_sf <- readRDS("data/processed/escolas_privadas_unificadas_sf.rds") |>
            dplyr::mutate(id_escola = as.character(id_escola)) |>
            dplyr::select(id_escola, geometry)
          conc_sf <- dplyr::left_join(
            dplyr::mutate(d$lista_concorrentes, id_escola = as.character(id_escola)),
            uni_sf, by = "id_escola"
          ) |>
            sf::st_as_sf()
        }
      }
      if (!is.null(conc_sf) && nrow(conc_sf) > 0) {
        if (!is.na(sf::st_crs(conc_sf))) conc_sf <- suppressWarnings(sf::st_transform(conc_sf, 4326))
        if ("id_escola" %in% names(conc_sf)) conc_sf <- dplyr::distinct(conc_sf, id_escola, .keep_all = TRUE)
        conc_sf <- conc_sf[!sf::st_is_empty(conc_sf), , drop = FALSE]
        if (nrow(conc_sf) == 0) conc_sf <- NULL
      } else conc_sf <- NULL
      
      # Mapa
      m <- leaflet::leaflet(options = leaflet::leafletOptions(minZoom = 3)) |>
        leaflet::addTiles(
          urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/{z}/{y}/{x}",
          options = leaflet::tileOptions(crossOrigin = TRUE)
        ) |>
        leaflet::addTiles(
          urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
          options = leaflet::tileOptions(crossOrigin = TRUE),
          group = "OSM (fallback)"
        ) |>
        leaflet::addAwesomeMarkers(
          data = escola_sf,
          icon = leaflet::awesomeIcons(icon = 'school', library = 'fa',
                                       markerColor = "blue", iconColor = '#FFFFFF'),
          popup = ~htmltools::htmlEscape(nome_escola)
        ) |>
        leaflet::addScaleBar(position = "bottomleft")
      
      if (!is.null(conc_sf)) {
        m <- m |>
          leaflet::addAwesomeMarkers(
            data = conc_sf,
            icon = leaflet::awesomeIcons(icon = 'school', library = 'fa',
                                         markerColor = "red", iconColor = '#FFFFFF'),
            popup = ~htmltools::htmlEscape(dplyr::coalesce(nome_escola, as.character(id_escola)))
          )
      }
      
      geoms <- c(sf::st_geometry(escola_sf),
                 if (!is.null(conc_sf)) sf::st_geometry(conc_sf) else NULL)
      if (length(geoms) > 0) {
        bb <- sf::st_bbox(geoms)
        m <- m |> leaflet::fitBounds(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"])
      } else {
        coords <- sf::st_coordinates(escola_sf)
        m <- m |> leaflet::setView(lng = coords[,"X"], lat = coords[,"Y"], zoom = 14)
      }
      
      # Salva e abre
      dir.create("www/maps", showWarnings = FALSE, recursive = TRUE)
      fname <- sprintf("mapa_%s_%s.html", codinep, format(Sys.time(), "%Y%m%d%H%M%S"))
      fpath <- file.path("www","maps", fname)
      suppressWarnings(htmlwidgets::saveWidget(m, fpath, selfcontained = TRUE))
      session$sendCustomMessage("openURL", list(url = paste0("maps/", fname)))
    })
    
    # =================== Google Maps – sempre por centróide (sem SF trick) ===================
    observeEvent(input$abrir_gmaps, {
      d <- dados_escola_reativo()
      req(d, !is.na(d$latitude), !is.na(d$longitude))
      
      dest <- paste0(round(as.numeric(d$latitude), 6), ",", round(as.numeric(d$longitude), 6))
      
      conc_sf <- d$lista_concorrentes
      if (!inherits(conc_sf, "sf")) {
        if (!is.null(conc_sf) && nrow(conc_sf) > 0) {
          uni_sf <- readRDS("data/processed/escolas_privadas_unificadas_sf.rds") |>
            dplyr::mutate(id_escola = as.character(id_escola)) |>
            dplyr::select(id_escola, geometry)
          conc_sf <- dplyr::left_join(
            dplyr::mutate(d$lista_concorrentes, id_escola = as.character(id_escola)),
            uni_sf, by = "id_escola"
          ) |>
            sf::st_as_sf()
        }
      }
      waypoints <- character(0)
      if (!is.null(conc_sf) && nrow(conc_sf) > 0) {
        conc_sf <- suppressWarnings(sf::st_transform(conc_sf, 4326))
        conc_sf <- suppressWarnings(sf::st_centroid(conc_sf))           # <- SEMPRE centróide (seguro)
        coords  <- sf::st_coordinates(conc_sf)
        n       <- min(nrow(coords), 8)                                 # limita tamanho da URL
        waypoints <- paste0(round(coords[seq_len(n), "Y"], 6), ",", round(coords[seq_len(n), "X"], 6))
      }
      
      base <- "https://www.google.com/maps/dir/?api=1"
      url  <- paste0(
        base,
        "&destination=", utils::URLencode(dest),
        if (length(waypoints)) paste0("&waypoints=", utils::URLencode(paste(waypoints, collapse = "|"))) else "",
        "&travelmode=driving"
      )
      
      session$sendCustomMessage("openURL", list(url = url))
    })
    
    # ----- Infraestrutura (resumo) -----
    dados_infra_resumidos <- reactive({
      d <- dados_escola_reativo(); req(d, d$dados_infraestrutura)
      df_infra <- d$dados_infraestrutura
      
      escola_principal <- df_infra |> dplyr::filter(id_escola == codinep)
      media_municipal  <- df_infra |> dplyr::filter(id_escola == "Média Municipal")
      media_conc <- df_infra |>
        dplyr::filter(id_escola != codinep, id_escola != "Média Municipal") |>
        dplyr::summarise(dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) |>
        dplyr::mutate(id_escola = "Média Concorrentes")
      
      df_comp <- dplyr::bind_rows(escola_principal, media_conc, media_municipal)
      
      df_comp |>
        tidyr::pivot_longer(cols = -id_escola, names_to = "indicador", values_to = "valor") |>
        dplyr::mutate(
          categoria = stringr::str_extract(indicador, "^[^_]+"),
          indicador_label = indicador |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_remove(categoria) |>
            stringr::str_trim() |>
            stringr::str_to_title()
        ) |>
        tidyr::pivot_wider(names_from = id_escola, values_from = valor) |>
        dplyr::rename(`Sua Escola` = !!rlang::sym(codinep))
    })
    
    output$cards_infra_detalhada <- renderUI({
      df_cards <- dados_infra_resumidos()
      
      criar_cartao_indicador <- function(row) {
        indicador_nome     <- row[["indicador_label"]]
        valor_escola       <- row[["Sua Escola"]]
        valor_concorrentes <- row[["Média Concorrentes"]]
        valor_municipal    <- row[["Média Municipal"]]
        is_count <- stringr::str_detect(row[["indicador"]], "total_dispositivos")
        
        formatar_linha <- function(label, valor) {
          valor_num <- as.numeric(valor)
          valor_fmt <- if (is_count) round(valor_num, 1) else scales::percent(valor_num, accuracy = 0.1)
          tagList(
            div(class = "indicator-row",
                shiny::span(class = "indicator-label", label),
                shiny::span(class = "indicator-value", valor_fmt)
            ),
            if (!is_count) {
              div(class = "progress-bar-container",
                  div(class = "progress-bar-fill", style = paste0("width: ", valor_num * 100, "%;"))
              )
            }
          )
        }
        
        column(
          4,
          div(class = "indicator-card",
              div(class = "indicator-card-title", indicador_nome),
              formatar_linha("Sua Escola", valor_escola),
              formatar_linha("Média Concorrentes", valor_concorrentes),
              formatar_linha("Média Municipal", valor_municipal)
          )
        )
      }
      
      categorias <- unique(df_cards$categoria)
      lapply(categorias, function(cat) {
        df_cat <- df_cards |> dplyr::filter(categoria == cat)
        tagList(
          h5(stringr::str_to_title(cat)),
          fluidRow(apply(df_cat, 1, criar_cartao_indicador)),
          br()
        )
      })
    })
    
    output$tabela_infra_detalhada_ui <- renderUI({
      d <- dados_escola_reativo(); req(d, d$dados_infraestrutura, d$lista_concorrentes)
      
      df_infra_raw <- d$dados_infraestrutura
      nomes_conc <- d$lista_concorrentes |>
        sf::st_drop_geometry() |>
        dplyr::select(id_escola, nome_escola)
      
      df_detalhada <- df_infra_raw |>
        dplyr::filter(id_escola %in% c(codinep, d$lista_concorrentes$id_escola)) |>
        dplyr::left_join(nomes_conc, by = "id_escola") |>
        dplyr::mutate(label = ifelse(id_escola == codinep, "Sua Escola", nome_escola)) |>
        dplyr::select(-id_escola, -nome_escola) |>
        tidyr::pivot_longer(cols = -label, names_to = "indicador", values_to = "valor") |>
        dplyr::mutate(
          indicador_label = indicador |>
            stringr::str_replace_all("_", " ") |>
            stringr::str_remove("^(essencial|lazer|tec|apoio)") |>
            stringr::str_trim() |>
            stringr::str_to_title()
        ) |>
        dplyr::select(Indicador = indicador_label, Escola = label, Valor = valor) |>
        tidyr::pivot_wider(names_from = Escola, values_from = Valor) |>
        dplyr::relocate(Indicador, `Sua Escola`)
      
      df_formatada <- df_detalhada |>
        dplyr::mutate(dplyr::across(-Indicador, ~ {
          ifelse(
            Indicador == "Total Dispositivos Aluno",
            as.character(round(as.numeric(.), 1)),
            ifelse(as.numeric(.) == 1,
                   as.character(icon("check", class = "table-icon", style = "color: green;")),
                   as.character(icon("times", class = "table-icon", style = "color: red;"))
            )
          )
        }))
      
      tabela_html <- knitr::kable(df_formatada, "html", escape = FALSE,
                                  align = c('l', rep('c', ncol(df_formatada) - 1))) |>
        kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = TRUE) |>
        kableExtra::scroll_box(width = "100%")
      
      HTML(tabela_html)
    })
    
    # ===== ENEM (consolidado) =====
    output$ui_desempenho_areas_consolidado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Comparativo de Médias por Área"),
               plotly::plotlyOutput(ns("plot_enem_areas_consolidado"))),
        column(5, h4("Tabela de Médias"),
               DT::dataTableOutput(ns("tabela_enem_areas_consolidado")))
      )
    })
    
    output$plot_enem_areas_consolidado <- plotly::renderPlotly({
      d <- dados_escola_reativo(); req(d, d$dados_enem_areas)
      df <- d$dados_enem_areas |>
        dplyr::filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df, x = ~area, y = ~nota, color = ~escola_label, type = 'bar',
                      text = ~round(nota, 1), textposition = 'outside') |>
        plotly::layout(yaxis = list(title = 'Nota Média'),
                       xaxis = list(title = 'Área de Conhecimento'),
                       barmode = 'group',
                       legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_consolidado <- DT::renderDataTable({
      d <- dados_escola_reativo(); req(d, d$dados_enem_areas)
      d$dados_enem_areas |>
        dplyr::filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) |>
        dplyr::mutate(nota = round(nota, 1)) |>
        dplyr::select(`Área` = area, `Categoria` = escola_label, `Nota` = nota) |>
        tidyr::pivot_wider(names_from = Categoria, values_from = Nota) |>
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    output$ui_desempenho_redacao_consolidado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Detalhamento da Nota de Redação"),
               plotly::plotlyOutput(ns("plot_enem_redacao_consolidado"))),
        column(5, h4("Tabela de Competências"),
               DT::dataTableOutput(ns("tabela_enem_redacao_consolidado")))
      )
    })
    
    output$plot_enem_redacao_consolidado <- plotly::renderPlotly({
      d <- dados_escola_reativo(); req(d, d$dados_enem_redacao)
      df <- d$dados_enem_redacao |>
        dplyr::filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal"))
      
      plotly::plot_ly(df, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar',
                      text = ~round(nota, 1), textposition = 'outside') |>
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)),
                       xaxis = list(title = 'Competência da Redação'),
                       barmode = 'group',
                       legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_consolidado <- DT::renderDataTable({
      d <- dados_escola_reativo(); req(d, d$dados_enem_redacao)
      d$dados_enem_redacao |>
        dplyr::filter(tipo %in% c("Sua Escola", "Média Concorrentes", "Média Municipal")) |>
        dplyr::mutate(nota = round(nota, 1)) |>
        dplyr::select(`Competência` = competencia, `Categoria` = escola_label, `Nota` = nota) |>
        tidyr::pivot_wider(names_from = Categoria, values_from = Nota) |>
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    # ===== ENEM (detalhado) =====
    output$ui_desempenho_areas_detalhado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Comparativo de Médias por Área"),
               plotly::plotlyOutput(ns("plot_enem_areas_detalhado"))),
        column(5, h4("Tabela de Médias"),
               DT::dataTableOutput(ns("tabela_enem_areas_detalhado")))
      )
    })
    
    output$plot_enem_areas_detalhado <- plotly::renderPlotly({
      d <- dados_escola_reativo(); req(d, d$dados_enem_areas)
      df <- d$dados_enem_areas |>
        dplyr::filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df, x = ~area, y = ~nota, color = ~escola_label, type = 'bar',
                      text = ~round(nota, 1), textposition = 'outside') |>
        plotly::layout(yaxis = list(title = 'Nota Média'),
                       xaxis = list(title = 'Área de Conhecimento'),
                       barmode = 'group',
                       legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_areas_detalhado <- DT::renderDataTable({
      d <- dados_escola_reativo(); req(d, d$dados_enem_areas)
      d$dados_enem_areas |>
        dplyr::filter(tipo %in% c("Sua Escola", "Concorrente")) |>
        dplyr::mutate(nota = round(nota, 1)) |>
        dplyr::select(`Área` = area, `Escola` = escola_label, `Nota` = nota) |>
        tidyr::pivot_wider(names_from = Escola, values_from = Nota) |>
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    output$ui_desempenho_redacao_detalhado <- renderUI({
      ns <- session$ns
      fluidRow(
        column(7, h4("Detalhamento da Nota de Redação"),
               plotly::plotlyOutput(ns("plot_enem_redacao_detalhado"))),
        column(5, h4("Tabela de Competências"),
               DT::dataTableOutput(ns("tabela_enem_redacao_detalhado")))
      )
    })
    
    output$plot_enem_redacao_detalhado <- plotly::renderPlotly({
      d <- dados_escola_reativo(); req(d, d$dados_enem_redacao)
      df <- d$dados_enem_redacao |>
        dplyr::filter(tipo %in% c("Sua Escola", "Concorrente"))
      
      plotly::plot_ly(df, x = ~competencia, y = ~nota, color = ~escola_label, type = 'bar',
                      text = ~round(nota, 1), textposition = 'outside') |>
        plotly::layout(yaxis = list(title = 'Nota Média', range = c(0, 220)),
                       xaxis = list(title = 'Competência da Redação'),
                       barmode = 'group',
                       legend = list(orientation = 'h', y = -0.2))
    })
    
    output$tabela_enem_redacao_detalhado <- DT::renderDataTable({
      d <- dados_escola_reativo(); req(d, d$dados_enem_redacao)
      d$dados_enem_redacao |>
        dplyr::filter(tipo %in% c("Sua Escola", "Concorrente")) |>
        dplyr::mutate(nota = round(nota, 1)) |>
        dplyr::select(`Competência` = competencia, `Escola` = escola_label, `Nota` = nota) |>
        tidyr::pivot_wider(names_from = Escola, values_from = Nota) |>
        DT::datatable(options = list(dom = 't'), rownames = FALSE)
    })
  })
}
