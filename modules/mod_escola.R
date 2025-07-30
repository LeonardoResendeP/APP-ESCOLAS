# --- Bibliotecas necessárias para as novas abas ---
# Certifique-se de ter instalado: install.packages(c("plotly", "knitr", "kableExtra", "scales", "leaflet"))

mod_escola_ui <- function(id) {
  ns <- NS(id)
  
  navbarPage(
    title = textOutput(ns("nome_escola_titulo")), # Título dinâmico com o nome da escola
    
    # --- Aba Visão Geral ---
    tabPanel("Visão Geral", 
             fluidRow(
               column(12,
                      h3("Resumo de Matrículas (Variação 2023 vs 2024)"),
                      p("Comparativo do desempenho da sua escola em relação à média dos 5 concorrentes mais próximos e ao mercado total do seu município.")
               )
             ),
             br(),
             fluidRow(
               # Coluna da Escola
               column(4, style="border-right: 1px solid #ddd;",
                      h4(icon("school"), "Sua Escola"),
                      hr(),
                      uiOutput(ns("kpis_escola"))
               ),
               # Coluna dos Concorrentes
               column(4, style="border-right: 1px solid #ddd;",
                      h4(icon("users"), "Média dos Concorrentes"),
                      hr(),
                      uiOutput(ns("kpis_concorrentes"))
               ),
               # Coluna do Mercado
               column(4,
                      h4(icon("chart-pie"), "Mercado (Município)"),
                      hr(),
                      uiOutput(ns("kpis_mercado"))
               )
             )
    ),
    
    # --- Aba Benchmark Concorrentes (ATUALIZADA COM MAPA) ---
    tabPanel("Benchmark Concorrentes", 
             h3("Análise Geográfica dos Concorrentes"),
             p("Visualização da sua escola e dos 5 concorrentes mais próximos no mapa, e detalhes na tabela."),
             fluidRow(
               column(6,
                      h4("Mapa de Concorrentes"),
                      leaflet::leafletOutput(ns("mapa_concorrentes"), height = "500px")
               ),
               column(6,
                      h4("Lista de Concorrentes"),
                      DT::dataTableOutput(ns("tabela_concorrentes"))
               )
             )
    ),
    
    # --- Aba Análise de Infraestrutura ---
    tabPanel("Análise de Infraestrutura",
             fluidRow(
               column(12,
                      h3("Comparativo de Infraestrutura (Censo 2024)"),
                      p("Análise comparativa dos principais indicadores de infraestrutura da sua escola em relação aos concorrentes e à média do município.")
               )
             ),
             hr(),
             fluidRow(
               column(12,
                      h4("Visão Geral Comparativa"),
                      # Gráfico de Radar
                      plotly::plotlyOutput(ns("radar_infra"), height = "600px")
               )
             ),
             br(),
             fluidRow(
               column(12,
                      h4("Análise Detalhada por Categoria"),
                      # Tabelas detalhadas por categoria
                      uiOutput(ns("tabelas_infra_detalhada"))
               )
             )
    ),
    
    tabPanel("Desempenho Acadêmico", h4("Análise de contexto do entorno")),
    tabPanel("Perfil Socioeconômico", h4("Análise de contexto do entorno")),
    tabPanel("Análise de Mercado", h4("Tendências de matrícula e oportunidades")),
    tabPanel("Relatórios", h4("Exportar PDF, Excel ou relatório online"))
  )
}


mod_escola_server <- function(id, user, codinep) {
  moduleServer(id, function(input, output, session) {
    
    # --- Carregamento dos Dados ---
    dados_escola <- reactive({
      req(codinep) # Garante que o codinep foi recebido
      
      caminho_arquivo <- file.path("data", "escolas", paste0(codinep, ".rds"))
      
      if (file.exists(caminho_arquivo)) {
        readRDS(caminho_arquivo)
      } else {
        NULL # Retorna nulo se o arquivo não for encontrado
      }
    })
    
    # --- Título Dinâmico ---
    output$nome_escola_titulo <- renderText({
      dados <- dados_escola()
      if (!is.null(dados)) {
        paste("Painel da Escola:", dados$nome_escola)
      } else {
        "Painel da Escola"
      }
    })
    
    # --- Lógica da Aba Visão Geral ---
    
    # Função auxiliar para criar as caixas de KPI
    criar_caixa_kpi <- function(dados_segmento) {
      valor1 <- dados_segmento$valor_ano_1
      valor2 <- dados_segmento$valor_ano_2
      taxa <- dados_segmento$taxa_de_variacao
      
      cor_taxa <- if (is.na(taxa) || taxa == "N/A") {
        "gray"
      } else if (as.numeric(sub("%", "", taxa)) > 0) {
        "green"
      } else {
        "red"
      }
      
      tagList(
        h5(dados_segmento$label),
        p(paste("2023:", valor1, "| 2024:", valor2)),
        h4(style = paste0("color:", cor_taxa, ";"), taxa)
      )
    }
    
    output$kpis_escola <- renderUI({
      dados <- dados_escola()
      req(dados)
      lapply(dados$dados_propria_escola, criar_caixa_kpi)
    })
    
    output$kpis_concorrentes <- renderUI({
      dados <- dados_escola()
      req(dados)
      lapply(dados$dados_concorrentes_proximos, criar_caixa_kpi)
    })
    
    output$kpis_mercado <- renderUI({
      dados <- dados_escola()
      req(dados)
      lapply(dados$dados_mercado_municipio, criar_caixa_kpi)
    })
    
    # --- Lógica da Aba Benchmark Concorrentes (ATUALIZADA COM MAPA) ---
    output$tabela_concorrentes <- DT::renderDataTable({
      dados <- dados_escola()
      req(dados, dados$lista_concorrentes)
      
      tabela <- dados$lista_concorrentes %>%
        mutate(dist_metros = round(dist_metros, 0)) %>%
        rename(
          `Cód. INEP` = id_escola,
          `Nome da Escola` = nome_escola,
          Latitude = latitude,
          Longitude = longitude,
          `Distância (metros)` = dist_metros
        )
      
      DT::datatable(tabela, options = list(pageLength = 10, scrollY = "350px"))
    })
    
    output$mapa_concorrentes <- leaflet::renderLeaflet({
      dados <- dados_escola()
      req(dados, dados$lista_concorrentes, !is.na(dados$latitude), !is.na(dados$longitude))
      
      # Dados da escola principal
      escola_principal <- tibble(
        nome_escola = dados$nome_escola,
        latitude = dados$latitude,
        longitude = dados$longitude,
        tipo = "Sua Escola"
      )
      
      # Dados dos concorrentes
      concorrentes <- dados$lista_concorrentes %>%
        mutate(tipo = "Concorrente") %>%
        select(nome_escola, latitude, longitude, tipo)
      
      # Combina os dados para o mapa
      df_mapa <- bind_rows(escola_principal, concorrentes) %>%
        filter(!is.na(latitude), !is.na(longitude))
      
      # Define ícones com cores diferentes
      icons <- leaflet::awesomeIcons(
        icon = 'school',
        library = 'fa',
        markerColor = ifelse(df_mapa$tipo == "Sua Escola", "blue", "red"),
        iconColor = '#FFFFFF'
      )
      
      # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< CORREÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
      # Alterado para usar uma string com o nome do provedor, que é a forma mais robusta.
      leaflet::leaflet(data = df_mapa) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
        leaflet::addAwesomeMarkers(
          lng = ~longitude,
          lat = ~latitude,
          icon = icons,
          popup = ~htmltools::htmlEscape(nome_escola)
        )
    })
    
    
    # --- Lógica da Aba Análise de Infraestrutura ---
    
    # 1. Prepara os dados de infraestrutura em um formato "tidy" (longo)
    dados_infra_preparados <- reactive({
      dados <- dados_escola()
      req(dados, dados$dados_infraestrutura, dados$lista_concorrentes)
      
      df_infra <- dados$dados_infraestrutura
      
      nomes_concorrentes <- dados$lista_concorrentes %>%
        select(id_escola, nome_escola)
      
      df_full <- df_infra %>%
        left_join(nomes_concorrentes, by = "id_escola") %>%
        mutate(
          label = case_when(
            id_escola == codinep ~ "Sua Escola",
            id_escola == "Média Municipal" ~ "Média Municipal",
            TRUE ~ nome_escola
          )
        )
      
      df_long <- df_full %>%
        select(-id_escola, -nome_escola) %>%
        pivot_longer(
          cols = -label,
          names_to = "indicador",
          values_to = "valor"
        ) %>%
        mutate(
          categoria = str_extract(indicador, "^[^_]+"),
          indicador_label = str_replace_all(indicador, "_", " ") %>% str_remove(categoria) %>% str_trim() %>% str_to_title()
        )
      
      return(df_long)
    })
    
    # 2. Renderiza o Gráfico de Radar com concorrentes individuais
    output$radar_infra <- plotly::renderPlotly({
      df_plot <- dados_infra_preparados()
      
      df_radar <- df_plot %>%
        filter(!str_detect(indicador, "total_dispositivos"))
      
      p <- plotly::plot_ly(
        type = 'scatterpolar',
        mode = 'lines',
        fill = 'toself'
      )
      
      entidades <- unique(df_radar$label)
      
      for (entidade_atual in entidades) {
        dados_entidade <- df_radar %>% filter(label == entidade_atual)
        
        p <- p %>% plotly::add_trace(
          r = dados_entidade$valor,
          theta = dados_entidade$indicador_label,
          name = entidade_atual
        )
      }
      
      p <- p %>% plotly::layout(
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 1)
          )
        ),
        showlegend = TRUE,
        legend = list(orientation = 'h', y = -0.1)
      )
      
      p
    })
    
    # 3. Renderiza as Tabelas Detalhadas
    output$tabelas_infra_detalhada <- renderUI({
      df_long <- dados_infra_preparados()
      
      categorias <- unique(df_long$categoria)
      
      lista_de_elementos <- lapply(categorias, function(cat) {
        
        df_cat_wide <- df_long %>%
          filter(categoria == cat) %>%
          select(Indicador = indicador_label, label, valor) %>%
          pivot_wider(names_from = label, values_from = valor)
        
        if("Sua Escola" %in% names(df_cat_wide)) {
          df_cat_wide <- df_cat_wide %>% relocate(Indicador, `Sua Escola`)
        }
        
        cols_numericas <- names(df_cat_wide)[sapply(df_cat_wide, is.numeric)]
        
        df_cat_formatada <- df_cat_wide %>%
          mutate(across(all_of(cols_numericas), ~ {
            is_count <- any(str_detect(Indicador, "Total Dispositivos"))
            
            if(is_count) {
              round(as.numeric(.), 1)
            } else {
              scales::percent(as.numeric(.), accuracy = 0.1)
            }
          }))
        
        tabela_html <- knitr::kable(df_cat_formatada, "html", align = 'l') %>%
          kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
        
        tagList(
          h5(str_to_title(cat)),
          HTML(tabela_html),
          br()
        )
      })
      
      do.call(tagList, lista_de_elementos)
    })
    
  })
}
