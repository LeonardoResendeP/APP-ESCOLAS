mod_escola_ui <- function(id) {
  ns <- NS(id)
  
  navbarPage(
    title = textOutput(ns("nome_escola_titulo")), # Título dinâmico com o nome da escola
    
    # --- Aba Visão Geral ---
    tabPanel("Visão Geral", 
             fluidRow(
               column(12,
                      h3("Resumo de Matrículas (Variação 2022 vs 2023)"),
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
    
    # --- Aba Benchmark Concorrentes ---
    tabPanel("Benchmark Concorrentes", 
             h3("Concorrentes Mais Próximos"),
             p("Lista dos 5 concorrentes mais próximos geograficamente."),
             DT::dataTableOutput(ns("tabela_concorrentes"))
    ),
    
    tabPanel("Perfil Socioeconômico", h4("Análise de contexto do entorno")),
    tabPanel("Desempenho Acadêmico", h4("Notas, aprovações e indicadores")),
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
      
      # Define a cor da taxa com base no valor
      cor_taxa <- if (is.na(taxa) || taxa == "N/A") {
        "gray"
      } else if (as.numeric(sub("%", "", taxa)) > 0) {
        "green"
      } else {
        "red"
      }
      
      # Estilo CSS para as caixas
      box_style <- "border: 1px solid #eee; border-radius: 5px; padding: 15px; margin-bottom: 15px; background-color: #f9f9f9;"
      
      tagList(
        h5(dados_segmento$label),
        p(paste("2022:", valor1, "| 2023:", valor2)),
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
    
    # --- Lógica da Aba Benchmark Concorrentes ---
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
      
      DT::datatable(tabela, options = list(pageLength = 5))
    })
    
  })
}
