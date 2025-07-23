# preprocessar_escola.R
preprocessar_escola <- function(codinep, lat = NULL, lon = NULL) {
  # Carrega todas as bibliotecas necessárias no início
  suppressMessages({
    if (!require(pacman)) install.packages("pacman");
    pacman::p_load(DBI, bigrquery, basedosdados, tidyverse, sf, glue, geosphere)
  })
  
  codinep <- as.character(codinep)
  cat("\n\U0001F504 Iniciando processamento da escola:", codinep, "\n")
  
  # --- Conexão com o BigQuery ---
  set_billing_id("able-study-331220")
  cat("\U0001F4F1 Conectando ao BigQuery...\n")
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "basedosdados",
    dataset = "br_inep_censo_escolar",
    billing = "able-study-331220"
  )
  
  # --- 1. Obter dados da própria escola (latitude, longitude, etc.) ---
  cat("\U0001F4E5 Buscando dados da escola principal...\n")
  
  # Carrega arquivos locais de referência
  nomes_all <- readRDS("data/escolas_privadas_nomelista.rds") %>% dplyr::mutate(CO_ENTIDADE = as.character(CO_ENTIDADE))
  geo_all_sf <- readRDS("data/escolas_geo_com_empty_flag.rds")
  
  # Converte o objeto sf para um data.frame padrão
  geo_all_df <- sf::st_drop_geometry(geo_all_sf) %>%
    dplyr::mutate(code_school = as.character(code_school))
  
  # Pega o nome da escola
  nome_escola <- nomes_all %>% dplyr::filter(CO_ENTIDADE == codinep) %>% dplyr::pull(NO_ENTIDADE) %>% first()
  if (is.na(nome_escola)) nome_escola <- "N/D"
  
  # Pega a geolocalização e o NOME do município da escola principal
  cat("\U0001F30E Verificando geolocalização...\n")
  if (is.null(lat) || is.null(lon)) {
    geo_escola <- geo_all_df %>% dplyr::filter(code_school == codinep)
    if (nrow(geo_escola) == 0 || is.na(geo_escola$latitude) || is.na(geo_escola$longitude)) {
      stop("Geolocalização para a escola principal não encontrada. Cadastre manualmente.")
    }
    lat <- geo_escola$latitude[1]
    lon <- geo_escola$longitude[1]
    nome_muni <- geo_escola$name_muni[1] # Usamos o NOME do município
  } else {
    # Se a geo for manual, ainda precisamos do nome do município
    geo_escola <- geo_all_df %>% dplyr::filter(code_school == codinep)
    nome_muni <- geo_escola$name_muni[1]
    if(is.na(nome_muni)) stop("Não foi possível determinar o município da escola.")
  }
  
  # --- 2. OTIMIZAÇÃO: Encontrar concorrentes próximos PRIMEIRO ---
  cat("\U0001F680 Otimização: Buscando 5 concorrentes mais próximos localmente...\n")
  
  concorrentes_locais <- geo_all_df %>%
    dplyr::filter(name_muni == nome_muni,         # Filtra pelo mesmo NOME de município
                  code_school != codinep,          # Exclui a própria escola
                  !is.na(latitude), !is.na(longitude)) # Garante que tenham geo
  
  # Calcula a distância para todos os concorrentes locais
  if (nrow(concorrentes_locais) > 0) {
    concorrentes_locais <- concorrentes_locais %>%
      dplyr::mutate(dist_metros = distHaversine(
        matrix(c(lon, lat), nrow = 1),
        matrix(c(longitude, latitude), ncol = 2)
      )) %>%
      dplyr::arrange(dist_metros) %>%
      dplyr::slice(1:5) # Pega os 5 mais próximos
  }
  
  # Agora temos uma lista pequena de IDs de concorrentes
  ids_concorrentes_proximos <- concorrentes_locais$code_school
  
  # --- 3. Buscar dados no BigQuery APENAS para a escola principal e os 5 concorrentes ---
  cat("\U0001F4E5 Buscando dados no BigQuery apenas para as escolas selecionadas...\n")
  
  # Monta a lista de todos os IDs necessários
  todos_os_ids <- c(codinep, ids_concorrentes_proximos)
  
  if(length(todos_os_ids) == 0 || (length(todos_os_ids) == 1 && is.na(todos_os_ids[1]))) {
    stop("Não foi possível gerar uma lista de IDs de escolas para a consulta.")
  }
  
  # Usando glue_sql para construir a query de forma segura.
  query_otimizada <- glue::glue_sql(
    "SELECT
     ano,
     CAST(id_escola AS STRING) AS id_escola,
     CAST(id_municipio AS STRING) AS id_municipio,
     COALESCE(quantidade_matricula_infantil_creche, 0) + 
     COALESCE(quantidade_matricula_infantil_pre_escola, 0) AS mat_infantil,
     COALESCE(quantidade_matricula_fundamental_anos_iniciais, 0) + 
     COALESCE(quantidade_matricula_fundamental_anos_finais, 0) AS mat_fundamental,
     COALESCE(quantidade_matricula_medio, 0) AS mat_medio,
     COALESCE(quantidade_matricula_eja, 0) AS mat_eja,
     COALESCE(quantidade_matricula_profissional, 0) AS mat_profissional
   FROM `basedosdados.br_inep_censo_escolar.escola`
   WHERE CAST(id_escola AS STRING) IN ({todos_os_ids*}) 
   AND ano IN (2022, 2023)",
    .con = con
  )
  
  cat("--- SQL Query (Escolas Selecionadas) Enviada ao BigQuery ---\n")
  print(query_otimizada)
  cat("-----------------------------------------------------------\n")
  
  df_escolas_selecionadas <- dbGetQuery(con, query_otimizada)
  
  # --- 4. Processar e Estruturar os dados ---
  cat("\U0001F4CA Processando e estruturando os resultados...\n")
  
  tryCatch({
    
    # Separa os dados da escola principal e dos concorrentes
    df_escola_principal <- df_escolas_selecionadas %>% dplyr::filter(id_escola == codinep)
    df_concorrentes <- df_escolas_selecionadas %>% dplyr::filter(id_escola != codinep)
    
    # Pega o ID do município a partir dos dados que acabamos de buscar no BigQuery
    id_muni <- if(nrow(df_escola_principal) > 0) df_escola_principal$id_municipio[1] else NA
    if(is.na(id_muni)) stop("Não foi possível obter o ID do município a partir do BigQuery.")
    
    make_bloco_matriculas <- function(df) {
      if (nrow(df) == 0) return(list())
      
      df_wide <- df %>%
        dplyr::group_by(ano) %>%
        dplyr::summarise(across(starts_with("mat_"), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
        tidyr::pivot_wider(names_from = ano, values_from = starts_with("mat_"))
      
      segmentos <- c("mat_infantil", "mat_fundamental", "mat_medio", "mat_eja", "mat_profissional")
      blocos <- list()
      for (seg in segmentos) {
        v1_col <- paste0(seg, "_2022")
        v2_col <- paste0(seg, "_2023")
        
        v1 <- if(v1_col %in% names(df_wide)) df_wide[[v1_col]][1] else 0
        v2 <- if(v2_col %in% names(df_wide)) df_wide[[v2_col]][1] else 0
        
        v1 <- ifelse(is.na(v1), 0, v1)
        v2 <- ifelse(is.na(v2), 0, v2)
        
        var <- ifelse(v1 == 0, NA, round((v2 - v1) / v1 * 100, 1))
        blocos[[seg]] <- list(
          label = paste("Matrículas -", stringr::str_to_title(gsub("mat_", "", seg))),
          valor_ano_1 = v1,
          valor_ano_2 = v2,
          taxa_de_variacao = ifelse(is.na(var), "N/A", paste0(var, "%"))
        )
      }
      blocos
    }
    
    dados_propria <- make_bloco_matriculas(df_escola_principal)
    dados_concorrentes <- make_bloco_matriculas(df_concorrentes)
    
    # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVA CORREÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
    # Consulta para dados de mercado: busca TODAS as escolas do município para ter o total.
    # Usa a tabela `escola` que tem a estrutura de colunas que a função `make_bloco_matriculas` espera.
    cat("\U0001F4C8 Buscando dados de mercado para o município...\n")
    query_mercado <- glue::glue_sql("
      SELECT
        ano,
        COALESCE(quantidade_matricula_infantil_creche, 0) + 
        COALESCE(quantidade_matricula_infantil_pre_escola, 0) AS mat_infantil,
        COALESCE(quantidade_matricula_fundamental_anos_iniciais, 0) + 
        COALESCE(quantidade_matricula_fundamental_anos_finais, 0) AS mat_fundamental,
        COALESCE(quantidade_matricula_medio, 0) AS mat_medio,
        COALESCE(quantidade_matricula_eja, 0) AS mat_eja,
        COALESCE(quantidade_matricula_profissional, 0) AS mat_profissional
      FROM `basedosdados.br_inep_censo_escolar.escola`
      WHERE CAST(id_municipio AS STRING) = {id_muni} AND ano IN (2022, 2023)
    ", .con = con)
    # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA CORREÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    cat("--- SQL Query (Mercado) Enviada ao BigQuery ---\n")
    print(query_mercado)
    cat("----------------------------------------------\n")
    
    dados_mercado <- dbGetQuery(con, query_mercado)
    dados_mercado_municipio <- make_bloco_matriculas(dados_mercado)
    
    # --- 5. Salvar o resultado final ---
    resultado <- list(
      id_escola = codinep,
      nome_escola = nome_escola,
      id_municipio = id_muni,
      nome_municipio = nome_muni,
      latitude = lat,
      longitude = lon,
      lista_concorrentes = concorrentes_locais %>% 
        dplyr::left_join(nomes_all, by = c("code_school" = "CO_ENTIDADE")) %>% 
        dplyr::select(id_escola = code_school, nome_escola = NO_ENTIDADE, latitude, longitude, dist_metros),
      dados_propria_escola = dados_propria,
      dados_concorrentes_proximos = dados_concorrentes,
      dados_mercado_municipio = dados_mercado_municipio
    )
    
    dir.create("data/escolas", recursive = TRUE, showWarnings = FALSE)
    path_out <- file.path("data", "escolas", paste0(codinep, ".rds"))
    saveRDS(resultado, path_out)
    cat("\U00002705 Dados processados e salvos com sucesso em:", path_out, "\n")
    return(TRUE)
    
  }, error = function(e) {
    # Se um erro ocorrer em qualquer lugar dentro do bloco tryCatch, ele será capturado aqui.
    cat("--- ERRO DURANTE O PROCESSAMENTO DOS DADOS ---\n")
    print(e)
    cat("--------------------------------------------\n")
    stop(e) # Re-lança o erro para que a mensagem apareça no app
  })
}