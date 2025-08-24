# Carrega as bibliotecas necessárias para o banco de dados
library(RSQLite)
library(DBI)
library(tibble)
library(dplyr)
library(scrypt)

# Define o caminho do arquivo do banco de dados
DB_PATH <- "config/database.sqlite"

# Função para conectar ao banco de dados
db_connect <- function() {
  # Garante que a pasta 'config' exista
  dir.create(dirname(DB_PATH), showWarnings = FALSE, recursive = TRUE)
  
  dbConnect(RSQLite::SQLite(), DB_PATH)
}

# Função para inicializar o banco de dados e as tabelas
db_init <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con)) # Garante que a conexão seja fechada
  
  # Cria a tabela 'users' se ela não existir
  if (!dbExistsTable(con, "users")) {
    dbExecute(con, "
      CREATE TABLE users (
        user TEXT PRIMARY KEY,
        pass TEXT NOT NULL,
        role TEXT NOT NULL,
        codinep TEXT
      )
    ")
    
    # Insere o usuário admin padrão com senha 'admin'
    admin_user <- tibble(
      user = "admin",
      pass = scrypt::hashPassword("admin"),
      role = "admin",
      codinep = NA_character_
    )
    dbWriteTable(con, "users", admin_user, append = TRUE)
    cat("Tabela 'users' e usuário 'admin' criados.\n")
  }
  
  # Cria a tabela 'escolas' se ela não existir
  if (!dbExistsTable(con, "escolas")) {
    dbExecute(con, "
      CREATE TABLE escolas (
        codinep TEXT PRIMARY KEY,
        nome TEXT NOT NULL,
        user TEXT NOT NULL,
        pass TEXT NOT NULL
      )
    ")
    cat("Tabela 'escolas' criada.\n")
  }
  
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVA TABELA PARA CONCORRENTES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Cria a tabela 'escola_concorrentes' para armazenar as seleções
  if (!dbExistsTable(con, "escola_concorrentes")) {
    dbExecute(con, "
      CREATE TABLE escola_concorrentes (
        codinep_principal TEXT NOT NULL,
        codinep_concorrente TEXT NOT NULL,
        PRIMARY KEY (codinep_principal, codinep_concorrente)
      )
    ")
    cat("Tabela 'escola_concorrentes' criada.\n")
  }
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA NOVA TABELA >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
}

# --- Funções para interagir com o banco de dados ---

# Busca todos os usuários
get_users_from_db <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbReadTable(con, "users") %>% as_tibble()
}

# Busca todas as escolas
get_escolas_from_db <- function() {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  dbReadTable(con, "escolas") %>% as_tibble()
}

# Salva uma nova escola e seu usuário
save_escola_to_db <- function(codinep, nome, user, pass) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  
  # Criptografa a senha
  hashed_pass <- scrypt::hashPassword(pass)
  
  # Cria os dataframes para inserção
  nova_escola <- tibble(codinep = codinep, nome = nome, user = user, pass = hashed_pass)
  novo_user <- tibble(user = user, pass = hashed_pass, role = "free", codinep = codinep)
  
  # Escreve nas tabelas
  dbWriteTable(con, "escolas", nova_escola, append = TRUE)
  dbWriteTable(con, "users", novo_user, append = TRUE)
}

# Exclui uma escola e seu usuário
delete_escola_from_db <- function(codinep_to_delete) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  
  # Exclui da tabela 'escolas'
  dbExecute(con, "DELETE FROM escolas WHERE codinep = ?", params = list(codinep_to_delete))
  
  # Exclui da tabela 'users'
  dbExecute(con, "DELETE FROM users WHERE codinep = ?", params = list(codinep_to_delete))
  
  # <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ATUALIZAÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  # Também exclui as preferências de concorrentes salvas para essa escola
  dbExecute(con, "DELETE FROM escola_concorrentes WHERE codinep_principal = ?", params = list(codinep_to_delete))
  # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA ATUALIZAÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # Remove o arquivo .rds correspondente
  rds_file_path <- file.path("data/escolas", paste0(codinep_to_delete, ".rds"))
  if (file.exists(rds_file_path)) {
    file.remove(rds_file_path)
    cat("Arquivo .rds removido:", rds_file_path, "\n")
  }
}


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVAS FUNÇÕES PARA CONCORRENTES <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Busca os concorrentes salvos para uma escola
get_concorrentes_selecionados <- function(codinep_principal) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  
  query <- "SELECT codinep_concorrente FROM escola_concorrentes WHERE codinep_principal = ?"
  result <- dbGetQuery(con, query, params = list(codinep_principal))
  
  # Retorna um vetor de códigos ou NULL se não houver nenhum
  if (nrow(result) > 0) {
    return(result$codinep_concorrente)
  } else {
    return(NULL)
  }
}

# Salva a seleção de concorrentes de uma escola
save_concorrentes_selecionados <- function(codinep_principal, codineps_concorrentes) {
  con <- db_connect()
  on.exit(dbDisconnect(con))
  
  # 1. Deleta as seleções antigas para esta escola
  dbExecute(con, "DELETE FROM escola_concorrentes WHERE codinep_principal = ?", params = list(codinep_principal))
  
  # 2. Insere as novas seleções, se houver alguma
  if (length(codineps_concorrentes) > 0) {
    df_to_insert <- tibble(
      codinep_principal = codinep_principal,
      codinep_concorrente = codineps_concorrentes
    )
    dbWriteTable(con, "escola_concorrentes", df_to_insert, append = TRUE)
  }
}

# Função para buscar nome da escola pelo código INEP
get_school_name_from_db <- function(codinep) {
  tryCatch({
    conn <- dbConnect(RSQLite::SQLite(), "database.sqlite")
    
    query <- "SELECT nome_escola FROM escolas WHERE codinep = ?"
    result <- dbGetQuery(conn, query, params = list(codinep))
    
    dbDisconnect(conn)
    
    if (nrow(result) > 0) {
      return(result$nome_escola[1])
    } else {
      return(NULL)
    }
  }, error = function(e) {
    warning("Erro ao buscar nome da escola: ", e$message)
    return(NULL)
  })
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DAS NOVAS FUNÇÕES >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

