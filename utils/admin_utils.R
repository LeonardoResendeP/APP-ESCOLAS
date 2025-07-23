library(jsonlite)
library(tibble)
library(dplyr)

# Função segura para leitura da base de escolas
get_escolas <- function(path = "config/escolas.json") {
  if (!file.exists(path)) {
    return(tibble(codinep = character(), nome = character(), user = character(), pass = character()))
  }
  
  tryCatch({
    fromJSON(path) %>% as_tibble()
  }, error = function(e) {
    warning("Erro ao ler escolas.json. Resetando como tabela vazia.")
    tibble(codinep = character(), nome = character(), user = character(), pass = character())
  })
}

# Função segura para leitura da base de usuários
get_users <- function(path = "config/users.json") {
  if (!file.exists(path)) {
    return(tibble(user = character(), pass = character(), role = character(), codinep = character()))
  }
  
  tryCatch({
    fromJSON(path) %>% as_tibble()
  }, error = function(e) {
    warning("Erro ao ler users.json. Resetando como vazio.")
    tibble(user = character(), pass = character(), role = character(), codinep = character())
  })
}

# Salvar uma nova escola + usuário vinculado
save_escola <- function(codinep, nome, user, pass,
                        escolas_path = "config/escolas.json",
                        users_path = "config/users.json") {
  # Validação
  stopifnot(nzchar(codinep), nzchar(user), nzchar(pass))
  
  escolas <- get_escolas(escolas_path)
  users <- get_users(users_path)
  
  if (codinep %in% escolas$codinep) stop("⚠️ Escola com este código INEP já existe.")
  if (user %in% users$user) stop("⚠️ Nome de usuário já está em uso.")
  
  # Append e salvar escolas
  nova_escola <- tibble(codinep = codinep, nome = nome, user = user, pass = pass)
  escolas_final <- bind_rows(escolas, nova_escola)
  write_json(escolas_final, escolas_path, pretty = TRUE, auto_unbox = TRUE)
  
  # Append e salvar usuários
  novo_user <- tibble(user = user, pass = pass, role = "free", codinep = codinep)
  users_final <- bind_rows(users, novo_user)
  write_json(users_final, users_path, pretty = TRUE, auto_unbox = TRUE)
}

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< NOVA FUNÇÃO <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
# Excluir uma escola cadastrada
delete_escola <- function(codinep_to_delete,
                          escolas_path = "config/escolas.json",
                          users_path = "config/users.json",
                          rds_folder_path = "data/escolas") {
  
  # Validação
  stopifnot(nzchar(codinep_to_delete))
  
  # Carrega os dados atuais
  escolas <- get_escolas(escolas_path)
  users <- get_users(users_path)
  
  # Remove a escola da tabela de escolas
  escolas_final <- escolas %>% 
    filter(codinep != codinep_to_delete)
  
  # Remove o usuário associado
  users_final <- users %>% 
    filter(codinep != codinep_to_delete)
  
  # Salva os arquivos JSON atualizados
  write_json(escolas_final, escolas_path, pretty = TRUE, auto_unbox = TRUE)
  write_json(users_final, users_path, pretty = TRUE, auto_unbox = TRUE)
  
  # Remove o arquivo .rds correspondente
  rds_file_path <- file.path(rds_folder_path, paste0(codinep_to_delete, ".rds"))
  if (file.exists(rds_file_path)) {
    file.remove(rds_file_path)
    cat("Arquivo .rds removido:", rds_file_path, "\n")
  }
  
  return(TRUE)
}
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> FIM DA NOVA FUNÇÃO >>>>>>>>>>>>>>>>>>>>>>>>>>>>>