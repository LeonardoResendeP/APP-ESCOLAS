# utils/global_reactives.R

# Variáveis reativas globais para compartilhamento entre módulos
.global_reactives <- reactiveValues(
  escola_nome = NULL,
  escola_carregada = FALSE
)

# Funções para acesso global
set_escola_nome <- function(nome) {
  .global_reactives$escola_nome <- nome
  .global_reactives$escola_carregada <- TRUE
}

get_escola_nome <- function() {
  .global_reactives$escola_nome
}

is_escola_carregada <- function() {
  .global_reactives$escola_carregada
}