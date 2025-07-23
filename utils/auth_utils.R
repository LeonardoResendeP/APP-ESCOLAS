library(jsonlite)
read_users <- function(path = "config/users.json") {
  jsonlite::fromJSON(path) %>% as_tibble()
}