# Pré-processamento para salvar base leve
escolas_raw <- read.csv("data/microdados_ed_basica_2024.csv", sep = ";")

escolas_privadas <- escolas_raw %>%
  #filter(TP_DEPENDENCIA == 4) %>%  # Dependência administrativa: 4 = privada
  distinct(CO_ENTIDADE, NO_ENTIDADE) %>%
  mutate(CO_ENTIDADE = as.character(CO_ENTIDADE)) %>%
  arrange(NO_ENTIDADE)

# Salvar
saveRDS(escolas_privadas, "data/escolas_privadas_nomelista.rds")




library(geobr)
library(dplyr)

escolas_geo2 <- read_schools(year = 2023)

# Certifique-se que o codinep está como caractere:
codinep <- as.character("33193088")  # substitua pelo codinep real da escola cadastrada

# Verifique se ela está presente
escolas_geo<-escolas_geo %>%
  filter(admin_category == "Privada" & service_restriction == "ESCOLA EM FUNCIONAMENTO E SEM RESTRIÇÃO DE ATENDIMENTO")

# 2. Extrai coordenadas X (longitude), Y (latitude)
coords <- sf::st_coordinates(escolas_geo)

# 3. Adiciona colunas e flag empty_geo
escolas_geo <- escolas_geo %>%
  mutate(
    code_school = as.character(code_school),
    longitude = coords[, "X"],
    latitude = coords[, "Y"],
    empty_geo = is.na(longitude) | is.na(latitude)
  ) %>%
  select(code_school, latitude, longitude, name_muni, empty_geo)

# 4. Salva para uso no app
saveRDS(escolas_geo, file = "escolas_geo_com_empty_flag.rds")

table(escolas_geo$empty_geo)



print(teste)
