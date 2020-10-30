# PACOTES NECESSARIOS

library(tidyverse)


# IMPORTACAO

theoffice_dados <- readxl::read_excel("data/the_office_series.xls")
theoffice_personagens <- readxl::read_excel("data/the_office_series.xls",
                                sheet = "elenco_personagens")

# ARRUMACAO

theoffice_dados <- theoffice_dados %>%
  janitor::clean_names()

theoffice_personagens <- theoffice_personagens %>%
  janitor::clean_names()


readr::write_rds(theoffice_dados, "data/theoffice_dados.rds")

readr::write_rds(theoffice_personagens, "data/theoffice_personagens.rds")


# LEITURA

theoffice_dados <- readr::read_rds("data/theoffice_dados.rds")

theoffice_personagens <- readr::read_rds("data/theoffice_personagens.rds")


# CARREGAMENTO DA FONTE UTILIZADA NOS GRAFICOS

extrafont::loadfonts(device = "win")
