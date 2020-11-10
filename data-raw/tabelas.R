# LISTA DE DIRETORES

diretor <- theoffice_dados %>%
  separate(col = direcao,
           into = c("diretor1", "diretor2"),
           sep = ", ") %>%
  pivot_longer(cols = starts_with("diretor"),
               names_to = "diretor_num",
               values_to = "diretor_nome") %>%
  rename(direcao = diretor_nome)

diretor_count <- diretor %>%
  count(direcao) %>%
  na.omit() %>%
  rename(qnt_episodios = n)


tab_diretor_media_imdb <- diretor %>%
  group_by(direcao) %>%
  summarise(media_imdb_diretor = mean(estrelas_imdb, na.rm = TRUE))


# LISTA DE ROTEIRISTAS

roteirista <- theoffice_dados %>%
  separate(col = roteiro,
           into = c("roteirista1", "roteirista2", "roteirista3"),
           sep = ", ") %>%
  select(episodio, titulo, starts_with("roteirista")) %>%
  pivot_longer(cols = starts_with("roteirista"),
               names_to = "roteirista_num",
               values_to = "roteirista_nome")

roteirista_count <- roteirista %>%
  count(roteirista_nome) %>%
  na.omit() %>%
  rename(roteirista = roteirista_nome, qnt_episodios = n) %>%
  arrange(desc(qnt_episodios))


# LISTA DE ELENCO

elenco <-  theoffice_dados %>%
  separate(col = elenco,
           into = c("elenco_01", "elenco_02", "elenco_03", "elenco_04", "elenco_05", "elenco_06", "elenco_07",
                    "elenco_08", "elenco_09", "elenco_10", "elenco_11", "elenco_12", "elenco_13", "elenco_14",
                    "elenco_15", "elenco_16", "elenco_17", "elenco_18", "elenco_19", "elenco_20", "elenco_21",
                    "elenco_22", "elenco_23", "elenco_24", "elenco_25", "elenco_26", "elenco_27", "elenco_28",
                    "elenco_29", "elenco_30", "elenco_31", "elenco_32", "elenco_33", "elenco_34", "elenco_35",
                    "elenco_36", "elenco_37", "elenco_38", "elenco_39", "elenco_40", "elenco_41", "elenco_42",
                    "elenco_43", "elenco_44", "elenco_45", "elenco_46", "elenco_47", "elenco_48", "elenco_49",
                    "elenco_50", "elenco_51", "elenco_52", "elenco_53", "elenco_54", "elenco_55", "elenco_56",
                    "elenco_57", "elenco_58", "elenco_59", "elenco_60", "elenco_61", "elenco_62", "elenco_63",
                    "elenco_64", "elenco_65", "elenco_66", "elenco_67", "elenco_68", "elenco_69", "elenco_70",
                    "elenco_71", "elenco_72", "elenco_73", "elenco_74", "elenco_75", "elenco_76", "elenco_77",
                    "elenco_78"),
           sep = ", ") %>%
  select(episodio, titulo, starts_with("elenco")) %>%
  pivot_longer(cols = starts_with("elenco"),
               names_to = "elenco_num",
               values_to = "elenco_nome")

elenco_count <- elenco %>%
  count(elenco_nome) %>%
  na.omit() %>%
  arrange(desc(n))


elenco_total <- elenco %>%
  count(elenco_nome) %>%
  na.omit() %>%
  rename(ator = elenco_nome, qnt_episodios = n) %>%
  nrow()












