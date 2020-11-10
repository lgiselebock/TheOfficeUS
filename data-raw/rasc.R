# AUDIENCIA

audiencia_coluna <- theoffice_dados %>%
  mutate(temporada = as.factor(temporada)) %>%
  ggplot(aes(x = episodio, width = 0.5,
             y = qntd_telespectadores_eua_milhoes, fill = temporada)) +
  geom_col() +
  labs(x = "Episodios", y = "Audiencia (em milhoes) nos EUA", fill = "Temporada",
       caption = "Figura 1: texto texto texto") +
  scale_y_continuous(limits = c(0, 23), breaks = seq(0, 25, 5)) +
  scale_x_continuous(breaks = seq(0, 188, 40)) +
  theoffice_theme1()

media_audiencia_por_temporada <- theoffice_dados %>%
  group_by(temporada) %>%
  summarise(mean_aud_season = mean(qntd_telespectadores_eua_milhoes)) %>%
  ggplot() +
  geom_col(aes(x = temporada, y = mean_aud_season)) +
  labs(x = "Temporada", y = "Media de audiencia (em milhoes) EUA") +
  coord_cartesian(ylim = c(4, 9)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
  scale_y_continuous(breaks = seq(4, 9, .5)) +
  theoffice_theme1()

sum(theoffice_dados$qntd_telespectadores_eua_milhoes)/188


top10_audiencia <- theoffice_dados %>%
  arrange(desc(qntd_telespectadores_eua_milhoes)) %>%
  slice_head(n = 10) %>%
  select(titulo, qntd_telespectadores_eua_milhoes) %>%
  ggplot() +
  geom_col(aes(x = qntd_telespectadores_eua_milhoes,
               y = reorder(titulo, +qntd_telespectadores_eua_milhoes))) +
  labs(x = "Audiencia (em milhoes) nos EUA", y = "Episodio") +
  coord_cartesian(xlim = c(8, 23)) +
  scale_x_continuous(breaks = seq(8, 23, 3)) +
  theoffice_theme2()


aud_med_tab <- theoffice_dados %>%
  group_by(temporada) %>%
  summarise(media_aud_temp = mean(qntd_telespectadores_eua_milhoes)) %>%
  rename("Temporada" = temporada, "Audiência média (em milhões)" = media_aud_temp)

# DURACAO

  duracao_temporada <- theoffice_dados %>%
    group_by(temporada) %>%
    summarise(duracao_min_temporada = sum(duracao),
              duraca_media_temp = (sum(duracao)/9)) %>%
    ggplot() +
    geom_col(aes(x = temporada, y = duracao_min_temporada)) +
    geom_line(aes(x = temporada, y = duracao_media_temporada)) +
    labs(x = "Temporada", y = "Duracao (em minutos)") +
    scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
    scale_y_continuous(limits = c(0, 800), breaks = seq(0, 750, 100)) +
    theoffice_theme1()

duracao_media_temporada <- theoffice_dados %>%
  summarise(duracao_media_temporada = sum(duracao)/9)

episodio_por_temporada <- theoffice_dados %>%
  ggplot() +
  geom_bar(aes(x = temporada), color = "black", fill = RColorBrewer::brewer.pal(9, "Paired")) +
  labs(x = "Temporada", y = "Episodios") +
  scale_y_continuous(limits = c(0, 28), breaks = seq(0, 30, 5)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
  theoffice_theme1()

glimpse(theoffice_dados)

188/9


# ESTRELAS (IMDB)

media_imdb_temporada <- theoffice_dados %>%
  group_by(temporada) %>%
  summarise(mean_imdb_season = mean(estrelas_imdb)) %>%
  ggplot() +
  geom_col(aes(x = temporada, y = mean_imdb_season)) +
  labs(x = "Temporada", y = "Estrelas (IMDb)") +
  coord_cartesian(ylim = c(7, 8.8)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
  scale_y_continuous(breaks = seq(7, 8.8, .5)) +
  theoffice_theme1()


estrelas_media_imdb_tab <- theoffice_dados %>%
  group_by(temporada) %>%
  summarise(media_stars_imdb_season = mean(estrelas_imdb)) %>%
  rename("remporada" = temporada, "Média de estrelas IMDb" = media_stars_imdb_season)



estrelas_imdb_hist <- theoffice_dados %>%
  ggplot() +
  geom_histogram(aes(x = estrelas_imdb), bins = 15, color = "black") +
  labs(x = "Estrelas (IMDB)", y = NULL) +
  scale_x_continuous(breaks = seq(6, 10, .5)) +
  theoffice_theme1()


top10_estrelas_imdb <- theoffice_dados %>%
  arrange(desc(estrelas_imdb)) %>%
  slice_head(n = 10) %>%
  select(titulo, estrelas_imdb) %>%
  ggplot() +
  geom_col(aes(x = estrelas_imdb, y = reorder(titulo, +estrelas_imdb))) +
  labs(x = "Estrelas (IMDb)", y = "Episodio") +
  coord_cartesian(xlim = c(9, 9.9)) +
  scale_x_continuous(breaks = seq(9, 10, .2)) +
  theoffice_theme2()

imdb_baixo <- theoffice_dados %>%
  filter(estrelas_imdb < 7.5)

imdb_alto <- theoffice_dados %>%
  filter(estrelas_imdb > 8.75)

theoffice_dados %>%
  filter(estrelas_imdb > 8.0, estrelas_imdb < 8.25) %>%
  View()

sum(theoffice_dados$estrelas_imdb)/188



epi_elenco <- elenco %>%
  na.omit() %>%
  count(titulo) %>%
  arrange(desc(n)) %>%
  slice_head(n = 10) %>%
  ggplot() +
  geom_col(aes(x = n, y = reorder(titulo, +n)))



elenco_personagem <- right_join(elenco_count, theoffice_personagens, by = "elenco_nome")

elenco_personagem <- inner_join(elenco_count, theoffice_personagens, by = "elenco_nome")


tab_elenco_personagem  <- elenco_personagem %>%
  select(elenco_nome, personagem_ns, n) %>%
  DT::datatable(colnames = c("Ator/Atriz", "Personagem(ns)", "Quantidade de episódios"),
                caption = "Tabela 2: Quantidade de episódios em que cada ator aparece")


















