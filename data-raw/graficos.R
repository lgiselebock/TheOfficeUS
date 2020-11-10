# AUDIENCIA DE CADA EPISODIO DA SERIE

audiencia_coluna <- theoffice_dados %>%
  mutate(temporada = as.factor(temporada)) %>%
  ggplot(aes(x = episodio, width = 0.5,
             y = qntd_telespectadores_eua_milhoes, fill = temporada)) +
  geom_col() +
  labs(x = "Episodios", y = "Audiencia (em milhoes) nos EUA", fill = "Temporada") +
  scale_y_continuous(limits = c(0, 23), breaks = seq(0, 25, 5)) +
  scale_x_continuous(breaks = seq(0, 188, 40)) +
  scale_fill_brewer("qual", "Paired") +
  theoffice_theme1()


audiencia_linha <- theoffice_dados %>%
  ggplot() +
  geom_line(aes(x = episodio, y = qntd_telespectadores_eua_milhoes)) +
  labs(x = "Episodios", y = "Audiencia (em milhoes) nos EUA") +
  scale_y_continuous(limits = c(0, 23), breaks = seq(0, 25, 5)) +
  theoffice_theme1()


# EPISODIOS POR TEMPORADA

episodio_por_temporada <- theoffice_dados %>%
  ggplot() +
  geom_bar(aes(x = temporada),
           color = "black", fill = cores) +
  labs(x = "Temporada", y = "Episodios") +
  scale_y_continuous(limits = c(0, 28), breaks = seq(0, 30, 5)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
  theoffice_theme1()


# TEMPO DE EXIBICAO (EM MINUTOS) DE CADA TEMPORADA

duracao_temporada <- theoffice_dados %>%
  group_by(temporada) %>%
  summarise(duracao_min_temporada = sum(duracao)) %>%
  ggplot() +
  geom_col(aes(x = temporada, y = duracao_min_temporada)) +
  labs(x = "Temporada", y = "Duracao (em minutos)") +
  scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 750, 100)) +
  theoffice_theme1()


# MEDIA DE ESTRELAS (IMDB) RECEBIDAS POR TEMPORADA

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


# DISTRIBUICAO DE TODAS AS ESTRELAS (IMDB) - HISTOGRAMA

estrelas_imdb_hist <- theoffice_dados %>%
  ggplot() +
  geom_histogram(aes(x = estrelas_imdb), bins = 15, color = "black") +
  labs(x = "Estrelas (IMDB)", y = NULL) +
  scale_x_continuous(breaks = seq(6, 10, .5)) +
  theoffice_theme1()


# MEDIA DE TELESPECTADORES (EM MILHOES) POR TEMPORADA

media_audiencia_por_temporada <- theoffice_dados %>%
  group_by(temporada) %>%
  summarise(mean_aud_season = mean(qntd_telespectadores_eua_milhoes)) %>%
  ggplot() +
  geom_col(aes(x = temporada, y = mean_aud_season)) +
  labs(x = "Temporada", y = "Media de audiencia (em milhoes) EUA", ) +
  coord_cartesian(ylim = c(4, 9)) +
  scale_x_continuous(limits = c(0, 10), breaks = seq(1, 9, 1)) +
  scale_y_continuous(breaks = seq(4, 9, .5)) +
  theoffice_theme1()


# TOP 10 EPISODIOS SEGUNDO AS ESTRELAS (IMDB)

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


# TOP 10 EPISODIOS SEGUNDO A AUDIENCIA (EM MILHOES) NOS EUA

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


# DIRETORES QUE TRABALHARAM NA SERIE

diretores <- diretor_count %>%
  ggplot() +
  geom_col(aes(x = qnt_episodios, y = diretor)) +
  labs(x = "Quantidade de episodios", y = "Diretores") +
  coord_cartesian(xlim = c(0, 16)) +
  scale_x_continuous(breaks = seq(0, 17, 3)) +
  theoffice_theme2()


# TOP 10 DIRETORES QUE MAIS TRABALHARAM NA SERIE

top10_diretores <- diretor_count %>%
  arrange(desc(qnt_episodios)) %>%
  slice_head(n = 10) %>%
  ggplot() +
  geom_col(aes(x = qnt_episodios, y = reorder(direcao, +qnt_episodios))) +
  labs(x = "Quantidade de episodios", y = "Diretores") +
  coord_cartesian(xlim = c(0, 16)) +
  scale_x_continuous(breaks = seq(0, 15, 3)) +
  theoffice_theme2()



# ROTEIRISTAS QUE TRABALHARAM NA SERIE

roteiristas <- roteirista_count %>%
  ggplot() +
  geom_col(aes(x = qnt_episodios, y = roteirista)) +
  labs(x = "Quantidade de episodios", y = "Roteiristas") +
  coord_cartesian(xlim = c(0, 23)) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  theoffice_theme2()


# TOP 10 ROTEIRISTAS QUE MAIS TRABALHARAM NA SERIE

top10_roteiristas <- roteirista_count %>%
  arrange(desc(qnt_episodios)) %>%
  slice_head(n = 10) %>%
  ggplot() +
  geom_col(aes(x = qnt_episodios, y = reorder(roteirista, +qnt_episodios)),
           color = "black", fill = cores) +
  labs(x = "Quantidade de episodios", y = "Roteiristas") +
  coord_cartesian(xlim = c(9, 23)) +
  scale_x_continuous(breaks = seq(10, 23, 3)) +
  theoffice_theme2()


# TOP 15 ATORES QUE MAIS APARECERAM NA SERIE

top15_elenco <- elenco_count %>%
  arrange(desc(qnt_episodios)) %>%
  slice_head(n = 15) %>%
  ggplot() +
  geom_col(aes(x = qnt_episodios, y = reorder(ator, +qnt_episodios))) +
  labs(x = "Quantidade de episodios", y = "Atores") +
  coord_cartesian(xlim = c(120, 190)) +
  scale_x_continuous(breaks = seq(100, 200, 10)) +
  theoffice_theme2()

elenco_tabela <- elenco %>%
  count(elenco_nome) %>%
  arrange(desc(n)) %>%
  slice_head(n = 30) %>%
  na.omit()


teste <- roteirista %>%
  count(roteirista_nome) %>%
  na.omit() %>%
  nrow()



