# LISTA DE DIRETORES

diretor <- theoffice_dados %>%
  separate(col = direcao,
           into = c("diretor1", "diretor2"),
           sep = ", ") %>%
  select(episodio, titulo, starts_with("diretor")) %>%
  pivot_longer(cols = starts_with("diretor"),
               names_to = "diretor_num",
               values_to = "diretor_nome")

diretor_count <- diretores %>%
  count(diretor_nome) %>%
  na.omit() %>%
  rename(diretor = diretor_nome, qnt_episodios = n)


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
