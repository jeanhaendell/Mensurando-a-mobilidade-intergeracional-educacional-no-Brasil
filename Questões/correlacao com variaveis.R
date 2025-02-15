library(ipeadatar)


###### pib
pib <- ipeadata("PIB_IBGE_5938_37") 
pib_2022 <- subset(pib, date == "2021-01-01") %>% 
  filter(uname == "Municipality")

municipios_com_pib <- resultados_municipios_wide %>% 
  left_join(pib_2022, join_by(municipio == tcode))

grafico_pib <- municipios_com_pib %>% 
  ggplot(aes(x = log(value), y = coef)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(ylim = c(-2, 2)) +
  geom_jitter(width = 0, height = 0.1, alpha = 0.5) +
  labs(
    title = expression("Correlação entre " * beta * " e o PIB municipal"),
    subtitle = "PIB Municipal - preços de mercado (preços de 2010)",
    x = "Log do PIB",
    y = "Estimativa",
    caption = "Ipeadata"
  ) +
  theme_minimal()

grafico_pib

ggsave("grafico_pib.png")

#################
escolaridade <- ipeadata("PNADCA_NMAE25UF_2539") 

escolaridade_2019 <- subset(escolaridade, date == "2019-01-01") %>% 
  filter(uname == "Municipality")

municipios_com_escolaridade <- resultados_municipios_wide %>% 
  left_join(escolaridade_2019, join_by(municipio == tcode)) %>% 
  filter(!is.na(value))

grafico_escolaridade <- municipios_com_escolaridade %>% 
  ggplot(aes(x = value, y = coef)) +
  geom_point() +
  geom_smooth() +
  coord_cartesian(ylim = c(0, 1)) + 
  labs(
    title = expression("Correlação entre " * beta * " e a escolaridade* para as capitais brasileiras"),
    subtitle = "*Número médio de anos de estudo das pessoas com 25 a 39 anos para o ano de 2019",
    x = "Anos de Estudo",
    y = "Estimativa",
    caption = "Ipeadata"
  ) +
  theme_minimal()

grafico_escolaridade

ggsave("grafico_escolaridade.png")

#################################################################
pobreza <- search_series("pobreza")
pobreza_2010 <- subset(pobreza, date == "2010-01-01") %>% 
  filter(uname == "Municipality")

municipios_com_pobreza <- resultados_municipios_wide %>% 
  left_join(pobreza_2010, join_by(municipio == tcode)) %>% 
  filter(!is.na(value))

grafico_pobreza <- municipios_com_pobreza %>% 
  ggplot(aes(x = value, y = coef)) +
  geom_point() +
  geom_smooth() + 
  geom_jitter(width = 0, height = 0.1, alpha = 0.5) +
  coord_cartesian(ylim = c(-2, 2)) + 
  labs(
    title = expression("Correlação entre " * beta * " e a pobreza* para os municípios brasileiros"),
    subtitle = "* Proporção de vulneráveis à pobreza para o ano de 2010",
    x = "Proporção de vulneráveis à pobreza",
    y = "Estimativa",
    caption = "Ipeadata",
  ) +
  theme_minimal()
  
grafico_pobreza

ggsave("grafico_pobreza.png")
