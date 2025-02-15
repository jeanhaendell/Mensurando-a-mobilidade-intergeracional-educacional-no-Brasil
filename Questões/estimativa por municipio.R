###  # Carregar pacotes necessários
library(tidyverse)
library(broom)
library(gt)
library(sf)


# Criar função para rodar regressões por municipio
rodar_regressao_municipio <- function(df, municipio) {
  modelo <- lm(rank_filho_municipio ~ rank_pais_media, data = df)
  resultados <- tidy(modelo) %>%
    select(term, estimate, std.error, p.value) %>%
    mutate(municipio = municipio)
  return(resultados)
}

dados_filtrados <- dados %>%
  group_by(co_municipio_esc) %>%
  filter(n() >= 5) %>%  # Filtra grupos com menos de 10 observações
  ungroup()
  

# Rodar regressões para cada municipio e armazenar resultados
resultados_municipios <- dados_filtrados %>%
  group_split(co_municipio_esc) %>%
  map_dfr(~ rodar_regressao_municipio(.x, unique(.x$co_municipio_esc)))

dados_filtrados2 <- dados_filtrados %>% 
  select(co_municipio_esc, no_municipio_esc)

resultados_municipios <- resultados_municipios %>% 
  left_join(dados_filtrados2, join_by(municipio == co_municipio_esc), relationship = "many-to-many") %>%
  group_by(municipio) %>% 
  filter(term %in% c("(Intercept)", "rank_pais_media")) %>% 
  distinct(municipio, term, .keep_all = TRUE)

# Formatar resultados
tabela_municipios <- resultados_municipios %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = case_when(
      p.value < 0.01 ~ "<0.01 ***",
      p.value < 0.05 ~ "<0.05 **",
      p.value < 0.1 ~ "<0.10 *",
      TRUE ~ as.character(round(p.value, 3))
    )
  ) %>%
  rename(
    Município = no_municipio_esc,
    Termo = term,
    Coeficiente = estimate,
    `Erro Padrão` = std.error,
    `Valor-p` = p.value
  ) %>%
  gt() %>%
  tab_header(
    title = "Estimativas do Modelo Rank-Rank por municipio",
    subtitle = "Regressão da Nota do Filho no ENEM sobre a Escolaridade dos Pais"
  ) %>%
  cols_label(
    municipio = "municipio",
    Coeficiente = "β (Efeito do Ranking Parental)",
    `Erro Padrão` = "Erro Padrão",
    `Valor-p` = "Significância",
    `Município` = "Município"
  ) %>%
  fmt_number(columns = c(Coeficiente, `Erro Padrão`), decimals = 3) %>%
  tab_options(table.font.size = "medium")

#passando o código do município para character
resultados_municipios <- resultados_municipios %>% 
  mutate(CD_MUN = as.character(municipio)) %>% 
  select(-municipio)
  
  

shapefile_mun <- st_read("BR_Municipios_2023/BR_Municipios_2023.shp")

resultados_municipios <- resultados_municipios %>% 
  left_join(shapefile_mun, join_by( CD_MUN == CD_MUN))

resultados_municipios_wide <- resultados_municipios %>%
  filter(term %in% c("(Intercept)", "rank_pais_media")) %>%
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = c(estimate, std.error),
    names_sep = "_"
  ) %>%
  rename(
    intercept = `estimate_(Intercept)`,
    se_intercept = `std.error_(Intercept)`,
    coef = estimate_rank_pais_media,
    se_coef = std.error_rank_pais_media
  )
resultados_municipios_wide <- resultados_municipios %>%
  filter(term %in% c("(Intercept)", "rank_pais_media")) %>%
  ungroup() %>%
  pivot_wider(
    names_from = term,
    values_from = c(estimate, std.error),
    names_sep = "_"
  ) %>%
  rename(
    intercept   = `estimate_(Intercept)`,
    se_intercept = `std.error_(Intercept)`,
    coef        = estimate_rank_pais_media,
    se_coef     = std.error_rank_pais_media
  ) %>%
  group_by(municipio) %>%
  summarise(
    # Usa coalesce para pegar o primeiro valor não-NA de cada coluna
    intercept    = coalesce(first(intercept), last(intercept)),
    se_intercept = coalesce(first(se_intercept), last(se_intercept)),
    coef         = coalesce(first(coef), last(coef)),
    se_coef      = coalesce(first(se_coef), last(se_coef)),
    NM_MUN       = first(NM_MUN),
    no_municipio_esc = first(no_municipio_esc),
    CD_MUN       = first(CD_MUN),
    geometry     = first(geometry)
  ) %>%
  ungroup() %>% 
  filter(!is.na(coef))

resultados_municipios_wide_sf <- st_as_sf(resultados_municipios_wide)

library(leaflet)

# Use uma paleta divergente (por exemplo, "RdYlBu") e inverta se necessário
pal <- colorNumeric(
  palette = "RdYlBu",
  domain = resultados_municipios_wide_sf$coef,
  reverse = TRUE
)

leaflet(resultados_municipios_wide_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(coef),
    weight = 1,
    opacity = 1,
    color = "black",
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", NM_MUN, "</b><br>",
      "Intercepto: ", round(intercept, 3), "<br>",
      "Coeficiente: ", round(coef, 3), "<br>",
      "Erro Padrão: ", round(se_coef, 3)
    )
  ) %>% 
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~coef,
    title = "Coeficiente",
    opacity = 1
  )

saveWidget(mapa, "Relatório/meu_mapa.html", selfcontained = TRUE)
