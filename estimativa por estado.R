# 📌 Carregar pacotes necessários
library(tidyverse)
library(broom)
library(ggplot2)

# 📌 Criar função para rodar regressões por estado
rodar_regressao_estado <- function(df, estado) {
  modelo <- lm(rank_filho_estado ~ rank_pais_media, data = df)
  resultados <- tidy(modelo) %>%
    select(term, estimate, std.error, p.value) %>%
    mutate(Estado = estado)
  return(resultados)
}

# 📌 Rodar regressões para cada estado e armazenar resultados
resultados_estados <- dados %>%
  group_split(sg_uf_prova) %>%
  map_dfr(~ rodar_regressao_estado(.x, unique(.x$sg_uf_prova)))

# 📌 Filtrar apenas o coeficiente da variável independente
resultados_estados <- resultados_estados %>%
  filter(term == "rank_pais_media") %>%
  mutate(Estado = factor(Estado, levels = Estado[order(estimate)]))  # Ordenar os estados pelo coeficiente

# 📌 Criar o gráfico de barras horizontais
grafico_estados <- resultados_estados %>%
  ggplot(aes(x = estimate, y = Estado, fill = estimate)) +
  geom_col() +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(
    title = "Efeito do Ranking Parental por Estado",
    subtitle = "Estimativas do coeficiente β por estado",
    x = "Coeficiente β",
    y = "Estado",
    fill = "β"
  ) +
  theme_minimal()

# 📌 Salvar o gráfico como PNG para testar visualmente
ggsave("grafico_resultados_estados.png", plot = grafico_estados, width = 8, height = 6)
