# Carregar os pacotes necessários
library(tidyverse)
library(broom)
library(gt)

###########################################
# 1. Análise: Escolas Públicas vs Privadas
###########################################

# Filtrar observações e criar a dummy (1 = Privada, 0 = Pública)
dados_escola <- dados %>% 
  filter(escola %in% c("Pública", "Privada")) %>%
  mutate(escola_privada = if_else(escola == "Privada", 1, 0))

# Modelo de interação para escolas
modelo_escola <- lm(rank_filho_estado ~ rank_pais_media * escola_privada, data = dados_escola)
summary(modelo_escola)  # para visualizar os resultados detalhadamente

# Extração dos coeficientes com broom
resultados_escola <- tidy(modelo_escola) %>% 
  mutate(Comparacao = "Escolas: Pública vs Privada")


###########################################
# 2. Análise: Estudantes Homens vs Mulheres
###########################################

# Filtrar observações e criar a dummy (1 = Masculino, 0 = Feminino)
dados_sexo <- dados %>% 
  filter(sexo %in% c("Masculino", "Feminino")) %>%
  mutate(masculino = if_else(sexo == "Masculino", 1, 0))

# Modelo de interação para sexo
modelo_sexo <- lm(rank_filho_estado ~ rank_pais_media * masculino, data = dados_sexo)
summary(modelo_sexo)

# Extração dos coeficientes
resultados_sexo <- tidy(modelo_sexo) %>% 
  mutate(Comparacao = "Sexo: Feminino vs Masculino")


###########################################
# 3. Análise: Estudantes Brancos vs Pretos
###########################################

# Filtrar observações e criar a dummy (1 = Preta, 0 = Branca)
dados_raca <- dados %>% 
  filter(raca %in% c("Branca", "Preta")) %>%
  mutate(preta = if_else(raca == "Preta", 1, 0))

# Modelo de interação para raça
modelo_raca <- lm(rank_filho_estado ~ rank_pais_media * preta, data = dados_raca)
summary(modelo_raca)

# Extração dos coeficientes
resultados_raca <- tidy(modelo_raca) %>% 
  mutate(Comparacao = "Raça: Branca vs Preta")


###########################################
# 4. Combinar os Resultados e Formatar
###########################################

# Combinar os resultados dos três modelos em um único data frame
resultados_combinados <- bind_rows(resultados_escola, resultados_sexo, resultados_raca) %>%
  mutate(
    estimate   = round(estimate, 3),
    std.error  = round(std.error, 3),
    # Formatação dos valores-p e inclusão de asteriscos de significância
    p.value = case_when(
      p.value < 0.01 ~ "<0.01 ***",
      p.value < 0.05 ~ "<0.05 **",
      p.value < 0.1  ~ "<0.10 *",
      TRUE ~ as.character(round(p.value, 3))
    )
  ) %>%
  # Reordenar e selecionar as colunas desejadas
  select(Comparacao, term, estimate, std.error, p.value) %>%
  arrange(Comparacao, term)

# Se você preferir exibir a tabela de forma integrada com gt:
tabela_dummies <- resultados_combinados %>%
  gt(groupname_col = "Comparacao") %>%
  # A coluna Comparacao serve apenas para agrupar e não precisa aparecer no corpo
  cols_hide(columns = c(Comparacao)) %>%
  tab_header(
    title = "Diferenças nos Parâmetros (α e β) Entre Subamostras",
    subtitle = "Modelo: rank_filho_estado ~ rank_pais_media + Dummy + (rank_pais_media * Dummy)"
  ) %>%
  cols_label(
    term = "Parâmetro",
    estimate = "Coeficiente",
    std.error = "Erro Padrão",
    p.value = "Valor-p"
  ) %>%
  fmt_number(
    columns = c(estimate, std.error),
    decimals = 3
  ) %>%
  tab_source_note(
    source_note = "Nota: 'Intercept' corresponde a α (valor base) e 'rank_pais_media' a β. Nos modelos, os termos da dummy e da interação indicam as diferenças entre os grupos."
  )

# Exibir a tabela
tabela_dummies

gtsave(tabela_dummies, "Relatório/tabela_resultados_grupos.html")

