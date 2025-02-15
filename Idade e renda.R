# Carregar pacotes necessários
library(tidyverse)
library(broom)
library(gt)
library(lmtest)
library(sandwich)

# Criar dicionário para converter faixa etária em valores médios
faixa_etaria_dict <- tibble(
  tp_faixa_etaria = 1:20,
  idade_media = c(
    16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 
    28, 33, 38, 43, 48, 53, 58, 63, 68, 72 # Média dos intervalos
  )
)

# Criar dicionário para converter renda familiar em valores médios
renda_dict <- tibble(
  q006 = LETTERS[1:17],  # A até Q
  renda_media = c(
    0,  # Nenhuma renda
    1212 / 2,  # Até R$ 1.212,00 -> Média (R$ 606,00)
    mean(c(1212, 1818)),  mean(c(1818, 2424)),  mean(c(2424, 3030)),  
    mean(c(3030, 3636)),  mean(c(3636, 4848)),  mean(c(4848, 6060)),  
    mean(c(6060, 7272)),  mean(c(7272, 8484)),  mean(c(8484, 9696)),  
    mean(c(9696, 10908)), mean(c(10908, 12120)), mean(c(12120, 14544)),  
    mean(c(14544, 18180)), mean(c(18180, 24240)), 27000  # Acima de 24.240,00
  )
)

# Incorporar os valores médios de idade e renda nos dados
dados <- dados %>%
  left_join(faixa_etaria_dict, by = "tp_faixa_etaria") %>%
  left_join(renda_dict, by = "q006") %>%
  mutate(
    rank_idade = percent_rank(idade_media),  # Ranking percentual da idade
    rank_renda = percent_rank(renda_media)   # Ranking percentual da renda
  )

# Criar o modelo de regressão incluindo idade e renda
modelo_completo <- lm(rank_filho_estado ~ rank_pais_media + rank_idade + rank_renda, data = dados)

# Extrair os resultados da regressão
resultados_completo <- tidy(modelo_completo) %>%
  select(term, estimate, std.error, p.value) %>%
  mutate(
    estimate = round(estimate, 3),
    std.error = round(std.error, 3),
    p.value = case_when(
      p.value < 0.01 ~ "<0.01 ***",
      p.value < 0.05 ~ "<0.05 **",
      p.value < 0.1 ~ "<0.10 *",
      TRUE ~ as.character(round(p.value, 3))
    )
  )

# Criar tabela formatada com gt
tabela_resultados <- resultados_completo %>%
  rename(
    Termo = term,
    Coeficiente = estimate,
    `Erro Padrão` = std.error,
    `Valor-p` = p.value
  ) %>%
  gt() %>%
  tab_header(
    title = "Estimativas do Modelo Rank-Rank com Idade e Renda",
    subtitle = "Regressão da Nota do Filho no ENEM sobre a Escolaridade dos Pais, Idade e Renda"
  ) %>%
  cols_label(
    Coeficiente = "β (Efeito das Variáveis)",
    `Erro Padrão` = "Erro Padrão",
    `Valor-p` = "Significância"
  ) %>%
  fmt_number(columns = c(Coeficiente, `Erro Padrão`), decimals = 3) %>%
  tab_options(table.font.size = "medium")

# Exibir tabela
tabela_resultados

gtsave(tabela_resultados, "Relatório/tabela_resultados_completo.html")
