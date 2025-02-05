# Carregar pacotes necessários
library(haven)
library(tidyverse)
library(gt)

# Carregar pacotes necessários
library(haven)
library(tidyverse)
library(gt)

# Importar os dados
dados <- read_dta("data.dta") %>% as_tibble()

# Criar uma função para gerar tabelas de frequência relativa
gerar_tabela <- function(variavel, dicionario) {
  freq_rel <- prop.table(table(dados[[variavel]])) %>%
    as.data.frame() %>%
    rename(valor = Var1, Freq_Relativa = Freq)
  
  tabela <- merge(freq_rel, dicionario, by.x = "valor", by.y = names(dicionario)[1]) %>%
    select(Descrição = descricao, Freq_Relativa) %>%
    arrange(Descrição)
  
  return(tabela)
}

# Criar dicionários para variáveis categóricas
dicionarios <- list(
  tp_faixa_etaria = data.frame(valor = as.character(1:20), descricao = c(
    "Menor de 17 anos", "17 anos", "18 anos", "19 anos", "20 anos",
    "21 anos", "22 anos", "23 anos", "24 anos", "25 anos",
    "Entre 26 e 30 anos", "Entre 31 e 35 anos", "Entre 36 e 40 anos",
    "Entre 41 e 45 anos", "Entre 46 e 50 anos", "Entre 51 e 55 anos",
    "Entre 56 e 60 anos", "Entre 61 e 65 anos", "Entre 66 e 70 anos",
    "Maior de 70 anos"
  )),
  tp_sexo = data.frame(valor = c("M", "F"), descricao = c("Masculino", "Feminino")),
  tp_cor_raca = data.frame(valor = as.character(0:6), descricao = c(
    "Não declarado", "Branca", "Preta", "Parda", "Amarela", "Indígena", "Não dispõe da informação"
  )),
  tp_escola = data.frame(valor = as.character(1:3), descricao = c("Não Respondeu", "Pública", "Privada")),
  q001 = data.frame(valor = LETTERS[1:8], descricao = c(
    "Nunca estudou.", "Não completou a 4ª série/5º ano do Ensino Fundamental.",
    "Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental.",
    "Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio.",
    "Completou o Ensino Médio, mas não completou a Faculdade.",
    "Completou a Faculdade, mas não completou a Pós-graduação.",
    "Completou a Pós-graduação.", "Não sei."
  )),
  q002 = data.frame(valor = LETTERS[1:8], descricao = c(
    "Nunca estudou.", "Não completou a 4ª série/5º ano do Ensino Fundamental.",
    "Completou a 4ª série/5º ano, mas não completou a 8ª série/9º ano do Ensino Fundamental.",
    "Completou a 8ª série/9º ano do Ensino Fundamental, mas não completou o Ensino Médio.",
    "Completou o Ensino Médio, mas não completou a Faculdade.",
    "Completou a Faculdade, mas não completou a Pós-graduação.",
    "Completou a Pós-graduação.", "Não sei."
  ))
)

# Gerar tabelas
tabelas <- map2(c("tp_faixa_etaria", "tp_sexo", "tp_cor_raca", "tp_escola", "q001", "q002"), dicionarios, gerar_tabela)

# Criar uma função para formatar tabelas com `gt`
gerar_tabela_gt <- function(tabela, nome_variavel) {
  tabela %>%
    gt() %>%
    tab_header(title = md(paste0("**", nome_variavel, "**")), subtitle = "Frequência Relativa das Respostas") %>%
    cols_label(Descrição = "Categoria", Freq_Relativa = "Frequência Relativa") %>%
    fmt_percent(columns = Freq_Relativa, decimals = 1) %>%
    tab_options(table.font.size = "medium", column_labels.font.weight = "bold") %>%
    tab_style(style = list(cell_text(weight = "bold", align = "center")), locations = cells_column_labels())
}

# Criar e salvar as tabelas individuais
tabelas_gt <- list(
  faixa_etaria = gerar_tabela_gt(tabelas[[1]], "Faixa Etária"),
  sexo = gerar_tabela_gt(tabelas[[2]], "Sexo"),
  cor_raca = gerar_tabela_gt(tabelas[[3]], "Raça/Cor"),
  escola = gerar_tabela_gt(tabelas[[4]], "Escola"),
  q001 = gerar_tabela_gt(tabelas[[5]], "Escolaridade do Pai"),
  q002 = gerar_tabela_gt(tabelas[[6]], "Escolaridade da Mãe")
)

# Salvar as tabelas em um arquivo RData para carregamento no Quarto
save(tabelas_gt, file = "tabelas_processadas.RData")

