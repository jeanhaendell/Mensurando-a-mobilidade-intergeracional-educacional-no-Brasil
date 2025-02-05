# Criar a variável nota_enem como a média das cinco notas
dados <- dados %>%
  mutate(nota_enem = rowMeans(select(., nu_nota_cn, nu_nota_ch, nu_nota_lc, nu_nota_mt, nu_nota_redacao), na.rm = TRUE))

# Remover colunas duplicadas antes do left_join (se houver)
dados <- dados %>%
  select(-starts_with("descricao"), -starts_with("sexo"), -starts_with("escola"), -starts_with("raca"), 
         -starts_with("escolaridade_pai"), -starts_with("escolaridade_mae"))

# **Garantir que todas as variáveis categóricas estejam no mesmo tipo**
dados <- dados %>%
  mutate(
    tp_escola = as.character(tp_escola),
    tp_cor_raca = as.character(tp_cor_raca),
    q001 = as.character(q001),
    q002 = as.character(q002)
  )

# Adicionar rótulos das variáveis categóricas corretamente
dados <- dados %>%
  left_join(dicionarios$tp_sexo %>% rename(sexo = descricao), by = c("tp_sexo" = "valor")) %>%
  left_join(dicionarios$tp_escola %>% rename(escola = descricao), by = c("tp_escola" = "valor")) %>%
  left_join(dicionarios$tp_cor_raca %>% rename(raca = descricao), by = c("tp_cor_raca" = "valor")) %>%
  left_join(dicionarios$q001 %>% rename(escolaridade_pai = descricao), by = c("q001" = "valor")) %>%
  left_join(dicionarios$q002 %>% rename(escolaridade_mae = descricao), by = c("q002" = "valor"))

# Criar categorias de pais educados e não educados
dados <- dados %>%
  mutate(
    pai_educado = ifelse(q001 %in% c("E", "F", "G"), "Educado", "Não Educado"),
    mae_educada = ifelse(q002 %in% c("E", "F", "G"), "Educado", "Não Educado"),
    pais_educados = case_when(
      pai_educado == "Educado" & mae_educada == "Educado" ~ "Ambos Educados",
      pai_educado == "Não Educado" & mae_educada == "Não Educado" ~ "Ambos Não Educados",
      TRUE ~ "Um Educado, Outro Não"
    )
  )

# Criar uma função para gerar e salvar os gráficos de densidade
gerar_grafico_densidade <- function(df, grupo, filtro = NULL, nome_arquivo, titulo) {
  if (!is.null(filtro)) {
    df <- df %>% filter(!!sym(grupo) %in% filtro)
  }
  
  grafico <- ggplot(df, aes(x = nota_enem, fill = !!sym(grupo), color = !!sym(grupo))) +
    geom_density(alpha = 0.4) +
    labs(
      title = titulo,
      x = "Nota no ENEM",
      y = "Densidade",
      fill = grupo,
      color = grupo
    ) +
    theme_minimal()
  
  ggsave(paste0("Relatório/", nome_arquivo, ".png"), grafico, width = 8, height = 5, dpi = 300)
}

# Gerar os quatro gráficos
gerar_grafico_densidade(dados, "sexo", nome_arquivo = "grafico_densidade_sexo", titulo = "Distribuição das Notas do ENEM por Sexo")

gerar_grafico_densidade(dados, "escola", filtro = c("Pública", "Privada"), 
                        nome_arquivo = "grafico_densidade_escola", titulo = "Distribuição das Notas do ENEM por Tipo de Escola")

gerar_grafico_densidade(dados, "raca", filtro = c("Preta", "Branca"), 
                        nome_arquivo = "grafico_densidade_raca", titulo = "Distribuição das Notas do ENEM por Raça")

gerar_grafico_densidade(dados, "pais_educados", 
                        nome_arquivo = "grafico_densidade_pais", titulo = "Distribuição das Notas do ENEM por Escolaridade dos Pais")
