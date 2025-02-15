# 📌 Carregar pacotes necessários
library(shiny)
library(shinydashboard)
library(gt)
library(broom)
library(tidyverse)

# 📌 Criar caminho para os arquivos HTML e imagens
shiny::addResourcePath("www", getwd())

# 📌 Carregar os dados
load("tabelas_processadas.RData")

# 📌 Criar a Interface do Usuário (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Análise da Mobilidade"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Estatísticas Descritivas", tabName = "estatisticas", icon = icon("table")),
      menuItem("Gráficos de Densidade", tabName = "densidade", icon = icon("chart-area")),
      menuItem("Estimativas β e α", tabName = "estimativas", icon = icon("calculator")),
      menuItem("Comparação entre Grupos", tabName = "grupos", icon = icon("users")),
      menuItem("Faixa Etária & Renda", tabName = "faixa_renda", icon = icon("dollar-sign")),
      menuItem("Resultados por Estado", tabName = "estados", icon = icon("globe")),
      menuItem("Mapa Interativo", tabName = "mapa", icon = icon("map")),
      menuItem("PIB, Escolaridade & Pobreza", tabName = "graficos", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tabItems(
      
      # 📌 Aba Estatísticas Descritivas
      tabItem(tabName = "estatisticas",
              fluidRow(
                box(
                  title = "Resumo dos Dados",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("Os dados analisados mostram que a maioria dos participantes tem entre 17 e 19 anos, com destaque para os 17 anos (19,0%) e 18 anos (23,2%). Em relação ao gênero, há uma predominância do sexo feminino (61,5%)."),
                  p("No quesito raça/cor, a maior parte dos respondentes se identifica como pardos (43,4%), seguidos por brancos (40,0%) e pretos (12,9%). Poucos participantes se identificam como indígenas (0,6%) ou amarelos (1,7%)."),
                  p("Sobre o tipo de escola frequentada, há uma proporção significativa de respostas não informadas (64,3%), mas entre os que responderam, a maioria estudou em escolas públicas (29,7%), enquanto 6,0% frequentaram escolas privadas."),
                  p("Quanto à escolaridade dos pais, observa-se que a maioria dos pais e mães não completou o Ensino Superior. A maior parte das mães e pais cursaram o Ensino Médio, mas não concluíram a faculdade (34,9% para as mães e 28,4% para os pais). Além disso, um percentual expressivo não concluiu nem o Ensino Fundamental (18,1% dos pais e 13,6% das mães), evidenciando um nível educacional mais baixo entre os responsáveis.")
                )
              ),
              fluidRow(
                box(title = "Faixa Etária", width = 6, gt_output("faixa_etaria")),
                box(title = "Sexo", width = 6, gt_output("sexo"))
              ),
              fluidRow(
                box(title = "Raça/Cor", width = 6, gt_output("cor_raca")),
                box(title = "Escola", width = 6, gt_output("escola"))
              ),
              fluidRow(
                box(title = "Escolaridade do Pai", width = 6, gt_output("q001")),
                box(title = "Escolaridade da Mãe", width = 6, gt_output("q002"))
              )
      ),
      
      # 📌 Aba Gráficos de Densidade
      tabItem(tabName = "densidade",
              fluidRow(
                box(
                  title = "Descrição dos Gráficos de Densidade",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("Os gráficos de densidade mostram a distribuição das notas do ENEM para diferentes grupos sociodemográficos."),
                  p(strong("Sexo:"), " As distribuições de notas para homens e mulheres são bastante semelhantes, mas a curva das mulheres (em vermelho) apresenta uma leve concentração maior em notas mais altas."),
                  p(strong("Tipo de Escola:"), " Há uma diferença significativa entre alunos de escolas públicas e privadas. Estudantes de escolas privadas (em vermelho) apresentam notas mais altas em média, enquanto alunos de escolas públicas (em azul) têm maior concentração em faixas de notas mais baixas."),
                  p(strong("Raça:"), " A distribuição de notas também revela disparidades raciais. Alunos brancos (em azul) apresentam uma distribuição de notas deslocada para a direita, indicando um desempenho médio superior em relação a alunos pretos (em vermelho), cujas notas estão mais concentradas em valores mais baixos."),
                  p(strong("Escolaridade dos Pais:"), " O nível educacional dos pais influencia diretamente o desempenho no ENEM. Estudantes cujos pais são mais escolarizados (linha azul) apresentam notas mais altas, enquanto aqueles cujos pais não possuem ensino superior (linha verde) têm maior concentração de notas mais baixas. Um pai é considerado educado se tiver, no mínimo, concluído o Ensino Médio.")
                )
              ),
              fluidRow(
                box(title = "Distribuição por Sexo", width = 6, imageOutput("densidade_sexo")),
                box(title = "Distribuição por Escola", width = 6, imageOutput("densidade_escola"))
              ),
              fluidRow(
                box(title = "Distribuição por Raça", width = 6, imageOutput("densidade_raca")),
                box(title = "Distribuição por Escolaridade dos Pais", width = 6, imageOutput("densidade_pais"))
              )
      ),
      
      # 📌 Aba Estimativas β e α
      tabItem(tabName = "estimativas",
              fluidRow(
                box(
                  title = "Descrição das Estimativas",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("Os resultados das regressões indicam uma forte relação entre a escolaridade dos pais e a dos filhos, evidenciando uma baixa mobilidade educacional no Brasil. Nos municípios, o efeito do ranking educacional do pai sobre o filho é de aproximadamente 0,317, enquanto o da mãe é um pouco maior, 0,349. Quando se considera o ranking médio dos pais, o coeficiente sobe para 0,419, sugerindo que a influência combinada da escolaridade dos pais é mais forte do que os efeitos individuais."),
                  p("Nos estados, os padrões são semelhantes. O coeficiente do pai é 0,352, o da mãe 0,372, e o efeito do ranking médio dos pais atinge 0,456, reforçando a ideia de que a escolaridade parental tem um impacto significativo na posição educacional dos filhos. Todos os coeficientes são estatisticamente significativos (p < 0,01), o que confirma a robustez das estimativas."),
                  p("De modo geral, os resultados mostram que a posição educacional dos pais afeta diretamente a trajetória educacional dos filhos, limitando a mobilidade intergeracional. Quanto maior o coeficiente β, menor a mobilidade, pois significa que a educação dos pais define, em grande parte, as oportunidades educacionais dos filhos.")
                )
              ),
              fluidRow(
                box(title = "Estimativas β e α", width = 12, imageOutput("tabela_resultados"))
              )
      ),
      
      # 📌 Aba Comparação entre Grupos
      tabItem(tabName = "grupos",
              fluidRow(
                box(
                  title = "Descrição da Comparação entre Grupos",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p(strong("Escolas: Pública vs. Privada"), 
                    "O coeficiente positivo e significativo para escola_privada (0.219) indica que estudantes de escolas privadas tendem a ter um ranking mais alto no ENEM em relação aos de escolas públicas, independentemente da escolaridade dos pais. O efeito do ranking dos pais (rank_pais_media) é significativo (0.326), sugerindo que a escolaridade dos pais tem um impacto positivo sobre o desempenho dos filhos. No entanto, a interação entre rank_pais_media e escola_privada (-0.030) não é estatisticamente significativa (p = 0.324), sugerindo que o impacto da escolaridade dos pais sobre o desempenho dos filhos é semelhante entre alunos de escolas públicas e privadas."),
                  p(strong("Raça: Branca vs. Preta"), 
                    "O coeficiente negativo para preta (-0.040) indica que estudantes pretos tendem a ter um ranking inferior no ENEM em comparação com estudantes brancos, controlando pela escolaridade dos pais. O efeito da escolaridade dos pais (rank_pais_media) é forte e significativo (0.455), indicando que quanto maior o nível educacional dos pais, melhor o desempenho dos filhos. A interação rank_pais_media:preta (-0.084) também é significativa, sugerindo que o efeito da escolaridade dos pais sobre o desempenho dos filhos é menor para estudantes pretos do que para brancos, evidenciando uma desigualdade na mobilidade educacional entre os grupos raciais."),
                  p(strong("Sexo: Feminino vs. Masculino"), 
                    "O coeficiente positivo para masculino (0.036) sugere que, em média, estudantes do sexo masculino têm um ranking ligeiramente superior no ENEM em comparação com estudantes do sexo feminino. O efeito da escolaridade dos pais (rank_pais_media = 0.485) continua forte e significativo. No entanto, a interação rank_pais_media:masculino (-0.073) é significativa e negativa, indicando que o efeito da escolaridade dos pais sobre o desempenho dos filhos é menor para meninos do que para meninas. Isso pode sugerir que a mobilidade educacional é mais forte entre estudantes do sexo feminino.")
                )
              ),
              fluidRow(
                box(title = "Comparação entre Grupos", width = 12, 
                    tags$iframe(src = "www/tabela_resultados_grupos.html", width = "100%", height = "600px"))
              )
      ),
      
      # 📌 Aba Faixa Etária e Renda
      tabItem(tabName = "faixa_renda",
              fluidRow(
                box(
                  title = "Descrição de Faixa Etária e Renda",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("O coeficiente de rank_pais_media (0.318) confirma que a escolaridade dos pais tem um impacto positivo e significativo sobre o desempenho dos filhos, ou seja, quanto maior a posição dos pais no ranking educacional, melhor tende a ser a nota dos filhos."),
                  p("A variável rank_idade (0.054) também apresenta um efeito positivo e significativo, sugerindo que alunos mais velhos tendem a ter um desempenho ligeiramente superior no ENEM. Isso pode estar relacionado a uma maior maturidade ou tempo adicional de estudo."),
                  p("Já rank_renda (0.204) indica que a renda familiar tem um impacto relevante sobre a nota no ENEM, reforçando a importância do contexto socioeconômico no desempenho educacional."),
                  p("Todos os coeficientes são estatisticamente significativos (p < 0.01), indicando que essas variáveis influenciam de maneira consistente os resultados dos estudantes.")
                )
              ),
              fluidRow(
                box(title = "Faixa Etária & Renda", width = 12, 
                    tags$iframe(src = "www/tabela_resultados_completo.html", width = "100%", height = "600px"))
              )
      ),
      
      # 📌 Aba Resultados por Estado (GRÁFICO DE BARRAS)
      tabItem(tabName = "estados",
              fluidRow(
                box(
                  title = "Observação",
                  width = 12,
                  status = "primary",
                  solidHeader = TRUE,
                  p("Não há uma relação clara entre relação entre a mobilidade intergeracional educacional e informações que conheço desses estados.")
                )
              ),
              fluidRow(
                box(title = "Coeficientes do Modelo Rank-Rank por Estado", width = 12, imageOutput("grafico_resultados_estados"))
              )
      ),
      
      # 📌 Aba Mapa Interativo
      tabItem(tabName = "mapa",
              fluidRow(
                box(title = "Mapa Interativo", width = 12, 
                    tags$iframe(src = "www/meu_mapa.html", width = "100%", height = "600px", seamless = "seamless"))
              )
      ),
      
      # 📌 Aba PIB, Escolaridade & Pobreza
      tabItem(tabName = "graficos",
              fluidRow(
                box(title = "Correlação entre β e PIB", width = 6, imageOutput("grafico_pib")),
                box(title = "Correlação entre β e Escolaridade", width = 6, imageOutput("grafico_escolaridade"))
              ),
              fluidRow(
                box(title = "Correlação entre β e Pobreza", width = 6, imageOutput("grafico_pobreza"))
              )
      )
    )
  )
)

# 📌 Definir o Servidor
server <- function(input, output) {
  
  # 📌 Função para verificar e renderizar imagem
  renderizar_imagem <- function(path) {
    if (file.exists(path)) {
      return(list(src = path, width = "100%"))
    } else {
      return(NULL)
    }
  }
  
  # 📌 Estatísticas Descritivas
  output$faixa_etaria <- render_gt({ tabelas_gt$faixa_etaria })
  output$sexo <- render_gt({ tabelas_gt$sexo })
  output$cor_raca <- render_gt({ tabelas_gt$cor_raca })
  output$escola <- render_gt({ tabelas_gt$escola })
  output$q001 <- render_gt({ tabelas_gt$q001 })
  output$q002 <- render_gt({ tabelas_gt$q002 })
  
  # 📌 Gráficos de Densidade
  output$densidade_sexo <- renderImage({ renderizar_imagem("grafico_densidade_sexo.png") }, deleteFile = FALSE)
  output$densidade_escola <- renderImage({ renderizar_imagem("grafico_densidade_escola.png") }, deleteFile = FALSE)
  output$densidade_raca <- renderImage({ renderizar_imagem("grafico_densidade_raca.png") }, deleteFile = FALSE)
  output$densidade_pais <- renderImage({ renderizar_imagem("grafico_densidade_pais.png") }, deleteFile = FALSE)
  
  # 📌 Estimativas β e α
  output$tabela_resultados <- renderImage({ renderizar_imagem("tabela_resultados.png") }, deleteFile = FALSE)
  
  # 📌 PIB, Escolaridade & Pobreza
  output$grafico_pib <- renderImage({ renderizar_imagem("grafico_pib.png") }, deleteFile = FALSE)
  output$grafico_escolaridade <- renderImage({ renderizar_imagem("grafico_escolaridade.png") }, deleteFile = FALSE)
  output$grafico_pobreza <- renderImage({ renderizar_imagem("grafico_pobreza.png") }, deleteFile = FALSE)
  
  # 📌 Gráfico de coeficientes por estado  
  output$grafico_resultados_estados <- renderImage({ 
    renderizar_imagem("grafico_resultados_estados.png") 
  }, deleteFile = FALSE)
  
}

# 📌 Rodar a aplicação
shinyApp(ui, server)
