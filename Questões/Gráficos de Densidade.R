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
              # Adicionando o texto explicativo
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
                box(title = "Estimativas β e α", width = 12, imageOutput("tabela_resultados"))
              )
      ),
      
      # 📌 Aba Comparação entre Grupos
      tabItem(tabName = "grupos",
              fluidRow(
                box(title = "Comparação entre Grupos", width = 12, 
                    tags$iframe(src = "www/tabela_resultados_grupos.html", width = "100%", height = "600px"))
              )
      ),
      
      # 📌 Aba Faixa Etária e Renda
      tabItem(tabName = "faixa_renda",
              fluidRow(
                box(title = "Faixa Etária & Renda", width = 12, 
                    tags$iframe(src = "www/tabela_resultados_completo.html", width = "100%", height = "600px"))
              )
      ),
      
      # 📌 Aba Resultados por Estado (GRÁFICO DE BARRAS)
      tabItem(tabName = "estados",
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
