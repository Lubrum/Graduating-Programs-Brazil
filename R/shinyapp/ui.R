if (!require(shiny)) install.packages("shiny")
library(shiny)
  
if (!require(shinydashboard)) install.packages("shinydashboard")
library(shinydashboard)

load(file = "data/all_data.RData")
  
ui <- navbarPage(
  "Data Science Broom",
  tabPanel(
    "Mapas",
    fluidPage(id = 'fluidPage', 
              shinyjs::useShinyjs(),
              titlePanel("Programas de Pós-Graduação em Computação - Brasil"),
              sidebarLayout(
                
                sidebarPanel(id = "sidebar",
                             selectInput(
                               "input_state",
                               "Estado",
                               choices = c("Todos", sort(unique(all_data$state_name))),
                               selected = "Todos"
                             ),
                             selectInput(
                               "input_university",
                               "Universidade",
                               choices = c("Todas", as.character(sort(unique(all_data$university)))),
                               selected = "Todas"
                             ),
                             selectInput(
                               "input_research_topic",
                               "Assunto de Pesquisa",
                               choices = c("Todos", as.character(sort(unique(all_data$research_name)))),
                               selected = "Todos"
                             ),
                             sliderInput(
                               "input_grade_range",
                               label = "Notas dos Programas:",
                               min = 3,
                               max = 7,
                               value = c(3, 7)
                             )
                ),
                mainPanel(id = 'mainPanel',
                          fluidRow(box(
                            background = "black",
                            plotOutput("map", height = 900), width = 12 )))
              ),
              includeCSS("css/styles.css"))
  ),
  tabPanel(
    "Gráficos",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar( disable = TRUE),
      dashboardBody(
        fluidRow(
          box(
            background = "black",
            title = "Programas de Pós-Graduação em Computação por Estado",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("barplot_1"),
            width = 6
          )
          ,
          box(
            background = "black",
            title = "Programas de Pós-Graduação em Computação por Tópico de Pesquisa",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotOutput("barplot_2"),
            width = 6
          )
        ),
        fluidRow(
          box(
            background = "black",
            title = "Histórico dos Programas de Pós-Graduação em Computação por Tópico de Pesquisa",
            solidHeader = TRUE,
            collapsible = TRUE,
            "Observação: o ano apresentado é o de surgimento do Programa que originou
                    o Tópico de Pesquisa. Não é o ano de surgimento do Tópico de Pesquisa em si,
                    já que a informação dos tópicos de pesquisa não foi coletada do site da
                    CAPES e sim da página de cada Programa de Pós-Graduação em Computação do
                    Brasil.",
            br(),br(),br(),
            plotOutput("lineplot_1"),
            width = 12
          ),
          box(
            background = "black",
            box(
              background = "black",
              selectInput(
                "input_research_1",
                "Tópico de Pesquisa 1",
                choices = sort(unique(lineplot_1_data$research_name)),
                selected = lineplot_1_data$research_name[1]
              )
            ),
            box(
              background = "black",
              selectInput(
                "input_research_2",
                "Tópico de Pesquisa 2",
                choices = c("Nenhum", as.character(sort(unique(lineplot_1_data$research_name)))),
                selected = "Nenhum"
              )
            ),
            width = 12
          )
        )
      )
    )
  )
)