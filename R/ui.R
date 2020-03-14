if (!require(shiny)) install.packages("shiny")
library(shiny)

if (!require(shinydashboard)) install.packages("shinydashboard")
library(shinydashboard)

load(file = "data/all_data.RData")

ui <- navbarPage(
  "Data Science Broom",
  tabPanel(
    "Mapas",
    fluidPage(
      shinyjs::useShinyjs(),
      titlePanel("Programas de Pós-Graduação em Computação - Brasil"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "Input_State",
            "Estado",
            choices = c("Todos", sort(unique(all_data$state_name))),
            selected = "Todos"
          ),
          selectInput(
            "Input_University",
            "Universidade",
            choices = c("Todas", as.character(sort(unique(all_data$university)))),
            selected = "Todas"
          ),
          selectInput(
            "Input_Research_Topic",
            "Assunto de Pesquisa",
            choices = c("Todos", as.character(sort(unique(all_data$research_name)))),
            selected = "Todos"
          ),
          sliderInput(
            "Input_grade_range",
            label = "Notas dos Programas:",
            min = 3,
            max = 7,
            value = c(3, 7)
          )
        ),
        mainPanel(plotOutput("map", height = 700, width = "100%"))
      )
    )
  ),
  tabPanel(
    "Gráficos",
    dashboardPage(
      dashboardHeader(disable = TRUE),
      dashboardSidebar(collapsed = TRUE,
                       sidebarMenu(
                         menuItem(
                           "Dashboard",
                           tabName = "dashboard",
                           icon = icon("dashboard")
                         )
                       )),
      dashboardBody(tags$head(tags$style(
        HTML('.content-wrapper, .right-side {
             background-color: #000000;
             }'
        )
      )),
      tabItems(
        tabItem(tabName = "dashboard",
                fluidRow(
                  box(
                    background = "black",
                    title = "Programas de Pós-Graduação em Computação por Estado",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("bar_1"),
                    width = 6
                  )
                  ,
                  box(
                    background = "black",
                    title = "Programas de Pós-Graduação em Computação por Tópico de Pesquisa",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    plotOutput("bar_2"),
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
                    plotOutput("line_1"),
                    width = 12
                  ),
                  box(
                    background = "black",
                    box(
                      background = "black",
                      selectInput(
                        "Input_Research_1",
                        "Tópico de Pesquisa 1",
                        choices = sort(unique(lineplot_1_data$research_name)),
                        selected = lineplot_1_data$research_name[1]
                      )
                    ),
                    box(
                      background = "black",
                      selectInput(
                        "Input_Research_2",
                        "Tópico de Pesquisa 2",
                        choices = c("Nenhum", as.character(sort(unique(lineplot_1_data$research_name)))),
                        selected = "Nenhum"
                      )
                    ),
                    width = 12
                  )
                ))
      ))
    )
  )
)