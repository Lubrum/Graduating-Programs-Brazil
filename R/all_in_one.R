setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# loading or installing packages
if (!require(xlsx)) install.packages("xlsx")
library(xlsx)

if (!require(rgdal)) install.packages("rgdal")
library(rgdal)

if (!require(rgeos)) install.packages("rgeos")
library(rgeos)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(maptools)) install.packages("maptools")
library(maptools)

if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(forcats)) install.packages("forcats")
library(forcats)

if (!require(shiny)) install.packages("shiny")
library(shiny)

if (!require(shinydashboard)) install.packages("shinydashboard")
library(shinydashboard)

if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

if(!require(viridis)) install.packages("viridis")
library(viridis)

if (!require(shinyjs)) install.packages("shinyjs")
library(shinyjs)

if(!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

if (!require(directlabels)) install.packages("directlabels")
library(directlabels)

if (!require(mapproj)) install.packages("mapproj")
library(mapproj)

if (!require(stringr)) install.packages("stringr")
library(stringr)

#if (!require(leaflet)) install.packages("leaflet")
#library(leaflet)

# reading data from csv
universities_path <- "../csv/universities_after.xlsx"
cities_path <- "../csv/brazilian_cities.csv"
states_path <- "../csv/brazilian_states.csv"
research_names_path <- "../csv/research_names.xlsx"
university_research_path <- "../csv/university_research.xlsx"
graduation_level_path <- "../csv/graduation_level.xlsx"
course_name_path <- "../csv/course_name.xlsx"
concentration_area_path <- "../csv/concentration_area.xlsx"
university_concentration_area_path <- "../csv/university_concentration_area.xlsx"

universities <- read.xlsx(universities_path, sheetIndex = 1, encoding = "UTF-8")
cities <- read.csv(cities_path, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
states <- read.csv(states_path, sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
research_names <- read.xlsx(research_names_path, sheetIndex = 1, encoding = "UTF-8")
university_research <- read.xlsx(university_research_path, sheetIndex = 1, encoding = "UTF-8")
graduation_level <- read.xlsx(graduation_level_path, sheetIndex = 1, encoding = "UTF-8")
course_name <- read.xlsx(course_name_path, sheetIndex = 1, encoding = "UTF-8")
concentration_area <- read.xlsx(concentration_area_path, sheetIndex = 1, encoding = "UTF-8")
university_concentration_area <- read.xlsx(university_concentration_area_path, sheetIndex = 1, encoding = "UTF-8")

# joining data into one dataframe
all_data <- universities %>% left_join(cities, by = c("city_id" = "ibge_code"))
all_data <- all_data %>% left_join(states, by = c("state_id" = "state_id"))
all_data <- all_data %>% left_join(university_research, by = c("id" = "university_id"))
all_data <- all_data %>% left_join(research_names, by = c("research_id" = "id"))
all_data <- all_data %>% left_join(graduation_level, by = c("level" = "id"))
all_data <- all_data %>% left_join(course_name, by = c("course" = "id"))
all_data <- all_data %>% left_join(university_concentration_area, by = c("id" = "university_id"))
all_data <- all_data %>% left_join(concentration_area, by = c("concentration_area_id" = "id"))

all_data <- all_data[,-9]
all_data <- all_data[,-14]
all_data <- all_data[,-16]
all_data <- all_data[,-19]
all_data$latitude <- as.numeric(as.character(all_data$latitude))
all_data$longitude <- as.numeric(as.character(all_data$longitude))
all_data$grade <- as.numeric(as.character(all_data$grade))

#reading shapefile 
shape_brazil_path <- "../shapefile/Brazil.shp"
shape_brazil <- readOGR(shape_brazil_path, "Brazil", encoding = "latin1")

#transforming shapefile to dataframe format
shape_brazil$id <- rownames(as.data.frame(shape_brazil))
coordinates_brazil <- fortify(shape_brazil, region = "id") # only coordinates
shape_brasil_df <- merge(coordinates_brazil, shape_brazil, by = "id", type = 'left') # add remaining attributes

# theme 
geral_1_theme <- theme(
  axis.text = element_text(size = 12, color = "#cccccc"),
  axis.title = element_text(size = 16, face = "bold", color = "#cccccc"),
  axis.ticks = element_line(colour = "#cccccc"),
  axis.ticks.length = unit(0.5, "cm"),
  plot.caption = element_text(size = 12, face = "bold", color = "#cccccc"),
  plot.title = element_text(size = 22, hjust = 0.5, color = "#ffffff"),
  plot.background = element_rect(fill = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = 'black'),
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(color = "gray", fill = "black"),
  legend.text = element_text(size = 14, color = "#cccccc"),
  text = element_text(color = "#cccccc", size = 14)
)

# map labs
map_labs <- labs(
  x = "Longitude", 
  y = "Latitude", 
  caption = "Fonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# data from barplot 1
barplot_1_data <- as.data.frame(
  all_data %>%
  group_by(state_code) %>%
  summarise(id = n_distinct(id)) %>%
  arrange(-id) %>%
  mutate(state_code = fct_reorder(state_code, -id))
)

# bar plot 1 labs
barplot_1_labs <- labs(
  x = "\nEstados do Brasil", 
  y = "\nNúmero de Programas\n", 
  caption = "\nFonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# data from barplot 2
barplot_2_data <- as.data.frame(
  all_data %>%
  group_by(research_name) %>%
  summarise(id = n_distinct(id)) %>%
  arrange(-id) %>%
  mutate(research_name = fct_reorder(research_name, -id)) %>%
  filter( id > 10 )
)

# labs from barplot 2
barplot_2_labs <- labs(
  y = "Número de Programas de Pós-Graduação", 
  caption = "\nFonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# data from line plot 1
lineplot_1_data <- as.data.frame(
  all_data %>%
  group_by(research_name, year) %>%
  summarise(id = n_distinct(id)) %>%
  arrange(year) %>%
  mutate( cumsum = cumsum(id))
)

# getting cumulative sum of research topics by year
for( i in 1:length(unique(lineplot_1_data$research_name)) ) {
  name <- as.character(unique(lineplot_1_data$research_name)[i])
  last_year <- 0
  last_cumsum <- 0
  for (j in min(lineplot_1_data$year):max(lineplot_1_data$year) ) {
    if (j %in% lineplot_1_data$year[lineplot_1_data$research_name == name]){
      last_year <- j
      last_cumsum <- lineplot_1_data$cumsum[lineplot_1_data$research_name == name & lineplot_1_data$year == j]
    } 
    else {
      lineplot_1_data <- rbind.data.frame(lineplot_1_data, c(name, j, 0, last_cumsum),stringsAsFactors = FALSE)
    }
  }
}

lineplot_1_data$year <- as.numeric(lineplot_1_data$year)
lineplot_1_data$id <- as.numeric(lineplot_1_data$id)
lineplot_1_data$cumsum <- as.numeric(lineplot_1_data$cumsum)
lineplot_1_data <- lineplot_1_data[order(lineplot_1_data$year),]

# labs from line plot 1
lineplot_1_labs <- labs(
  x = "\nAnos", 
  y = "Número de Programas\n", 
  caption = "\nFonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

shinyUI <- navbarPage(
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

shinyServer <- function(input, output, session) {
  
  observe({
    if (input$input_state != "Todos") {
      shinyjs::enable("input_university")
      states <- c("Todas", as.character(sort(unique(all_data$university[which(all_data$state_name == input$input_state)]))))
      updateSelectInput(session, "input_university", choices = states, selected = input$input_university)
    } else { 
      shinyjs::disable("input_university")
    }
    
    if (input$input_university == "Todas" || input$input_university == "") {
      shinyjs::enable("input_state")
    } else {
      shinyjs::disable("input_state")
    }
  })
  
  map_data <- reactive({ 
    if(input$input_state == "Todos") {
      if(input$input_university == "Todas") {
        if(input$input_research_topic == "Todos") { 
          map_data <- all_data[all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$research_name == input$input_research_topic & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
        }
      } else {
        if(input$input_research_topic == "Todos") {
          map_data <- all_data[all_data$university == input$input_university & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$university == input$input_university & all_data$research_name == input$input_research_topic & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
        }
      }
    } else {
      if(input$input_university == "Todas") {
        if(input$input_research_topic == "Todos") {
          map_data <- all_data[all_data$state_name == input$input_state & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$state_name == input$input_state & all_data$research_name == input$input_research_topic & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
        }
      } else {
        if(input$input_research_topic == "Todos") {
          map_data <- all_data[all_data$university == input$input_university & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
             } else {
            map_data <- all_data[all_data$university == input$input_university & all_data$research_name == input$input_research_topic & all_data$grade >= input$input_grade_range[1] & all_data$grade <= input$input_grade_range[2],]
          }
        
      }
    } 
  })
  
  map_shapes <- reactive({ 
    if(input$input_state != "Todos") {
      map_shapes = shape_brasil_df[which(shape_brasil_df$ESTADOS == input$input_state),]
    }
    else{
      map_shapes = shape_brasil_df
    }
  })
  
  lineplot_data_1 <- reactive({ 
    
    lineplot_data_1 <- if (input$input_research_2 == "Nenhum") lineplot_1_data[lineplot_1_data$research_name == input$input_research_1,] 
    else lineplot_1_data[lineplot_1_data$research_name == input$input_research_1 | lineplot_1_data$research_name == input$input_research_2,]
    
  })
  
  output$map <- renderPlot({  
    validate(
      need(nrow(map_data()) != 0, 'Para esta(s) universidade(s) não há o selecionado tema de pesquisa!'),
      need(map_shapes(), 'Selecione um estado válido.')
    )
    ggplot() + 
    geom_polygon(data = map_shapes(), aes(x = long, y = lat, group = group), fill = "#3d3d3d", color = "black", size = 0.15) +
    geom_point(data = map_data()[!duplicated(map_data()$code),], aes(x = longitude, y = latitude, color = as.factor(grade), size = 10, shape = graduation_level)) + 
    scale_color_manual(name = "Conceito", values=c("red", "orange", "yellow", "darkgreen", "green")) +
    scale_shape_discrete(name = "Nível do Programa") +
    coord_map() + 
    guides(colour = guide_legend(override.aes = list(size = 5)), shape = guide_legend(override.aes = list(size = 5, color = "white")), size = FALSE) +
    ggtitle("Programas de Pós-Graduação em Computação") +
    geom_text_repel(data = map_data()[!duplicated(map_data()$code),], aes(x = longitude, y = latitude, label = code), hjust = 0, nudge_x = 2, direction = "y", color = "#cccccc") +
    map_labs + geral_1_theme + theme( panel.grid.minor.y = element_line(size =.1, color = "grey"),
                                      panel.grid.minor.x = element_line(size =.1, color = "grey"),
                                      panel.grid.major.y = element_line(size =.1, color = "grey"),
                                      panel.grid.major.x = element_line(size =.1, color = "grey"),
                                      legend.key = element_rect(color = "black", fill = "black"))
  }, bg = "transparent", execOnResize = TRUE)
  
  output$barplot_1 <- renderPlot({ 
    ggplot(data = barplot_1_data, aes(x = state_code, y = id)) +
    geom_bar(stat = "identity", fill = "black", color = "#E4F00A" ) + 
    geom_text(data = barplot_1_data, aes(label = id), vjust = 1.6, color = "white", size = 6) +
    scale_y_continuous(breaks = seq(0, max(barplot_1_data$id), 2)) +
    barplot_1_labs + geral_1_theme + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$barplot_2 <- renderPlot({ 
    ggplot(data = barplot_2_data, aes(x = research_name, y = id)) +
    geom_bar(stat = 'identity', fill = "black", color = "#22FF00") +
    geom_text(data = barplot_2_data, aes(label = id), hjust = 1.2, color = "white", size = 5) +
    scale_y_continuous(breaks = seq(0, max(barplot_2_data$id), 5)) +
    coord_flip() +
    barplot_2_labs + geral_1_theme +  theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  output$lineplot_1 <- renderPlot({ 
    ggplot(data = lineplot_data_1(), aes(x = year, y = cumsum, group = research_name)) +
    geom_line(aes(color = research_name)) +
    scale_color_manual(values = c("#E4F00A", "#22FF00")) +
    guides(colour = FALSE) +
    scale_x_continuous(expand = c(0, 0), limits = c(1965, 2030), breaks = seq(min(lineplot_data_1()$year), max(lineplot_data_1()$year), 2)) +
    geom_dl(aes(label = str_wrap(research_name, 30), color = research_name), method = list(cex = 1, dl.trans(x = x + .3), "last.qp")) +
    lineplot_1_labs + geral_1_theme + theme(panel.grid.major = element_line(color = "#666666"))
  })
}


shinyApp(shinyUI, shinyServer)

#leaflet(all_data[!duplicated(all_data$code),]) %>%
#addTiles() %>%
#addMarkers(clusterOptions = markerClusterOptions(), ~longitude, ~latitude, popup = ~as.character(city_name), label = ~as.character(code))

