if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

if(!require(viridis)) install.packages("viridis")
library(viridis)

if (!require(shinyjs)) install.packages("shinyjs")
library(shinyjs)

if(!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

if (!require(shiny)) install.packages("shiny")
library(shiny)

if (!require(directlabels)) install.packages("directlabels")
library(directlabels)

if (!require(mapproj)) install.packages("mapproj")
library(mapproj)

if (!require(stringr)) install.packages("stringr")
library(stringr)

load(file = "data/all_data.RData")

# Server logic ----
server <- function(input, output, session) {
  
  observe({
    if (input$Input_State != "Todos") {
      shinyjs::enable("Input_University")
      states <- c("Todas", as.character(sort(unique(all_data$university[which(all_data$state_name == input$Input_State)]))))
      updateSelectInput(session, "Input_University", choices = states, selected = input$Input_University)
    } else { 
      shinyjs::disable("Input_University")
    }
    
    if (input$Input_University == "Todas" || input$Input_University == "") {
      shinyjs::enable("Input_State")
    } else {
      shinyjs::disable("Input_State")
    }
  })
  
  map_data <- reactive({ 
    if(input$Input_State == "Todos" || input$Input_State == "") {
      if(input$Input_University == "Todas" || input$Input_University == "") {
        if(input$Input_Research_Topic == "Todos") { 
          map_data <- all_data[all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$research_name == input$Input_Research_Topic & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
        }
      } else {
        if(input$Input_Research_Topic == "Todos") {
          map_data <- all_data[all_data$university == input$Input_University & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$university == input$Input_University & all_data$research_name == input$Input_Research_Topic & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
        }
      }
    } else {
      if(input$Input_University == "Todas" || input$Input_University == "") {
        if(input$Input_Research_Topic == "Todos") {
          map_data <- all_data[all_data$state_name == input$Input_State & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$state_name == input$Input_State & all_data$research_name == input$Input_Research_Topic & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
        }
      } else {
        if(input$Input_Research_Topic == "Todos") {
          map_data <- all_data[all_data$university == input$Input_University & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
          5   } else {
            map_data <- all_data[all_data$university == input$Input_University & all_data$research_name == input$Input_Research_Topic & all_data$grade >= input$Input_grade_range[1] & all_data$grade <= input$Input_grade_range[2],]
          }
        
      }
    } 
    
    req(map_data)
    
  })
  
  map_shapes <- reactive({ 
    if(input$Input_State != "Todos" & input$Input_State != "") {
      map_shapes = shape_brasil_df[which(shape_brasil_df$ESTADOS == input$Input_State),]
    }
    else{
      map_shapes = shape_brasil_df
    }
  })
  
  line_data <- reactive({ 
    
    line_data <- if (input$Input_Research_2 == "Nenhum") lineplot_1_data[lineplot_1_data$research_name == input$Input_Research_1,] 
    else lineplot_1_data[lineplot_1_data$research_name == input$Input_Research_1 | lineplot_1_data$research_name == input$Input_Research_2,]
    
  })
  
  output$map <- renderPlot({  
    validate(
      need(nrow(map_data()) != 0, 'Para esta(s) universidade(s) não há o selecionado tema de pesquisa!'),
      need(map_shapes(), 'Selecione um estado válido.')
    )
    ggplot() + 
      geom_polygon(data = map_shapes(), aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.15) +
      geom_point(data = map_data()[!duplicated(map_data()$code),], aes(x = longitude, y = latitude, color = grade, size = 10, shape = graduation_level, alpha = I(2/grade))) + 
      scale_color_viridis(name = "Conceito", breaks = mybreaks) +
      scale_shape_discrete(name = "Nível do Programa") +
      coord_map() + 
      guides(colour = guide_legend(override.aes = list(size = 5)), shape = guide_legend(override.aes = list(size = 5)), size = FALSE) +
      ggtitle("Programas de Pós-Graduação em Computação") +
      map_labs + map_theme + 
      geom_text_repel(data = map_data()[!duplicated(map_data()$code),], aes(x = longitude, y = latitude, label = code), position = position_dodge(width = 5), hjust = -0.5)
  })
  
  output$bar_1 <- renderPlot({ 
    ggplot(data = barplot_1_data, aes(x = state_code, y = id)) +
      geom_bar(stat = "identity", fill = "black", color = "#E4F00A" ) + 
      geom_text(data = barplot_1_data, aes(label = id), vjust = 1.6, color = "white", size = 6) +
      scale_y_continuous(breaks = seq(0, max(barplot_1_data$id), 2)) +
      barplot_1_labs + geral_1_theme + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  })
  
  output$bar_2 <- renderPlot({ 
    ggplot(data = barplot_2_data, aes(x = research_name, y = id)) +
      geom_bar(stat = 'identity', position = position_stack(reverse = TRUE), fill = "black", color = "#22FF00") +
      geom_text(data = barplot_2_data, aes(label = id), hjust = 1.2, color = "white", size = 5) +
      scale_y_continuous(breaks = seq(0, max(barplot_2_data$id), 5)) +
      coord_flip() +
      barplot_2_labs + geral_1_theme +  theme(axis.title.y = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  
  output$line_1 <- renderPlot({ 
    ggplot(data = line_data(), aes(x = year, y = cumsum, group = research_name)) +
      geom_line(aes(color = research_name)) +
      scale_color_manual(values = c("#E4F00A", "#22FF00")) +
      guides(colour = FALSE) +
      scale_x_continuous(expand = c(0, 0), limits = c(1965, 2030), breaks = seq(min(line_data()$year), max(line_data()$year), 2)) +
      lineplot_1_labs + geral_1_theme + theme(panel.grid.major = element_line(color = "#666666")) +
      geom_dl(aes(label = str_wrap(research_name, 30), color = research_name), method = list(cex = 1, dl.trans(x = x + .3), "last.qp")) 
  })
}