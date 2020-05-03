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

shinyServer <- function(input, output, session) {
  
  observe({
    # casos para desabilitar menu de universidades através de seleção específica no menu de estados
    if (input$input_state != "Todos") {
      shinyjs::enable("input_university")
      universities <- c("Todas", as.character(sort(unique(all_data$university[which(all_data$state_name == input$input_state)]))))
      updateSelectInput(session, "input_university", choices = universities, selected = input$input_university)
    } else { 
      shinyjs::disable("input_university")
    }
    # casos para desabilitar menu de estados através de seleção específica no menu de universidades
    if (input$input_university == "Todas" || input$input_university == "") {
      shinyjs::enable("input_state")
    } else {
      shinyjs::disable("input_state")
    }
  })
  
  # mudança/filtro nos dados do dataframe all_data com base nos inputs do usuário (mapa)
  map_data <- reactive({ 
    if(input$input_state == "Todos") {
      if(input$input_university == "Todas") {
        if(input$input_research_topic == "Todos") { 
          map_data <- all_data[all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$research_name == input$input_research_topic & 
                                 all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        }
      } else {
        if(input$input_research_topic == "Todos") {
          map_data <- all_data[all_data$university == input$input_university & 
                                 all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$university == input$input_university & 
                                 all_data$research_name == input$input_research_topic & 
                                 all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        }
      }
    } else {
      if(input$input_university == "Todas") {
        if(input$input_research_topic == "Todos") {
          map_data <- all_data[all_data$state_name == input$input_state & 
                                 all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$state_name == input$input_state & 
                                 all_data$research_name == input$input_research_topic & 
                                 all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        }
      } else {
        if(input$input_research_topic == "Todos") {
          map_data <- all_data[all_data$university == input$input_university & 
                                 all_data$grade >= input$input_grade_range[1] &
                                 all_data$grade <= input$input_grade_range[2],]
        } else {
          map_data <- all_data[all_data$university == input$input_university & 
                                 all_data$research_name == input$input_research_topic & 
                                 all_data$grade >= input$input_grade_range[1] & 
                                 all_data$grade <= input$input_grade_range[2],]
        }
      }
    } 
  })
  
  # mudança/filtro nos dados do dataframe shape_brasil_df com base nos inputs do usuário (mapa)
  map_shapes <- reactive({ 
    if(input$input_state != "Todos") {
      map_shapes = shape_brasil_df[which(shape_brasil_df$ESTADOS == input$input_state),]
    }
    else{
      map_shapes = shape_brasil_df
    }
  })
  
  # mudança/filtro nos dados do dataframe lineplot_1_data com base nos inputs do usuário (tela de gráficos)
  lineplot_data_1 <- reactive({ 
    
    lineplot_data_1 <- if (input$input_research_2 == "Nenhum") 
      lineplot_1_data[lineplot_1_data$research_name == input$input_research_1,] 
    else 
      lineplot_1_data[lineplot_1_data$research_name == input$input_research_1 | 
                        lineplot_1_data$research_name == input$input_research_2,]
    
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