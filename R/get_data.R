# loading / installing packages
#sudo apt install default-jdk -> rjava package dependency
if (!require(xlsx)) install.packages("xlsx")
library(xlsx)

#sudo apt install libgdal-dev -> https://stackoverflow.com/questions/12141422/error-gdal-config-not-found
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

setwd("~/Lubrum/Graduating-Programs-Brazil")

# reading data from csv
universities <- read.xlsx("csv/universities_after.xlsx", stringsAsFactors = FALSE, sheetIndex = 1, encoding = "UTF-8")
brazilian_cities <- read.csv("csv/brazilian_cities.csv", sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
brazilian_states <- read.csv("csv/brazilian_states.csv", sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
research_names <- read.xlsx("csv/research_names.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_research <- read.xlsx("csv/university_research.xlsx", sheetIndex = 1, encoding = "UTF-8")
graduation_level <- read.xlsx("csv/graduation_level.xlsx", sheetIndex = 1, encoding = "UTF-8")
course_name <- read.xlsx("csv/course_name.xlsx", sheetIndex = 1, encoding = "UTF-8")
concentration_area <- read.xlsx("csv/concentration_area.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_concentration_area <- read.xlsx("csv/university_concentration_area.xlsx", sheetIndex = 1, encoding = "UTF-8")

# joining data into one dataframe
all_data <- inner_join(universities, brazilian_cities, by = c("city_id" = "ibge_code"))
all_data <- inner_join(all_data, brazilian_states, by = c("state_id" = "state_id"))
all_data <- inner_join(all_data, university_research, by = c("id" = "university_id"))
all_data <- inner_join(all_data, research_names, by = c("research_id" = "id"))
all_data <- inner_join(all_data, graduation_level, by = c("level" = "id"))
all_data <- inner_join(all_data, course_name, by = c("course" = "id"))
all_data <- inner_join(all_data, university_concentration_area, by = c("id" = "university_id"))
all_data <- inner_join(all_data, concentration_area, by = c("concentration_area_id" = "id"))

# cleaning and preparing data
all_data <- all_data[,-9]
all_data <- all_data[,-14]
all_data <- all_data[,-16]
all_data <- all_data[,-19]
all_data$latitude <- as.numeric(as.character(all_data$latitude))
all_data$longitude <- as.numeric(as.character(all_data$longitude))
all_data$grade <- as.numeric(as.character(all_data$grade))

# reading shapefile 
shape_brazil <- readOGR("shapefile/Brazil.shp", "Brazil", encoding = "latin1")
#s <- as(shape_brazil, "data.frame") do not work !!

# transforming shapefile to dataframe format
shape_brazil$id <- rownames(as.data.frame(shape_brazil))
coordinates_brazil <- fortify(shape_brazil, region = "id") # only coordinates
shape_brasil_df <- merge(coordinates_brazil, shape_brazil, by = "id", type = 'left') # add remaining attributes

# data from barplot 1
barplot_1_data <- as.data.frame(
  all_data %>%
  group_by(state_code) %>%
  summarise(id = n_distinct(id)) %>%
  arrange(-id) %>%
  mutate(state_code = fct_reorder(state_code, -id))
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

# map theme and labs
map_theme <- theme(
  axis.text = element_text(size = 16),
  plot.caption = element_text(size = 12, face = "bold"),
  axis.title = element_text(size = 18, face = "bold"),
  legend.title = element_text(size = 18), 
  legend.text = element_text(size = 14),
  legend.key.size = unit(1.0, "cm"),
  legend.key.width = unit(0.4,"cm"),
  text = element_text(color = "#22211d"), 
  legend.background = element_rect(fill = "#dddddd", color = "black"),
  panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
  plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47")
)

map_labs <- labs(
  x = "Longitude", 
  y = "Latitude", 
  caption = "Fonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# theme and labs from barplot 1
geral_1_theme <- theme(
  axis.text = element_text(size = 12, color = "#cccccc"),
  axis.title = element_text(size = 16, face = "bold", color = "#cccccc"),
  axis.ticks = element_line(colour = "#cccccc"),
  axis.ticks.length = unit(0.5, "cm"),
  axis.ticks.margin = unit(0.8, "cm"),
  plot.caption = element_text(size = 12, face = "bold", color = "#cccccc"),
  plot.title = element_text(size = 22, hjust = 0.5, color = "#ffffff"),
  plot.background = element_rect(fill = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = 'black'),
  legend.background = element_rect(fill = "black", color = NA),
  legend.key = element_rect(color = "gray", fill = "black"),
  text = element_text(color = "#cccccc")
)

barplot_1_labs <- labs(
  x = "\nEstados do Brasil", 
  y = "\nNúmero de Programas\n", 
  caption = "\nFonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# labs from barplot 2
barplot_2_labs <- labs(
  y = "Número de Programas de Pós-Graduação", 
  caption = "\nFonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# labs from line plot 1
lineplot_1_labs <- labs(
  x = "\nAnos", 
  y = "Número de Programas\n", 
  caption = "\nFonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira."
) 

# saving data
save.image(file = "R/data/all_data.RData")