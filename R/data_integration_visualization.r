if (!require(dplyr)) install.packages("dplyr")
library(dplyr)

if (!require(readr)) install.packages("readr")
library(readr)

if (!require(stringi)) install.packages("stringi")
library(stringi)

if (!require(stringr)) install.packages("stringr")
library(stringr)

if (!require(xlsx)) install.packages("xlsx")
library(xlsx)

if (!require(rgdal)) install.packages("rgdal")
library(rgdal)

if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

if(!require(RColorBrewer)) install.packages("RColorBrewer")
library(RColorBrewer)

if(!require(viridis)) install.packages("viridis")
library(viridis)

if(!require(gganimate)) install.packages("gganimate")
library(gganimate)

if(!require(ggrepel)) install.packages("ggrepel")
library(ggrepel)

if(!require(rgeos)) install.packages("rgeos")
library(rgeos)

if(!require(maptools)) install.packages("maptools")
library(maptools)

if(!require(mapproj)) install.packages("mapproj")
library(mapproj)

if(!require(gifski)) install.packages("gifski")
library(gifski)

if(!require(png)) install.packages("png")
library(png)

if(!require(av)) install.packages("av")
library(av)

if(!require(openssl)) install.packages("openssl")
library(openssl)

# reading data from csv
universities <- read.xlsx("csv/universities_after.xlsx", sheetIndex = 1, encoding = "UTF-8")
brazilian_cities <- read.csv("csv/brazilian_cities.csv", sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
brazilian_states <- read.csv("csv/brazilian_states.csv",sep = ",", stringsAsFactors = FALSE, encoding = "UTF-8")
research_names <- read.xlsx("csv/research_names.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_research <- read.xlsx("csv/university_research.xlsx", sheetIndex = 1, encoding = "UTF-8")
graduation_level <- read.xlsx("csv/graduation_level.xlsx", sheetIndex = 1, encoding = "UTF-8")
course_name <- read.xlsx("csv/course_name.xlsx", sheetIndex = 1, encoding = "UTF-8")
concentration_area <- read.xlsx("csv/concentration_area.xlsx", sheetIndex = 1, encoding = "UTF-8")
university_concentration_area <- read.xlsx("csv/university_concentration_area.xlsx", sheetIndex = 1, encoding = "UTF-8")

# reading data from shapefile
shape_brazil <- readOGR("shapefile/Brazil.shp", "Brazil", encoding = "latin1")

# joining data into one dataframe
all_data <- universities %>% left_join(brazilian_cities, by = c("city_id" = "ibge_code"))
all_data <- all_data %>% left_join(brazilian_states, by = c("state_id" = "state_id"))
all_data <- all_data %>% left_join(university_research, by = c("id" = "university_id"))
all_data <- all_data %>% left_join(research_names, by = c("research_id" = "id"))
all_data <- all_data %>% left_join(graduation_level, by = c("level" = "id"))
all_data <- all_data %>% left_join(course_name, by = c("course" = "id"))
all_data <- all_data %>% left_join(university_concentration_area, by = c("id" = "university_id"))
all_data <- all_data %>% left_join(concentration_area, by = c("concentration_area_id" = "id"))

# cleaning and preparing data
all_data <- all_data[,-9]
all_data <- all_data[,-14]
all_data <- all_data[,-16]
all_data <- all_data[,-19]
all_data$latitude <- as.numeric(as.character(all_data$latitude))
all_data$longitude <- as.numeric(as.character(all_data$longitude))
all_data$grade <- as.numeric(as.character(all_data$grade))

# possible values for CAPES concept 
mybreaks <- as.numeric(c(3, 4, 5, 6, 7))

# general map theme and labs
theme <- theme(axis.text = element_text(size = 16),
              plot.caption = element_text(size = 12, face = "bold"),
              axis.title = element_text(size = 18, face = "bold"),
              legend.title = element_text(size = 18), 
              legend.text = element_text(size = 14),
              legend.key.size = unit(1.0, "cm"),
              legend.key.width = unit(0.4,"cm"),
              text = element_text(color = "#22211d"), 
              legend.background = element_rect(fill = "#dddddd", color = "black"),
              panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1"),
              plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47"))

labs <- labs(x = "Longitude", y = "Latitude", caption = "Fonte: Programas de Pós-Graduação em Computação - Plataforma Sucupira.") 

# map plots
ggplot() + 
  geom_polygon( data = shape_brazil, aes(x = long, y = lat, group = group), fill = "white", size = 0.1, color = "black") +
  geom_point( data = all_data[!duplicated(all_data$id),], aes(x = longitude, y = latitude, size = grade, color = grade, alpha = I(2/(grade)))) +
  scale_color_viridis(name = "Conceito") +
  scale_size_continuous(name = "Conceito") +
  coord_map() + 
  guides( colour = guide_legend(), alpha = FALSE) +
  ggtitle("Programas de Pós-Graduação em Computação") +
  labs + theme
  
all_data$shape <- all_data$level + 14

p <- ggplot(data = all_data, aes(group = year)) +
     geom_polygon(data = shape_brazil, aes( x = long, y = lat, group = group), fill = "white", size = 0.1, color = "black") +
     geom_point(data = all_data, aes(x = longitude, y = latitude,  size = 4, color = as.factor(grade), alpha = 0.05,  group = year, shape = as.factor(shape))) +
     scale_color_manual(name = "Conceito", breaks = mybreaks, values = c("red","orange","yellow","dark green","light green")) +
     scale_shape_manual(name = "Nível", values = c(15, 17, 18, 19), labels = c("Mestrado Profissional","Mestrado","Doutorado","Mestrado + Doutorado")) +
     coord_map() +
     shadow_mark(past = TRUE) +
     guides(colour = guide_legend(override.aes = list(size = 8)), shape = guide_legend(override.aes = list(size = 8)), alpha = FALSE, size = FALSE) +
     ggtitle("Programas de Pós-Graduação em Computação em {round(frame_time,0)}") +
     transition_time(year) +
     theme + labs

#animate(p, nframes = 52, fps = 2, width = 980, height = 980, renderer = gifski_renderer("images/figure2.gif", loop = FALSE)) + ease_aes('cubic-in-out')

#animate(p, width = 1800, height = 1200, renderer = ffmpeg_renderer())
#anim_save("nations.mp4")

#animate(p, width = 980, nframes = 52, fps = 2, height = 980, renderer = av_renderer('animation.mp4'))

all_data_2 <- subset(all_data, all_data$research_name=="Inteligencia Artificial")

ggplot() + 
geom_polygon(data = shape_brazil, aes(x = long, y = lat, group = group), fill = "white", size = 0.1, color = "black") +
geom_point(data = all_data_2[!duplicated(all_data_2$id),], aes(x = longitude, y = latitude, size = as.factor(grade), color = as.factor(grade), alpha = I(2/grade))) +
scale_colour_manual(name = "Conceito", values = c("red","blue","dark green","green","yellow")) +
scale_size_manual(name = "Conceito", values = mybreaks) +
geom_text_repel(data = all_data_2[all_data_2$longitude > -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = as.factor(6)), hjust = 0, nudge_x = -34 - subset(all_data_2, all_data_2$longitude > -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) +
geom_text_repel(data = all_data_2[all_data_2$longitude <= -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = as.factor(6)), hjust = 1, nudge_x = -72 - subset(all_data_2, all_data_2$longitude <= -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) + 
coord_map() + 
guides() +
ggtitle("Programas de Pós-Graduação com Linha de Pesquisa em Inteligência Artificial") +
labs + theme

all_data_2 <- subset(all_data, all_data$concentration_area == "Teoria da Computação")

ggplot() + 
geom_polygon(data = shape_brazil, aes(x = long, y = lat, group = group), fill = "white", size = 0.15, color = "black") +
geom_point(data = all_data_2[!duplicated(all_data_2$id),], aes(x = longitude, y = latitude, size = grade, color = grade)) +
geom_text_repel(data = all_data_2[!duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = 6), hjust = 0, nudge_x = -34 - subset(all_data_2, !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) +
scale_color_viridis(name = "Conceito", breaks = mybreaks) +
scale_size_continuous(name = "Conceito", breaks = mybreaks) +
coord_map() + 
guides(colour = guide_legend(), alpha = FALSE) +
ggtitle("Programas de Pós-Graduação com Área de Concentração em Teoria da Computação") +
labs + theme

all_data_2 <- subset(all_data, all_data$graduation_level == "Mestrado e Doutorado Acadêmicos")

ggplot() + 
geom_polygon(data = shape_brazil, aes(x = long, y = lat, group = group), fill = "white", color = "black", size = 0.15) +
geom_point(data = all_data_2[!duplicated(all_data_2$id),], aes(x = longitude, y = latitude, size = grade, color = grade, shape = 19, alpha = I(2/grade))) +
geom_text_repel(data = all_data_2[all_data_2$longitude > -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = 6), hjust = 0, nudge_x = -34 - subset(all_data_2, all_data_2$longitude > -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) +
geom_text_repel(data = all_data_2[all_data_2$longitude <= -45 & !duplicated(all_data_2$code),], aes(x = longitude, y = latitude, label = code, size = 6), hjust = 1, nudge_x = -72 - subset(all_data_2, all_data_2$longitude <= -45 & !duplicated(all_data_2$code))$longitude, direction = "y", show.legend = FALSE) + 
scale_shape_identity() +
scale_color_viridis(name = "Conceito", breaks = mybreaks) +
scale_size_continuous(name = "Conceito", breaks = mybreaks) +
coord_map() + 
guides(colour = guide_legend(), size = guide_legend()) +
ggtitle("Programas de Pós-Graduação com Mestrado e Doutorado") +
labs + theme

# 
# install.packages("RPostgreSQL")
# require("RPostgreSQL")
# 
# password <- {
#   "my_password"
# }
# 
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname = "my_database",
#                  host = "localhost", port = 5432,
#                  user = "my_user", password = password)
# rm(pw) 
# 
# colnames(universities)[4] <- "course_id"
# colnames(universities)[5] <- "level_id"
# 
# dbWriteTable(con, "university", value = universities, append = TRUE, row.names = FALSE)
# 
# colnames(research_names)[2] <- "name"
# dbWriteTable(con, "research_topics", value = research_names, append = TRUE, row.names = FALSE)
# 
# dbWriteTable(con, "graduation_level", value = graduation_level, append = TRUE, row.names = FALSE)
# 
# colnames(brazilian_cities)[3] <- "latitude"
# colnames(brazilian_cities)[4] <- "longitude"
# dbWriteTable(con, "brazilian_cities", value = brazilian_cities, append = TRUE, row.names = FALSE)
# 
# dbWriteTable(con, "brazilian_states", value = brazilian_states, append = TRUE, row.names = FALSE)
# 
# dbWriteTable(con, "course_name", value = course_name, append = TRUE, row.names = FALSE)
# 
# dbWriteTable(con, "concentration_area", value = concentration_area, append = TRUE, row.names = FALSE)
# 
# colnames(university_research)[1] <- "id_university"
# colnames(university_research)[2] <- "id_research_topics"
# dbWriteTable(con, "university_research_topics", value = university_research, append = TRUE, row.names = FALSE)
# 
# colnames(university_concentration_area)[1] <- "id_university"
# colnames(university_concentration_area)[2] <- "id_concentration_area"
# dbWriteTable(con, "university_concentration_area", value = university_concentration_area, append = TRUE, row.names = FALSE)
# 
# dbDisconnect(con)
# dbUnloadDriver(drv)