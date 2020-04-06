#Created by: Luciano Brum
#Last modified: 5 apr, 2020

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require(pdftools)) install.packages("pdftools")
library(pdftools)

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

pdf_path <- "../pdf/Evaluation report - Computer science.pdf"
file_pdf <- pdf_text(pdf_path) %>% read_lines()
file_pdf <- file_pdf[1098:1190]

file_pdf <- file_pdf[-(grep("Coordenação", file_pdf))]
file_pdf <- file_pdf[-(grep("Diretoria", file_pdf))]

file_pdf <- file_pdf[-(24:28)] #Remove UFBA-UNIFACS-UEFS 
file_pdf <- file_pdf[-(34:35)]
file_pdf <- file_pdf[-(75:76)]
file_pdf <- file_pdf[-(44:46)] #Remove UFSCAR-Sorocaba
file_pdf <- file_pdf[-(47:49)] #Remove USP-ICMC

file_pdf[56]<-gsub("-CA-MP", "  ", file_pdf[56]) 
file_pdf[57]<-gsub("-IN-MP", "  ", file_pdf[57]) 

file_pdf <- strsplit(file_pdf, "  ")

list_pdf <- 0
a <- 1

for(i in 1:length(file_pdf)){
    info_number <- length(file_pdf[[i]][!stri_isempty(file_pdf[[i]])])
    for(j in 1:info_number){
        list_pdf[a] <- str_trim(file_pdf[[i]][!stri_isempty(file_pdf[[i]])][j])
        a <- a + 1
    }
}

data_pdf <- data.frame(stringsAsFactors = FALSE, "Universidade" = 0, "Curso" = 0, "Nivel" = 0, "Nota" = 0)
a <- 1
for(i in seq(from = 2, to = length(list_pdf), by = 5)){
    data_pdf[a, 1] <- list_pdf[i]
    data_pdf[a, 2] <- list_pdf[i + 1]
    data_pdf[a, 3] <- list_pdf[i + 2]
    data_pdf[a, 4] <- list_pdf[i + 3]
    a <- a + 1
}

write.xlsx(data_pdf, file = "../pdf/universities_before.xlsx", col.names = TRUE, row.names = TRUE, append = FALSE)
