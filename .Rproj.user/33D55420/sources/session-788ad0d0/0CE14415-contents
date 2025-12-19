# outras analises-  overlap de spp 

#12.12.2025


setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BblioteCAS----
library(readr)
library(dplyr)
library(ggplot2)
library(geobr)
library(ggspatial)
library(rnaturalearth)
library(sf)
library(gridExtra)
library(writexl)
library(tidyverse)
library(devtools)
library(rnaturalearthhires)
library(vegan)
library(dplyr)
library(raster)
library(reshape2)
library(extrafont)
library(tidyverse)

df1<- readxl::read_xlsx("Data/COPIA-Database_epizoism_hydroids-bea.xlsx")
colnames(df1
         )
library(vegan)


df2<-dplyr::select(df1,"Scientific_name_EP", "Scientific_name_BS")

# comparando as espécies entre epibiontes e basebiontes

library(vegan)

# listas únicas
epi <- unique(df1$Scientific_name_EP)
base <- unique(df1$Scientific_name_BS)

# remove NAs
as.data.frame(epi <- epi[!is.na(epi)])
as.data.frame(base <- base[!is.na(base)])

# universo de espécies
sp_all <- sort(unique(c(epi, base)))

# matriz presença/ausência: linhas = grupos, colunas = espécies
mat <- rbind(
  Epibionte  = as.integer(sp_all %in% epi),
  Basibionte = as.integer(sp_all %in% base)
)

# distância de Jaccard (0 = igual, 1 = totalmente diferente)
dist_jaccard <- vegdist(mat, method = "jaccard", binary = TRUE)
dist_jaccard

# se quiser a similaridade (1 - distância)
sim_jaccard <- 1 - as.numeric(dist_jaccard)
sim_jaccard

# ver  quanto da de overlap
overlap <- length(intersect(epi, base)) / length(union(epi, base))
overlap

#quero ver quais espécies estão fazendo o overlap
# espécies que aparecem nos dois papéis
overlap_species <- intersect(epi, base)

# visualizar
as.data.frame(overlap_species)

epi_exclusivas <-as.data.frame( setdiff(epi, overlap_species))
base_exclusivas <- as.data.frame(setdiff(base, overlap_species))

