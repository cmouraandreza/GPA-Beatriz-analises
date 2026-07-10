install.packages("bipartite")



setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BblioteCAS----
library(readr)
library(dplyr)

library(writexl)
library(tidyverse)
library(devtools)
library(vegan)
library(dplyr)
library(extrafont)
library(tidyverse)
library(bipartite)
library(readxl)
library(tibble)

df1<- readxl::read_xlsx("Data/matriz-bipartida-gen-fam.xlsx")


# Transformar a primeira coluna em nomes das linhas
dados <- column_to_rownames(df1, var = "BS_Family")

# Converter para matriz
matriz <- as.matrix(dados)

# Conferir o resultado
matriz

# Visualizar a rede bipartida
plotweb(matriz)

# Métricas da rede
networklevel(matriz)

# Número de interações por espécie
specieslevel(matriz)



# Salvar primeiro como PNG
png("Plots/rede_bipartida.png", width = 1400, height = 900, res = 150)

plotweb(
  matriz,
  method = "normal",
  text.rot = 90,
  col.interaction = "gray",
  bor.col.interaction = "gray"
)

dev.off()

# Criar HTML com a imagem
html <- tags$html(
  tags$head(
    tags$title("Rede Bipartida")
  ),
  tags$body(
    tags$h1("Rede Bipartida"),
    tags$img(src = "rede_bipartida.png", style = "width:100%; height:auto;")
  )
)

save_html(html, "rede_bipartida-fam-gen.html")




# exemplo do modelo -----
rede1 <- matrix(c(
  1, 1, 0,
  0, 1, 1,
  1, 0, 0
), 
nrow = 3, 
byrow = TRUE)

rownames(rede1) <- c("Planta_1", "Planta_2", "Planta_3")
colnames(rede1) <- c("Beija_flor_A", "Beija_flor_B", "Beija_flor_C")

library(bipartite)
plotweb(rede1)
networklevel(rede1)

#-------------------------------------#
Detalhes: 

O que pode ser feito aqui é a subdvisão das famílias mais abundantes para
diminuir a dimensionalidade dos dados