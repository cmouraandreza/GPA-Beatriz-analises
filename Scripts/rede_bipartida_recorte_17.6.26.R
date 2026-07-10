# Teste com espécies selecionadas por bea - 17.06


#
setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BlioteCAS----
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




# subir os dados
exemplo<- readxl::read_xlsx("Data/matriz-bipartida-gen-fam.xlsx")
df1<- readxl::read_xlsx("Data/analise-bea-espec-selecionadas.xlsx")

colnames(df1)
# vamos fazer uma tabela dessa de presença com o df1
tabela<-df1 %>%
  select(espc_epi, spec_basi) %>%
  distinct() %>%   # remove duplicatas
  mutate(presenca = 1) %>%
  pivot_wider(
    names_from = spec_basi,
    values_from = presenca,
    values_fill = 0
  )
## rapaz, os nomes ficaram sem _, será que vai dar certo?
# Transformar a primeira coluna em nomes das linhas
dados <- column_to_rownames(tabela, var = "espc_epi")

# Converter para matriz
matriz <- as.matrix(dados)

# Conferir o resultado
matriz

### plotando

# Visualizar a rede bipartida
plotweb(matriz)

# Métricas da rede
networklevel(matriz)

# Número de interações por espécie
specieslevel(matriz)


# 2. teste com espec- epi e genero de basi------

colnames(df1)

# vamos fazer uma tabela dessa de presença com o df1
tabela2<-df1 %>%
  select(espc_epi, gen_basi) %>%
  distinct() %>%   # remove duplicatas
  mutate(presenca = 1) %>%
  pivot_wider(
    names_from = gen_basi,
    values_from = presenca,
    values_fill = 0
  )
## rapaz, os nomes ficaram sem _, será que vai dar certo?
# Transformar a primeira coluna em nomes das linhas
dados2 <- column_to_rownames(tabela2, var = "espc_epi")

# Converter para matriz
matriz2 <- as.matrix(dados2)

# Conferir o resultado
matriz2


### plotando

# Visualizar a rede bipartida
plotweb(matriz2)

# Métricas da rede
networklevel(matriz2)

# Número de interações por espécie
specieslevel(matriz2)


# 3. teste -  spec de epi e fam de basi------

colnames(df1)

# vamos fazer uma tabela dessa de presença com o df1
tabela3<-df1 %>%
  select(espc_epi, fam_basi) %>%
  distinct() %>%   # remove duplicatas
  mutate(presenca = 1) %>%
  pivot_wider(
    names_from = fam_basi,
    values_from = presenca,
    values_fill = 0
  )
## rapaz, os nomes ficaram sem _, será que vai dar certo?
# Transformar a primeira coluna em nomes das linhas
dados3 <- column_to_rownames(tabela3, var = "espc_epi")

# Converter para matriz
matriz3 <- as.matrix(dados3)

# Conferir o resultado
matriz3


### plotando

# Visualizar a rede bipartida
plotweb(matriz3)

# Métricas da rede
networklevel(matriz3)

# Número de interações por espécie
specieslevel(matriz3)

# 4. teste de mudança na diagramação -----

rownames(matriz3)  # lower - epibiontes
colnames(matriz3)  # higher - basibiontes

# mudando coloração
cores_epi <- setNames(
  rainbow(nrow(matriz3)),
  rownames(matriz3)
)
library(bipartite)

bipartite::plotweb(
  matriz3,
  sorting = "normal",
  
  #srt = 90,
  text_size = 1,
  
  lower_color = cores_epi,
  higher_color = "steelblue",
  
 # lower_text_color = "black",
  #higher_text_color = "black",
 lower_italic = TRUE,
 
  link_color = "grey50",
  link_alpha = 0.3,
  
  horizontal = TRUE
)

### aqui as espécies  de epibiontes estão do lado direito----

# soma total de ocorrências por família
peso_familias <- colSums(matriz3)

# nomes das top 10 famílias
top10_familias <- names(sort(peso_familias, decreasing = TRUE))[1:10]

# matriz filtrada só com top 10 famílias
matriz_top10 <- matriz3[, top10_familias]

matriz_top10 <- t(matriz_top10)
cores_discretas <- c(
  "#4E79e9",  # azul médio
  "#A0CBE8",  # azul claro
  "#59A14F",  # verde
  "#8CD17D",  # verde claro
  "#B07AA1",  # lilás
  "#D4A6C8",  # rosa/lilás claro
  "#9C755F",  # marrom suave
  "#BAB0AC",  # cinza quente
  "#F28E2B",  # laranja suave
  "#FFBE7D",  # pêssego
  "#EDC948",  # amarelo fechado
  "#B6992D",  # mostarda
  "#76B7B2",  # azul-esverdeado
  "#86BCB6"   # teal claro
)

png("Plots/rede_bipartite_17.06.png", width = 10, height = 8, units = "in", res = 300)
svg("Plots/rede_bipartite_17.06_svg.svg", width = 10, height = 8)

bipartite::plotweb(
matriz_top10,
horizontal = TRUE,
srt = 0,
text_size = 1.5,
lower_italic = FALSE,
higher_italic = TRUE,
curved_links = TRUE,
higher_color = "steelblue",
lower_color = cores_discretas,
link_color = "grey70",
link_alpha = 0.7)

# perímetro da área do plot
#rect(xleft = 3, ybottom = 0,xright = 3, ytop = 3,border = "grey",lwd = 2, xpd = TRUE)

# títulos dos lados # não ficou bom
#text( x = 0.02, y = 1.01,labels = "Epibiontes",font = 1, cex = 1.5,xpd = TRUE)

#text( x = 0.93, y = 1.01, labels = "Basibiontes", font = 1, cex = 1.5, xpd = TRUE)

dev.off()


# versão final - 02/07 ----
# soma total de ocorrências por família
png("Plots/rede_bipartite_fam_02.07_col.png.png", width = 10, height = 8, units = "in", res = 300)

ordem_epi_plot <- c(
  "Bimeria vestita",
  "Campanularia hincksii",
  "Clytia gracilis",
  "Clytia paulensis",
  "Clytia hemisphaerica",
  "Obelia bidentata",
  "Obelia dichotoma",
  "Antennella secundaria",
  "Hebella scandens",
  "Filellum serpens",
  "Plumularia setacea",
  "Filellum serratum",
  "Amphisbetia distans",
  "Modeeria rotunda"
)


ordem_basi_plot <- c(
  "Sertulariidae",
  "Aglaopheniidae",
  "Plumulariidae",
  "Syntheciidae",
  "Campanulariidae",
  "Tubulariidae",
  "Halopterididae",
  "Haleciidae",
  "Eudendriidae",
  "Thyroscyphidae"
)

# Cores dos basibiontes
cores_basi <- c(
  "Sertulariidae"    = "#FFBE7D",
  "Aglaopheniidae"   = "#F28E2B",
  "Plumulariidae"    = "#BAB0AC",
  "Syntheciidae"     = "#9C755F",
  "Campanulariidae"  = "#D4A6C8",
  "Tubulariidae"     = "#B07AA1",
  "Halopterididae"   = "#8CD17D",
  "Haleciidae"       = "#59A14F",
  "Eudendriidae"     = "#A0CBE8",
  "Thyroscyphidae"   = "#4E79E9"
)


cores_basi_plot <- cores_basi[rownames(matriz_plot)]

matriz_plot <- matriz_top10[
  ordem_basi_plot,
 ordem_epi_plot
]


bipartite::plotweb(
  matriz_plot,
  sorting = "normal",
  horizontal = TRUE,
  curved_links = TRUE,
  
  # lado esquerdo = epibiontes
  higher_color = "steelblue",
  higher_italic = TRUE,
  higher_text_color = "black",
  # lado direito = famílias de basibiontes
  lower_color = cores_basi_plot,
  lower_italic = FALSE,
  lower_text_color = "black",
  # linhas seguindo a cor das famílias da direita
  link_color = "lower",
  link_alpha = 0.45,
  text_size = 1.2,
  box_size = 0.08,
  mar = c(2, 4, 5, 4)
)

text(
  x = -0.13, y = 1.05,
  labels = "Epibiontes",
  font = 2,
  cex = 1.8,
  xpd = TRUE
)

text(
  x = 1.13, y = 1.05,
  labels = "Basibiontes",
  font = 2,
  cex = 1.8,
  xpd = TRUE
)

dev.off()
