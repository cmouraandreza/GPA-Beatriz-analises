# Rede bipartida - recorte de generos 26/06

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

#2026.07.21 
# tem que fazer outra estração de dados

# subir os dados
exemplo<- readxl::read_xlsx("Data/matriz-bipartida-gen-fam.xlsx")
df<- readxl::read_xlsx("Data/analise-bea-espec-selecionadas.xlsx")

colnames(df)
# vamos fazer uma tabela dessa de presença com o df1
tabela_gen<-df %>%
  dplyr::select(espc_epi, gen_basi) %>%
  distinct() %>%   # remove duplicatas
  mutate(presenca = 1) %>%
  pivot_wider(
    names_from = gen_basi,
    values_from = presenca,
    values_fill = 0
  )

# Transformar a primeira coluna em nomes das linhas
dados_gen <- column_to_rownames(tabela_gen, var = "espc_epi")

# Converter para matriz
matriz_gen <- as.matrix(dados_gen)

# Visualizar a rede bipartida
bipartite::plotweb(matriz_gen)

# Selecionando os 10 gêneros ----
# soma total de ocorrências por família
peso_gen <- colSums(matriz_gen)

# nomes das top 10 famílias
top10_gen <- names(sort(peso_gen, decreasing = TRUE))[1:10]

# matriz filtrada só com top 10 famílias
matriz_top10_gen <- matriz_gen[, top10_gen]

matriz_top10_gen <- t(matriz_top10_gen)

# fazendo o plot ----

#criando e concatenando cores discretas
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
library(bipartite)

png("Plots/rede_bipartite_gen_26.06.png", width = 10, height = 8, units = "in", res = 300)
svg("Plots/rede_bipartite_gen_26.06_svg.svg", width = 10, height = 8)

bipartite::plotweb(
  matriz_top10_gen,
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

dev.off()



#--------------------------------------#
# matriz filtrada só com top 10 famílias
matriz_top10_gen <- matriz_gen[, top10_gen]

matriz_top10_gen <- t(matriz_top10_gen)

# fazendo o plot ----

#criando e concatenando cores discretas
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
library(bipartite)
coll.names(tabela_gen)
colnames(tabela_gen)


ordem_basi <- c(
  "Sertularella",
  "Aglaophenia",
    "Lytocarpia",
  "Streptocaulus",
  "Nemertesia",
  "Monostaechas",
  "Plumularia",
  "Halecium",
  "Eudendrium",
  "Symplectoscyphus")# coloque aqui sua ordem completa


matriz_top10_gen_ord <- matriz_top10_gen[ordem_basi, ]
cores_basi <- c(
  "Sertularella"      = "#FFBE7D",
  "Aglaophenia"       = "#F28E2B",
  "Lytocarpia"        = "#F28E2B",
  "Streptocaulus"     = "#F28E2B",
  "Nemertesia"        = "#BAB0AC",
  "Monostaechas"      = "#BAB0AC",
  "Plumularia"        = "#BAB0AC",
  "Halecium"          = "#59A14F",
  "Eudendrium"        = "#A0CBE8",
  "Symplectoscyphus"  = "#EDC948"
)

bipartite::plotweb(
  matriz_top10_gen_ord,
  horizontal = TRUE,
  srt = 0,
  text_size = 1.5,
  lower_italic = FALSE,
  higher_italic = TRUE,
  curved_links = TRUE,
  higher_color = "steelblue",
  lower_color = cores_basi,
  link_color = "grey70",
  link_alpha = 0.7)


# tentando plotar com cor----
png("Plots/rede_bipartite_gen_02.07_col.png", width = 10, height = 8, units = "in", res = 300)

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
# Ordem desejada dos basibiontes, de cima para baixo no gráfico
ordem_basi_plot <- c(
  "Sertularella",
  "Aglaophenia",
  "Lytocarpia",
  "Streptocaulus",
  "Nemertesia",
  "Monostaechas",
  "Plumularia",
  "Halecium",
  "Eudendrium",
  "Symplectoscyphus")

# Mantém só nomes que existem na matriz
ordem_basi_plot <- ordem_basi_plot[ordem_basi_plot %in% rownames(matriz_top10_gen)]

# Ordem dos epibiontes, de cima para baixo no gráfico
#ordem_epi_plot <- colnames(matriz_top10_gen)
ordem_epi_plot <- ordem_epi_plot[ordem_epi_plot %in% colnames(matriz_top10_gen)]

# IMPORTANTE:
# plotweb(horizontal = TRUE) inverte internamente a ordem.
# Por isso usamos rev() aqui para que no gráfico saia na ordem desejada.
matriz_plot <- matriz_top10_gen[
  ordem_basi_plot,
 ordem_epi_plot
]

# Cores dos basibiontes
cores_basi <- c(
  "Sertularella"      = "#FFBE7D",
  "Aglaophenia"       = "#F28E2B",
  "Lytocarpia"        = "#F28E2B",
  "Streptocaulus"     = "#F28E2B",
  "Nemertesia"        = "#BAB0AC",
  "Monostaechas"      = "#BAB0AC",
  "Plumularia"        = "#BAB0AC",
  "Halecium"          = "#59A14F",
  "Eudendrium"        = "#A0CBE8",
  "Symplectoscyphus"  = "#EDC948"
)

# Cores do lado direito, isto é, basibiontes
cores_basi_plot <- cores_basi[rownames(matriz_plot)]

# Matriz de cores para as linhas: cada linha recebe a cor do basibionte
link_cols <- matrix(
  rep(cores_basi_plot, times = ncol(matriz_plot)),
  nrow = nrow(matriz_plot),
  ncol = ncol(matriz_plot),
  dimnames = dimnames(matriz_plot)
)

# Plot
bipartite::plotweb(
  matriz_plot,
  sorting = "normal",
  horizontal = TRUE,
  curved_links = TRUE,
  
  # esquerda = epibiontes
  higher_color = "steelblue",
  higher_italic = TRUE,
  higher_text_color = "black",
  
  # direita = basibiontes
  lower_color = cores_basi_plot,
  lower_italic = FALSE,
  lower_text_color = "black",
  
  # linhas coloridas pela cor do basibionte
  link_color = link_cols,
  link_alpha = 0.45,
  
  text_size = 1.2,
  box_size = 0.08,
  mar = c(2, 4, 5, 4)
)

# Títulos dos lados
text(
  x = -0.14, y = 1.05,
  labels = "Epibiontes",
  font = 2,
  cex = 1.8,
  xpd = TRUE
)

text(
  x = 1.14, y = 1.05,
  labels = "Basibiontes",
  font = 2,
  cex = 1.8,
  xpd = TRUE
)
dev.off()

