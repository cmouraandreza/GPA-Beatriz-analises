#outras formas de representar as spp compartilhadas 


setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()


# BblioteCAS----
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(writexl)
library(tidyverse)
library(devtools)
library(vegan)
library(dplyr)
library(raster)
library(reshape2)
library(extrafont)
library(treemapify)
library(patchwork)
library(RColorBrewer)
library(gridExtra)
install.packages("gridExtra")

dados<- readxl::read_xlsx("Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")#✔️
colnames(dados)

# pular - tratamento dos dados- add gen-----
dados <- dados |>
  dplyr::mutate(
    genero_ep = sub("_.*$", "", nome_ep),
    genero_basi= sub("_.*$", "", nome_basi)
  )
writexl::write_xlsx(dados, "Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")

is.na(dados$nome_basi)

# Limpa valores vazios, NA e duplicados------
  
eulerr::eulerr_options(
  margin = grid::unit(1, "mm"),
  padding = grid::unit(1, "mm"),
  composition = list(
    spacing = grid::unit(2, "mm")
  )
)

limpar_taxons <- function(x) {
    
    x <- as.character(x)
    x <- trimws(x)
    
    x <- x[
      !is.na(x) &
        x != "" &
        x != "NA"
    ]
    
    unique(x)
  }
  
  
  # Cria o diagrama de Euler
criar_euler <- function(dataset,
                        coluna_ep,
                        coluna_bs,
                        titulo,
                        extrair_genero = FALSE) {
  
  epibiontes <- limpar_taxons(
    dataset[[coluna_ep]]
  )
  
  basibiontes <- limpar_taxons(
    dataset[[coluna_bs]]
  )
  
  if (extrair_genero) {
    
    epibiontes <- unique(
      sub("\\s+.*$", "", epibiontes)
    )
    
    basibiontes <- unique(
      sub("\\s+.*$", "", basibiontes)
    )
  }
  
  ajuste <- eulerr::euler(
    list(
      Epibiontes = epibiontes,
      Basibiontes = basibiontes
    ),
    shape = "circle",
    loss = "square"
  )
  
  grafico <- plot(
    ajuste,
    
    fills = list(
      fill = c("#f2a65a", "#335c67"),
      alpha = 0.30
    ),
    
    edges = list(
      col = c("#b08968", "#0b3954"),
      lwd = 3
    ),
    
    labels = FALSE,
    
    legend = list(
      side = "bottom",
      labels = c("Epibiontes", "Basibiontes"),
      fontsize = 11,
      symbol_size = 0.8
    ),
    
    quantities = list(
      type = c("counts", "percent"),
      template = "{counts}\n({percent})",
      fontsize = 13,
      font = 2
    ),
    
    main = list(
      label = titulo,
      gp = grid::gpar(
        fontsize = 17,
        fontface = "bold"
      )
    )
  )
  
  return(grafico)
}
  
  
  grafico_especies <- criar_euler(
    dataset = dados,
    coluna_ep = "nome_ep",
    coluna_bs = "nome_basi",
    titulo = "Espécies"
  )
  
  
  grafico_generos <- criar_euler(
    dataset = dados,
    coluna_ep = "genero_ep",
    coluna_bs = "genero_basi",
    titulo = "Gêneros"
  )
  
  
  grafico_familias <- criar_euler(
    dataset = dados,
    coluna_ep = "familia_ep",
    coluna_bs = "familia_basi",
    titulo = "Famílias"
  ) 
  
  figura_horizontal <- gridExtra::arrangeGrob(
    grafico_especies,
    grafico_generos,
    grafico_familias,
    
    nrow = 1,
    ncol = 3,
    
    widths = grid::unit(
      c(1, 1, 1),
      "null"
    ),
    
    heights = grid::unit(
      1,
      "null"
    ),
    
    respect = TRUE,
    
    top = grid::textGrob(
      "Sobreposição taxonômica entre epibiontes e basibiontes",
      gp = grid::gpar(
        fontsize = 18,
        fontface = "bold"
      )
    )
  )
  
  grid::grid.newpage()
  grid::grid.draw(figura_horizontal)
  
  
# salvando ---- 
  png(
    filename = "Plots/Euler_overlap_horizontal.png",
    width = 5500,
    height = 2000,
    res = 300,
    bg = "white"
  )
  
  grid::grid.draw(figura_horizontal)
  
  dev.off()
  