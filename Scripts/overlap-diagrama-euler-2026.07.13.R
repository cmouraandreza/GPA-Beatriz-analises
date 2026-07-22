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
library(eulerr)
#install.packages("gridExtra")

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
    loss = "sum_squared"
  )
  
  grafico <- plot(
    ajuste,
    
    fills = list(
      fill = c("#DBA588", "#C2CED2"),
      alpha = 0.50
    ),
    
    edges = list(
      col = c("#CB997E", "#0B3954"),
      lwd = 3
    ),
    
    labels = FALSE,
    
    legend = list(
      side = "bottom",
      labels = c("Epibionts", "Basibionts"),
      fontsize = 12,
      symbol_size = 0.8
    ),
    
    quantities = list(
      type = c("counts", "percent"),
      template = "{counts}\n({percent})",
      fontsize = 14,
      font = 2
    ),
    
    main = list(
      label = titulo,
      gp = grid::gpar(
        fontsize = 18,
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
    titulo = "(a) Species"
  )
  
  
  grafico_generos <- criar_euler(
    dataset = dados,
    coluna_ep = "genero_ep",
    coluna_bs = "genero_basi",
    titulo = "(b) Genus"
  )
  
  
  grafico_familias <- criar_euler(
    dataset = dados,
    coluna_ep = "familia_ep",
    coluna_bs = "familia_basi",
    titulo = "(c) Family"
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
      "Taxonomic overlap between epibionts and basibionts",
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
    filename = "Plots/Euler_overlap_horizontal_eng.2.png",
    width = 5500,
    height = 2000,
    res = 300,
    bg = "white"
  )
  
  grid::grid.draw(figura_horizontal)
  
  dev.off()
# overlap de espécies ------
  
  # Overlap de espécies
  overlap_especies <- intersect(
    limpar_taxons(dados$nome_ep),
    limpar_taxons(dados$nome_basi)
  ) |>
    sort()
  
  
  # Overlap de gêneros
  overlap_generos <- intersect(
    limpar_taxons(dados$genero_ep),
    limpar_taxons(dados$genero_basi)
  ) |>
    sort()
  
  
  # Overlap de famílias
  overlap_familias <- intersect(
    limpar_taxons(dados$familia_ep),
    limpar_taxons(dados$familia_basi)
  ) |>
    sort()
  
  
  print(overlap_especies)
  
  overlap_generos
  overlap_familias
  
  resumo_overlap <- data.frame(
    Nivel_taxonomico = c("Espécies", "Gêneros", "Famílias"),
    Numero_compartilhado = c(
      length(overlap_especies),
      length(overlap_generos),
      length(overlap_familias)
    )
  )
  
  resumo_overlap

  
  # resumo geral----
  
  
  # Espécies
  especies_ep <- limpar_taxons(dados$nome_ep)
  especies_bs <- limpar_taxons(dados$nome_basi)
  
  # Gêneros
  generos_ep <- limpar_taxons(dados$genero_ep)
  generos_bs <- limpar_taxons(dados$genero_basi)
  
  # Famílias
  familias_ep <- limpar_taxons(dados$familia_ep)
  familias_bs <- limpar_taxons(dados$familia_basi)
  
  
  resumo_geral <- data.frame(
    
    Nivel_taxonomico = c(
      "Espécies",
      "Gêneros",
      "Famílias"
    ),
    
    Total_epibiontes = c(
      length(especies_ep),
      length(generos_ep),
      length(familias_ep)
    ),
    
    Total_basibiontes = c(
      length(especies_bs),
      length(generos_bs),
      length(familias_bs)
    ),
    
    Compartilhados = c(
      length(overlap_especies),
      length(overlap_generos),
      length(overlap_familias)
    ),
    
    Exclusivos_epibiontes = c(
      length(setdiff(especies_ep, especies_bs)),
      length(setdiff(generos_ep, generos_bs)),
      length(setdiff(familias_ep, familias_bs))
    ),
    
    Exclusivos_basibiontes = c(
      length(setdiff(especies_bs, especies_ep)),
      length(setdiff(generos_bs, generos_ep)),
      length(setdiff(familias_bs, familias_ep))
    )
  )
  
  resumo_geral
  
  
# spp, gen e fam sem overlap----
  
  # Espécies exclusivas
  especies_exclusivas_ep <- setdiff(especies_ep, especies_bs)
  especies_exclusivas_bs <- setdiff(especies_bs, especies_ep)
  
  # Gêneros exclusivos
  generos_exclusivos_ep <- setdiff(generos_ep, generos_bs)
  generos_exclusivos_bs <- setdiff(generos_bs, generos_ep)
  
  # Famílias exclusivas
  familias_exclusivas_ep <- setdiff(familias_ep, familias_bs)
  familias_exclusivas_bs <- setdiff(familias_bs, familias_ep)
  # salvando ----
  dir.create(
    "Resultados",
    showWarnings = FALSE,
    recursive = TRUE
  )
  
  writexl::write_xlsx(
    list(
      
      Resumo = resumo_geral,
      
      Especies_compartilhadas = data.frame(
        Especie = sort(overlap_especies)
      ),
      
      Especies_exclusivas_EP = data.frame(
        Especie = sort(especies_exclusivas_ep)
      ),
      
      Especies_exclusivas_BS = data.frame(
        Especie = sort(especies_exclusivas_bs)
      ),
      
      Generos_compartilhados = data.frame(
        Genero = sort(overlap_generos)
      ),
      
      Generos_exclusivos_EP = data.frame(
        Genero = sort(generos_exclusivos_ep)
      ),
      
      Generos_exclusivos_BS = data.frame(
        Genero = sort(generos_exclusivos_bs)
      ),
      
      Familias_compartilhadas = data.frame(
        Familia = sort(overlap_familias)
      ),
      
      Familias_exclusivas_EP = data.frame(
        Familia = sort(familias_exclusivas_ep)
      ),
      
      Familias_exclusivas_BS = data.frame(
        Familia = sort(familias_exclusivas_bs)
      )
    ),
    
    path = "Resultados/overlap_taxonomico.xlsx"
  )