# ========================================================================
# REDE BIPARTIDA
# ESPÉCIES EPIBIONTES × FAMÍLIAS BASIBIONTES
# ========================================================================


# ------------------------------------------------------------------------
# 1. LIMPAR O AMBIENTE + DIRETÓRIO
# ------------------------------------------------------------------------

rm(list = ls())
gc()

setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")


# ------------------------------------------------------------------------
# 2. PACOTES + IMPORTAR OS DADOS
# ------------------------------------------------------------------------

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

dados<- readxl::read_xlsx("Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")#✔️

# ------------------------------------------------------------------------
# 3. SELEÇÃO DOS DADOS
# ------------------------------------------------------------------------

slct_data <- dados %>%
  dplyr::select(
    nome_ep,
    genero_basi,
    familia_basi
  ) %>%
  dplyr::mutate(
    nome_ep = stringr::str_replace_all(nome_ep, "_", " "),
    nome_ep = stringr::str_squish(nome_ep),
    familia_basi = stringr::str_squish(familia_basi)
  )
# Ja pega o bine e tira a _ e seleciona as colunas


# ========================================================================
# 4. SELEÇÃO DAS 14 ESPÉCIES EPIBIONTES COM MAIS REGISTROS
# ========================================================================

# 14 espécies com mais registros na base
top14_spp <- slct_data %>%
  dplyr::filter(!is.na(nome_ep), nome_ep != "") %>%
  dplyr::count(nome_ep, name = "n_registros", sort = TRUE) %>%
  dplyr::slice_head(n = 14) %>%
  dplyr::pull(nome_ep)


# Associações únicas dessas 14 espécies com todas as famílias
tabela_rede <- slct_data %>%
  dplyr::filter(
    nome_ep %in% top14_spp,
    !is.na(familia_basi),
    familia_basi != ""
  ) %>%
  dplyr::distinct(nome_ep, familia_basi) %>%
  dplyr::mutate(presenca = 1) %>%
  tidyr::pivot_wider(
    names_from = familia_basi,
    values_from = presenca,
    values_fill = 0
  )
# ========================================================================
# 5. CRIAÇÃO DA TABELA DE ASSOCIAÇÕES
# ========================================================================
matriz_rede <- tabela_rede %>%
  tibble::column_to_rownames("nome_ep") %>%
  as.matrix()

n_especies_por_familia <- colSums(matriz_rede > 0)

sort(n_especies_por_familia, decreasing = TRUE)

# o criterio de inclusão foi 6 espécies e ficaram 12 espécies
familias_manter <- names(
  n_especies_por_familia[n_especies_por_familia >= 6] ###
)

# Criar uma matriz reduzida contendo:
# - somente as 14 espécies epibiontes selecionadas;
# - somente as famílias que possuem associação
#   com pelo menos seis dessas espécies.
matriz_rede_reduzida <- matriz_rede[
  top14_spp,
  familias_manter,
  drop = FALSE
]

# Ordenar as famílias de modo decrescente
ordem_familias <- names(
  sort(
    colSums(matriz_rede_reduzida > 0),
    decreasing = TRUE
  )
)

# ------------------------------------------------------------------------
# 10. ORDEM MANUAL DAS ESPÉCIES E FAMÍLIAS
# ------------------------------------------------------------------------
ordem_epi_desejada <- c(
  "Bimeria vestita", 
  "Campanularia hincksii",
  "Clytia hemisphaerica",
  "Clytia gracilis",
  "Clytia paulensis" ,
  "Obelia bidentada" ,
  "Obelia dichotoma",
  "Antennella secundaria",
  "Hebella scandens",
  "Filellum serratum",
  "Filellum serpens",
  "Plumularia setacea" ,
  "Amphisbetia distans"  ,
  "Modeeria rotunda" 
)

ordem_basi_desejada <- c(
  "Aglaopheniidae", "Sertulariidae", "Sertularellidae",   
  "Plumulariidae" ,"Halopterididae","Campanulariidae" ,  
  "Tubulariidae" ,"Haleciidae" ,"Symplectoscyphidae",
  "Eudendriidae",  "Thyroscyphidae",   "Syntheciidae"
)
# ordem das familias
matriz_rede_reduzida <- matriz_rede_reduzida[
  ordem_epi_desejada,
  ordem_basi_desejada,
  drop = FALSE
]

# Transpor para o plotweb
matriz_plot <- t(matriz_rede_reduzida)

rownames(matriz_plot)

# ------------------------------------------------------------------------
# 12. CORES DAS FAMÍLIAS BASIBIONTES
# ------------------------------------------------------------------------
cores_basi <- c(
  "Aglaopheniidae"     = "#f06b50",
  "Sertulariidae"      = "#d17878", 
  "Sertularellidae"    = "#FFADAD",   
  "Plumulariidae"      = "#ae8fba",
  "Halopterididae"     = "#accec0",
  "Campanulariidae"    = "#8eb2c5",  
  "Tubulariidae"       = "#53a08e",
  "Haleciidae"         = "#7c8071",
  "Symplectoscyphidae" = "#edb552",
  "Eudendriidae"       = "#c8b080", 
  "Thyroscyphidae"     = "#e1d772", 
  "Syntheciidae"       = "#eedd99"
)

cores_basi_plot <- cores_basi[rownames(matriz_plot)]


# ------------------------------------------------------------------------
# 13.  DESENHAR O GRÁFICO
# ------------------------------------------------------------------------
png("Plots/rede_bipartite_fam_07.22.png",
    width = 8, #largura
    height = 7, #altura
    units = "in", res = 300)
bipartite::plotweb(
  matriz_plot,
  sorting = "normal",
  horizontal = TRUE,
  curved_links = TRUE,
  
  # lado esquerdo = epibiontes
  higher_color = "gray65",
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
  x = -0.13,
  y = 1.05,
  labels = "Epibionts",
  font = 2,
  cex = 1.2,
  xpd = TRUE)


text(
  x = 1.13,
  y = 1.05,
  labels = "Basibionts",
  font = 2,
  cex = 1.2,
  xpd = TRUE)

text(
  x = -0.09,
  y = 1.13,
  labels = "(a) Links between epibionts and basibiont families",
  font = 2,
  cex = 1.3,
  xpd = TRUE)

#plot por assim dizer -----

dev.off()
