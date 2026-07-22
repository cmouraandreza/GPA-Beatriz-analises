


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

dados<- readxl::read_xlsx("Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")#✔️
colnames(dados)



#teste -----
library(dplyr)

dados_modelo_epi <- dados %>%
  filter(
    !is.na(GonPro1),
    !is.na(LifCyc1),
    !is.na(SexRep1),
    !is.na(MedLifCyc1),
    !is.na(Col1)
  ) %>%
  count(
    GonPro1,
    LifCyc1,
    SexRep1,
    MedLifCyc1,
    Col1,
    name = "freq"
  )

dados_modelo_basi <- dados %>%
  filter(
    !is.na(GonPro2),
    !is.na(LifCyc2),
    !is.na(SexRep2),
    !is.na(MedLifCyc2),
    !is.na(Col2)
  ) %>%
  count(
    GonPro2,
    LifCyc2,
    SexRep2,
    MedLifCyc2,
    Col2,
    name = "freq"
  )
modelo_epi <- glm(
  freq ~ GonPro1 + LifCyc1 + SexRep1 + MedLifCyc1 + Col1,
  family = poisson(link = "log"),
  data = dados_modelo_epi
)

dados_col <- dados %>%
  filter(
    !is.na(Col1),
    !is.na(Col2)
  ) %>%
  count(
    Col1,
    Col2,
    name = "freq"
  )
modelo_col <- glm(
  freq ~ Col1 * Col2,
  family = poisson(link = "log"),
  data = dados_col
)

summary(modelo_col)



library(ggplot2)

ggplot(dados_col, aes(x = Col2, y = freq, fill = Col1)) +
  geom_col(position = "stack") +
  labs(
    x = "Colonialismo do basibionte",
    y = "Frequência de epibiose",
    fill = "Colonialismo do epibionte"
  ) +
  theme_bw()


#medusa_BasArrSte2----

#medusa x arranjo basal
medusa_BasArrSte2<- dados %>%
  filter(
    !is.na(MedLifCyc2),
    !is.na(BasArrSte2)
  ) %>%
  count(
    MedLifCyc2,
    BasArrSte2,
    name = "freq"
  )

modelo_medusa_medusa_BasArrSte2 <- glm(
  freq ~ MedLifCyc2 * BasArrSte2,
  family = poisson(link = "log"),
  data = medusa_BasArrSte2
)

summary(modelo_medusa_medusa_BasArrSte2)


## Criar uma função com os ajustes finos-----
tema_personalizado <- function() {
  
  font <- "sans"
  
  ggplot2::theme(
    
    # Fundo externo
    plot.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    
    # Fundo interno, atrás das barras
    panel.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    
    # Linhas da grade
    panel.grid.major =ggplot2::element_blank(),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    # Borda da área do gráfico
    panel.border = ggplot2::element_rect(
      colour = "#d3d3d3",
      fill = NA,
      linewidth = 0.5
    ),
    
    axis.ticks = ggplot2::element_blank(),
    
    # Fundo da legenda
    legend.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    
    legend.box.background = ggplot2::element_rect(
      fill = "white",
      colour = NA
    ),
    
    # Legenda embaixo
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    
    # Título
    plot.title = ggplot2::element_text(
      family = font,
      size = 11,
      face = "bold",
      hjust = 0,
      vjust = 2
    ),
    
    # Subtítulo
    plot.subtitle = ggplot2::element_text(
      family = font,
      size = 12
    ),
    
    # Rúbrica
    plot.caption = ggplot2::element_text(
      family = font,
      size = 12,
      hjust = 1
    ),
    
    # Títulos dos eixos
    axis.title = ggplot2::element_text(
      family = font,
      size = 12
    ),
    
    # Texto do eixo Y
    axis.text.y = ggplot2::element_text(
      family = font,
      angle = 90,
      hjust = 0.5,
      size = 10
    ),
    
    # Texto do eixo X
    axis.text.x = ggplot2::element_text(
      family = font,
      size = 12
    )
  )
}

#plot -   -----
medusa_x_arranj<-ggplot(medusa_BasArrSte2,
  aes(
    x = MedLifCyc2,
    y = freq,
    fill = BasArrSte2
  )
) +
  geom_col(
    position = "fill",
    colour = "#4d4d4d",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c("#f4acb7", "#a3b18a")
  ) +
  labs(
    title = "(b) Basal arrangement of the stem and Medusa/medusoid in the life cycle (Basibiont) ",
    x = "Basal arrangement of the stem (Basibiont)",
    y = "Medusa/medusoid in the life cycle (Basibiont)",
    fill = "Ecological function"
  ) +
  tema_personalizado()


medusa_x_arranj


# plot 2 ----
#medusa x exoesqueleto hidrante
medusa_exoes<- dados %>%
  filter(
    !is.na(MedLifCyc1),
    !is.na(HydExo1)
  ) %>%
  count(
    MedLifCyc1,
    HydExo1,
    name = "freq"
  )
#medusa_ciclo<-medusa_ciclo[-3, ]

modelo_medusa_exo1 <- glm(
  freq ~ MedLifCyc1 * HydExo1,
  family = poisson(link = "log"),
  data = medusa_exoes
)

summary(modelo_medusa_exo1)

# plot -----
medusa_x_exo<-ggplot(
  modelo_medusa_exo1,
  aes(
    x = HydExo1,
    y = freq,
    fill = MedLifCyc1
  )
) +
  geom_col(
    position = "fill",
    colour = "#4d4d4d",
    linewidth = 0.5
  ) +
  scale_fill_manual(
    values = c("#f4acb7", "#a3b18a")
  ) +
  labs(
    title = "(a) Medusa/medusoid in the life cycle and exoskeleton of the hydroid (Epibiont)",
    x = "Hydroid exoskeleton (Epibiont)",
    y = "Medusa/medusoid in the life cycle (Epibiont)",
    fill = "Ecological function"
  ) +
  tema_personalizado()


medusa_x_exo



graficos_juntos <- medusa_x_exo + medusa_x_arranj +
  plot_layout(ncol = 2)

ggsave(
  filename = "Plots/graficos_proporcao_andre.png",
  plot = graficos_juntos,
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)


dev.off()


# não era o mesmo gráfico
# tome outro --- mosaic plot ----
library(ggplot2)
library(ggmosaic)
library(ggmosaic)

grafico_mosaico_B <- ggplot(medusa_BasArrSte2) +
  geom_mosaic(
    aes(
      x = product(BasArrSte2),
      fill = MedLifCyc2,
      weight = freq
    ),
    colour = "#4d4d4d",
    linewidth = 0.5
  ) +
  # Título do eixo X
  ggmosaic::scale_x_productlist(
    name = "Basal arrangement of the stem (Basibiont)"
  ) +
  
  # Título do eixo Y
  ggmosaic::scale_y_productlist(
    name = "Medusa/medusoid in the life cycle (Basibiont)"
  ) +
  
  scale_fill_manual(
    values = c(
      "#F6BDC5",
      "#A3B18A"
    )
  ) +
  labs(
    title = "(b) Basal arrangement of the stem and Medusa/medusoid in the life cycle (Basibiont)",
  ) +
  tema_personalizado()+
  theme(legend.position = "none")

grafico_mosaico_B



grafico_mosaico_A <- ggplot(modelo_medusa_exo1) +
  geom_mosaic(
    aes(
      x = product(HydExo1),
      fill = MedLifCyc1,
      weight = freq
    ),
    colour = "#4d4d4d",
    linewidth = 0.5
  ) +
  # Título do eixo X
  ggmosaic::scale_x_productlist(
    name = "Hydroid exoskeleton (Epibiont)"
  ) +
  
  # Título do eixo Y
  ggmosaic::scale_y_productlist(
    name = "Medusa/medusoid in the life cycle (Epibiont)"
  ) +
  
  scale_fill_manual(
    values = c(
      "#F6BDC5",
      "#A3B18A"
    )
  ) +
  labs(
    title = "(a) Medusa/medusoid in the life cycle and exoskeleton of the hydroid (Epibiont)",
  ) +
  tema_personalizado()+
  theme(legend.position = "none")
grafico_mosaico_A

#SALVANDO
graficos_juntos2 <- grafico_mosaico_A + grafico_mosaico_B +
  plot_layout(ncol = 2)

ggsave(
  filename = "Plots/graficos_proporcao_andre_mosaico_2026.07.20.3.png",
  plot = graficos_juntos2,
  width = 14,
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)
dev.off()
