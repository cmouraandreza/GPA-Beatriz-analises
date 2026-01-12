# scater plot
#1.10.2025


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

df1<- readxl::read_xlsx("Data/COPIA-Database_epizoism_hydroids-bea.xlsx")

colnames(df1)

EP-family
EP-Genera
Genera-BS
Family-BS


#familias e generos-----
familias_epi <- df1 %>%
  count(EP_family, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))     
familias_base <- df %>%
  count(BS_Family, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))
# generos
generos_epi <- df1 %>%
  count(EP_Genera, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))
generos_base <- df1 %>%
  count(BS_Genera, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))





# agora eu quero pelo numero de espécies unicas ------

# EPI-SPP UNI

familias_epi_especies <- df1 %>%
  distinct(EP_family, Scientific_name_EP) %>%         # remove duplicatas da combinação
  count(EP_family, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))

list_spp<-df1 %>% 
  distinct(Scientific_name_EP)

# BASE-SPP UNI
familias_base_especies <- df1 %>%
  distinct(BS_Family, Scientific_name_BS) %>%         # remove duplicatas da combinação
  count(BS_Family, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))

list_spp_base<-df1 %>% 
  distinct(Scientific_name_BS)


# criando o mesmo df para generos -------

#antes de prosseguir com as análises mesmo

# BASE-SPP UNI

generos_epi <- df1 %>%
  count(EP_Genera, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))

generos_base <- df1 %>%
  count(BS_Genera, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))



generos_base_especies <- df1 %>%
  distinct(BS_Genera, Scientific_name_BS) %>%         # remove duplicatas da combinação
  count(BS_Genera, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))


generos_epi_especies <- df1 %>%
  distinct(EP_Genera, Scientific_name_EP) %>%         # remove duplicatas da combinação
  count(EP_Genera, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))


#   familias_base: colunas  BS_Family                  + frequencia----

epi  <- familias_epi  %>%
  rename(family = EP_family, n_epi = frequencia)  # se já for EP_family, use rename(family = EP_family)
base <- familias_base %>%
  rename(family = BS_Family,  n_base = frequencia)

df <- full_join(epi, base, by = "family") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))

# (opcional) ordenar por total absoluto
df <- df %>%
  mutate(total = n_epi + n_base,
         family = fct_reorder(family, total, .desc = TRUE))

#escreve varias pastas do arquivo <3----

writexl::write_xlsx(
  list(
    "Resumo-frequencia-fam" = df,
    "spp-fam-epi" = list_spp,
    "num-spp-fam-epi"=familias_epi_especies,
    "spp-fam-base"=list_spp_base,
    "num-spp-fam-base"=familias_base_especies,
    "spp-gen-epi"=  generos_epi_especies,
    "spp-gen-base"=  generos_base_especies
    
  ),
  path = "data/fam-gen-freq-contg.xlsx"
)



#-----------------------------------------------------------------------------#
#
#                               PULE PARA CÁ                                  #
#                           E PLOTFY KKKKK                                    #



# 3. Plotando o grafico  comum ----


library(ggplot2)
library(forcats)  # pra usar fct_reorder()

plot.fam.base <- ggplot(familias_base, aes(
  y = frequencia,
  x = fct_reorder(BS_Family, frequencia)  # ordena pela frequência
)) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Famílias de hidroides basebiontes", x = "Família") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    text = element_text(size = 14),
    legend.position = "top"
  ) +
  geom_text(
    aes(label = ifelse(round(frequencia, 1) > 1,
                       paste0(round(frequencia, 1)), "")), 
    size = 4, 
    position = position_stack(vjust = 0.5), 
    color = "white"
  ) +
  coord_flip()  # deixa as barras horizontais

plot.fam.base

##### 



plot.fam.epi <- ggplot(familias_epi, 
                       aes(
                           y = frequencia,
                           x = fct_reorder(EP_family, frequencia)  # ordena pela frequência
)) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Famílias de hidroides epibiontes", x = "Family") +
#  title("Basebionts Families")+
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    # axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14),
    legend.position = "top"  # excluindo legenda para ficar apenas
  )+
  theme_bw() +
  geom_text(
    aes(label = ifelse(round(frequencia, 1) > 1,
                       paste0(round(frequencia, 1)), "")), 
    size = 4, 
    position = position_stack(vjust = 0.5), 
    color = "white"
  )+ coord_flip()
plot.fam.epi












plot.gen

ggsave("Plots/genera.png",
       plot = plot.gen, width = 10, height = 10, dpi = 300, units = "in")



