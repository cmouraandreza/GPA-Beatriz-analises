# scater plot +TREE MAPS
#1.10.2025

# atualizado em 06.01

setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BblioteCAS----
library(readr)
library(ggplot2)
library(sf)
library(writexl)
library(tidyverse)
library(devtools)
library(vegan)
library(dplyr)
library(extrafont)

New.data<- readxl::read_xlsx("Data/atualizadada-Database_epizoism_hydroids X basibionts PARA ANÁLISE EPI e BASI.xlsx")


EpiHydSpe
EpiHydFam

BasHydSpe
BasHydFam

df1 <- New.data %>%
  mutate(
    Genus_EP = word(EpiHydSpe, 1),
    Genus_BS = word(BasHydSpe, 1)
  )%>%
rename(
  Family_EP= EpiHydFam,
  Family_BS = BasHydFam
)



colnames(New.data)

EP-family
EP-Genera
Genera-BS
Family-BS<-BasHydFam



#familias e generos-----
familias_epi <- df1 %>%
  count(Family_EP, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))     
familias_base <- df1 %>%
  count(Family_BS, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))
# generos
generos_epi <- df1 %>%
  count(Genus_EP, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))
generos_base <- df1 %>%
  count(Genus_BS, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))



writexl::write_xlsx(df1, "Data/dados-atualizados-bea-janeiro26.xlsx")

# agora eu quero pelo numero de espécies unicas ------

# EPI-SPP UNI

familias_epi_especies <- df1 %>%
  distinct(Family_EP, EpiHydSpe) %>%         # remove duplicatas da combinação
  count(Family_EP, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))

#LISTA DE ESPÉCIES ----
list_spp<-df1 %>% 
  distinct(EpiHydSpe)

# BASE-SPP UNI
familias_base_especies <- df1 %>%
  distinct(Family_BS, BasHydSpe) %>%         # remove duplicatas da combinação
  count(Family_BS, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))

list_spp_base<-df1 %>% 
  distinct(BasHydSpe)


# criando o mesmo df para generos -------

#antes de prosseguir com as análises mesmo

# BASE-SPP UNI

generos_epi <- df1 %>%
  count(Genus_EP, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))

generos_base <- df1 %>%
  count(Genus_BS, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))



generos_base_especies <- df1 %>%
  distinct(Genus_BS, BasHydSpe) %>%         # remove duplicatas da combinação
  count(Genus_BS, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))


generos_epi_especies <- df1 %>%
  distinct(Genus_EP, EpiHydSpe) %>%         # remove duplicatas da combinação
  count(Genus_EP, name = "n_especies") %>%   # conta quantas espécies únicas por família
  arrange(desc(n_especies))


#   familias_base: colunas  Family_BS                  + frequencia----

epi  <- familias_epi  %>%
  rename(family = Family_EP, n_epi = frequencia)  # se já for Family_EP, use rename(family = Family_EP)
base <- familias_base %>%
  rename(family = Family_BS,  n_base = frequencia)

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

plot.fam.base  <- familias_base%>%
  dplyr::filter(frequencia > 1) %>%   # 👈 remove gêneros com frequência 1
       
  ggplot( aes(
  y = frequencia,
  x = fct_reorder(Family_BS, frequencia)  # ordena pela frequência
)) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Famílias de hidroides basibiontes", x = "Família") +
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

#guardando
png(
  filename = "Plots/fam_basi.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)

plot.fam.base   # ou print(plot.fam.base) se for ggplot

dev.off()

#plot com familias----


plot.fam.epi <- familias_epi%>%
  dplyr::filter(frequencia > 1) %>%   # 👈 remove gêneros com frequência 1
  ggplot( aes( y = frequencia,
               x = fct_reorder(Family_EP, frequencia)  # ordena pela frequência
)) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Famílias de hidroides epibiontes", x = "Family") +
#  title("Basebionts Families")+
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    text = element_text(size = 14),
    legend.position = "top"
  )+
  geom_text(
    aes(label = ifelse(round(frequencia, 1) > 1,
                       paste0(round(frequencia, 1)), "")), 
    size = 4, 
    position = position_stack(vjust = 0.5), 
    color = "white"
  )+ coord_flip()
plot.fam.epi


#guardando
png(
  filename = "Plots/fam_epi.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)

plot.fam.epi   # ou print(plot.fam.base) se for ggplot

dev.off()

# pplotando generos -----



plot.gen.epi <- generos_epi_especies %>%
  dplyr::filter(n_especies > 1) %>%   # 👈 remove gêneros com frequência 1
  ggplot(aes(
    y = n_especies,
    x = fct_reorder(Genus_EP, n_especies)
  )) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Gêneros de hidroides epibiontes", x = "Gêneros") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    text = element_text(size = 14),
    legend.position = "top"
  ) +
  geom_text(
    aes(label = ifelse(n_especies > 1, n_especies, "")),
    size = 4,
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  coord_flip()



#guardando
png(
  filename = "Plots/gen_epi.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)

plot.gen.epi   # ou print(plot.fam.base) se for ggplot

dev.off()


# plot de genero para base
plot.gen.basi <- generos_base_especies %>%
  dplyr::filter(n_especies > 1) %>%   # 👈 remove gêneros com frequência 1
  ggplot(aes(
    y = n_especies,
    x = fct_reorder(Genus_BS, n_especies)
  )) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Gêneros de hidroides basibiontes", x = "Gêneros") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    text = element_text(size = 14),
    legend.position = "top"
  ) +
  geom_text(
    aes(label = ifelse(n_especies > 1, n_especies, "")),
    size = 4,
    position = position_stack(vjust = 0.5),
    color = "white"
  ) +
  coord_flip()

plot.gen.basi

#guardando
png(
  filename = "Plots/plot.gen.basi.png",
  width = 10,
  height = 12,
  units = "in",
  res = 300
)

plot.gen.basi   # ou print(plot.fam.base) se for ggplot

dev.off()



#-----------------------------------------------------------------------------#

# aqui faremos o tratamento dos dados - para as proximas analises----

epi  <- familias_epi  %>%
  rename(family = Family_EP, n_epi = frequencia)  # se já for EP_family, use rename(family = EP_family)
base <- familias_base %>%
  rename(family = Family_BS,  n_base = frequencia)

df <- full_join(epi, base, by = "family") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))


# (opcional) ordenar por total absoluto
df <- df %>%
  mutate(total = n_epi + n_base,
         family = fct_reorder(family, total, .desc = TRUE))

# --- 2) Long + sinal negativo para basebiontes ---
df_long <- df %>%
  pivot_longer(c(n_epi, n_base), names_to = "grupo", values_to = "n") %>%
  mutate(
    grupo = recode(grupo, n_epi = "Epibiontes", n_base = "Basebiontes"),
    n = if_else(grupo == "Basebiontes", -n, n)
  )

df_long <- df_long %>%
  group_by(grupo) %>%
  mutate(
    total_abs = sum(abs(n), na.rm = TRUE),
    percentual = 100 * n / total_abs
  ) %>%
  ungroup()


library(treemapify)
library(ggplot2)
library(patchwork)

# 1️⃣ Definir uma paleta única por família
library(RColorBrewer)

# todas as famílias envolvidas
familias_unicas <- unique(df_long$family)

# gerar paleta (pode ajustar n de cores conforme necessário)
palette_fam <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(familias_unicas)),
  familias_unicas
)

# Separar os dados
df_epi <- df_long %>% filter(grupo == "Epibiontes", abs(percentual) >= 1.5)
df_base <- df_long %>% filter(grupo == "Basebiontes", abs(percentual) >= 1.5)

# 2️⃣ Gera os treemaps com fill = family e scale_fill_manual(values = palette_fam)


#SCATER PLOT

# Epibiontes
g_epi <- ggplot(df_epi, aes(
  area = abs(n),
  fill = family,
  label = paste0(family, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Epibiontes",
       fontface = "bold", size = 22) +
  theme_minimal() +
  theme(
    legend.position = "none",
     plot.title = element_text(
                hjust = 0.5,      # centraliza
                face = "bold",    # negrito
                size = 22     ))    # tamanho da fonte)

#guardando
png(
  filename = "Plots/treeplot_fam_epi.png",
  width = 10,
  height = 10,
  units = "in",
  res = 300
)

g_epi   # ou print(plot.fam.base) se for ggplot

dev.off()

# Basebiontes
g_base <- ggplot(df_base, aes(
  area = abs(n),
  fill = family,
  label = paste0(family, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Basibiontes") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(
          hjust = 0.5,      # centraliza
          face = "bold",    # negrito
          size = 22     ))    # tamanho da fonte)

#guardando
png(
  filename = "Plots/treeplot_fam_base.png",
  width = 10,
  height = 10,
  units = "in",
  res = 300
)

g_base   # ou print(plot.fam.base) se for ggplot

dev.off()


#generos-----

### GÊNEROS ----
colnames(df1)


gen_epi_spp <- df1 %>%
  distinct(Genus_EP, EpiHydSpe) %>%  # remove duplicatas de espécie dentro da família
  count(Genus_EP, name = "n_epi") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_epi))%>%
  rename(Genera = Genus_EP)  # se já for EP_family, use rename(family = EP_family)

gen_base_spp <- df1 %>%
  distinct(Genus_BS, BasHydSpe) %>%  # remove duplicatas de espécie dentro da família
  count(Genus_BS, name = "n_base") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_base))%>%
  rename(Genera = Genus_BS)



# --- 1) Padroniza nomes e junta (união de generos) ---✔️


df <- full_join(gen_base_spp, gen_epi_spp, by = "Genera") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))

# (opcional) ordenar por total absoluto
df <- df %>%
  mutate(total = n_epi + n_base,
         Genera = fct_reorder(Genera, total, .desc = TRUE))

# --- 2) Long + sinal negativo para basebiontes ---
df_long <- df %>%
  pivot_longer(c(n_epi, n_base), names_to = "grupo", values_to = "n") %>%
  mutate(
    grupo = recode(grupo, n_epi = "Epibiontes", n_base = "Basebiontes"),
    n = if_else(grupo == "Basebiontes", -n, n)
  )



df_long <- df_long %>%
  mutate(n_abs = abs(n)) %>%
  group_by(grupo) %>%
  mutate(
    total_grupo = sum(n_abs, na.rm = TRUE),
    percentual = 100 * n_abs / total_grupo
  ) %>%
  ungroup()

## 🎨 agora vamos plotar o treemap ----

#install.packages("treemapify")
library(treemapify)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# todas as famílias envolvidas✔️
Species_unicas <- unique(df_long$Genera)

# gerar paleta (pode ajustar n de cores conforme necessário)
palette_gen <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(Species_unicas)),
  Species_unicas
)

# Separar os dados✔️
df_epi <- df_long %>% filter(grupo == "Epibiontes", abs(percentual) >= 1.5)
df_base <- df_long %>% filter(grupo == "Basebiontes", abs(percentual) >= 1.5)

# 2️⃣ Gerar os treemaps com fill = family e scale_fill_manual(values = palette_fam)
# Epibiontes✔️----
g_epi <- ggplot(df_epi, aes(
  area = abs(n),
  fill = Genera,
  label = paste0(Genera, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_gen) +
  labs(title = "Epibiontes") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(
          hjust = 0.5,      # centraliza
          face = "bold",    # negrito
          size = 22     ))

png(
  filename = "Plots/treeplot_gen_epi.png",
  width = 10,
  height = 10,
  units = "in",
  res = 300
)

g_epi   # ou print(plot.fam.base) se for ggplot

dev.off()


# Basebiontes✔️----
g_base <- ggplot(df_base, aes(
  area = abs(n),
  fill = Genera,
  label = paste0(Genera, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_gen) +
  labs(title = "Basibiontes") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(
          hjust = 0.5,      # centraliza
          face = "bold",    # negrito
          size = 22     ))
png(
  filename = "Plots/treeplot_gen_basi.png",
  width = 10,
  height = 10,
  units = "in",
  res = 300
)

g_base   # ou print(plot.fam.base) se for ggplot

dev.off()
