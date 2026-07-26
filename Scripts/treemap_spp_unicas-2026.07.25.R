# TREEMAP - ESPÉCIES UNICAS - TODO PERFEITINHO 🧠

setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BblioteCAS----
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(gridExtra)
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


df1<- readxl::read_xlsx("Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")#✔️
colnames(df1)

### FAMILIAS ------> -----
familias_epi_spp <- df1 %>%
  distinct(familia_ep, nome_ep) %>%  # remove duplicatas de espécie dentro da família
  count(familia_ep, name = "n_epi") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_epi))%>%
  rename(family = familia_ep)  # se já for familia_ep, use rename(family = familia_ep)

familias_base_spp <- df1 %>%
  distinct(familia_basi, nome_ep) %>%  # remove duplicatas de espécie dentro da família
  count(familia_basi, name = "n_base") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_base))%>%
  rename(family = familia_basi)



# ----- 1) Padroniza nomes e junta (união de famílias) ---✔️----


df <- full_join(familias_base_spp, familias_epi_spp, by = "family") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))

# (opcional) ordenar por total absoluto
df <- df %>%
  mutate(total = n_epi + n_base,
         family = fct_reorder(family, total, .desc = TRUE))

# --- 2) Long + sinal negativo para basebiontes ---
df_long <- df %>%
  pivot_longer(c(n_epi, n_base), names_to = "grupo", values_to = "n") %>%
  mutate(
    grupo = recode(grupo, n_epi = "Epibionts", n_base = "Basibionts"),
    n = if_else(grupo == "Basibionts", -n, n)
  )

df_long <- df_long %>%
  group_by(grupo) %>%
  mutate(
    total_abs = sum(abs(n), na.rm = TRUE),
    percentual = 100 * n / total_abs
  ) %>%
  ungroup()

## 🎨 agora vamos plotar o treemap ----

#install.packages("treemapify")
library(treemapify)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# todas as famílias envolvidas✔️
Species_unicas <- unique(df_long$family)

# gerar paleta (pode ajustar n de cores conforme necessário)
palette_fam <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(Species_unicas)),
  Species_unicas
)

# Separar os dados✔️
df_epi <- df_long %>% filter(grupo == "Epibionts", abs(percentual) >= 1.5)
df_base <- df_long %>% filter(grupo == "Basibionts", abs(percentual) >= 1.5)

# 2️⃣ Gerar os treemaps com fill = family e scale_fill_manual(values = palette_fam)
# Epibiontes✔️
FAM_epi <- ggplot(df_epi, aes(
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
  labs(title = "(a)  Epibionts families") +
  theme_minimal() +
  theme(legend.position = "none")

# Basebiontes✔️
GEM_base <- ggplot(df_base, aes(
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
  labs(title = "(b)  Basibionts families") +
  theme_minimal() +
  theme(legend.position = "none")

#3️⃣ Juntar os dois:
plots_tree<- g_epi + g_base + plot_layout(ncol = 2)
plots_tree



# ✔️SALVANDO✔️
ggsave("Plots/Posdefesa/suplementar-treemap-familias-2026-07-25.png",
      plot = plots_tree, width = 14, height = 8, dpi = 600, units = "in")
####
#==========================================================================#
### GÊNEROS ----
colnames(df1)


gen_epi_spp <- df1 %>%
  distinct(genero_ep, nome_ep) %>%  # remove duplicatas de espécie dentro da família
  count(genero_ep, name = "n_epi") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_epi))%>%
  rename(Genera = genero_ep)  # se já for familia_ep, use rename(family = familia_ep)

gen_base_spp <- df1 %>%
  distinct(genero_basi, nome_basi) %>%  # remove duplicatas de espécie dentro da família
  count(genero_basi, name = "n_base") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_base))%>%
  rename(Genera = genero_basi)



# --- 1) Padroniza nomes e junta (união de generos) ---✔️


df2 <- full_join(gen_base_spp, gen_epi_spp, by = "Genera") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))

# (opcional) ordenar por total absoluto
df2 <- df2 %>%
  mutate(total = n_epi + n_base,
         Genera = fct_reorder(Genera, total, .desc = TRUE))

# --- 2) Long + sinal negativo para basebiontes ---
df_long2 <- df2 %>%
  pivot_longer(c(n_epi, n_base), names_to = "grupo", values_to = "n") %>%
  mutate(
    grupo = recode(grupo, n_epi = "Epibionts", n_base = "Basibionts"),
    n = if_else(grupo == "Basibionts", -n, n)
  )



df_long2 <- df_long2 %>%
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
Species_unicas <- unique(df_long2$Genera)

# gerar paleta (pode ajustar n de cores conforme necessário)
palette_gen <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(Species_unicas)),
  Species_unicas
)

# Separar os dados✔️
df_epi2 <- df_long2 %>% filter(grupo == "Epibionts", abs(percentual) >= 1.5)
df_base2 <- df_long2 %>% 
  filter(grupo == "Basibionts",
         abs(percentual) >= 1.5)

# 2️⃣ Gerar os treemaps com fill = family e scale_fill_manual(values = palette_fam)
# Epibiontes✔️
g_epi2 <- ggplot(df_epi2, aes(
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
  labs(title = "(c)  Epibionts genera") +
  theme_minimal() +
  theme(legend.position = "none")

# Basebiontes✔️
g_base2 <- ggplot(df_base2, aes(
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
  labs(title = "(d)  Basibionts genera") +
  theme_minimal() +
  theme(legend.position = "none")

#3️⃣ Juntar os dois:
plots_tree<- g_epi + g_base + plot_layout(ncol = 2)
plots_tree



p<-(FAM_epi+GEM_base)/(g_epi2+g_base2)+ 
  plot_layout(guides = "collect")


# ✔️SALVANDO✔️
ggsave("Plots/Posdefesa/suplementar-treemap_famlias+generos-contagem-2026.07.25.png",
       plot = p, width = 14, height = 16, dpi = 300, units = "in")


#===========================================================================

# 2026.07.25 - abaixo daqui eu não mexi
### ESPÉCIES ----------------------------------------------------------------

colnames(df1)

epi_spp_count <- df1 %>%
  count(nome_ep, name = "n_epi") %>%  # conta quantas vezes cada espécie aparece
  arrange(desc(n_epi))  %>%
  rename(Species = nome_ep)  # se já for familia_ep, use rename(family = familia_ep)
                         # ordena da mais frequente pra menos

base_spp_count <- df1 %>%
  count(Scientific_name_BS, name = "n_base") %>%  # conta quantas vezes cada espécie aparece
  arrange(desc(n_base))%>%
  rename(Species = Scientific_name_BS)   

#

# --- 1) Padroniza nomes e junta (união de spp) ---✔️----

df <- full_join(epi_spp_count, base_spp_count, by = "Species") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))



# fazemos a porcentagem%
df_cont <- df %>%
  group_by(grupo) %>%
  mutate(
    total_abs = sum(abs(n), na.rm = TRUE),
    percentual = 100 * n / total_abs
  ) %>%
  ungroup()# esse nao ta funcionando
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# cria uma cópia com percentual absoluto
df_long_excel <- df_long %>%
  mutate(percentual = abs(percentual))  # deixa todos os percentuais positivos

#salvando algum bagulho
#writexl::write_xlsx(df_long_excel, "especies_contagem.xlsx")


# (opcional) ordenar por total absoluto
df <- df %>%
  mutate(total = n_epi + n_base,
         Species = fct_reorder(Species, total, .desc = TRUE))

# --- 2) Long + sinal negativo para basebiontes ---
df_long <- df %>%
  pivot_longer(c(n_epi, n_base), names_to = "grupo", values_to = "n") %>%
  mutate(
    grupo = recode(grupo, n_epi = "Epibiontes", n_base = "Basebiontes"),
    n = if_else(grupo == "Basebiontes", -n, n)
  )

# fazemos a porcentagem%
df_long <- df_long %>%
  group_by(grupo) %>%
  mutate(
    total_abs = sum(abs(n), na.rm = TRUE),
    percentual = 100 * n / total_abs
  ) %>%
  ungroup()



## 🎨 agora vamos plotar o treemap ----

#install.packages("treemapify")
library(treemapify)
library(ggplot2)
library(patchwork)
library(RColorBrewer)

# todas as famílias envolvidas✔️
Species_unicas <- unique(df_long$Species)

# gerar paleta (pode ajustar n de cores conforme necessário)
palette_fam <- setNames(
  colorRampPalette(brewer.pal(9, "Set1"))(length(Species_unicas)),
  Species_unicas
)

# Separar os dados✔️
df_epi <- df_long %>% filter(grupo == "Epibiontes", abs(percentual) >= 1.5)
df_base <- df_long %>% filter(grupo == "Basebiontes", abs(percentual) >= 1.5)

# 2️⃣ Gerar os treemaps com fill = family e scale_fill_manual(values = palette_fam)


# Epibiontes✔️
g_epi <- ggplot(df_epi, aes(
  area = abs(n),
  fill = Species,
  label = paste0(Species, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Epibiontes") +
  theme_minimal() +
  theme(legend.position = "none")

# Basebiontes✔️
g_base <- ggplot(df_base, aes(
  area = abs(n),
  fill = Species,
  label = paste0(Species, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Basibiontes") +
  theme_minimal() +
  theme(legend.position = "none")

#3️⃣ Juntar os dois:
plots_tree<- g_epi + g_base + plot_layout(ncol = 2)
plots_tree

# ✔️SALVANDO✔️
#ggsave("Plots/treemap_spp-contagem.png",
#       plot = plots_tree, width = 14, height = 8, dpi = 600, units = "in")




# 20 mais abundantes ---- não precisa plotar -----


library(dplyr)
library(ggplot2)
library(treemapify)

# 1️⃣ Seleciona as 10 mais abundantes (em ambos os grupos somados)
top10_species <- df %>%
  arrange(desc(total)) %>%
  slice_head(n = 20) %>%
  pull(Species)

# 2️⃣ Marca essas espécies dentro do dataset longo
df_long <- df_long %>%
  mutate(
    destaque = if_else(Species %in% top10_species, TRUE, FALSE)
  )

# 3️⃣ Gráfico dos epibiontes com destaque (negrito e itálico para top 10)
g_epi <- ggplot(df_long %>% filter(grupo == "Epibiontes", abs(percentual) >= 1.5),
                aes(area = abs(n), fill = Species)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    aes(
      label = ifelse(
        destaque,
        paste0("*", Species, "*\n", abs(n), " (", round(abs(percentual), 1), "%)"),
        ""
      )
    ),
    color = "white", grow = TRUE, reflow = TRUE, size = 10,
    fontface = "bold", lineheight = 0.9
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Epibiontes – 10 espécies mais abundantes") +
  theme_minimal() +
  theme(legend.position = "none")

# 4️⃣ Gráfico dos basibiontes (mesma lógica)
g_base <- ggplot(df_long %>% filter(grupo == "Basebiontes", abs(percentual) >= 1.5),
                 aes(area = abs(n), fill = Species)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    aes(
      label = ifelse(
        destaque,
        paste0("*", Species, "*\n", abs(n), " (", round(abs(percentual), 1), "%)"),
        ""
      )
    ),
    color = "white", grow = TRUE, reflow = TRUE, size = 10,
    fontface = "bold", lineheight = 0.9
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Basibiontes – 10 espécies mais abundantes") +
  theme_minimal() +
  theme(legend.position = "none")

# 5️⃣ Junta os dois gráficos
plots_tree_top10 <- g_epi + g_base + patchwork::plot_layout(ncol = 2)
plots_tree_top10


#
#       🌿 Código completo do gráfico espelhado - html -----
library(ggplot2)
library(dplyr)
library(forcats)

# Ordena as espécies pela soma total de registros
df_long <- df_long %>%
  mutate(Species = fct_reorder(Species, total, .desc = TRUE))

# gráfico base com texto customizado
g_piramide <- ggplot(df_long, aes(
  x = n, y = Species, fill = grupo,
  text = paste0(
    "<b>Espécie:</b> <i>", Species, "</i><br>",
    "<b>Grupo:</b> ", grupo, "<br>",
    "<b>Registros:</b> ", abs(n), "<br>",
    "<b>Percentual:</b> ", round(abs(percentual), 2), "%"
  )
)) +
  geom_col(width = 0.8, color = "white") +
  scale_fill_manual(values = c("Epibiontes"="#2E8B57","Basebiontes"="#E74C3C")) +
  scale_x_continuous(labels = abs) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_text(face = "italic"),
    legend.position = "bottom"
  )

# converte e limpa tooltips automáticos
g_piramide_interativo <- ggplotly(g_piramide, tooltip = "text",
                                  width = 1000, height = 3000)

for(i in seq_along(g_piramide_interativo$x$data)){
  g_piramide_interativo$x$data[[i]]$hoverinfo <- "text"
  g_piramide_interativo$x$data[[i]]$text <- g_piramide_interativo$x$data[[i]]$text
}


htmlwidgets::saveWidget(
  g_piramide_interativo,
  "Plots/grafico_piramide_spp_interativo.html",
  selfcontained = TRUE
)


# Mostra o gráfico
g_piramide
# ✔️SALVANDO✔️
ggsave("Plots/espelhado_spp-contagem.png",
       plot = g_piramide, width = 10, height = 40, dpi = 600, units = "in",
       bg = "white" )  # 👈 fundo branco garantido)


library(htmltools)

library(plotly)
library(htmlwidgets)



browseURL("Plots/grafico_piramide_spp_interativo.html")





#1) Contagem epibiontes por família de basibionte (Top N por família)----
library(dplyr)

epi_por_familia_pct <- df1 %>%
  filter(!is.na(nome_ep), !is.na(familia_basi)) %>%
  count(familia_basi, nome_ep, name = "n_ocorrencias") %>%
  group_by(familia_basi) %>%
  mutate(
    total_familia = sum(n_ocorrencias),
    percentual_na_familia = 100 * n_ocorrencias / total_familia
  ) %>%
  ungroup() %>%
  rename(
    basibionte_family = familia_basi,
    epibionte_species = nome_ep
  )

#3) Exportar pra Excel
library(writexl)

#write_xlsx(
  list(
    "Top_epibiontes_por_familia" = epi_por_familia,
    "Epibiontes_por_familia_pct" = epi_por_familia_pct,
   "Generos_autoepibiose" = genus,
   "Familias_autoepibiose" =family
  ),
  "epibiontes_por_familia_basibionte.xlsx"
)

#4) (Opcional) Gráfico rápido (top epibiontes por família)
library(ggplot2)



# em genero
dfgen <- df1 %>%
  mutate(
    EP_genus = word(nome_ep, 1),
    BS_genus = word(Scientific_name_BS, 1)
  )

auto_genus <- dfgen %>%
  filter(!is.na(EP_genus), !is.na(BS_genus)) %>%
  filter(EP_genus == BS_genus,
         nome_ep != Scientific_name_BS)

genus<-dplyr::select(auto_genus,
                     "ID", "EP_genus","BS_genus","nome_ep",
                     "Scientific_name_BS")


# em familia
auto_family <- df1 %>%
  filter(!is.na(familia_ep), !is.na(familia_basi)) %>%
  filter(familia_ep == familia_basi)
colnames(auto_family)
auto_family

family<-dplyr::select(auto_family,
                      "ID", "familia_ep","familia_basi",
                      "nome_ep","Scientific_name_BS")

# aquela pergunta desgraçada de espécies por  família de basibionte ----




#1️⃣ Contagem absoluta espécie × família
library(dplyr)
library(tidyr)

tab_epi_fam <- df1 %>%
  filter(!is.na(nome_ep), !is.na(familia_basi)) %>%
  count(nome_ep, familia_basi, name = "n") 

write.csv(tab_epi_fam, "Data/epibiontes-familiabase-tab.csv")


#2️⃣ Transformar em matriz (wide)
mat_epi_fam <-as.data.frame( tab_epi_fam %>%
  pivot_wider(
    names_from = familia_basi,
    values_from = n,
    values_fill = 0
  ))

writexl::write_xlsx(mat_epi_fam, "Data/epibiontes-familiabase.xlsx")
write.csv(mat_epi_fam, "Data/epibiontes-familiabase.csv")

#procurando os valores de na 
# - Está tudo tranquilo, sem nas
colSums(is.na(mat_epi_fam))

colunas<- mat_epi_fam[2-20] #seleciona só as colunas

  
  
  ##################################################################


mat_ca <- mat_epi_fam %>%
  column_to_rownames("nome_ep") %>%
  as.matrix()

library(vegan)

ca_res <- cca(mat_ca)
summary(ca_res)

plot(ca_res, scaling = 2)~~

#🎨 6️⃣ Gráfico bonito com ggplot (recomendado)
library(ggplot2)

scores_sp <- scores(ca_res, display = "species", scaling = 2) %>%
  as.data.frame() %>%
  rownames_to_column("Species")

scores_fam <- scores(ca_res, display = "sites", scaling = 2) %>%
  as.data.frame() %>%
  rownames_to_column("Family")

ggplot() +
  geom_point(data = scores_sp,
             aes(x = CA1, y = CA2),
             color = "steelblue", size = 2) +
  geom_text(data = scores_sp,
            aes(x = CA1, y = CA2, label = Species),
            size = 3, fontface = "italic", hjust = 0.5) +
  geom_point(data = scores_fam,
             aes(x = CA1, y = CA2),
             color = "firebrick", size = 3) +
  geom_text(data = scores_fam,
            aes(x = CA1, y = CA2, label = Family),
            size = 3.5, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Correspondence Analysis (CA)",
    x = "CA1",
    y = "CA2"
  )
#






#testanto outra coisa
library(dplyr)

edges <- df1 %>%
  filter(!is.na(nome_ep), !is.na(familia_basi)) %>%
  count(nome_ep, familia_basi, name = "weight") %>%
  rename(
    epibionte = nome_ep,
    basifamily = familia_basi
  )

#B) top N espécies por família (recomendado)
topN <- 15
edges_f <- edges %>%
  group_by(basifamily) %>%
  arrange(desc(weight), .by_group = TRUE) %>%
  slice_head(n = topN) %>%
  ungroup()
#3) Criar grafo e plotar (bipartido + labels)
library(igraph)
library(ggraph)
library(tibble)

nodes <- tibble(
  name = unique(c(edges_f$species, edges_f$basifamily)),
  type = ifelse(name %in% edges_f$basifamily, "Family", "Species")
)

g <- graph_from_data_frame(
  d = edges_f %>% rename(from = epibionte, to = family),
  vertices = nodes,
  directed = FALSE
)

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight), alpha = 0.4) +
  geom_node_point(aes(shape = type), size = 3) +
  geom_node_text(
    aes(label = name, fontface = ifelse(type == "Species", "italic", "bold")),
    repel = TRUE, size = 3
  ) +
  theme_void() +
  labs(title = "Rede: Espécies epibiontes × Famílias de basibiontes")


# upset plot graph - 2026/07/10 -----
dados_upset_fam <- full_join(
  familias_epi_spp,
  familias_base_spp,
  by = "family"
) %>%
  mutate(
    n_epi = replace_na(n_epi, 0),
    n_base = replace_na(n_base, 0),
    
    Epibiontes = n_epi > 0,
    Basibiontes = n_base > 0
  )

install.packages("ComplexUpset")
library(ComplexUpset)
library(ggplot2)

dados_upset <- dados_upset %>%
  mutate(
    categoria = case_when(
      Epibiontes & Basibiontes ~ "Epibiontes e basibiontes",
      Epibiontes & !Basibiontes ~ "Somente epibiontes",
      !Epibiontes & Basibiontes ~ "Somente basibiontes"
    )
  )
ComplexUpset::upset(
  dados_upset_fam,
  intersect = c("Epibiontes", "Basibiontes"),
  name = "Papel ecológico"
)
library(grid)
library(UpSetR)
theme(
  axis.ticks.length = unit(2, "mm")
)

