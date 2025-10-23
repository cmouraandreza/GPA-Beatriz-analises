# TREEMAP - ESPÉCIES UNICAS - TODO PERFEITINHO 🧠

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
library(treemapify)
library(ggplot2)
library(patchwork)
library(RColorBrewer)


df1<- readxl::read_xlsx("Data/COPIA-Database_epizoism_hydroids-bea.xlsx")#✔️



familias_epi_spp <- df1 %>%
  distinct(EP_family, Scientific_name_EP) %>%  # remove duplicatas de espécie dentro da família
  count(EP_family, name = "n_epi") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_epi))%>%
  rename(family = EP_family)  # se já for EP_family, use rename(family = EP_family)

familias_base_spp <- df1 %>%
  distinct(BS_Family, Scientific_name_EP) %>%  # remove duplicatas de espécie dentro da família
  count(BS_Family, name = "n_base") %>%    # conta quantas espécies únicas por família
  arrange(desc(n_base))%>%
  rename(family = BS_Family)



# --- 1) Padroniza nomes e junta (união de famílias) ---✔️


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

## 🎨 agora vamos plotar o treemap -----

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
  fill = family,
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
  fill = family,
  label = paste0(family, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", grow = TRUE, reflow = TRUE,
    color = "white", fontface = "bold", size = 10
  ) +
  scale_fill_manual(values = palette_fam) +
  labs(title = "Basebiontes") +
  theme_minimal() +
  theme(legend.position = "none")

#3️⃣ Juntar os dois:
plots_tree<- g_epi + g_base + plot_layout(ncol = 2)
plots_tree



# ✔️SALVANDO✔️
ggsave("Plots/treemap_sppunica-contagem.png",
       plot = plots_tree, width = 14, height = 8, dpi = 600, units = "in")
####

#        grafico de spp ----

colnames(df1)

epi_spp_count <- df1 %>%
  count(Scientific_name_EP, name = "n_epi") %>%  # conta quantas vezes cada espécie aparece
  arrange(desc(n_epi))  %>%
  rename(Species = Scientific_name_EP)  # se já for EP_family, use rename(family = EP_family)
                         # ordena da mais frequente pra menos

base_spp_count <- df1 %>%
  count(Scientific_name_BS, name = "n_base") %>%  # conta quantas vezes cada espécie aparece
  arrange(desc(n_base))%>%
  rename(Species = Scientific_name_BS)   

#

# --- 1) Padroniza nomes e junta (união de spp) ---✔️


df <- full_join(epi_spp_count, base_spp_count, by = "Species") %>%
  mutate(across(c(n_epi, n_base), ~replace_na(.x, 0)))



# fazemos a porcentagem%
df_cont <- df %>%
  group_by(grupo) %>%
  mutate(
    total_abs = sum(abs(n), na.rm = TRUE),
    percentual = 100 * n / total_abs
  ) %>%
  ungroup()

# cria uma cópia com percentual absoluto
df_long_excel <- df_long %>%
  mutate(percentual = abs(percentual))  # deixa todos os percentuais positivos

writexl::write_xlsx(df_long_excel, "especies_contagem.xlsx")
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



## 🎨 agora vamos plotar o treemap -----

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
ggsave("Plots/treemap_spp-contagem.png",
       plot = plots_tree, width = 14, height = 8, dpi = 600, units = "in")




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
