# aviso - tem que abrir grafico de barras espelhado- p funcionar



#install.packages("treemapify")
library(treemapify)
library(ggplot2)

ggplot(df_long, aes(
  area = abs(n),            # tamanho proporcional ao número de indivíduos
  fill = grupo,             # cor por grupo
  label = paste0(family, "\n", abs(n), " (", round(abs(percentual), 1), "%)")
)) +
  geom_treemap(color = "white") +
  geom_treemap_text(
    place = "centre", 
    grow = TRUE, 
    reflow = TRUE,
    color = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = c("Epibiontes" = "darkblue", "Basebiontes" = "darkorange")) +
  labs(fill = "Grupo") +
  theme_minimal()


#  teste 2


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

# 2️⃣ Gerar os treemaps com fill = family e scale_fill_manual(values = palette_fam)
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
  labs(title = "Epibiontes") +
  theme_minimal() +
  theme(legend.position = "none")

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
  labs(title = "Basebiontes") +
  theme_minimal() +
  theme(legend.position = "none")

#3️⃣ Juntar os dois:
plots_tree<- g_epi + g_base + plot_layout(ncol = 2)
plots_tree


ggsave("Plots/treemap.png",
       plot = plots_tree, width = 14, height = 8, dpi = 600, units = "in")

