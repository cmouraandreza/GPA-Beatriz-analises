

#grafico de barra paralelo - frequencia - bea

#1.10.2025
# obs. 6.01 - não precisa fazer com os novos dados ok

setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BblioteCAS----
library(readr)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyverse)
library(devtools)
library(vegan)
library(dplyr)

df<- readxl::read_xlsx("Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")

colnames(df)

familia_ep
genero_ep

genero_basi
familia_basi


familias_epi <- df %>%
  count(familia_ep, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))     
familias_base <- df %>%
  count(familia_basi, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))

generos_epi <- df %>%
  count(genero_ep, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))
generos_base <- df %>%
  count(genero_basi, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))


windows(12,9)

library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(scales)

# --- 1) Padroniza nomes e junta (união de famílias) ---
# supondo:
#   familias_epi:  colunas  `EP-family` ou EP_family  + frequencia
#   familias_base: colunas  BS_Family                  + frequencia

epi  <- familias_epi  %>%
  rename(family = familia_ep, n_epi = frequencia)  # se já for EP_family, use rename(family = EP_family)
base <- familias_base %>%
  rename(family = familia_basi,  n_base = frequencia)

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
    grupo = recode(grupo, n_epi = "Epibionts", n_base = "Basibionts"),
    n = if_else(grupo == "Basibionts", -n, n)
  )

df_long <- df_long %>%
  group_by(family) %>%
  mutate(
    total_registros = sum(abs(n), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(total_registros > 15) %>%
  mutate(
    family = fct_reorder(
      family,
      total_registros,
      .desc = FALSE
    )
  )

df_long <- df_long %>%
  group_by(grupo) %>%
  mutate(
    total_abs = sum(abs(n), na.rm = TRUE),
    percentual = 100 * n / total_abs
  ) %>%
  ungroup()

#writexl::write_xlsx(df, "data/famílias_frequencias.xlsx")



#nomes das familias 
labs_right <- df_long %>%
  group_by(family) %>%
  summarise(
    y_pos = ifelse(
      any(percentual > 0),
      max(percentual) + 1.5,  # joga pra direita da barra
      min(percentual) - 1.5   # joga pra esquerda da barra negativa
    ),
    hjust = ifelse(any(percentual > 0), 0, 1)
  )



#------------------------------
# esse grafico deu certo - ficou bonito

pad <- max(abs(df_long$n), na.rm = TRUE) * 0.02

# nomes somente do lado positivo
labs_right <- df_long %>%
  group_by(family) %>%
  summarise(
    y_pos = max(abs(percentual)) + 1.5,  # coloca um pouco além da maior barra (sempre positivo)
    hjust = 0
  )



library(ggplot2)

g <- ggplot(df_long, aes(x = family, y = percentual)) +
  # barras agora representam porcentagem
  geom_col(aes(fill = grupo), width = 0.75, color = "white", linewidth = 0.2) +
  
  # --------- RÓTULOS DENTRO DAS BARRAS (positivas) ---------
geom_text(
  data = subset(df_long, percentual > 0 & abs(percentual) >= 1.5),
  inherit.aes = FALSE,
  aes(
    x = family,
    y = percentual / 2,
    label = paste0(n, " (", round(percentual, 1), "%)")
  ),
  hjust = 0.5, vjust = 0.5,
  size = 4.2, color = "gray10", fontface = "bold"
)+
  
  # --------- RÓTULOS DENTRO DAS BARRAS (negativas) ---------
geom_text(
  data = subset(df_long, percentual < 0 & abs(percentual) >= 1.5),
  inherit.aes = FALSE,
  aes(
    x = family,
    y = percentual / 2,
    label = paste0(abs(n), " (", round(abs(percentual), 1), "%)")
  ),
  hjust = 0.5, vjust = 0.5,
  size = 4.2, color = "gray10", fontface = "bold"
)+
  
  
  # --------- NOMES DAS FAMÍLIAS (lado direito) ---------

  geom_text(
    data = labs_right,
    aes(x = family, y = y_pos, label = family, hjust = hjust),
    inherit.aes = FALSE,
    vjust = 0.5,
    size = 3.8,
    fontface = "bold",
    color = "gray10"
  )+
  
  
  
  # eixo em porcentagem (mostrando sempre valor positivo no rótulo)
  scale_y_continuous(labels = function(v) paste0(abs(v), "%")) +
  
  scale_fill_manual(values = c("Epibiontes"="#eecfc4",
                               "Basebiontes"="#c0d8d8"), name=NULL) +
  coord_flip(clip = "off") +
  
  labs(y = "porcentagem (%)", x = "Family",
       fill = "Regions", color = "Regions") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    
    plot.margin = margin(10, 40, 10, 40),
    
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
g


ggsave("Plots/espelhado.png",
       plot = g, width = 14, height = 8, dpi = 600, units = "in")


#===============================================================================

#teste com as barras deitadas
library(dplyr)
library(forcats)
library(ggplot2)
library(scales)

# ------------------------------------------------------------
# DADOS PARA O GRÁFICO
# ------------------------------------------------------------

df_empilhado <- df_long %>%
  mutate(
    # Remove o sinal negativo que havia sido usado no gráfico divergente
    n = abs(n)
  ) %>%
  
  # Soma epi + basi dentro de cada família
  group_by(family) %>%
  mutate(
    total_registros = sum(n, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Mantém somente famílias com mais de 15 registros
  filter(total_registros > 15) %>%
  
  # Calcula a porcentagem dentro de cada família
  group_by(family) %>%
  mutate(
    percentual_familia = 100 * n / sum(n, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  
  # Ordena as famílias pelo número total de registros
  mutate(
    family = fct_reorder(
      family,
      total_registros,
      .desc = TRUE
    )
  )


# ------------------------------------------------------------
# GRÁFICO DE BARRAS EMPILHADAS A 100%
# ------------------------------------------------------------

g_empilhado <- ggplot(
  df_empilhado,
  aes(
    x = family,
    y = percentual_familia,
    fill = grupo
  )
) +
  
  geom_col(
    width = 0.75,
   #color = "white",
    linewidth = 0.35
  ) +
  
  geom_text(
    aes(
      label = ifelse(
        percentual_familia >= 5,
        paste0(
          n,
          " (",
          round(percentual_familia, 1),
          "%)"
        ),   ""  )
    ),
    position = position_stack(vjust = 0.5),
    angle = 0,
    color = "gray12",
    fontface = "bold",
    size = 3.6
  ) +
  coord_flip() +
  
  scale_y_continuous(
    labels = label_percent(scale = 1),
    breaks = seq(0, 100, 25),
    expand = expansion(mult = c(0, 0.01))) +
  
  scale_fill_manual(
    values = c(
      "Epibionts" = "#eecfc4",
      "Basibionts" = "#C2CED2"
    )
  ) +
  scale_colour_manual(
    values = c( "#CB997E", "#0B3954"
    )
  ) +
  
  labs(
    x = "Family",
    y = "Relative frequency within family (%)" ) +
  
  theme_minimal(base_size = 13) +
  
  theme(  panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
    
    axis.text.x = element_text(
      hjust = 1,
      size = 10  ),
    
    axis.title = element_text(
      face = "bold" ),
    
    legend.position = "bottom",
    legend.direction = "horizontal")

g_empilhado
