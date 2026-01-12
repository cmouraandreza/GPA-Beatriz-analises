

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

df1<- readxl::read_xlsx("Data/COPIA-Database_epizoism_hydroids-bea.xlsx")

colnames(df)

EP-family
EP-Genera
Genera-BS
Family-BS



familias_epi <- df1 %>%
  count(EP_family, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))     
familias_base <- df1 %>%
  count(BS_Family, name = "frequencia") %>%   # conta por família
  arrange(desc(frequencia))

generos_epi <- df1 %>%
  count(EP_Genera, name = "frequencia") %>%   # conta por genero
  arrange(desc(frequencia))
generos_base <- df1 %>%
  count(BS_Genera, name = "frequencia") %>%   # conta por genero
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
  rename(family = EP_family, n_epi = frequencia)  # se já for EP_family, use rename(family = EP_family)
base <- familias_base %>%
  rename(family = BS_Family,  n_base = frequencia)

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

writexl::write_xlsx(df, "data/famílias_frequencias.xlsx")



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




p <- ggplot(df_long, aes(x = family, y = n)) +
  geom_col(aes(fill = grupo), 
           width = 0.75, color = "white", linewidth = 0.2) +
  
  # valores (opcional)
  
  
  geom_text(
     data = subset(df_long, n > 0 & n != 1),  # exclui valor 1
    inherit.aes = FALSE,
    aes(
      x = family,
      y = n / 2,  # posição no meio da barra positiva
      label = scales::comma(n)
    ),
    hjust = 0.5,      # centraliza o texto horizontalmente
    vjust = 0.5,      # centraliza verticalmente
    size = 4.2,
    color = "white",
    fontface = "bold" 
  ) +
  
  
  geom_text(
    data = subset(df_long, n < 0 & abs(n) != 1),  # exclui -1 e +1
    inherit.aes = FALSE,
    aes(
      x = family,
      y = n / 2,  # Posição no meio da barra negativa
      label = scales::comma(abs(n))
    ),
    hjust = 0.5,    # Centralizado horizontalmente
    vjust = 0.5,    # Centralizado verticalmente
    size = 4.2,
    color = "white",
    fontface = "bold"  # Cor clara pra destacar dentro da barra
  )+
  

  
  # 👉 nome só do lado direito
  
  
  geom_text(data = labs_right, inherit.aes = FALSE,
            aes(x = family, y = y_pos, label = family, hjust = hjust),
            vjust = .2, size = 3.8, fontface = "bold", color = "gray10") +
  
  
  scale_y_continuous(labels = function(v) scales::comma(abs(v))) +
  scale_fill_manual(values = c("Epibiontes"="darkblue","Basebiontes"="darkorange"), name=NULL) +
  coord_flip(clip = "off") +
 
 #theme_minimal(base_size = 12) +
  labs(y = "Species number", x = "Family", fill = "Regions", color = "Regions") +
  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
        axis.text.y = element_blank(), axis.title.y = element_blank(),
        


        
        plot.margin = margin(10, 40, 10, 40),
        legend.position = "bottom",            # corrigido
        legend.direction = "horizontal")
p





















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
  size = 4.2, color = "white", fontface = "bold"
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
  size = 4.2, color = "white", fontface = "bold"
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
  
  scale_fill_manual(values = c("Epibiontes"="darkblue","Basebiontes"="darkorange"), name=NULL) +
  coord_flip(clip = "off") +
  
  labs(y = "porcentagem (%)", x = "Family", fill = "Regions", color = "Regions") +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(), axis.title.y = element_blank(),
    plot.margin = margin(10, 40, 10, 40),
    legend.position = "bottom",
    legend.direction = "horizontal"
  )
g


ggsave("Plots/espelhado.png",
       plot = g, width = 14, height = 8, dpi = 600, units = "in")


