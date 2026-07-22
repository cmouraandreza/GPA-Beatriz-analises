# DISTRIBUICAO RELATIVA DAS ESPECIES

setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

library(ade4)
library(readxl)
library(tidyverse)
library(janitor)
library(dplyr)
library(naniar)
#pre-processamento -----


# recuperando tabela utilizada
#dados<-readxl::read_xlsx("Data/BaseBeat3Geral-atualizada-12.01.xlsx")
dados<- readxl::read_xlsx("Data/Database_epizoism_hydroids_BEA_2026.07.13.xlsx")#✔️

str(dados)
colnames(dados)

# fazendo manualmente -----
cols_epi<- dados%>%
  dplyr::select("BasArrSte1","TypGro1","BraTyp1" ,"HydExo1","InsHyd1","HydInsPat1", "Nem1",
         "GonPro1","SexRep1" , "SexRep1" ,"MedLifCyc1","Col1","RimThe1")
cols_base<- dados%>%
  dplyr::select("BasArrSte2","TypGro2","BraTyp2" ,"HydExo2","InsHyd2","HydInsPat2", "Nem2",
         "GonPro2","SexRep2" , "SexRep2" ,"MedLifCyc2","Col2","RimThe2")

# fazendo de forma mais automatica-----

#seleciona as colunas pelo numero
cols_epi  <- grep("1$", names(dados), value = TRUE)
cols_base <- grep("2$", names(dados), value = TRUE)

# Criar df long para EPIBIONTE
dados_epi_long <- dados %>%
  dplyr::select(all_of(cols_epi)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "atributo",
    values_to = "categoria"
  ) %>%
  mutate(
    grupo = "Epibionts",
    atributo = gsub("1$", "", atributo)
  )

# Criar df long para BASIBIONTE
dados_base_long <- dados %>%
  dplyr:: select(all_of(cols_base)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "atributo",
    values_to = "categoria"
  ) %>%
  mutate(
    grupo = "Basibionts",
    atributo = gsub("2$", "", atributo)
  )

# Unindo num df long
dados_long <- bind_rows(dados_epi_long, dados_base_long) %>%
  filter(!is.na(categoria), categoria != "")

# Limpar prefixos B- e E-
dados_long <- dados_long %>%
  mutate(
    categoria = gsub("^[BE]-", "", categoria)
  )

# AGORA CONTA AS OCORRÊNCIAS
dados_long <- dados_long %>%
  group_by(grupo, atributo, categoria) %>%
  summarise(n = n(), .groups = "drop")

# Verificar atributos únicos
unique(dados_long$atributo)

# Testar plot
library(ggplot2)

ggplot(dados_long, aes(x = grupo, fill = categoria, weight = n)) +
  geom_bar(position = "fill") +
  facet_wrap(~ atributo, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = NULL,
    y = "Distribuição relativa (%)",
    fill = "Categorias"
  ) +
  theme_classic()

# Função do plot permanece igual
# funcção sem contagem-----
plot_atributo_sctg <- function(df, nome_atributo, titulo_legenda) {
  
  df_plot <- df %>%
    dplyr::filter(atributo == nome_atributo) %>%
    dplyr::group_by(grupo, categoria) %>%
    dplyr::summarise(n_cat = sum(n), .groups = "drop") %>%
    dplyr::group_by(grupo) %>%
    dplyr::mutate(
      prop = n_cat / sum(n_cat),
      label = paste0(n_cat)   # só contagem
      # se quiser % também: paste0(n_cat, " (", round(prop*100), "%)")
    )

  ggplot(df_plot, aes(x = grupo, y = prop, fill = categoria)) +
    geom_col(width = 0.8) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 3,
      color = "black"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = NULL,
      y = "Distribuição relativa (%)",
      fill = titulo_legenda
    ) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      panel.background = element_rect(fill = "white"),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    )
}

# função com contagem -----
plot_atributo_contagem <- function(df, nome_atributo, titulo_legenda) {
  
  df_plot <- df %>%
    dplyr::filter(atributo == nome_atributo) %>%
    dplyr::group_by(grupo, categoria) %>%
    dplyr::summarise(n_cat = sum(n), .groups = "drop") %>%
    dplyr::group_by(grupo) %>%
    dplyr::mutate(
      prop = n_cat / sum(n_cat),
      label = paste0(n_cat)   # só contagem
      # se quiser % também: paste0(n_cat, " (", round(prop*100), "%)")
    )
  
  ggplot(df_plot, aes(x = grupo, y = prop, fill = categoria)) +
    geom_col(width = 0.8) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 4,
      color = "gray"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(
      x = NULL,
      y = "Distribuição relativa (%)",
      fill = titulo_legenda
    ) +
    theme_classic() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      panel.background = element_rect(fill = "white"),
      legend.position = "top",
      legend.title = element_text(face = "bold"),
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    ) 
  
}
# funcao com contagem e porcentagem-----

 
#funcao com contagem e titulo e legenda-----

cores_suaves <- c(
  "#e6d0b1", # laranja suave
  "#faba52", # bege
  "#c1ddc7", # azul lavanda
  "#bbcd77", # rosa
  "#bf9f88", # verde claro
  "#b88bad", # amarelo
  "#f8f4c4" # verde água
)

windowsFonts(
  Arial = windowsFont("Arial"),
  Calibri = windowsFont("Calibri")
)

plot_atributo <- function(df, nome_atributo, titulo) {
  
  df_plot <- df %>%
    dplyr::filter(atributo == nome_atributo) %>%
    dplyr::group_by(grupo, categoria) %>%
    dplyr::summarise(n_cat = sum(n), .groups = "drop") %>%
    dplyr::group_by(grupo) %>%
    dplyr::mutate(
      prop = n_cat / sum(n_cat),
      perc = round(prop * 100, 1),
      label = paste0(n_cat, " (", perc, "%)")
    ) %>%
    dplyr::ungroup()
  
  ggplot2::ggplot(
    df_plot,
    ggplot2::aes(x = grupo, y = prop, fill = categoria)
  ) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::geom_text(
      ggplot2::aes(label = label),
      position = ggplot2::position_stack(vjust = 0.5),
      size = 3.8,
      color = "#1c232e",
      fontface = "bold") +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format()
    ) +
    ggplot2::scale_fill_manual(
      values = cores_suaves
    ) +
    ggplot2::labs(
      title = titulo,
      x = NULL,
      y = "Relative distribution (%)",
      fill = titulo
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5
      ),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(
        color = "black",
        fill = NA
      ),
      legend.position = "right",
      legend.title = ggplot2::element_text(face = "bold"),
      axis.text.y = ggplot2::element_text(
        angle = 90,
        hjust = 0.5
      )
    )
}

plot_atributo

#criando os plots individuais 
library(patchwork)

#figura do exoesqueleto-----
#exemplo<-plot_atributo(dados_long, "HydInsPat", "(d) Hydrant insertion pattern")+
# annotate("text",x = -Inf, y = Inf, label = "(d)", hjust = -0.3,
 #          vjust = 1.3,size = 5, fontface = "bold")


a<-plot_atributo(dados_long, "BasArrSte","(a)   Basal arrangement of the stem")
b<-plot_atributo(dados_long, "TypGro", "(b)   Type of growth")
c<-plot_atributo(dados_long, "BraTyp", "(c)  Branching pattern")
d<-plot_atributo(dados_long, "HydInsPat", "(d)  Hydrant insertion pattern")
 

perissarco<-(a+b)/(c+d)+ 
  plot_layout(guides = "collect")

perissarco

ggsave("Plots/PosDefesa/distribuicao_relativa_perissarco_2026.07.22.png", 
       plot = perissarco, width = 12, height = 9, units = "in")
dev.off()

#figura da hidroteca e nemato----
e<-plot_atributo(dados_long, "InsHyd", "(a)   Hydrant insertion")
f<-plot_atributo(dados_long, "RimThe", "(b)   Hydrothecal margin")
g<-plot_atributo(dados_long, "HydExo", "(c)   hydrant Exoskeleton")
h<-plot_atributo(dados_long, "Nem","(d)   Nematophore")

hidroteca_nemato<-(e+f)/(g+h)+ 
  plot_layout(guides = "collect")
hidroteca_nemato

ggsave("Plots/PosDefesa/distribuicao_relativa_hidroteca_nemato_2026.07.22.png", 
       plot = hidroteca_nemato, width = 12, height = 9, units = "in")
dev.off()

#figura da reprodução----
i<-plot_atributo(dados_long, "GonPro","(a)   Gonophore protection")
j<-plot_atributo(dados_long, "SexRep","(b)   Reproduction")
k<-plot_atributo(dados_long, "MedLifCyc", "(c)  Medusa/medusoid life cycle")
l<-plot_atributo(dados_long, "Col","(d)   Colonialism")


reproducao <- (
  (i + j) /
    (k + l)
) +
  plot_layout(guides = "collect")

reproducao

ggsave(
  filename = "Plots/PosDefesa/distribuicao_relativa_reproducao_2026.07.22.png",
  plot = reproducao,
  width = 12,
  height = 9,
  units = "in",
  dpi = 300,
  bg = "white"
)
# plotando tudo junto ----

p<-(a+b)/(c+d)/ (e+f)/(g+h)/(i+j)+ (k+l)+ 
  plot_layout(guides = "collect")

ggsave("Plots/quadro1_distribuicao_relativa_color2.png", 
       plot = p, width = 12, height = 18, units = "in")