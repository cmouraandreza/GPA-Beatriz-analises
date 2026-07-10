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
dados<-readxl::read_xlsx("Data/BaseBeat3Geral-atualizada-12.01.xlsx")

str(dados)
colnames(dados)

# fazendo manualmente -----
cols_epi<- dados%>%
  select("BasArrSte1","TypGro1","BraTyp1" ,"HydExo1","InsHyd1","HydInsPat1", "Nem1",
         "GonPro1","SexRep1" , "SexRep1" ,"MedLifCyc1","Col1","RimThe1")
cols_base<- dados%>%
  select("BasArrSte2","TypGro2","BraTyp2" ,"HydExo2","InsHyd2","HydInsPat2", "Nem2",
         "GonPro2","SexRep2" , "SexRep2" ,"MedLifCyc2","Col2","RimThe2")

# fazendo de forma mais automatica-----

#seleciona as colunas pelo numero
cols_epi  <- grep("1$", names(dados), value = TRUE)
cols_base <- grep("2$", names(dados), value = TRUE)

# Criar df long para EPIBIONTE
dados_epi_long <- dados %>%
  select(all_of(cols_epi)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "atributo",
    values_to = "categoria"
  ) %>%
  mutate(
    grupo = "Epibiontes",
    atributo = gsub("1$", "", atributo)
  )

# Criar df long para BASIBIONTE
dados_base_long <- dados %>%
  select(all_of(cols_base)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "atributo",
    values_to = "categoria"
  ) %>%
  mutate(
    grupo = "Basibiontes",
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
plot_atributo <- function(df, nome_atributo, titulo_legenda) {
  
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

  plot_atributo <- function(df, nome_atributo, titulo_grafico, titulo_legenda) {
    
    df_plot <- df %>%
      dplyr::filter(atributo == nome_atributo) %>%
      dplyr::group_by(grupo, categoria) %>%
      dplyr::summarise(n_cat = sum(n), .groups = "drop") %>%
      dplyr::group_by(grupo) %>%
      dplyr::mutate(
        prop = n_cat / sum(n_cat),
        perc = round(prop * 100, 1),
        label = paste0(n_cat, " (", perc, "%)")
      )
    
    ggplot(df_plot, aes(x = grupo, y = prop, fill = categoria)) +
      geom_col(width = 0.8) +
      geom_text(
        aes(label = label),
        position = position_stack(vjust = 0.5),
        size = 4,
        color = "white",
        fontface = "bold",
        family = "Times New Roman"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        title = titulo_grafico,          # <<< TÍTULO DO GRÁFICO
        x = NULL,
        y = "Distribuição relativa (%)",
        fill = titulo_legenda
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5), # centralizado
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        panel.background = element_rect(fill = "white"),
        legend.position = "left",
        legend.title = element_text(face = "bold"),
        axis.text.y = element_text(angle = 90, hjust = 0.5)
      )
  }
  
#funcao com contagem e titulo e legenda-----

cores_suaves <- c(
  "#FC8D62", # laranja suave
  "#E5C494", # bege
  "#8DA0CB", # azul lavanda
  "#E78AC3", # rosa
  "#aeb4a9", # verde claro
  "#f9c74f", # amarelo
  "#c5283d" # verde água
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
    )
  
  ggplot(df_plot, aes(x = grupo, y = prop, fill = categoria)) +
    geom_col(width = 0.8) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      size = 4,
      color = "#640d14",
      fontface = "bold",
      family = "Times New Roman"
    ) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = titulo,      # <<< mesmo texto
      x = NULL,
      y = "Distribuição relativa (%)",
      fill = titulo        # <<< mesmo texto
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA),
      panel.background = element_rect(fill = "white"),
      legend.position = "right",
      legend.title = element_text(face = "bold"),
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    )+
    scale_fill_manual(values = cores_suaves)
  
}



#criando os plots individuais 
library(patchwork)

#figura do exoesqueleto-----
a<-plot_atributo(dados_long, "BasArrSte","Arranjamento basal do hidrocaule")+
  annotate("text",x = -Inf, y = Inf, label = "(a)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")
b<-plot_atributo(dados_long, "TypGro", "Tipo de crescimento")+
  annotate("text",x = -Inf, y = Inf, label = "(b)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")

c<-plot_atributo(dados_long, "BraTyp", "Tipo de ramificação")+
  annotate("text",x = -Inf, y = Inf, label = "(c)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")


d<-plot_atributo(dados_long, "HydInsPat", "Padrão de inserção do hidrante")+
  annotate("text",x = -Inf, y = Inf, label = "(d)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")

perissarco<-(a+b)/(c+d)+ 
  plot_layout(guides = "collect")

ggsave("Plots/Defesa/distribuicao_relativa_perissarco.png", 
       plot = perissarco, width = 12, height = 8, units = "in")


#figura da hidroteca e nemato----
e<-plot_atributo(dados_long, "InsHyd", "Inserção do hidrante")+
  annotate("text",x = -Inf, y = Inf, label = "(a)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")

f<-plot_atributo(dados_long, "RimThe", "Margem hidrotecal")+
  annotate("text",x = -Inf, y = Inf, label = "(b)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")

g<-plot_atributo(dados_long, "HydExo", "Exoesqueleto no hidrante")+
  annotate("text",x = -Inf, y = Inf, label = "(c)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")
h<-plot_atributo(dados_long, "Nem","Nematóforo")+
  annotate("text",x = -Inf, y = Inf, label = "(d)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")

hidroteca_nemato<-(e+f)/(g+h)+ 
  plot_layout(guides = "collect")

ggsave("Plots/Defesa/distribuicao_relativa_hidroteca_nemato.png", 
       plot = hidroteca_nemato, width = 12, height = 8, units = "in")
#figura da reprodução----
i<-plot_atributo(dados_long, "GonPro","Proteção do gonóforo")+
  annotate("text",x = -Inf, y = Inf, label = "(a)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")
j<-plot_atributo(dados_long, "SexRep","Reprodução")+
  annotate("text",x = -Inf, y = Inf, label = "(b)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")

k<-plot_atributo(dados_long, "MedLifCyc", "Medusa ou medusoide no ciclo de vida")+
  annotate("text",x = -Inf, y = Inf, label = "(c)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")
l<-plot_atributo(dados_long, "Col","Colonialismo")+
  annotate("text",x = -Inf, y = Inf, label = "(d)", hjust = -0.3,
           vjust = 1.3,size = 5, fontface = "bold")



p2<-(i+j)/(k+l)+ 
  plot_layout(guides = "collect")
p2
ggsave("Plots/Defesa/distribuicao_relativa_reproducao.png", 
       plot = p2, width = 12, height = 8, units = "in")

# plotando tudo junto ----

p<-(a+b)/(c+d)/ (e+f)/(g+h)/(i+j)+ (k+l)+ 
  plot_layout(guides = "collect")

ggsave("Plots/quadro1_distribuicao_relativa_color2.png", 
       plot = p, width = 12, height = 18, units = "in")