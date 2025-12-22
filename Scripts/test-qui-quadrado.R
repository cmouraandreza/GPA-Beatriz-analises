
setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# Pacotes
library(dplyr)
library(tidyr)
library(readr)
library(igraph)
library(bipartite)

# ---------------------------
# 0) CONFIG: ajuste aqui
# ---------------------------

# Se você está lendo do GitHub:
# url <- "https://raw.githubusercontent.com/SEU_USER/SEU_REPO/main/Data/epibiontes-familiabase.csv"
# df_raw <- read_csv(url, show_col_types = FALSE)
df_raw<-read.csv("Data/epibiontes-familiabase-tab.csv")
unique(df_raw$Scientific_name_EP)
head(df_raw)

nomes_separados <- separate(df_raw, 
                            col = "Scientific_name_EP", 
                            into = c("genero", "especifico"), 
                            sep = " ")




# juntar 2 colunas e se o valor de especifico for na, não substituir


nomes_separados <- nomes_separados %>%
  mutate(
    genero_inicial = paste0(substr(genero, 1, 1), "."),
    
    nome_abrev = if_else(
      is.na(especifico) | especifico == "",
      genero,
      paste(genero_inicial, especifico)
    )
  )
head(nomes_separados)

df_modif<-nomes_separados%>%
  select(nome_abrev, BS_Family, n)
unique(nomes_separados$BS_Family)


library(tidyr) 

head(df_modif)

df_test<-df_modif |>
  pivot_wider(
    names_from  = BS_Family,
    values_from = n,
    values_fill = 0# enche os na de zeros
  )

mat <- df_modif |>
  pivot_wider(
    names_from  = BS_Family,
    values_from = n,
    values_fill = 0
  )

head(mat)
mat_num <- as.matrix(mat[ , -1])


#teste de qui-quadrado
res <- chisq.test(mat_num)

res$expected |> summary()
sum(res$expected < 5)
sum(res$expected < 1)
mean(res$expected < 5)  # proporção



# teste de fisher
fisher.test(mat_num)
fisher.test(mat_num, simulate.p.value = TRUE, B = 10000)
#p-value = 0.0227
#alternative hypothesis: two.sided


#qui-quadrado
chisq.test(mat_num, simulate.p.value = TRUE, B = 10000)
# X-squared = 3238.3, df = NA, p-value = 9.999e-05
