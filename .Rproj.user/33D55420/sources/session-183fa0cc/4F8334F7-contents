# outras analises-  teste de qui-quadrado e fisher 

#23.12.2025


setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")
getwd()

# BblioteCAS----
library(readr)
library(sf)
library(writexl)
library(tidyverse)
library(devtools)
library(vegan)
library(reshape2)

df1<- readxl::read_xlsx("Data/COPIA-Database_epizoism_hydroids-bea.xlsx")
head(df1)
df2<-dplyr::select(df1,"Scientific_name_EP", "EP_family", "Scientific_name_BS","BS_Family")
head(df2)
df2

# transformando o df em formato long

df_long <- df1 |>
  select(EP_family, BS_Family) |>
  pivot_longer(
    cols = c(EP_family, BS_Family),
    names_to = "role", #cria a coluna role- basi e epi
    values_to = "family"
  ) |>
  mutate(
    role = ifelse(role == "EP_family", "Epibionte", "Basibionte")
  )

#contagem das repeticoes

df_count <- df_long |>
  count(family, role)

#transfome em wider - separando basi e epi- matriz de contigência

df_wider <- df_count |>
  pivot_wider(names_from  = role,
              values_from = n,
              values_fill = 0) 

# Matriz para o teste - transformando em matriz- abundancia
mat_num <- df_wider |>
  column_to_rownames("family") |>
  as.matrix()

# 🧪 Teste de fisher simulado com permutações

fisher.test(mat_num, simulate.p.value = TRUE, B = 10000)

#-------------------------------------------------------------------------------
#resultado
#data:  mat_num
#p-value = 9.999e-05
#alternative hypothesis: two.sided
#p-valor veio de simulação (Monte Carlo)
#Existe associação entre família e papel ecológico.

#As famílias de hidroides diferiram significativamente em seus papéis ecológicos,
#com uma distribuição não aleatória entre funções de epibionte e basibionte (χ²,
#Monte Carlo p < 0,001).
#-------------------------------------------------------------------------------
# qui-quadrado -----
count<-table(df_wider$Epibionte, df_wider$Basibionte)

chisq.test(count)

# erros associados ao qui-quadrado normal - não funcionou aqui:

# muitas famílias raras;
# muitas células com zero observado;
# totais marginais pequenos;
# frequências esperadas muito baixas (muitas < 1).
# não é um erro, mas o teste normal não aguenta tantos zeros


# qui-quadrado simulado-----

chisq.test(mat_num, simulate.p.value = TRUE, B = 10000)

#df= na - os graus de liberdade não foram usados, pois a simulação  vem do monte-carlo
#os graus de liberdade não são usados para calcular o p-valor

#--- - ---------------------------------------------------------------------------
#resultado
#data:  mat_num
#X-squared = 165.61, df = NA, p-value = 9.999e-05
# não é ao acaso

#--- - ---------------------------------------------------------------------------

#residuos do qui-quadrado simulado----

res <- chisq.test(mat_num, simulate.p.value = TRUE, B = 10000)

res_std <- res$stdres
res_std

#extraindo famílias significantes
library(dplyr)
library(tibble)

res_df <- as.data.frame(res_std) |>
  rownames_to_column("family") |>
  pivot_longer(-family, names_to = "role", values_to = "std_resid") |>
  filter(abs(std_resid) > 2) |>
  arrange(desc(abs(std_resid)))

res_df
#salvando os residuos
#writexl::write_xlsx(res_df, "Data/residuos-qui-familias-frequentes.xlsx")

# no caso dos dois testes, tanto quii-quadrado, uanto fisher, simulado respondem a nossa pergunta
# sem cair em erros metodológicos
# sim o qui-quadrado simulado pode ser usado em matrizes de contigência maiores que 2x2
# isso não altera a natureza dos dados categóricos
# consulte legendre & legendre: Numerical ecology, 2012

