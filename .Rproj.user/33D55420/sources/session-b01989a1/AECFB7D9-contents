
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
new.data<- readxl::read_xlsx("Data/dados-atualizados-bea-janeiro26.xlsx")

unique(df_raw$Scientific_name_EP)
head(df_raw)

nomes_separados <- separate(df_raw, 
                            col = "Scientific_name_EP", 
                            into = c("genero", "especifico"), 
                            sep = " ")


# esse teste eestá baseado nas espécies por família, falta por gen e familia de basibionte


# juntar 2 colunas e se o valor de especifico for na, não substituir

#pula esse daqui-----
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
  dplyr::select(nome_abrev, BS_Family, n)
unique(nomes_separados$BS_Family)


#criando a tabela - matriz -----

head(df_modif)



mat <- df_modif |>
  pivot_wider(
    names_from  = BS_Family,
    values_from = n,
    values_fill = 0
  )

head(mat)
mat_num <- as.matrix(mat[ , -1])


#teste de qui-quadrado-----
res <- chisq.test(mat_num)

res$expected |> summary()
sum(res$expected < 5)
sum(res$expected < 1)
mean(res$expected < 5)  # proporção



# teste de fisher----
fisher.test(mat_num) # não funciona 
fisher.test(mat_num, simulate.p.value = TRUE, B = 10000)
#p-value = 0.0227
#alternative hypothesis: two.sided


#qui-quadrado
chisq.test(mat_num, simulate.p.value = TRUE, B = 10000)
# X-squared = 3238.3, df = NA, p-value = 9.999e-05





# teste de genero por familia ------
colnames(new.data)

#Contagem de genero de epibionte por familia de basi

df_count <- new.data %>%
  transmute(
    Genus_EP  = as.character(Genus_EP),
    Family_BS = as.character(Family_BS)
  ) %>%
  filter(
    !is.na(Genus_EP),
    !is.na(Family_BS)
  ) %>%
  count(Genus_EP, Family_BS, name = "n")



# 2) Matriz: linhas = gêneros (epibiontes), colunas = famílias (basibiontes)
mat_wide <- df_count %>%
  pivot_wider(
    id_cols     = Genus_EP,
    names_from  = Family_BS,
    values_from = n,
    values_fill = list(n = 0)
  )



# 3) Converter pra matrix numérica, com rownames = gêneros
mat_num <- mat_wide %>%
  tibble::column_to_rownames("Genus_EP") %>%
  as.matrix()
# opcional: garantir numeric mesmo
storage.mode(mat_num) <- "numeric"

# 🧪 Teste de fisher simulado com permutações

fisher.test(mat_num, simulate.p.value = TRUE, B = 10000)

#resultado
# data:  mat_num
# p-value = 9.999e-05
# alternative hypothesis: two.sided


# qui-quadrado simulado-----

res <-chisq.test(mat_num, simulate.p.value = TRUE, B = 10000)
res$p.value
# resultado
# data:  mat_num
# X-squared = 4424.9, df = NA, p-value = 0.0023


#teste de qui-quadrado-----
res <- chisq.test(mat_num)
chi_sim <- chisq.test(mat_num, simulate.p.value = TRUE, B = 9999)
chi_sim$p.value

obs <- chi_sim$observed
exp <- chi_sim$expected
std <- chi_sim$stdres

X-squared = 4424.9, df = NA, p-value = 0.0028

##-----
library(vegan)

mat_bin <- (mat_num > 0) * 1



set.seed(1)
sim <- vegan::commsim("swap")   # preserva graus em matriz 0/1
nulls <- simulate(mat_bin,
                  nsim = 9999,
                  method = "swap")
# Exemplo: comparar qui-quadrado observado vs distribuição nula
chi_obs <- suppressWarnings(chisq.test(mat_bin)$statistic)

chi_null <- apply(nulls, 3, function(m) suppressWarnings(chisq.test(m)$statistic))

p_perm <- mean(chi_null >= chi_obs)
p_perm


df_cells <- data.frame(
  Genus_EP  = rownames(obs)[row(obs)],
  Family_BS = colnames(obs)[col(obs)],
  observed  = as.vector(obs),
  expected  = as.vector(exp),
  stdres    = as.vector(std)
)


# foque no que é minimamente interpretável:
top <- df_cells |>
  dplyr::filter(expected >= 1) |>
  dplyr::arrange(dplyr::desc(abs(stdres))) |>
  dplyr::slice(1:20)

top

#isso aqui plota o boxplot- mas nao usaremos ---- 
boxplot(stdres ~ Family_BS, data = df_cells,
        las = 2, ylab = "Resíduo padronizado", main = "Resíduos por família")
abline(h = c(-2, 2), lty = 2)






# teste de qui -quadrado entre familias de epi e basi

colnames(new.data)


df_countf <- new.data %>%
  transmute(
    Family_EP  = as.character(Family_EP),
    Family_BS = as.character(Family_BS)
  ) %>%
  filter(
    !is.na(Family_EP),
    !is.na(Family_BS)
  ) %>%
  count(Family_EP, Family_BS, name = "n")



# 2) Matriz: linhas = gêneros (epibiontes), colunas = famílias (basibiontes)
mat_widef <- df_countf %>%
  pivot_wider(
    id_cols     = Family_EP,
    names_from  = Family_BS,
    values_from = n,
    values_fill = list(n = 0)
  )



# 3) Converter pra matrix numérica, com rownames = gêneros
mat_numf <- mat_widef %>%
  tibble::column_to_rownames("Family_EP") %>%
  as.matrix()
# opcional: garantir numeric mesmo
storage.mode(mat_numf) <- "numeric"

# 🧪 Teste de fisher simulado com permutações

fisher.test(mat_numf, simulate.p.value = TRUE, B = 10000)

#resultado
# data:  mat_num
# p-value = 9.999e-05
# alternative hypothesis: two.sided


# qui-quadrado simulado-----

resf <-chisq.test(mat_numf, simulate.p.value = TRUE, B = 10000)
resf$p.value
# resultado
# data:  mat_num
# X-squared = 4424.9, df = NA, p-value = 0.0023


#teste de qui-quadrado-----
resf <- chisq.test(mat_numf)
chi_simf <- chisq.test(mat_numf, simulate.p.value = TRUE, B = 9999)

chi_simf$p.value
#resultado
#X-squared = 2448.1, df = NA, p-value = 0.0027

obsf <- chi_simf$observed
expf <- chi_simf$expected
stdf <- chi_simf$stdres