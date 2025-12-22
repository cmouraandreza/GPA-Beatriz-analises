# ============================================================
# WORKFLOW ÚNICO: CSV -> matriz bipartida (famílias x espécies)
# -> edges (from/to/weight) -> plot bipartido
# ============================================================

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

head(df_raw)

# Nomes das colunas do seu dataframe:
# (troque aqui se necessário)
COL_FAMILIA <- "basibionte_family"   # ex: "BS_Family" ou "Family_base"
COL_ESPECIE <- "epibionte_species"   # ex: "Scientific_name_EP" ou "Species_epi"
COL_WEIGHT  <- "n"                 # se você já tem contagem numa coluna (ex: "n"), coloque "n"

# ---------------------------
# 1) Limpeza mínima
# ---------------------------

df1 <- df_raw %>%
  # garante que as colunas existem
  rename(
    familia = all_of("BS_Family"),
    especie = all_of("Scientific_name_EP")
  )%>%
  # remove NAs e strings vazias
  filter(
    !is.na(familia), !is.na(especie),
    familia != "", especie != ""
  ) # mudou de nome mas ñ verificou celulas NA

# ---------------------------
# 2) Construir matriz famílias x espécies
#    - se NÃO existir COL_WEIGHT: conta ocorrências
#    - se existir COL_WEIGHT: soma os pesos
# ---------------------------

if (is.null(COL_WEIGHT)) {
  
  # Conta ocorrências (cada linha = uma ocorrência)
  mat_df <- df1 %>%
    count(familia, especie, name = "weight")
  
} else {
  
  # Soma pesos (caso já exista uma coluna com valores)
  mat_df <- df1 %>%
    mutate(weight = as.numeric(.data[[COL_WEIGHT]])) %>%
    filter(!is.na(weight)) %>%
    group_by(familia, especie) %>%
    summarise(weight = sum(weight), .groups = "drop")
}

# Pivot para matriz (família nas linhas, espécie nas colunas)
m <- mat_df %>%
  pivot_wider(
    names_from = especie,
    values_from = weight,
    values_fill = 0
  ) %>%
  as.data.frame()

# ---------------------------
# 3) Garantir rownames/colnames corretos + matriz numérica
# ---------------------------

# primeira coluna = nomes das famílias
rownames(m) <- m$familia
m$familia <- NULL

# vira matriz e força numérico
m <- as.matrix(m)
storage.mode(m) <- "numeric"

# remove linhas/colunas totalmente zeradas (boa prática)
m <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]

# checagens rápidas
stopifnot(!is.null(rownames(m)))
stopifnot(!is.null(colnames(m)))
stopifnot(is.numeric(m[1, 1]))

# ---------------------------
# 4) Criar df_edges (from/to/weight) SEM quebrar colnames
# ---------------------------

edges <- which(m > 0, arr.ind = TRUE)

df_edges <- data.frame(
  from   = rownames(m)[edges[, 1]],        # famílias
  to     = colnames(m)[edges[, 2]],        # espécies
  weight = as.numeric(m[edges]),           # peso da interação
  row.names = NULL
)

# ---------------------------
# 5) Criar grafo bipartido no igraph
# ---------------------------

g <- graph_from_data_frame(df_edges, directed = FALSE)
plot(g)

# marca tipos (bipartido) para layout/plot
V(g)$type <- V(g)$name %in% df_edges$to  # TRUE = espécies, FALSE = famílias

# ---------------------------
# 6) Plot bipartido "clássico" (bipartite::plotweb)
#     - ordena por grau pra diminuir o macarrão
# ---------------------------

# garante matriz limpa
 m2 <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]
 
edges <- which(m2 > 0, arr.ind = TRUE)

df_edges <- data.frame(
       from = rownames(m2)[edges[,1]],  # famílias
      to   = colnames(m2)[edges[,2]],  # espécies
       weight = as.numeric(m2[edges]),
       row.names = NULL)
 
   g <- graph_from_data_frame(df_edges, directed = FALSE)
 
   # marca bipartido
   V(g)$type <- V(g)$name %in% df_edges$to
   
  #####

   
   
   
   
   # garantir pesos
   E(g)$weight <- as.numeric(df_edges$weight)
   
   # força do nó = soma dos pesos (use igraph explicitamente)
   V(g)$strength <- igraph::strength(g)   # <- resolve o conflito
   
   # se ainda der erro (igraph MUITO antigo), use:
   # V(g)$strength <- igraph::graph.strength(g)
   
   max_strength <- max(V(g)$strength, na.rm = TRUE)
   if (!is.finite(max_strength) || max_strength == 0) max_strength <- 1
   
   max_w <- max(E(g)$weight, na.rm = TRUE)
   if (!is.finite(max_w) || max_w == 0) max_w <- 1
   
   V(g)$color <- ifelse(V(g)$type, "deepskyblue3", "goldenrod1")
   V(g)$frame.color <- "gray30"
   V(g)$size <- 4 + 16 * (V(g)$strength / max_strength)
   
   V(g)$size <- ifelse(
     V(g)$type,
     V(g)$size,          # espécies
     V(g)$size * 1.3     # famílias
   )
   
   E(g)$color <- rgb(0, 0, 0, alpha = 0.18)
   w_norm <- sqrt(E(g)$weight / max_w)
   E(g)$width <- 0.3 + 2.2 * w_norm
   
   top_labels <- 25
   keep <- order(V(g)$strength, decreasing = TRUE)[1:min(top_labels, vcount(g))]
   #V(g)$label <- ""
   V(g)$label <- V(g)$name
   V(g)$label.cex <- ifelse(V(g)$type, 0.55, 0.8)
   V(g)$label.color <- ifelse(V(g)$type, "navy", "black")
   
   V(g)$label[keep] <- V(g)$name[keep]
   #V(g)$label.cex <- 0.7
   #V(g)$label.color <- "navy"
   
   set.seed(42)
   lay <- igraph::layout_with_fr(g)
   par(mar = c(0, 0, 0, 0))  # remove margens
   lay_zoom <- lay * 3
   
   plot(
     g,
     layout = lay,
     vertex.label = V(g)$label,
     vertex.label.dist = ifelse(V(g)$type, 0.6, 0.2),
     vertex.label.degree = -pi/4,
     main = "Rede Epibiontes × Famílias (Bipartida)",
     sub  = "Amarelo = Famílias (basibiontes) | Azul = Espécies (epibiontes)"
   )
   
  #salvando a bagaça 
   
   par(mar = c(0, 0, 0, 0))
   
   lay_zoom <- lay * 4.5
   
   svg("rede_epibiontes_zoom.svg", width = 16, height = 14)
   
   plot(
     g,
     layout = lay_zoom,
     vertex.label = V(g)$label,
     vertex.label.cex = V(g)$label.cex,
     main = ""
   )
   
   dev.off()
   