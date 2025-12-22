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

# Nomes das colunas do seu dataframe:
# (troque aqui se necessário)
COL_FAMILIA <- "basibionte_family"   # ex: "BS_Family" ou "Family_base"
COL_ESPECIE <- "nome_abrev"   # ex: "Scientific_name_EP" ou "Species_epi"
COL_WEIGHT  <- "n"                 # se você já tem contagem numa coluna (ex: "n"), coloque "n"

# ---------------------------
# 1) Limpeza mínima
# ---------------------------

df1 <- df_modif %>%
  # garante que as colunas existem
  rename(
    familia = all_of("BS_Family"),
    especie = all_of("nome_abrev")
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
  from   = colnames(m2)[edges[,2]],  # espécie
  to     = rownames(m2)[edges[,1]],  # família
  weight = m2[edges]
)


   g <- graph_from_data_frame(df_edges, directed = TRUE) #GATANTE SETAS

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
   
   #V(g)$color <- ifelse(V(g)$type, "salmon", "goldenrod1")
  
    V(g)$color[V(g)$type]  <- adjustcolor("salmon", alpha.f = 0.8)  # famílias
    V(g)$color[!V(g)$type] <- adjustcolor("#457b9d", alpha.f = 0.4)      # espécies
    V(g)$frame.width <- 0.1 # borda das figuras
  
    V(g)$frame.color <- "gray30"
   V(g)$size <- 4 + 16 * (V(g)$strength / max_strength)
   
V(g)$size <- ifelse(
  V(g)$type,
     V(g)$size,          # espécies
     V(g)$size * 0.9     # famílias
   )




V(g)$frame.color <- NA
V(g)$frame.width <- 0
  
   E(g)$color <- adjustcolor("red", alpha.f = 0.3)
  # E(g)$color <- rgb(0, 0, 0, alpha = 0.18)
   w_norm <- sqrt(E(g)$weight / max_w)
   E(g)$width <- 0.3 + 2.2 * w_norm
  
   #TAMANHO  e largura das setas
   E(g)$arrow.size  <- 0.3
   E(g)$arrow.width <- 0.3
   E(g)$curved <- 0.15
   
    top_labels <- 25
   keep <- order(V(g)$strength, decreasing = TRUE)[1:min(top_labels, vcount(g))]
   
   #V(g)$label <- ""
   V(g)$label <- V(g)$name
   V(g)$label.family <- "sans"
   
   V(g)$label.font <- ifelse(V(g)$type, 1, 3) # 3 = itálico (perfeito pra nomes científicos)

   V(g)$label.cex <- ifelse(V(g)$type, 0.6, 0.55) # tamanho das bolas
   V(g)$label.color <- ifelse(V(g)$type, "black", "navy")
   
   V(g)$label[keep] <- V(g)$name[keep]
   #V(g)$label.cex <- 0.7
   #V(g)$label.color <- "navy"
  
    V(g)$shape <- ifelse(
     V(g)$type,        # famílias
     "circle",
     "circle"          # espécies
   )
    
   set.seed(42)
   lay <- igraph::layout_with_fr(g)
   par(mar = c(0, 0, 0, 0))  # remove margens
   lay_zoom <- lay * 6
   
   plot(
     g,
     layout = lay,
     vertex.label = V(g)$label,
     vertex.label.dist = ifelse(V(g)$type, 4, 0.2),
     vertex.label.degree = -pi/4
    # main = "Rede Epibiontes × Famílias (Bipartida)"
   )
   
   lay <- layout_with_fr(
     g,
     niter = 4000,                     # mais iterações = mais “arruma”
     area  = (vcount(g)^2) * 20,       # ↑ area = espalha
     repulserad = (vcount(g)^3) * 10   # ↑ repulsão = espalha mais
   )
   
   plot(g, layout = lay, asp = 0)
   
   
   
    #salvando a bagaça 
   
   par(mar = c(0, 0, 0, 0))
   
   lay_zoom <- lay * 6
   
   svg("rede_epibiontes3.svg", width = 16, height = 14)
   
   dev.off()
   
   
# plot melhorado - pule para cá ----
   library(igraph)
   
   #========================
   # 1) Matriz limpa
   #========================
   m2 <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]
   
   # (opcional, mas seguro) garantir numeric
   storage.mode(m2) <- "numeric"
   m2[is.na(m2)] <- 0
   
   familias <- rownames(m2)
   especies <- colnames(m2)
   
   #========================
   # 2) Edges: espécie -> família
   #========================
   edges <- which(m2 > 0, arr.ind = TRUE)
   
   df_edges <- data.frame(
     from   = especies[edges[,2]],   # espécie
     to     = familias[edges[,1]],   # família
     weight = m2[edges]
   )
   
   df_edges$weight <- as.numeric(df_edges$weight)
   
   g <- graph_from_data_frame(df_edges, directed = TRUE)
   
   #========================
   # 3) Bipartição robusta
   #========================
   V(g)$type <- V(g)$name %in% familias   # TRUE = famílias / FALSE = espécies
   V(g)$group <- ifelse(V(g)$type, "familia", "especie")
   
   #========================
   # 4) Pesos e força
   #========================
   E(g)$weight <- df_edges$weight
   
   # força ponderada
   V(g)$strength <- igraph::graph.strength(g)
   
   max_strength <- max(V(g)$strength, na.rm = TRUE)
   if (!is.finite(max_strength) || max_strength == 0) max_strength <- 1
   
   max_w <- max(E(g)$weight, na.rm = TRUE)
   if (!is.finite(max_w) || max_w == 0) max_w <- 1
   
   #========================
   # 5) Estética: nós
   #========================
   # cores com transparência
   V(g)$color <- NA
   V(g)$color[V(g)$type]  <- adjustcolor("salmon",  alpha.f = 0.85)  # famílias
   V(g)$color[!V(g)$type] <- adjustcolor("#457b9d", alpha.f = 0.40)  # espécies
   
   # borda suave (ou sem borda)
   V(g)$frame.color <- NA
   V(g)$frame.width <- 0
   
   # tamanhos (comprimidos com sqrt pra não virar bola gigante)
   # famílias um pouco maiores, espécies menores
   V(g)$size <- ifelse(
     V(g)$type,
     6 + 7 * sqrt(V(g)$strength / max_strength),   # famílias
     3 + 4 * sqrt(V(g)$strength / max_strength)    # espécies
   )
   
   #========================
   # 6) Estética: arestas e setas
   #========================
   E(g)$color <- adjustcolor("red", alpha.f = 0.22)
   E(g)$curved <- 0.12
   
   w_norm <- sqrt(E(g)$weight / max_w)
   E(g)$width <- 0.2 + 1.8 * w_norm
   
   E(g)$arrow.size  <- 0.35
   E(g)$arrow.width <- 0.6
   
   #========================
   # 7) Rótulos (só top N)
   #========================
   top_labels <- 28
   keep <- order(V(g)$strength, decreasing = TRUE)[1:min(top_labels, vcount(g))]
   
   
   #V(g)$label <- NA
   #V(g)$label[keep] <- V(g)$name[keep]
   
   V(g)$label <- V(g)$name   # mostra TODOS (famílias + espécies)
   
   
   # itálico apenas para espécies
   V(g)$label.font <- ifelse(V(g)$type, 1, 3)
   
   V(g)$label.cex <- ifelse(V(g)$type, 0.75, 0.55)
   V(g)$label.color <- ifelse(V(g)$type, "black", "navy")
   
   # distância do rótulo: famílias mais afastadas
   label_dist <- ifelse(V(g)$type, 0.9, 0.35)
   
   #========================
   # 8) Layout (uma vez só, com repulsão)
   #========================
   set.seed(42)
   lay <- layout_with_fr(
     g,
     niter = 5000,
     area = (vcount(g)^2) * 35,
     repulserad = (vcount(g)^3) * 18
   )
   
   # se quiser “espalhar” mais sem recalcular:
   lay <- lay * 1.8
   
   #========================
   # 9) Plot na tela
   #========================
   par(mar = c(0, 0, 0, 0))
   plot(
     g,
     layout = lay,
     vertex.label.dist = label_dist,
     vertex.label.degree = -pi/6,
     asp = 0
   )
   
   #========================
   # 10) Salvar (SEM SVG vazio)
   #========================
   # SVG
   svg("rede_bipartida.svg", width = 12, height = 7)
   par(mar = c(0, 0, 0, 0))
   plot(
     g,
     layout = lay,
     vertex.label.dist = label_dist,
     vertex.label.degree = -pi/6,
     asp = 0
   )
   dev.off()
   
   # PNG alta resolução
   png("rede_bipartida.png", width = 3600, height = 2100, res = 300)
   par(mar = c(0, 0, 0, 0))
   plot(
     g,
     layout = lay,
     vertex.label.dist = label_dist,
     vertex.label.degree = -pi/6,
     asp = 0
   )
   dev.off()
   
   
   # tentando plotar de outra forma ------
  # melhor ignorar 
   
   g_tbl <- as_tbl_graph(g)
   
   ggraph(g_tbl, layout = "bipartite") +
     geom_edge_link(alpha = 0.3) +
     geom_node_point(
       aes(
         shape = ifelse(type, "family", "species"),
         color = ifelse(type, "family", "family", "species")
       ),
       size = 4
     ) +
     scale_shape_manual(values = c(
       family = 23,   # losango
       species = 16   # círculo
     )) +
     theme_void()
   
   plot(
     g,
     layout = lay_zoom,
     vertex.label = V(g)$label,
     vertex.label.cex = V(g)$label.cex,
     main = ""
   )
   
   dev.off()
   