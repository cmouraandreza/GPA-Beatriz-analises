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
#subindo os dados
dados<-readxl::read_xlsx("Data/COPIA-Database_epizoism_hydroids-bea.xlsx")
colnames(dados
         )
cut_data<-dados%>%
  count(EP_Genera, BS_Family)%>%
  rename(
    familia = all_of("BS_Family"),
    especie = all_of("EP_Genera"))




# Nomes das colunas do seu dataframe:
# (troque aqui se necessário)
COL_FAMILIA <- "BS_Family"   # ex: "BS_Family" ou "Family_base"
COL_GENERO <- "EP_Genera"   # ex: "Scientific_name_EP" ou "Species_epi"
COL_WEIGHT  <- "n"                 # se você já tem contagem numa coluna (ex: "n"), coloque "n"

# ---------------------------
# 1) Limpeza mínima - pule ----
# ---------------------------
rm(df1)# para remover do enviroment

df1 <- cut_data %>%
  # garante que as colunas existem
 rename(
    familia = all_of("BS_Family"),
    especie = all_of("EP_Genera")
  )%>%
  # remove NAs e strings vazias
  filter(
    !is.na(familia), !is.na(especie),
    familia != "", especie != "") # mudou de nome mas ñ verificou celulas NA

# ---------------------------
# 2) Construir matriz famílias x espécies
#    - se NÃO existir COL_WEIGHT: conta ocorrências
#    - se existir COL_WEIGHT: soma os pesos
# ---------------------------

if (is.null(COL_WEIGHT)) {
  
  # Conta ocorrências (cada linha = uma ocorrência)
  mat_df <- cut_data %>%
    count(familia, especie, name = "weight")
  
} else {
  
  # Soma pesos (caso já exista uma coluna com valores)
  mat_df <- cut_data %>%
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
     V(g)$size,          # GENEROS
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
  # V(g)$size <- ifelse(
   #  V(g)$type,
    # 6 + 7 * sqrt(V(g)$strength / max_strength),   # famílias
     #3 + 4 * sqrt(V(g)$strength / max_strength))    # espécies
   
   
   V(g)$size <- ifelse(
     V(g)$type,
     4 + 5 * sqrt(V(g)$strength / max_strength),  # famílias menores
     2 + 3 * sqrt(V(g)$strength / max_strength)   # espécies menores
   )
   
   #========================
   # 6) Estética: arestas e setas
   #========================
   E(g)$color <- adjustcolor("red", alpha.f = 0.22)

   E(g)$curved <- 0.25  # (era 0.12)# Aumentar curvatura (menos sobreposição)
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
   #lay <- lay * 1.8
   lay <- lay * 2.5  # ou até 3.0
   
   
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
   svg("rede_bipartida-generos2.svg", width = 12, height = 7)
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


   library(igraph)
   
   #========================
   # 1) Matriz limpa
   #========================
   m2 <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]
   
   storage.mode(m2) <- "numeric"
   m2[is.na(m2)] <- 0
   
   familias <- rownames(m2)
   especies <- colnames(m2)
   
   #========================
   # 2) Edges: espécie -> família
   #========================
   edges <- which(m2 > 0, arr.ind = TRUE)
   
   df_edges <- data.frame(
     from   = especies[edges[,2]],
     to     = familias[edges[,1]],
     weight = m2[edges]
   )
   
   df_edges$weight <- as.numeric(df_edges$weight)
   
   g <- graph_from_data_frame(df_edges, directed = TRUE)
   
   #========================
   # 3) Bipartição robusta
   #========================
   V(g)$type <- V(g)$name %in% familias
   V(g)$group <- ifelse(V(g)$type, "familia", "especie")
   
   #========================
   # 4) Pesos e força
   #========================
   E(g)$weight <- df_edges$weight
   V(g)$strength <- igraph::graph.strength(g)
   
   max_strength <- max(V(g)$strength, na.rm = TRUE)
   if (!is.finite(max_strength) || max_strength == 0) max_strength <- 1
   
   max_w <- max(E(g)$weight, na.rm = TRUE)
   if (!is.finite(max_w) || max_w == 0) max_w <- 1
   
   #========================
   # 5) Estética: nós
   #========================
   V(g)$color <- NA
   V(g)$color[V(g)$type]  <- adjustcolor("salmon",  alpha.f = 0.85)
   V(g)$color[!V(g)$type] <- adjustcolor("#457b9d", alpha.f = 0.40)
   
   V(g)$frame.color <- NA
   V(g)$frame.width <- 0
   
   V(g)$size <- ifelse(
     V(g)$type,
     3 + 5 * sqrt(V(g)$strength / max_strength),
     2 + 3 * sqrt(V(g)$strength / max_strength)
   )
   
   #========================
   # 6) Estética: arestas e setas
   #========================
   E(g)$color <- adjustcolor("red", alpha.f = 0.15)
   E(g)$curved <- 0.2
   
   w_norm <- sqrt(E(g)$weight / max_w)
   E(g)$width <- 0.15 + 1.0 * w_norm
   
   E(g)$arrow.size  <- 0.3
   E(g)$arrow.width <- 0.5
   
   #========================
   # 7) Rótulos
   #========================
   top_labels <- 30
   keep <- order(V(g)$strength, decreasing = TRUE)[1:min(top_labels, vcount(g))]
   
   V(g)$label <- NA
   V(g)$label[keep] <- V(g)$name[keep]
   
   V(g)$label.font <- ifelse(V(g)$type, 1, 3)
   V(g)$label.cex <- ifelse(V(g)$type, 0.7, 0.52)
   V(g)$label.color <- ifelse(V(g)$type, "black", "navy")
   
   label_dist <- ifelse(V(g)$type, 1.0, 0.4)
   
   #========================
   # 8) LAYOUT BIPARTIDO MELHORADO
   #    Fortes nas pontas, fracos no meio
   #========================
   set.seed(42)
   
   # Layout bipartido básico
   lay <- layout_as_bipartite(g, types = V(g)$type)
   
   # Separa índices por tipo
   especies_idx <- which(!V(g)$type)
   familias_idx <- which(V(g)$type)
   
   # Pega força de cada grupo
   esp_str <- V(g)$strength[especies_idx]
   fam_str <- V(g)$strength[familias_idx]
   
   # Ordena DECRESCENTE (fortes primeiro)
   esp_order <- especies_idx[order(esp_str, decreasing = TRUE)]
   fam_order <- familias_idx[order(fam_str, decreasing = TRUE)]
   
   n_esp <- length(esp_order)
   n_fam <- length(fam_order)
   
   # Cria distribuição em "U" (parábola)
   # Fortes nas pontas (±max), fracos no meio (0)
   if (n_esp > 1) {
     idx_esp <- (1:n_esp - (n_esp+1)/2) / (n_esp/2)  # -1 a +1
     pos_esp <- sign(idx_esp) * abs(idx_esp)^1.3 * (n_esp * 0.5)
     lay[esp_order, 2] <- pos_esp
   }
   
   if (n_fam > 1) {
     idx_fam <- (1:n_fam - (n_fam+1)/2) / (n_fam/2)
     pos_fam <- sign(idx_fam) * abs(idx_fam)^1.3 * (n_fam * 0.5)
     lay[fam_order, 2] <- pos_fam
   }
   
   # Ajusta espaçamento horizontal (distância entre colunas)
   lay[especies_idx, 1] <- -3
   lay[familias_idx, 1] <- 3
   
   # Zoom vertical (espalha mais)
   lay[, 2] <- lay[, 2] * 2.5
   
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
   # 10) Salvar
   #========================
   svg("rede_bipartida_ordenada.svg", width = 14, height = 18)
   par(mar = c(0, 0, 0, 0))
   plot(
     g,
     layout = lay,
     vertex.label.dist = label_dist,
     vertex.label.degree = -pi/6,
     asp = 0
   )
   dev.off()
   
   png("rede_bipartida_ordenada.png", width = 3200, height = 4200, res = 300)
   par(mar = c(0, 0, 0, 0))
   plot(
     g,
     layout = lay,
     vertex.label.dist = label_dist,
     vertex.label.degree = -pi/6,
     asp = 0
   )
   dev.off()