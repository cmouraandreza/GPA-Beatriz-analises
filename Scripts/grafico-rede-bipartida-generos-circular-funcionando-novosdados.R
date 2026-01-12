# ==============================================================================
# ANÁLISE DE REDE BIPARTIDA: GÊNEROS EPIZÓICOS × FAMÍLIAS SUBSTRATO
# ==============================================================================

# Configuração inicial
setwd("E:/GitHub/GPA-Beatriz/GPA-Beatriz-analises")

# Pacotes
library(dplyr)
library(tidyr)
library(readr)
library(igraph)

# ==============================================================================
# 1. CARREGAMENTO E PREPARAÇÃO DOS DADOS
# ==============================================================================

# Carregar dados
dados <- readxl::read_xlsx("Data/dados-atualizados-bea-janeiro26.xlsx")
unique(dados$EP_Genera)
colnames(dados)
# NOME NOVO X NOME ANTIGO
dados <- dados %>%
  dplyr::rename(
    EP_Genera = Genus_EP,
    BS_Family = Family_BS
  )

# Criar matriz de interações (agregando contagens)
mat_df <- dados %>%
  filter(!is.na(BS_Family), !is.na(EP_Genera),
         BS_Family != "", EP_Genera != "") %>%
  count(BS_Family, EP_Genera, name = "weight")

# Converter para matriz (famílias nas linhas, gêneros nas colunas)
m <- mat_df %>%
  pivot_wider(names_from = EP_Genera,
              values_from = weight,
              values_fill = 0) %>%
  as.data.frame()

# Definir nomes das linhas e remover coluna de família
rownames(m) <- m$BS_Family
m$BS_Family <- NULL

# Converter para matriz numérica
m <- as.matrix(m)
storage.mode(m) <- "numeric"

# Remover linhas/colunas vazias
m <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]

library(igraph)

# ==============================================================================
# 2. CONSTRUÇÃO DO GRAFO BIPARTIDO
# ==============================================================================

familias <- rownames(m)
generos <- colnames(m)

# Criar data frame de arestas
edges <- which(m > 0, arr.ind = TRUE)
df_edges <- data.frame(
  from   = generos[edges[, 2]],
  to     = familias[edges[, 1]],
  weight = m[edges]
)

# Criar grafo direcionado
g <- graph_from_data_frame(df_edges, directed = TRUE)

# Definir bipartição (type = TRUE para famílias)
V(g)$type <- V(g)$name %in% familias

# Calcular força dos nós (soma dos pesos das arestas)
V(g)$strength <- igraph::strength(g)

# ==============================================================================
# FILTRAR GÊNEROS COM GRAU >= 3
# ==============================================================================

# Calcular grau de saída para gêneros (quantas famílias cada gênero se associa)
deg_out <- igraph::degree(g, mode = "out")

# Identificar gêneros (type = FALSE)
generos_idx <- which(!V(g)$type)

# Gêneros que devem ser mantidos (grau >= 3)
generos_manter <- V(g)$name[generos_idx][deg_out[generos_idx] >= 3]

# Também manter todas as famílias
familias_manter <- V(g)$name[V(g)$type]

# Nós a manter
nos_manter <- c(generos_manter, familias_manter)

# Criar subgrafo filtrado
g <- induced_subgraph(g, vids = nos_manter)

# Recalcular força após filtragem
V(g)$strength <- igraph::strength(g)

cat("Gêneros filtrados (grau >= 3):", sum(!V(g)$type), "\n")
cat("Famílias mantidas:", sum(V(g)$type), "\n")
cat("Total de nós:", vcount(g), "\n")
cat("Total de arestas:", ecount(g), "\n")

# ==============================================================================
# 3. ESTÉTICA DO GRAFO
# ==============================================================================

# Normalização para tamanhos
max_strength <- max(V(g)$strength, na.rm = TRUE)
max_weight <- max(E(g)$weight, na.rm = TRUE)

# --- VÉRTICES ---
V(g)$color <- ifelse(V(g)$type,
                     adjustcolor("salmon", alpha.f = 0.9),    # Famílias
                     adjustcolor("gold", alpha.f = 0.85))     # Gêneros

V(g)$frame.color <- "gray30"
V(g)$frame.width <- 0.8

V(g)$size <- ifelse(V(g)$type,
                    5 + 8 * sqrt(V(g)$strength / max_strength),   # Famílias
                    3 + 6 * sqrt(V(g)$strength / max_strength))   # Gêneros

# --- RÓTULOS ---
V(g)$label <- V(g)$name
V(g)$label.cex <- ifelse(V(g)$type, 0.9, 0.75)  # AUMENTADO tamanho da fonte (era 0.75 e 0.55)
V(g)$label.font <- ifelse(V(g)$type, 2, 3)
V(g)$label.color <- ifelse(V(g)$type, "black", "navy")
V(g)$label.dist <- ifelse(V(g)$type, 1.5, 0.8)  # AUMENTADO (era 1.3 e 0.7)
V(g)$label.bg <- adjustcolor("white", alpha.f = 0.8)  # Mais opaco (era 0.7)

# --- ARESTAS ---
E(g)$color <- adjustcolor("salmon3", alpha.f = 0.25)
E(g)$curved <- 0
E(g)$width <- 0.3 + 2.2 * sqrt(E(g)$weight / max_weight)
E(g)$arrow.size <- 0.3
E(g)$arrow.width <- 1.0

# ==============================================================================
# 4. LAYOUT RADIAL (FAMÍLIAS NA PERIFERIA, GÊNEROS EM ANÉIS INTERNOS)
# ==============================================================================

set.seed(42)

generos_idx <- which(!V(g)$type)
familias_idx <- which(V(g)$type)

n_gen <- length(generos_idx)
n_fam <- length(familias_idx)

lay <- matrix(0, nrow = vcount(g), ncol = 2)

# FAMÍLIAS: círculo externo
if (n_fam > 0) {
  ang_fam <- seq(0, 2 * pi, length.out = n_fam + 1)[1:n_fam]
  raio_fam <- 20
  lay[familias_idx, 1] <- raio_fam * cos(ang_fam)
  lay[familias_idx, 2] <- raio_fam * sin(ang_fam)
}

# GÊNEROS: múltiplos anéis internos (baseado na força)
if (n_gen > 0) {
  gen_strength <- V(g)$strength[generos_idx]
  
  # Evitar divisão por zero
  if (max(gen_strength) > 0) {
    gen_strength_norm <- gen_strength / max(gen_strength)
  } else {
    gen_strength_norm <- rep(0.5, n_gen)
  }
  
  # Número de anéis baseado na quantidade de gêneros
  n_rings <- max(3, min(7, ceiling(n_gen / 12)))
  
  # Atribuir anel baseado na força (usando intervalos uniformes se possível)
  if (length(unique(gen_strength_norm)) > 1) {
    ring_id <- cut(gen_strength_norm,
                   breaks = n_rings,
                   labels = FALSE,
                   include.lowest = TRUE)
  } else {
    # Se todos têm mesma força, distribui uniformemente
    ring_id <- rep(1:n_rings, length.out = n_gen)
  }
  
#  raios <- seq(3, 15, length.out = n_rings)
  raios <- seq(10, 26, length.out = n_rings)  # ainda maior
  
  # Ordenar para melhor distribuição
  ord <- order(ring_id, -gen_strength_norm)
  gen_ordered <- generos_idx[ord]
  ring_ordered <- ring_id[ord]
  
  for (r in 1:n_rings) {
    nodes_r <- gen_ordered[ring_ordered == r]
    k <- length(nodes_r)
    if (k == 0) next
    
    ang <- seq(0, 2 * pi, length.out = k + 1)[1:k]
    ang <- ang + runif(k, -0.05, 0.05)  # Pequeno jitter
    
    lay[nodes_r, 1] <- raios[r] * cos(ang)
    lay[nodes_r, 2] <- raios[r] * sin(ang)
  }
}

# ==============================================================================
# 5. VISUALIZAÇÃO
# ==============================================================================

# Plot na tela
par(mar = c(0, 0, 0, 0), bg = "white")
plot(g, layout = lay, vertex.label.degree = -pi/2, asp = 0)

# Salvar como SVG (dimensões maiores)
svg("Plots/rede_bipartida-generos-novo.12.1.svg", width = 16, height = 10)  # AUMENTADO de 12x7
par(mar = c(0, 0, 0, 0), bg = "white")
plot(g, layout = lay, vertex.label.degree = -pi/2, asp = 0)
dev.off()

# ==============================================================================
# 6. ANÁLISES DESCRITIVAS
# ==============================================================================

# Grau dos nós
deg <- igraph::degree(g, mode = "all")
deg_in <- igraph::degree(g, mode = "in")

# Tabela de famílias com métricas
tabela_familias <- data.frame(
  familia  = V(g)$name[familias_idx],
  grau     = deg_in[familias_idx],
  strength = V(g)$strength[familias_idx]
) %>%
  arrange(desc(grau), desc(strength))

print(tabela_familias)

# Identificar famílias com grau 1 (especialistas)
familias_grau1 <- tabela_familias %>%
  filter(grau == 1)

print("Famílias especialistas (grau = 1):")
print(familias_grau1)

# Para cada família grau 1, identificar o gênero associado
if (nrow(familias_grau1) > 0) {
  result <- lapply(familias_grau1$familia, function(fam) {
    neigh <- neighbors(g, fam, mode = "in")
    data.frame(familia = fam, genero = V(g)$name[neigh])
  })
  df_especialistas <- do.call(rbind, result)
  print("Associações especialistas:")
  print(df_especialistas)
}
