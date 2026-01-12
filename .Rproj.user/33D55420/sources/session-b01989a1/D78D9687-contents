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

# ==============================================================================
# 2. CONSTRUÇÃO DO GRAFO BIPARTIDO
# ==============================================================================
familias <- rownames(m)
generos  <- colnames(m)

edges <- which(m > 0, arr.ind = TRUE)
df_edges <- data.frame(
  from   = generos[edges[, 2]],
  to     = familias[edges[, 1]],
  weight = m[edges]
)

# (ALTERADO) Para visualização, é melhor NÃO direcionado (menos ruído visual).
# Se você precisa das setas por algum motivo, volte para directed = TRUE.
g <- graph_from_data_frame(df_edges, directed = FALSE)

# Bipartição
V(g)$type <- V(g)$name %in% familias

# (ALTERADO) strength com pesos explicitamente
V(g)$strength <- igraph::strength(g, weights = E(g)$weight)

# ==============================================================================
# FILTRAR GÊNEROS COM GRAU >= 3  (ALTERADO)
# ==============================================================================

# (ALTERADO) Em grafo não-direcionado, degree() já é o nº de conexões
deg_all <- igraph::degree(g)

generos_idx  <- which(!V(g)$type)
familias_idx <- which(V(g)$type)

generos_manter <- V(g)$name[generos_idx][deg_all[generos_idx] >= 3]

# (ALTERADO) Também filtra famílias muito soltas (opcional, deixa o gráfico mais limpo)
# Se você quiser manter TODAS as famílias, comente as 2 linhas abaixo.
familias_manter <- V(g)$name[familias_idx][deg_all[familias_idx] >= 2]

nos_manter <- c(generos_manter, familias_manter)

g <- induced_subgraph(g, vids = nos_manter)

# Recalcular métricas após filtragem
V(g)$strength <- igraph::strength(g, weights = E(g)$weight)
deg_all <- igraph::degree(g)

cat("Gêneros (grau>=3):", sum(!V(g)$type), "\n")
cat("Famílias (grau>=2):", sum(V(g)$type), "\n")
cat("Total de nós:", vcount(g), "\n")
cat("Total de arestas:", ecount(g), "\n")

# ==============================================================================
# 3. ESTÉTICA DO GRAFO  (ALTERADO)
# ==============================================================================

max_strength <- max(V(g)$strength, na.rm = TRUE)
max_weight   <- max(E(g)$weight, na.rm = TRUE)

# --- VÉRTICES ---
V(g)$color <- ifelse(V(g)$type,
                     adjustcolor("salmon", alpha.f = 0.90),  # famílias
                     adjustcolor("gold",   alpha.f = 0.90))  # gêneros

V(g)$frame.color <- "gray35"
V(g)$frame.width <- 1.0

# (ALTERADO) tamanhos + cap pra não esmagar a variação
V(g)$size <- ifelse(
  V(g)$type,
  10 + 16 * sqrt(V(g)$strength / max_strength),  # famílias maiores
  4  + 10 * sqrt(V(g)$strength / max_strength)   # gêneros menores
)
V(g)$size <- pmin(V(g)$size, ifelse(V(g)$type, 28, 16))  # cap visual

# --- RÓTULOS (ALTERADO: reduz MUITO a densidade) ---
# (ALTERADO) Famílias: sempre com label
# (ALTERADO) Gêneros: só top N por força (ou por grau)
top_gen_labels <- 18  # ajuste aqui (ex.: 10–25)

gen_nodes <- which(!V(g)$type)
fam_nodes <- which(V(g)$type)

gen_strength <- V(g)$strength[gen_nodes]
top_gen_idx  <- gen_nodes[order(gen_strength, decreasing = TRUE)]
top_gen_idx  <- head(top_gen_idx, top_gen_labels)

V(g)$label <- NA_character_
V(g)$label[fam_nodes] <- V(g)$name[fam_nodes]
V(g)$label[top_gen_idx] <- V(g)$name[top_gen_idx]

# (ALTERADO) estética dos labels
V(g)$label.cex <- ifelse(V(g)$type, 0.95, 0.60)
V(g)$label.font <- ifelse(V(g)$type, 2, 3)
V(g)$label.color <- ifelse(V(g)$type, "black", "navy")
V(g)$label.dist <- ifelse(V(g)$type, 1.6, 0.8)  # afasta famílias da borda
V(g)$label.degree <- 0
V(g)$label.bg <- adjustcolor("white", alpha.f = 0.70)

# --- ARESTAS (ALTERADO: “some” mais e não engole o gráfico) ---
E(g)$color <- adjustcolor("gray50", alpha.f = 0.18)
E(g)$curved <- 0.05

E(g)$width <- 0.15 + 2.0 * sqrt(E(g)$weight / max_weight)
E(g)$width <- pmin(E(g)$width, 2.2)

# ==============================================================================
# 4. LAYOUT RADIAL (ALTERADO: mais espaço pros gêneros)
# ==============================================================================

set.seed(42)

generos_idx  <- which(!V(g)$type)
familias_idx <- which(V(g)$type)

n_gen <- length(generos_idx)
n_fam <- length(familias_idx)

#lay <- matrix(0, nrow = vcount(g), ncol = 2)
lay <- layout_with_fr(
  g,
  niter = 2000,          # mais iterações = mais relaxado
  area  = vcount(g)^2,   # aumenta o “espaço disponível”
  repulserad = vcount(g)^2.5  # <<< ISSO separa o miolo
)
# FAMÍLIAS: círculo externo (ALTERADO: raio maior pra caber labels)
if (n_fam > 0) {
  ang_fam <- seq(0, 2 * pi, length.out = n_fam + 1)[1:n_fam]
  raio_fam <- 26  # era 20
  lay[familias_idx, 1] <- raio_fam * cos(ang_fam)
  lay[familias_idx, 2] <- raio_fam * sin(ang_fam)
}

# GÊNEROS: múltiplos anéis internos
if (n_gen > 0) {
  gen_strength <- V(g)$strength[generos_idx]

  gen_strength_norm <- if (max(gen_strength) > 0) gen_strength / max(gen_strength) else rep(0.5, n_gen)

  # (ALTERADO) mais anéis e mais espaçados
  n_rings <- max(4, min(9, ceiling(n_gen / 10)))

  ring_id <- if (length(unique(gen_strength_norm)) > 1) {
    cut(gen_strength_norm, breaks = n_rings, labels = FALSE, include.lowest = TRUE)
  } else {
    rep(1:n_rings, length.out = n_gen)
  }

  # (ALTERADO) raios mais amplos (miolo menos colado)
  raios <- seq(4, 20, length.out = n_rings)

  ord <- order(ring_id, -gen_strength_norm)
  gen_ordered  <- generos_idx[ord]
  ring_ordered <- ring_id[ord]

  for (r in 1:n_rings) {
    nodes_r <- gen_ordered[ring_ordered == r]
    k <- length(nodes_r)
    if (k == 0) next

    ang <- seq(0, 2 * pi, length.out = k + 1)[1:k]
    ang <- ang + runif(k, -0.08, 0.08)  # jitter um pouco maior

    lay[nodes_r, 1] <- raios[r] * cos(ang)
    lay[nodes_r, 2] <- raios[r] * sin(ang)
  }
}

# ==============================================================================
# 5. VISUALIZAÇÃO  (ALTERADO)
# ==============================================================================

# (ALTERADO) NÃO use asp=0 (pode dar erro e distorce). Use asp=1 ou omita.
par(mar = c(0, 0, 0, 0), bg = "white")
plot(g,  asp = 1)

# Salvar como SVG (dimensões maiores)
svg("Plots/rede_bipartida-generos2.svg", width = 18, height = 14)
par(mar = c(0, 0, 0, 0), bg = "white")
plot(g, layout = lay, asp = 0)
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