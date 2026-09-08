# 33 — The joint space across the three presidencies (robustness, supplement).
#
# The programmatic side of demand is recomposed with each government whilst
# the deployment of anchored units stays fixed. If the joint structure is a
# structure and not a trend, the plane fitted on each presidency alone should
# reproduce the one fitted on the whole window. Each era's dyadic table is
# fitted on its own (units with 20 or more suppliers in that era), the
# supplier coordinates are aligned to the full-window solution by Procrustes on
# the suppliers both hold, and the agreement is reported per axis, together
# with the RV coefficient between the two configurations.
#
# Run: Rscript 33_conjunto_eras.R   (from the repository root, after 32; ~2 min)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow)})
N_MIN_SUP <- 20

adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  select(doc_contractual, uoc_id)
full <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet"))
G0 <- as.matrix(full[, c("g1", "g2")]); rownames(G0) <- full$cuit

ties_all <- adj |> inner_join(unidad, by = "doc_contractual") |>
  filter(cuit %in% full$cuit)

rv <- function(A, B) {
  A <- scale(A, scale = FALSE); B <- scale(B, scale = FALSE)
  sum(diag(t(A) %*% B %*% t(B) %*% A)) /
    sqrt(sum(diag(t(A) %*% A %*% t(A) %*% A)) * sum(diag(t(B) %*% B %*% t(B) %*% B)))
}
procr <- function(G0, G1) {
  common <- intersect(rownames(G0), rownames(G1))
  A <- scale(G0[common, ], scale = FALSE); Bm <- scale(G1[common, ], scale = FALSE)
  sv <- svd(t(Bm) %*% A); Br <- Bm %*% (sv$u %*% t(sv$v))
  c(n = length(common), cor1 = cor(A[, 1], Br[, 1]), cor2 = cor(A[, 2], Br[, 2]),
    rv = rv(A, Br))
}

out <- bind_rows(lapply(ERAS, function(e) {
  t <- ties_all |> filter(era == e) |> count(uoc_id, cuit, name = "n") |>
    group_by(uoc_id) |> filter(n_distinct(cuit) >= N_MIN_SUP) |> ungroup()
  M <- dyadic_table(t)
  f <- ca_fit_svd(M, 2)
  G1 <- f$G; rownames(G1) <- colnames(M)
  p <- procr(G0, G1)
  data.frame(presidencia = ERA_LABELS[e], unidades = nrow(M), proveedores = ncol(M),
             autovalor_1 = round(f$ev[1], 4), autovalor_2 = round(f$ev[2], 4),
             proveedores_comunes = p["n"], cor_eje1 = round(p["cor1"], 3),
             cor_eje2 = round(p["cor2"], 3), rv = round(p["rv"], 3), row.names = NULL)
}))
write.csv(out, file.path(DIR_TAB, "tab_conjunto_eras.csv"), row.names = FALSE)
cat("el plano conjunto por presidencia, alineado al de la ventana completa:\n")
print(out, row.names = FALSE)
cat("OK -> tables/tab_conjunto_eras.csv\n")
