# 32 — The market in one plane: correspondence analysis of the dyadic table
# of buying units by suppliers.
#
# Every award is a tie between a buying unit and a supplier. The table of
# counts units x suppliers is a contingency table, and its correspondence
# analysis is the canonical geometric object (Benzécri): rows and columns in
# one plane, with no state category entering the construction. The categories
# the state records — on both sides — and the seats of the units are projected
# afterwards, as supplementary elements, to READ the structure. That is what
# separates this from every construction that starts from categories: if the
# gradient appears here, it was not put in.
#
# Outputs (tables/):
#   tab_conjunto_autovalores.csv   eigenvalues against a margin-preserving
#                                  permutation null
#   tab_conjunto_lectura.csv       supplementary categories: mean coordinate,
#                                  n, test value, per axis
#   tab_conjunto_r2.csv            R2 of each supplementary variable per axis
#   tab_conjunto_estabilidad.csv   bootstrap over units and threshold checks
#   tab_conjunto_puente.csv        the bridge to the unit space and to the
#                                  mean access of suppliers
#   tab_conjunto_unidades.csv      unit coordinates with reading variables
#   data/processed/conjunto_proveedores.parquet   supplier coordinates
#
# Run: Rscript 32_conjunto_ca.R   (from the repository root, after 30 and 31; ~15 min)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow)})
set.seed(42)
HAS_IRLBA <- requireNamespace("irlba", quietly = TRUE)
B_PERM <- if (HAS_IRLBA) 200 else 100
B_BOOT <- if (HAS_IRLBA) 100 else 50
N_MIN_SUP <- 20

# ── the dyadic table ─────────────────────────────────────────────────────────
adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  select(doc_contractual, uoc_id)
sup <- read_parquet(file.path(DIR_PROC3, "proveedores_c10.parquet"))
panel <- read.csv(file.path(DIR_TAB, "tab_unidad_panel.csv"), encoding = "UTF-8",
                  stringsAsFactors = FALSE)
ucoords <- read.csv(file.path(DIR_TAB, "tab_unidad_coords.csv"), encoding = "UTF-8")

ties <- adj |> inner_join(unidad, by = "doc_contractual") |>
  filter(uoc_id %in% panel$uoc_id, cuit %in% sup$cuit) |>
  count(uoc_id, cuit, name = "n")
units <- sort(unique(ties$uoc_id)); sups <- sort(unique(ties$cuit))
M <- matrix(0, length(units), length(sups), dimnames = list(units, sups))
M[cbind(match(ties$uoc_id, units), match(ties$cuit, sups))] <- ties$n
cat(sprintf("tabla diádica: %d unidades x %d proveedores, %d adjudicaciones, %d celdas > 0, densidad %.4f\n",
            nrow(M), ncol(M), sum(M), sum(M > 0), mean(M > 0)))

# ── correspondence analysis by SVD of the standardised residuals ─────────────
ca_fit <- function(M, k = 3) {
  P <- M / sum(M); r <- rowSums(P); c <- colSums(P)
  S <- (P - outer(r, c)) / sqrt(outer(r, c))
  sv <- if (HAS_IRLBA) irlba::irlba(S, nv = k, nu = k) else svd(S, nu = k, nv = k)
  d <- sv$d[1:k]
  list(ev = d^2, F = sweep(sv$u[, 1:k, drop = FALSE], 1, sqrt(r), "/") %*% diag(d, k),
       G = sweep(sv$v[, 1:k, drop = FALSE], 1, sqrt(c), "/") %*% diag(d, k),
       r = r, c = c, total = if (HAS_IRLBA) NA_real_ else sum(sv$d^2))
}
top_ev <- function(M, k = 2) {
  P <- M / sum(M); r <- rowSums(P); c <- colSums(P)
  S <- (P - outer(r, c)) / sqrt(outer(r, c))
  if (HAS_IRLBA) irlba::irlba(S, nv = k, nu = 0)$d[1:k]^2 else svd(S, nu = 0, nv = 0)$d[1:k]^2
}
K <- 3
fit <- ca_fit(M, K)

# A sparse table lets one row of tiny mass whose columns tie to nothing else
# own an entire axis: on the first fit a training institute of 22 suppliers
# carried 99.9 per cent of the first axis at a coordinate of -55. That is the
# rare-object problem of correspondence analysis (Greenacre 2013), and the
# remedy is to set such a row aside, refit, and report it. A row is set aside
# when it carries more than half of an axis; at most three, each recorded.
excluidas <- data.frame()
repeat {
  ctr1 <- fit$r * fit$F[, 1]^2 / fit$ev[1]
  if (max(ctr1) < 0.5 || nrow(excluidas) >= 3) break
  i <- which.max(ctr1)
  excluidas <- bind_rows(excluidas, data.frame(
    uoc_id = rownames(M)[i], n_sup = sum(M[i, ] > 0), masa = round(fit$r[i], 5),
    coordenada_eje1 = round(fit$F[i, 1], 2), contribucion_eje1 = round(ctr1[i], 3)))
  M <- M[-i, , drop = FALSE]; M <- M[, colSums(M) > 0, drop = FALSE]
  fit <- ca_fit(M, K)
}
write.csv(excluidas, file.path(DIR_TAB, "tab_conjunto_excluidas.csv"), row.names = FALSE)
cat(sprintf("filas apartadas por dominar un eje: %d\n", nrow(excluidas)))
if (nrow(excluidas)) print(excluidas, row.names = FALSE)
units <- rownames(M); sups <- colnames(M)
cat(sprintf("tabla final: %d unidades x %d proveedores, %d adjudicaciones\n",
            nrow(M), ncol(M), sum(M)))

if (is.na(fit$total)) fit$total <- sum(svd((M / sum(M) - outer(fit$r, fit$c)) /
                                             sqrt(outer(fit$r, fit$c)), nu = 0, nv = 0)$d^2)
F <- fit$F; G <- fit$G
rownames(F) <- units; rownames(G) <- sups

# orientation, so that signs are stable across runs: every axis positive
# towards the suppliers domiciled in the capital (the corporate end)
s <- data.frame(cuit = sups, g1 = G[, 1], g2 = G[, 2], g3 = G[, 3]) |>
  inner_join(sup, by = "cuit")
for (k in 1:K) {
  gk <- s[[paste0("g", k)]]
  sk <- sign(mean(gk[s$provincia == JURIS_REF]) - mean(gk[s$provincia != JURIS_REF]))
  if (sk == 0) sk <- 1
  F[, k] <- F[, k] * sk; G[, k] <- G[, k] * sk; s[[paste0("g", k)]] <- gk * sk
}

# ── eigenvalues against a margin-preserving permutation null ─────────────────
cat(sprintf("autovalores: %s | inercia total %.2f | B_perm = %d (irlba: %s)\n",
            paste(round(fit$ev, 4), collapse = " "), fit$total, B_PERM, HAS_IRLBA))
rm_i <- as.integer(round(rowSums(M))); cm_i <- as.integer(round(colSums(M)))
# the null draws are the slow part (one full SVD each); they are cached, keyed
# on the shape of the table, so a rerun that changes nothing upstream is fast
nulo_path <- file.path(DIR_TAB, "tab_conjunto_nulo_draws.csv")
nulo <- NULL
if (file.exists(nulo_path)) {
  prev <- read.csv(nulo_path)
  if (nrow(prev) >= B_PERM && all(prev$unidades == nrow(M)) &&
      all(prev$proveedores == ncol(M)) && all(prev$adjudicaciones == sum(M)))
    nulo <- as.matrix(prev[seq_len(B_PERM), c("ev1", "ev2")])
}
if (is.null(nulo)) {
  nulo <- t(replicate(B_PERM, top_ev(r2dtable(1, rm_i, cm_i)[[1]], 2)))
  write.csv(data.frame(draw = seq_len(B_PERM), ev1 = nulo[, 1], ev2 = nulo[, 2],
                       unidades = nrow(M), proveedores = ncol(M),
                       adjudicaciones = sum(M)), nulo_path, row.names = FALSE)
}
autov <- data.frame(dim = 1:K, autovalor = round(fit$ev, 4),
                    share_inercia_pct = round(100 * fit$ev / fit$total, 2),
                    nulo_media = c(round(colMeans(nulo), 4), NA),
                    nulo_sd = c(round(apply(nulo, 2, sd), 5), NA),
                    nulo_mediana = c(round(apply(nulo, 2, median), 4), NA),
                    nulo_p95 = c(round(apply(nulo, 2, quantile, 0.95), 4), NA),
                    nulo_p99 = c(round(apply(nulo, 2, quantile, 0.99), 4), NA),
                    nulo_max = c(round(apply(nulo, 2, max), 4), NA),
                    B = B_PERM)
write.csv(autov, file.path(DIR_TAB, "tab_conjunto_autovalores.csv"), row.names = FALSE)
cat("nulo por permutación (mediana / p99 / max), ejes 1-2:\n"); print(autov[1:2, ])

# ── supplementary elements that read the plane ──────────────────────────────
u <- data.frame(uoc_id = units, f1 = F[, 1], f2 = F[, 2], f3 = F[, 3], masa = fit$r) |>
  left_join(panel |> select(uoc_id, saf_id, A_juris, sede_provincia, anclado,
                            A_tipo, A_rango, A_antiguedad, A_presupuesto,
                            N_tamano, N_distribuido, n_sup, acceso, contenido),
            by = "uoc_id") |>
  left_join(ucoords |> select(uoc_id, unidad_dim1, unidad_dim2), by = "uoc_id") |>
  mutate(anclado = ifelse(anclado, "anclado", "programatico"))

# the ground of the exception per supplier (S_fundamento) already travels in
# proveedores_c10.parquet: build_supplier_space() derives it once, on the same
# window, and a second derivation here duplicated the column and emptied it
stopifnot("S_fundamento" %in% names(s))
s <- s |> mutate(masa = fit$c[match(cuit, sups)])

# vtest() — Le Roux & Rouanet's test value — lives in unidad_helpers.R
lectura <- function(df, vars, axes, lado) {
  bind_rows(lapply(vars, function(v) bind_rows(lapply(axes, function(a) {
    d <- df[!is.na(df[[v]]), ]
    vt <- vtest(d[[a]], d[[v]])
    # a variable that takes one value in the table cannot be regressed on
    r2 <- if (length(unique(d[[v]])) > 1)
      summary(lm(d[[a]] ~ factor(d[[v]])))$r.squared else NA_real_
    data.frame(lado = lado, variable = v, eje = a, vt, r2_variable = round(r2, 3),
               row.names = NULL)
  }))))
}
AX_U <- c("f1", "f2", "f3"); AX_S <- c("g1", "g2", "g3")
# the same 24 jurisdictions read from the two ends of a tie: the seat of the
# unit and the fiscal domicile of the supplier; no coarser class on either side
lect_u <- lectura(u, c("sede_provincia", "anclado", "A_tipo", "A_rango",
                       "A_antiguedad", "A_presupuesto", "N_tamano",
                       "N_distribuido"), AX_U, "unidad")
lect_s <- lectura(s, c("A_personeria", "provincia", "A_rubro", "A_registro",
                       "A_cliente", "A_escala", "A_directa", "S_fundamento"), AX_S,
                  "proveedor")
lect <- bind_rows(lect_u, lect_s) |>
  mutate(media = round(media, 3), vtest = round(vtest, 1))
write.csv(lect, file.path(DIR_TAB, "tab_conjunto_lectura.csv"), row.names = FALSE)
r2tab <- lect |> distinct(lado, variable, eje, r2_variable) |>
  tidyr::pivot_wider(names_from = eje, values_from = r2_variable)
write.csv(r2tab, file.path(DIR_TAB, "tab_conjunto_r2.csv"), row.names = FALSE)
cat("\nR2 de cada suplementaria sobre los ejes:\n"); print(as.data.frame(r2tab))

# ── stability: bootstrap over units, thresholds ──────────────────────────────
procr_cor <- function(G0, G1) {
  # align G1 to G0 on the common rows (rotation + reflection), correlate per axis
  common <- intersect(rownames(G0), rownames(G1))
  A <- scale(G0[common, 1:2], scale = FALSE); Bm <- scale(G1[common, 1:2], scale = FALSE)
  sv <- svd(t(Bm) %*% A); R <- sv$u %*% t(sv$v); Br <- Bm %*% R
  c(cor(A[, 1], Br[, 1]), cor(A[, 2], Br[, 2]))
}
boot <- t(replicate(B_BOOT, {
  i <- sample(nrow(M), replace = TRUE)
  Mb <- M[i, , drop = FALSE]; Mb <- Mb[, colSums(Mb) > 0, drop = FALSE]
  Gb <- ca_fit(Mb, 2)$G; rownames(Gb) <- colnames(Mb)
  procr_cor(G, Gb)
}))
umbral <- lapply(c(30, 50), function(k) {
  keep <- panel$uoc_id[panel$n_sup >= k]
  Mk <- M[rownames(M) %in% keep, , drop = FALSE]; Mk <- Mk[, colSums(Mk) > 0, drop = FALSE]
  Gk <- ca_fit(Mk, 2)$G; rownames(Gk) <- colnames(Mk)
  data.frame(prueba = sprintf("threshold %d suppliers", k), unidades = nrow(Mk),
             proveedores = ncol(Mk), cor_eje1 = NA, cor_eje2 = NA,
             t(procr_cor(G, Gk)))
})
# is the first axis carried by a handful of units? refit without the fifty
# units furthest out on it
ext <- order(-abs(F[, 1]))[1:50]
Mx <- M[-ext, , drop = FALSE]; Mx <- Mx[, colSums(Mx) > 0, drop = FALSE]
Gx <- ca_fit(Mx, 2)$G; rownames(Gx) <- colnames(Mx)
px <- procr_cor(G, Gx)
extremas <- data.frame(prueba = "without the 50 units furthest out on axis 1",
                       unidades = nrow(Mx), proveedores = ncol(Mx),
                       cor_eje1 = as.character(round(px[1], 3)),
                       cor_eje2 = as.character(round(px[2], 3)))
estab <- bind_rows(
  data.frame(prueba = sprintf("bootstrap over units, B = %d (2.5 / 50 / 97.5)", B_BOOT),
             unidades = nrow(M), proveedores = ncol(M),
             cor_eje1 = paste(round(quantile(boot[, 1], c(.025, .5, .975)), 3), collapse = " / "),
             cor_eje2 = paste(round(quantile(boot[, 2], c(.025, .5, .975)), 3), collapse = " / ")),
  bind_rows(umbral) |> transmute(prueba, unidades, proveedores,
                                 cor_eje1 = as.character(round(X1, 3)),
                                 cor_eje2 = as.character(round(X2, 3))),
  extremas)
write.csv(estab, file.path(DIR_TAB, "tab_conjunto_estabilidad.csv"), row.names = FALSE)
cat("\nestabilidad:\n"); print(estab, row.names = FALSE)

# ── the bridge: joint axes against the unit space and the mean access ────────
puente <- data.frame(
  medida = c("r(joint axis 1, mean supplier access)", "r(joint axis 2, mean supplier access)",
             "r(joint axis 1, unit-space axis 1)", "r(joint axis 2, unit-space axis 1)",
             "r(joint axis 2, unit-space axis 2)",
             "R2 of joint axis 2 on the jurisdiction of the seat",
             "R2 of joint axis 1 on the jurisdiction of the seat",
             "R2 of supplier axis 2 on legal form + jurisdiction + modal client",
             "R2 of supplier axis 1 on legal form + jurisdiction + modal client"),
  valor = round(c(cor(u$f1, u$acceso), cor(u$f2, u$acceso),
                  cor(u$f1, u$unidad_dim1, use = "complete.obs"),
                  cor(u$f2, u$unidad_dim1, use = "complete.obs"),
                  cor(u$f2, u$unidad_dim2, use = "complete.obs"),
                  summary(lm(f2 ~ sede_provincia, u))$r.squared,
                  summary(lm(f1 ~ sede_provincia, u))$r.squared,
                  summary(lm(g2 ~ A_personeria + provincia + A_cliente, s))$r.squared,
                  summary(lm(g1 ~ A_personeria + provincia + A_cliente, s))$r.squared), 3))
write.csv(puente, file.path(DIR_TAB, "tab_conjunto_puente.csv"), row.names = FALSE)
cat("\nel puente:\n"); print(puente, row.names = FALSE)

write.csv(u, file.path(DIR_TAB, "tab_conjunto_unidades.csv"), row.names = FALSE)
write_parquet(s, file.path(DIR_PROC3, "conjunto_proveedores.parquet"))
cat("\nOK -> tables/tab_conjunto_*.csv, data/processed/conjunto_proveedores.parquet\n")
