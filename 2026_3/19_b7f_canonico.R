# ─────────────────────────────────────────────────────────────────────────────
# 2026_3 — 19: solución canónica B7f + tablas + homología (referencia del texto).
#
# B7f = escala + directa + tipo + fundación + anclaje + institucional +
# presupuesto. Escribe las tablas canónicas que el Results cita:
#   tab_b7f_benzecri.csv, tab_b7f_categorias.csv, tab_b7f_variables.csv,
#   tab_b7f_homologia.csv
#
# Correr:  Rscript 19_b7f_canonico.R   (desde 2026_3/)
# ─────────────────────────────────────────────────────────────────────────────

source("../2026_2/theme_house.R", chdir = TRUE)
DIR_PROC <- "../2026_2/data/processed"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressPackageStartupMessages({library(arrow); library(GDAtools)})

saf_of <- function(x) sub("^([0-9]+).*", "\\1", as.character(x))

# ── panel B7f ────────────────────────────────────────────────────────────────
saf <- read.csv(file.path(DIR_TAB, "tab_saf_panel.csv"), encoding = "UTF-8")
fund <- read.csv(file.path("data", "processed", "tab_saf_fundacion.csv"),
                 encoding = "UTF-8")
saf$saf_id <- as.character(saf$saf_id)
fund$saf_id <- as.character(fund$saf_id)
saf <- saf |> left_join(fund |> select(saf_id, anio_fundacion), by = "saf_id")

tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- mid; out[ok & x <= q[1]] <- lo; out[ok & x > q[2]] <- hi
  out
}
saf <- saf |> mutate(
  A_escala = factor(A_escala), A_directa = factor(A_directa),
  A_tipo = factor(A_tipo), A_anclaje = factor(A_anclaje),
  A_institucional = factor(A_institucional), A_presupuesto = factor(A_presupuesto),
  A_fundacion = factor(tercile_cut(anio_fundacion, "antiguo", "medio", "reciente")))

X <- saf[, c("A_escala", "A_directa", "A_tipo", "A_fundacion", "A_anclaje",
             "A_institucional", "A_presupuesto")] |>
  mutate(across(everything(), as.factor))
RARE <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
mr <- modif.rate(mca)$modif$mrate
n_org <- nrow(X)

benz <- data.frame(dim = 1:3, pct_benzecri = round(mr[1:3], 1))
write.csv(benz, file.path(DIR_TAB, "tab_b7f_benzecri.csv"), row.names = FALSE)

frec <- unlist(lapply(names(X), function(v)
  setNames(as.numeric(table(X[[v]])), paste0(v, ".", levels(X[[v]])))))
cats <- data.frame(categoria = rownames(mca$var$coord),
                   n = frec[rownames(mca$var$coord)],
                   round(mca$var$coord[, 1:3], 2),
                   ctr = round(mca$var$contrib[, 1:3], 1),
                   cos2 = round(mca$var$coord[, 1:3]^2 /
                                  (n_org / frec[rownames(mca$var$coord)] - 1), 3),
                   check.names = FALSE)
write.csv(cats, file.path(DIR_TAB, "tab_b7f_categorias.csv"), row.names = FALSE)

vc <- data.frame(variable = sub("[.].*$", "", rownames(mca$var$contrib)),
                 mca$var$contrib[, 1:3], check.names = FALSE) |>
  aggregate(. ~ variable, data = _, FUN = sum)
vc <- vc[order(-vc[[2]]), ]
write.csv(vc, file.path(DIR_TAB, "tab_b7f_variables.csv"), row.names = FALSE)

# ── homología ────────────────────────────────────────────────────────────────
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
Xs <- active_matrix(m)
mca_sup <- speMCA(Xs, excl = excl_index(Xs, rare_categories(Xs)), ncp = 2)
m$sup_dim2 <- mca_sup$ind$coord[, 2]
m$sup_dim1 <- mca_sup$ind$coord[, 1]

adj$saf_id <- saf_of(adj$organismo)
w <- adj |> filter(es_nuevo, between(ejercicio, WIN0, WIN1)) |>
  left_join(m |> select(cuit, sup_dim2, sup_dim1), by = "cuit")
saf_sup <- w |> group_by(saf_id) |>
  summarise(mean_sup_dim2 = mean(sup_dim2, na.rm = TRUE),
            mean_sup_dim1 = mean(sup_dim1, na.rm = TRUE),
            n_sup = n_distinct(cuit), .groups = "drop")

saf$org_dim1 <- mca$ind$coord[, 1]
saf$org_dim2 <- mca$ind$coord[, 2]
hom <- saf_sup |> left_join(
  saf |> select(saf_id, org_dim1, org_dim2, A_anclaje), by = "saf_id")

c1 <- cor(hom$org_dim1, hom$mean_sup_dim2, use = "complete.obs")
c2 <- cor(hom$org_dim2, hom$mean_sup_dim2, use = "complete.obs")
fit <- lm(mean_sup_dim2 ~ org_dim1 + org_dim2, data = hom)
sfit <- summary(fit)
polos <- hom |> group_by(A_anclaje) |>
  summarise(n = n(), mean_access = round(mean(mean_sup_dim2, na.rm = TRUE), 3),
            .groups = "drop")

write.csv(hom, file.path(DIR_TAB, "tab_b7f_homologia.csv"), row.names = FALSE)

# ── resumen a consola ────────────────────────────────────────────────────────
cat(sprintf("n órganos: %d | Benzécri: %.1f / %.1f / %.1f\n",
            n_org, mr[1], mr[2], mr[3]))
cat(sprintf("pasivadas: %s\n", paste(RARE, collapse = ", ")))
cat("\ncontribución por variable (Dim.1/2/3):\n"); print(vc, row.names = FALSE)
for (d in 1:3) {
  top <- order(-mca$var$contrib[, d])[1:8]
  cat(sprintf("\nDim.%d top:\n", d))
  print(data.frame(cat = rownames(mca$var$coord)[top],
                   coord = round(mca$var$coord[top, d], 2),
                   ctr = round(mca$var$contrib[top, d], 1)), row.names = FALSE)
}
cat(sprintf("\nHomología: cor(dim1,access)=%.3f | cor(dim2,access)=%.3f | R²=%.3f\n",
            c1, c2, sfit$r.squared))
cat(sprintf("  org_dim1 β=%.3f (p=%.3g) | org_dim2 β=%.3f (p=%.3g)\n",
            coef(fit)[2], sfit$coefficients[2, 4],
            coef(fit)[3], sfit$coefficients[3, 4]))
cat("\nmedia de acceso por anclaje:\n"); print(as.data.frame(polos), row.names = FALSE)
cat("\nOK -> tables/tab_b7f_{benzecri,categorias,variables,homologia}.csv\n")
