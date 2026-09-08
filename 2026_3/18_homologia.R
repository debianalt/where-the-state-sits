# ─────────────────────────────────────────────────────────────────────────────
# 2026_3 — 18: homología entre el espacio de órganos (B7f) y el de proveedores.
#
# Testea el mecanismo central: la posición de un SAF en el espacio de órganos
# (volumen económico / presencia territorial) predice la posición media de
# acceso de sus proveedores en el espacio de proveedores (eje de acceso, dim2).
# Reconstruye el espacio de proveedores C10 (build_supplier_space) y el B7f.
#
# Correr:  Rscript 18_homologia.R   (desde 2026_3/)
# ─────────────────────────────────────────────────────────────────────────────

source("../2026_2/theme_house.R", chdir = TRUE)
DIR_PROC <- "../2026_2/data/processed"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressPackageStartupMessages({library(arrow); library(GDAtools)})

saf_of <- function(x) sub("^([0-9]+).*", "\\1", as.character(x))

# ── espacio de proveedores (C10) ─────────────────────────────────────────────
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC, "supplier_master.parquet"))
m <- build_supplier_space(adj, master)
X <- active_matrix(m)
mca_sup <- speMCA(X, excl = excl_index(X, rare_categories(X)), ncp = 2)
m$sup_dim2 <- mca_sup$ind$coord[, 2]   # eje de acceso (dim2)
m$sup_dim1 <- mca_sup$ind$coord[, 1]
cat(sprintf("proveedores: %d | Benzécri sup dim2: %.1f%%\n",
            nrow(m), modif.rate(mca_sup)$modif$mrate[2]))

# ── SAF → posición media de acceso de sus proveedores ────────────────────────
adj$saf_id <- saf_of(adj$organismo)
w <- adj |> filter(es_nuevo, between(ejercicio, WIN0, WIN1)) |>
  left_join(m |> select(cuit, sup_dim2, sup_dim1), by = "cuit")
saf_sup <- w |> group_by(saf_id) |>
  summarise(mean_sup_dim2 = mean(sup_dim2, na.rm = TRUE),
            mean_sup_dim1 = mean(sup_dim1, na.rm = TRUE),
            n_sup = n_distinct(cuit), .groups = "drop")

# ── espacio de órganos B7f ───────────────────────────────────────────────────
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

Xo <- saf[, c("A_escala", "A_directa", "A_tipo", "A_fundacion", "A_anclaje",
              "A_institucional", "A_presupuesto")] |>
  mutate(across(everything(), as.factor))
mca_org <- speMCA(Xo, excl = excl_index(Xo, rare_categories(Xo)), ncp = 2)
saf$org_dim1 <- mca_org$ind$coord[, 1]
saf$org_dim2 <- mca_org$ind$coord[, 2]
cat(sprintf("órganos B7f: %d | Benzécri org: %.1f / %.1f\n",
            nrow(saf), modif.rate(mca_org)$modif$mrate[1],
            modif.rate(mca_org)$modif$mrate[2]))

# ── homología ────────────────────────────────────────────────────────────────
hom <- saf_sup |> left_join(
  saf |> select(saf_id, org_dim1, org_dim2, tipo, estructura, anclaje = A_anclaje),
  by = "saf_id")

c1 <- cor(hom$org_dim1, hom$mean_sup_dim2, use = "complete.obs")
c2 <- cor(hom$org_dim2, hom$mean_sup_dim2, use = "complete.obs")
fit <- lm(mean_sup_dim2 ~ org_dim1 + org_dim2, data = hom)
s <- summary(fit)
cat(sprintf("\nn = %d SAFs\n", nrow(hom)))
cat(sprintf("cor(org_dim1, mean_sup_dim2) = %.3f\n", c1))
cat(sprintf("cor(org_dim2, mean_sup_dim2) = %.3f\n", c2))
cat(sprintf("lm R² = %.3f | org_dim1 β = %.3f (p = %.3g) | org_dim2 β = %.3f (p = %.3g)\n",
            s$r.squared, coef(fit)[2], s$coefficients[2, 4],
            coef(fit)[3], s$coefficients[3, 4]))

# lectura por polo: media de acceso de proveedores por categoría de anclaje
polos <- hom |> group_by(anclaje) |>
  summarise(n = n(), mean_access = round(mean(mean_sup_dim2, na.rm = TRUE), 3),
            .groups = "drop")
cat("\nmedia de acceso (dim2) de proveedores, por anclaje del órgano:\n")
print(as.data.frame(polos), row.names = FALSE)

write.csv(hom, file.path(DIR_TAB, "tab_homologia.csv"), row.names = FALSE)
cat("\nOK -> tables/tab_homologia.csv\n")
