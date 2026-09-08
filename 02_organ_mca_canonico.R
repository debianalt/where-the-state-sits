# ─────────────────────────────────────────────────────────────────────────────
# this project — 02: solución canónica del espacio de órganos (B5a) + suplementarias.
#
# B5a = escala + directa + tipo + cohorte + anclaje (decisión de 01_organ_mca.R,
# registrada en notes/RESULTS_MEMO.md). Anclaje se re-binea aquí como
# una_sede/multi_sede/sin_geo; alcance (descartado como activa por colinealidad)
# entra como suplementaria junto con el flag binario y el tamaño en
# adjudicaciones.
#
# Correr:  Rscript 02_organ_mca_canonico.R   (desde la raíz del repositorio)
# ─────────────────────────────────────────────────────────────────────────────

source("theme_house.R", chdir = TRUE)
DIR_PROC <- "data/processed"
DIR_RAW  <- "data/raw"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressMessages(library(GDAtools))

panel <- read.csv(file.path(DIR_TAB, "tab_organ_panel.csv"),
                  encoding = "UTF-8", check.names = FALSE)
n_org <- nrow(panel)

panel <- panel |> mutate(
  A_escala  = factor(A_escala),
  A_directa = factor(A_directa),
  A_tipo    = factor(A_tipo),
  A_cohorte = factor(A_cohorte),
  A_anclaje = factor(ifelse(is.na(n_loc), "sin_geo",
                            ifelse(n_loc == 1, "una_sede", "multi_sede"))),
  # suplementarias
  S_flag_anclado = factor(ifelse(anclaje_flag, "anclado", "no_anclado")),
  S_alcance = factor(ifelse(is.na(med_km), "sin_geo",
                            ifelse(med_km == 0, "local", "lejano"))),
  S_tam = factor(cut(n_adj, quantile(n_adj, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     labels = c("chico", "medio", "grande"), include.lowest = TRUE)))

X <- panel[, c("A_escala", "A_directa", "A_tipo", "A_cohorte", "A_anclaje")]
RARE <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
mr <- modif.rate(mca)$modif$mrate

# ── tablas canónicas ─────────────────────────────────────────────────────────
benz <- data.frame(dim = 1:3, eigenvalue = round(mca$eig$eigen[1:3], 4),
                   pct_benzecri = round(mr[1:3], 1))
write.csv(benz, file.path(DIR_TAB, "tab_organ_benzecri.csv"), row.names = FALSE)

frec <- unlist(lapply(names(X), function(v)
  setNames(as.numeric(table(X[[v]])), paste0(v, ".", levels(X[[v]])))))
cats <- data.frame(categoria = rownames(mca$var$coord),
                   n = frec[rownames(mca$var$coord)],
                   round(mca$var$coord[, 1:3], 2),
                   ctr = round(mca$var$contrib[, 1:3], 1),
                   cos2 = round(mca$var$coord[, 1:3]^2 /
                                  (n_org / frec[rownames(mca$var$coord)] - 1), 3),
                   check.names = FALSE)
write.csv(cats, file.path(DIR_TAB, "tab_organ_categorias.csv"), row.names = FALSE)

vars_ctr <- data.frame(variable = sub("[.].*$", "", rownames(mca$var$contrib)),
                       mca$var$contrib[, 1:3], check.names = FALSE) |>
  aggregate(. ~ variable, data = _, FUN = sum)
write.csv(vars_ctr, file.path(DIR_TAB, "tab_organ_variables.csv"), row.names = FALSE)

# ── suplementarias ───────────────────────────────────────────────────────────
sup_of <- function(f, nombre) {
  s <- supvar(mca, f)
  data.frame(variable = nombre, categoria = rownames(s$coord),
             round(s$coord[, 1:3], 3), typic = round(s$typic[, 1:3], 1),
             check.names = FALSE)
}
supvars <- bind_rows(
  sup_of(panel$S_flag_anclado, "flag_anclado"),
  sup_of(panel$S_alcance, "alcance"),
  sup_of(panel$S_tam, "tamano"))
write.csv(supvars, file.path(DIR_TAB, "tab_organ_supvars.csv"), row.names = FALSE)

# ── coordenadas por órgano (para la homología) ───────────────────────────────
coords <- as.data.frame(mca$ind$coord)[, 1:3] |>
  setNames(c("dim1", "dim2", "dim3"))
panel_out <- bind_cols(panel, coords)
write.csv(panel_out, file.path(DIR_TAB, "tab_organ_coords.csv"), row.names = FALSE)

# ── lectura de ejes ──────────────────────────────────────────────────────────
cat(sprintf("Benzécri dim1/2/3: %.1f / %.1f / %.1f\n", mr[1], mr[2], mr[3]))
cat(sprintf("pasivadas: %s\n", paste(RARE, collapse = ", ")))
for (d in 1:3) {
  top <- order(-mca$var$contrib[, d])[1:6]
  cat(sprintf("Dim.%d top: %s\n", d, paste(sprintf(
    "%s (%.2f, ctr %.1f)", rownames(mca$var$coord)[top],
    mca$var$coord[top, d], mca$var$contrib[top, d]), collapse = " | ")))
}
cat("\nSuplementarias (coord / typic por dim1):\n")
print(supvars, row.names = FALSE)
cat("\nOK -> tables/tab_organ_{benzecri,categorias,variables,supvars,coords}.csv\n")
