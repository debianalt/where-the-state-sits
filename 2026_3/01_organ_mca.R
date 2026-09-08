# ─────────────────────────────────────────────────────────────────────────────
# 2026_3 — 01: búsqueda de especificación del espacio de órganos (MCA).
#
# Compara baterías activas candidatas y decide cuál da la solución más limpia.
# Criterio del autor (31 ago): cuantas más activas mejor, pero con sentido
# bourdieuano y contribuciones/cos2 que cierren. Las dos variables físicas se
# re-binean aquí (en el script 0 sus terciles colapsaban): anclaje como
# una_sede/multi_sede, alcance como local/lejano, con sin_geo para los 5
# órganos sin flujo georreferenciado (es el análogo de "sin_ars" del proveedor).
#
# Correr:  Rscript 01_organ_mca.R   (desde 2026_3/)
# ─────────────────────────────────────────────────────────────────────────────

source("../2026_2/theme_house.R", chdir = TRUE)
DIR_PROC <- "../2026_2/data/processed"
DIR_RAW  <- "../2026_2/data/raw"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressMessages(library(GDAtools))

panel <- read.csv(file.path(DIR_TAB, "tab_organ_panel.csv"),
                  encoding = "UTF-8", check.names = FALSE)
n_org <- nrow(panel)

# re-binning de las dos físicas (los terciles del script 0 colapsaban)
panel <- panel |> mutate(
  A_escala  = factor(A_escala),
  A_directa = factor(A_directa),
  A_tipo    = factor(A_tipo),
  A_cohorte = factor(A_cohorte),
  A_anclaje = factor(ifelse(is.na(n_loc), "sin_geo",
                            ifelse(n_loc == 1, "una_sede", "multi_sede"))),
  A_alcance = factor(ifelse(is.na(med_km), "sin_geo",
                            ifelse(med_km == 0, "local", "lejano"))))

run_org <- function(vars) {
  X <- panel[, vars, drop = FALSE]
  RARE <- rare_categories(X)
  mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
  list(mca = mca, mr = modif.rate(mca)$modif$mrate, rare = RARE,
       n_cats = sum(sapply(X, nlevels)), n_ret = sum(sapply(X, nlevels)) - length(RARE))
}

batteries <- list(
  B4  = c("A_escala", "A_directa", "A_tipo", "A_cohorte"),
  B5a = c("A_escala", "A_directa", "A_tipo", "A_cohorte", "A_anclaje"),
  B5b = c("A_escala", "A_directa", "A_tipo", "A_cohorte", "A_alcance"),
  B6  = c("A_escala", "A_directa", "A_tipo", "A_cohorte", "A_anclaje", "A_alcance"))

comparacion <- list()
for (nm in names(batteries)) {
  r <- run_org(batteries[[nm]])
  comparacion[[nm]] <- r
  cat(sprintf("\n══════ %s — %d activas, %d categorías (%d pasivadas) ══════\n",
              nm, length(batteries[[nm]]), r$n_cats, length(r$rare)))
  cat(sprintf("Benzécri dim1/2/3: %.1f / %.1f / %.1f\n", r$mr[1], r$mr[2], r$mr[3]))
  cat(sprintf("pasivadas: %s\n", paste(r$rare, collapse = ", ")))

  vc <- data.frame(variable = sub("[.].*$", "", rownames(r$mca$var$contrib)),
                   r$mca$var$contrib[, 1:3], check.names = FALSE)
  vc <- aggregate(. ~ variable, vc, sum)
  vc <- vc[order(-vc[[2]]), ]
  cat("contribución por variable (Dim.1/Dim.2/Dim.3):\n")
  print(vc, row.names = FALSE)

  for (d in 1:3) {
    top <- order(-r$mca$var$contrib[, d])[1:6]
    cat(sprintf("Dim.%d top: %s\n", d, paste(sprintf(
      "%s (%.2f, ctr %.1f)", rownames(r$mca$var$coord)[top],
      r$mca$var$coord[top, d], r$mca$var$contrib[top, d]), collapse = " | ")))
  }
}

comp <- data.frame(
  bateria = names(batteries),
  n_activas = sapply(batteries, length),
  benzecri_1 = sapply(comparacion, function(r) round(r$mr[1], 1)),
  benzecri_2 = sapply(comparacion, function(r) round(r$mr[2], 1)),
  benzecri_3 = sapply(comparacion, function(r) round(r$mr[3], 1)),
  n_pasivadas = sapply(comparacion, function(r) length(r$rare)),
  row.names = NULL)
write.csv(comp, file.path(DIR_TAB, "tab_organ_mca_comparacion.csv"), row.names = FALSE)

cat(sprintf("\n=== comparación (n = %d órganos) ===\n", n_org))
print(comp, row.names = FALSE)
cat("\nOK -> tables/tab_organ_mca_comparacion.csv\n")
