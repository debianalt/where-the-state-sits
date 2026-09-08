# 25 — Robustez del sector: el espacio sin la variable sector (A_tipo).
#
# La tipología de seis tipos funcionales es hand-checked; si una categoría mal
# asignada distorsiona los ejes, el espacio cambia al quitarla. Aquí se re-estima
# el MCA con 6 activas (sin A_tipo) y se compara la geometría con la solución
# canónica B7f (7 activas) correlacionando los ejes.
#
# Correr:  Rscript 25_robustez_sector.R   (desde 2026_3/)
source("../2026_2/theme_house.R", chdir = TRUE)
DIR_PROC <- "../2026_2/data/processed"
DIR_TAB  <- "tables"
suppressPackageStartupMessages({library(arrow); library(GDAtools)})

saf <- read.csv(file.path(DIR_TAB, "tab_saf_panel.csv"), encoding = "UTF-8")
fund <- read.csv(file.path("data", "processed", "tab_saf_fundacion.csv"),
                 encoding = "UTF-8")
saf$saf_id <- as.character(saf$saf_id); fund$saf_id <- as.character(fund$saf_id)
saf <- saf |> left_join(fund |> select(saf_id, anio_fundacion), by = "saf_id")

tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- mid; out[ok & x <= q[1]] <- lo; out[ok & x > q[2]] <- hi; out
}
saf <- saf |> mutate(
  A_escala = factor(A_escala), A_directa = factor(A_directa),
  A_tipo = factor(A_tipo), A_anclaje = factor(A_anclaje),
  A_institucional = factor(A_institucional), A_presupuesto = factor(A_presupuesto),
  A_fundacion = factor(tercile_cut(anio_fundacion, "antiguo", "medio", "reciente")))

vars7 <- c("A_escala", "A_directa", "A_tipo", "A_fundacion", "A_anclaje",
           "A_institucional", "A_presupuesto")
vars6 <- setdiff(vars7, "A_tipo")

run <- function(vars) {
  X <- saf[, vars, drop = FALSE] |> mutate(across(everything(), as.factor))
  mca <- speMCA(X, excl = excl_index(X, rare_categories(X)), ncp = 2)
  list(mca = mca, mr = modif.rate(mca)$modif$mrate)
}
b7 <- run(vars7)
b6 <- run(vars6)

cc <- cor(b7$mca$ind$coord[, 1:2], b6$mca$ind$coord[, 1:2])
cat(sprintf("B7f (7 activas): Benzécri %.1f / %.1f\n", b7$mr[1], b7$mr[2]))
cat(sprintf("Sin sector (6 activas): Benzécri %.1f / %.1f\n", b6$mr[1], b6$mr[2]))
cat("Correlación de los ejes (7-act vs 6-act):\n")
print(round(cc, 3))

res <- data.frame(
  bateria = c("B7f", "sin_sector"),
  benzecri_1 = round(c(b7$mr[1], b6$mr[1]), 1),
  benzecri_2 = round(c(b7$mr[2], b6$mr[2]), 1),
  r_dim1 = c(1, round(cc[1, 1], 3)),
  r_dim2 = c(1, round(cc[2, 2], 3)))
write.csv(res, "tables/tab_robustez_sector.csv", row.names = FALSE)
cat("\nOK -> tables/tab_robustez_sector.csv\n")
