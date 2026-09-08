# ─────────────────────────────────────────────────────────────────────────────
# this project — 03: re-especificación con la variable institucional (B6i) + dotación.
#
# Agrega A_institucional (estructura organizativa, desde el link DeepSeek al
# INDEC) como activa al B5a, y proyecta la dotación como suplementaria.
# Compara Benzécri / contribuciones contra B5a para ver si la jerarquía
# bourdieuana (centralizada vs descentralizada vs desconcentrada) mejora el
# espacio.
#
# Correr:  Rscript 03_organ_mca_institucional.R   (desde la raíz del repositorio)
# ─────────────────────────────────────────────────────────────────────────────

source("theme_house.R", chdir = TRUE)
DIR_PROC <- "data/processed"
DIR_RAW  <- "data/raw"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressMessages(library(GDAtools))

panel <- read.csv(file.path(DIR_TAB, "tab_organ_panel.csv"),
                  encoding = "UTF-8", check.names = FALSE)
enr <- read.csv(file.path("data", "processed", "tab_organ_institucional_dotacion.csv"),
                encoding = "UTF-8")

panel <- panel |> left_join(
  enr |> select(organismo, estructura, dotacion_ref), by = "organismo")
stopifnot(nrow(panel) == 152)

panel <- panel |> mutate(
  A_escala  = factor(A_escala),
  A_directa = factor(A_directa),
  A_tipo    = factor(A_tipo),
  A_cohorte = factor(A_cohorte),
  A_anclaje = factor(ifelse(is.na(n_loc), "sin_geo",
                            ifelse(n_loc == 1, "una_sede", "multi_sede"))),
  A_institucional = factor(estructura),
  # dotación suplementaria: terciles de la no-missing + sin_dato
  S_dotacion = ifelse(is.na(dotacion_ref), "sin_dato",
                      cut(dotacion_ref,
                          quantile(dotacion_ref, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                          labels = c("chica", "media", "grande"),
                          include.lowest = TRUE)))

run <- function(vars) {
  X <- panel[, vars, drop = FALSE]
  RARE <- rare_categories(X)
  mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
  list(mca = mca, mr = modif.rate(mca)$modif$mrate, rare = RARE)
}

b5a  <- run(c("A_escala", "A_directa", "A_tipo", "A_cohorte", "A_anclaje"))
b6i  <- run(c("A_escala", "A_directa", "A_tipo", "A_cohorte", "A_anclaje",
              "A_institucional"))

report <- function(nm, r) {
  cat(sprintf("\n══════ %s ══════\n", nm))
  cat(sprintf("Benzécri dim1/2/3: %.1f / %.1f / %.1f\n", r$mr[1], r$mr[2], r$mr[3]))
  cat(sprintf("pasivadas: %s\n", paste(r$rare, collapse = ", ")))
  vc <- data.frame(variable = sub("[.].*$", "", rownames(r$mca$var$contrib)),
                   r$mca$var$contrib[, 1:3], check.names = FALSE) |>
    aggregate(. ~ variable, data = _, FUN = sum)
  vc <- vc[order(-vc[[2]]), ]
  cat("contribución por variable (Dim.1/2/3):\n")
  print(vc, row.names = FALSE)
  for (d in 1:2) {
    top <- order(-r$mca$var$contrib[, d])[1:6]
    cat(sprintf("Dim.%d top: %s\n", d, paste(sprintf(
      "%s (%.2f, ctr %.1f)", rownames(r$mca$var$coord)[top],
      r$mca$var$coord[top, d], r$mca$var$contrib[top, d]), collapse = " | ")))
  }
  r
}

report("B5a (línea de base, 5 activas)", b5a)
report("B6i (+ institucional, 6 activas)", b6i)

# suplementaria dotación en el espacio enriquecido
s <- supvar(b6i$mca, factor(panel$S_dotacion))
cat("\nDotación (suplementaria, coord / typic):\n")
print(data.frame(categoria = rownames(s$coord),
                 dim1 = round(s$coord[, 1], 3), dim2 = round(s$coord[, 2], 3),
                 typic1 = round(s$typic[, 1], 1), typic2 = round(s$typic[, 2], 1)),
      row.names = FALSE)

comp <- data.frame(bateria = c("B5a", "B6i"),
                   n_activas = c(5, 6),
                   benzecri_1 = c(b5a$mr[1], b6i$mr[1]),
                   benzecri_2 = c(b5a$mr[2], b6i$mr[2]),
                   benzecri_3 = c(b5a$mr[3], b6i$mr[3]))
comp[, 3:5] <- round(comp[, 3:5], 1)
write.csv(comp, file.path(DIR_TAB, "tab_organ_mca_b5a_vs_b6i.csv"), row.names = FALSE)
cat("\n=== comparación ===\n")
print(comp, row.names = FALSE)
