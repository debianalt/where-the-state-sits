# ─────────────────────────────────────────────────────────────────────────────
# this project — 17: refinamiento de la batería — fundación vs cohorte, capital humano,
# y "sin_presupuesto".
#
# Testea: (1) A_fundacion (año de creación) contra A_cohorte (entrada a
# plataforma); (2) capital humano completo vía gastos en personal (inciso 1) —
# volumen (gastos_personal) o estructura (share_personal); (3) si soltar los 16
# "sin_presupuesto" cambia el espacio.
#
# Correr:  Rscript 17_mca_refinamiento.R   (desde la raíz del repositorio)
# ─────────────────────────────────────────────────────────────────────────────

source("theme_house.R", chdir = TRUE)
DIR_PROC <- "data/processed"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressMessages(library(GDAtools))

saf <- read.csv(file.path(DIR_TAB, "tab_saf_panel.csv"), encoding = "UTF-8")
fund <- read.csv(file.path("data", "processed", "tab_saf_fundacion.csv"),
                 encoding = "UTF-8")
gp <- read.csv(file.path("data", "processed", "tab_saf_gastos_personal.csv"),
               encoding = "UTF-8")

saf <- saf |> left_join(fund |> select(saf_id, anio_fundacion), by = "saf_id") |>
  left_join(gp |> select(saf_id, gastos_personal, share_personal), by = "saf_id")

tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- mid; out[ok & x <= q[1]] <- lo; out[ok & x > q[2]] <- hi
  out
}

saf <- saf |> mutate(
  A_escala  = factor(A_escala),
  A_directa = factor(A_directa),
  A_tipo    = factor(A_tipo),
  A_cohorte = factor(A_cohorte),
  A_anclaje = factor(A_anclaje),
  A_institucional = factor(A_institucional),
  A_presupuesto = factor(A_presupuesto),
  A_fundacion = factor(tercile_cut(anio_fundacion, "antiguo", "medio", "reciente")),
  A_personal  = factor(tercile_cut(gastos_personal, "bajo", "medio", "alto")),
  A_personal_share = factor(tercile_cut(share_personal, "baja", "media", "alta")))

cat(sprintf("cortes fundación (año): %.0f / %.0f\n",
            quantile(saf$anio_fundacion, 1 / 3), quantile(saf$anio_fundacion, 2 / 3)))
cat(sprintf("cortes share_personal: %.3f / %.3f\n",
            quantile(saf$share_personal, 1 / 3, na.rm = TRUE),
            quantile(saf$share_personal, 2 / 3, na.rm = TRUE)))

run <- function(vars, drop_sin = FALSE) {
  d <- saf
  if (drop_sin) d <- saf |> filter(A_presupuesto != "sin_presupuesto")
  X <- d[, vars, drop = FALSE] |> mutate(across(everything(), as.factor))
  RARE <- rare_categories(X)
  mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
  list(mca = mca, mr = modif.rate(mca)$modif$mrate, rare = RARE, n = nrow(X))
}

base6 <- c("A_escala", "A_directa", "A_tipo", "A_anclaje",
           "A_institucional", "A_presupuesto")
bats <- list(
  B7_cohorte  = c(base6, "A_cohorte"),
  B7f_fund    = c(base6, "A_fundacion"),
  B8_personal = c(base6, "A_fundacion", "A_personal"),
  B8_share    = c(base6, "A_fundacion", "A_personal_share"),
  B7f_drop    = c(base6, "A_fundacion"))

res <- list()
for (nm in names(bats)) {
  drop <- nm == "B7f_drop"
  r <- run(bats[[nm]], drop_sin = drop)
  res[[nm]] <- r
  cat(sprintf("\n══════ %s (n=%d, %d activas) ══════\n",
              nm, r$n, length(bats[[nm]])))
  cat(sprintf("Benzécri dim1/2/3: %.1f / %.1f / %.1f\n", r$mr[1], r$mr[2], r$mr[3]))
  vc <- data.frame(variable = sub("[.].*$", "", rownames(r$mca$var$contrib)),
                   r$mca$var$contrib[, 1:3], check.names = FALSE) |>
    aggregate(. ~ variable, data = _, FUN = sum)
  vc <- vc[order(-vc[[2]]), ]
  cat("contribución por variable (Dim.1/2/3):\n"); print(vc, row.names = FALSE)
  for (d in 1:2) {
    top <- order(-r$mca$var$contrib[, d])[1:5]
    cat(sprintf("Dim.%d top: %s\n", d, paste(sprintf(
      "%s (%.2f)", rownames(r$mca$var$coord)[top],
      r$mca$var$coord[top, d]), collapse = " | ")))
  }
}

comp <- data.frame(
  bateria = names(bats),
  n_activas = sapply(bats, length),
  n = sapply(res, function(r) r$n),
  benzecri_1 = sapply(res, function(r) round(r$mr[1], 1)),
  benzecri_2 = sapply(res, function(r) round(r$mr[2], 1)),
  benzecri_3 = sapply(res, function(r) round(r$mr[3], 1)),
  row.names = NULL)
write.csv(comp, file.path(DIR_TAB, "tab_saf_mca_refinamiento.csv"), row.names = FALSE)
cat("\n=== comparación de baterías ===\n"); print(comp, row.names = FALSE)
cat("\nOK -> tables/tab_saf_mca_refinamiento.csv\n")
