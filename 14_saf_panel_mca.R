# ─────────────────────────────────────────────────────────────────────────────
# this project — 14: consolidación por SAF + presupuesto + re-especificación.
#
# El prefijo numérico del nombre del órgano es el código SAF (servicio_id):
# re-agrega el panel a nivel SAF (152 name-strings → ~122 SAFs) y agrega
# A_presupuesto (credito_presupuestado 2024, el capital económico real) como
# séptima activa. Compara B6i (6 activas) contra B7 (7 activas) a nivel SAF.
#
# Correr:  Rscript 14_saf_panel_mca.R   (desde la raíz del repositorio)
# ─────────────────────────────────────────────────────────────────────────────

source("theme_house.R", chdir = TRUE)
DIR_PROC <- "data/processed"
DIR_RAW  <- "data/raw"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
suppressPackageStartupMessages({library(arrow); library(GDAtools)})

saf_of <- function(x) sub("^([0-9]+).*", "\\1", as.character(x))

# ── adjudicaciones a nivel SAF ───────────────────────────────────────────────
adj <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
adj$saf_id <- saf_of(adj$organismo)
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))

w <- adj |> filter(es_nuevo, between(ejercicio, WIN0, WIN1)) |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024), by = "ejercicio") |>
  mutate(monto_real = ifelse(moneda == "ARS", monto * deflactor_2024, NA_real_))

saf <- w |> group_by(saf_id) |>
  summarise(n_adj = n(),
            tipo = names(sort(table(tipo_organismo), decreasing = TRUE))[1],
            med_real = median(monto_real, na.rm = TRUE),
            share_directa = mean(grepl("Directa", procedimiento,
                                       ignore.case = TRUE)),
            .groups = "drop")

cohorte <- adj |> filter(es_nuevo) |> group_by(saf_id) |>
  summarise(min_ej = min(ejercicio, na.rm = TRUE), .groups = "drop")
saf <- saf |> left_join(cohorte, by = "saf_id")

# ── variables físicas a nivel SAF ────────────────────────────────────────────
fl <- read_parquet(file.path(DIR_PROC, "flujos_adjudicacion.parquet")) |>
  mutate(saf_id = saf_of(saf),
         Ejercicio = as.numeric(Ejercicio),
         lat_u = as.numeric(lat_u), lon_u = as.numeric(lon_u),
         lat_p = as.numeric(lat_p), lon_p = as.numeric(lon_p)) |>
  filter(between(Ejercicio, WIN0, WIN1))

hav_km <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 +
    cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371 * asin(pmin(1, sqrt(a)))
}
fisico <- fl |>
  mutate(km = hav_km(lat_u, lon_u, lat_p, lon_p)) |>
  group_by(saf_id) |>
  summarise(n_loc = n_distinct(loc_uoc[!is.na(loc_uoc)]),
            med_km = median(km, na.rm = TRUE), .groups = "drop")
saf <- saf |> left_join(fisico, by = "saf_id")

# ── presupuesto + institucional (SAF-level) ─────────────────────────────────
pres <- read.csv(file.path("data", "processed", "tab_saf_presupuesto.csv"),
                 encoding = "UTF-8")
pres <- pres |> mutate(saf_id = as.character(saf_id)) |>
  group_by(saf_id) |>
  summarise(presupuesto = first(credito_presupuestado), .groups = "drop")

inst <- read.csv(file.path("data", "processed",
                           "tab_organ_institucional_dotacion.csv"),
                 encoding = "UTF-8")
inst <- inst |> mutate(saf_id = saf_of(organismo)) |>
  group_by(saf_id) |>
  summarise(estructura = names(sort(table(estructura), decreasing = TRUE))[1],
            dotacion = first(na.omit(dotacion_ref)) |> suppressWarnings(),
            .groups = "drop")

saf <- saf |> left_join(pres, by = "saf_id") |> left_join(inst, by = "saf_id")

cat(sprintf("SAFs en el panel consolidado: %d\n", nrow(saf)))
cat(sprintf("con presupuesto: %d / %d\n",
            sum(!is.na(saf$presupuesto)), nrow(saf)))

# ── activas (7) ──────────────────────────────────────────────────────────────
tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- mid; out[ok & x <= q[1]] <- lo; out[ok & x > q[2]] <- hi
  out
}
saf <- saf |> mutate(
  A_escala = ifelse(is.na(med_real), "sin_ars",
                    tercile_cut(med_real, "chica", "media", "grande")),
  A_directa = ifelse(share_directa <= 0.001, "sin_directa",
                     ifelse(share_directa <= 0.5, "mixto", "mayoria_directa")),
  A_tipo = factor(tipo),
  A_cohorte = ifelse(min_ej < 2019, "pre2019",
                     ifelse(min_ej <= 2021, "2019_21", "2022plus")),
  A_anclaje = factor(ifelse(is.na(n_loc), "sin_geo",
                            ifelse(n_loc == 1, "una_sede", "multi_sede"))),
  A_institucional = factor(estructura),
  A_presupuesto = ifelse(is.na(presupuesto), "sin_presupuesto",
                         tercile_cut(presupuesto, "bajo", "medio", "alto")),
  S_dotacion = ifelse(is.na(dotacion), "sin_dato",
                      cut(dotacion, quantile(dotacion, c(0, 1/3, 2/3, 1),
                                             na.rm = TRUE),
                          labels = c("chica", "media", "grande"),
                          include.lowest = TRUE)))

run <- function(vars) {
  X <- saf[, vars, drop = FALSE] |> mutate(across(everything(), as.factor))
  RARE <- rare_categories(X)
  mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
  list(mca = mca, mr = modif.rate(mca)$modif$mrate, rare = RARE)
}

b6i <- run(c("A_escala", "A_directa", "A_tipo", "A_cohorte",
             "A_anclaje", "A_institucional"))
b7  <- run(c("A_escala", "A_directa", "A_tipo", "A_cohorte",
             "A_anclaje", "A_institucional", "A_presupuesto"))

report <- function(nm, r) {
  cat(sprintf("\n══════ %s ══════\n", nm))
  cat(sprintf("Benzécri dim1/2/3: %.1f / %.1f / %.1f\n", r$mr[1], r$mr[2], r$mr[3]))
  cat(sprintf("pasivadas: %s\n", paste(r$rare, collapse = ", ")))
  vc <- data.frame(variable = sub("[.].*$", "", rownames(r$mca$var$contrib)),
                   r$mca$var$contrib[, 1:3], check.names = FALSE) |>
    aggregate(. ~ variable, data = _, FUN = sum)
  vc <- vc[order(-vc[[2]]), ]
  cat("contribución por variable (Dim.1/2/3):\n"); print(vc, row.names = FALSE)
  for (d in 1:2) {
    top <- order(-r$mca$var$contrib[, d])[1:6]
    cat(sprintf("Dim.%d top: %s\n", d, paste(sprintf(
      "%s (%.2f, ctr %.1f)", rownames(r$mca$var$coord)[top],
      r$mca$var$coord[top, d], r$mca$var$contrib[top, d]), collapse = " | ")))
  }
  invisible(NULL)
}
report("B6i (6 activas, nivel SAF)", b6i)
report("B7 (+ presupuesto, 7 activas, nivel SAF)", b7)

s <- supvar(b7$mca, factor(saf$S_dotacion))
cat("\nDotación (suplementaria en B7):\n")
print(data.frame(categoria = rownames(s$coord),
                 dim1 = round(s$coord[, 1], 3), dim2 = round(s$coord[, 2], 3),
                 typic1 = round(s$typic[, 1], 1), typic2 = round(s$typic[, 2], 1)),
      row.names = FALSE)

comp <- data.frame(bateria = c("B6i", "B7"), n_activas = c(6, 7),
                   benzecri_1 = c(b6i$mr[1], b7$mr[1]),
                   benzecri_2 = c(b6i$mr[2], b7$mr[2]),
                   benzecri_3 = c(b6i$mr[3], b7$mr[3]))
comp[, 3:5] <- round(comp[, 3:5], 1)
write.csv(comp, file.path(DIR_TAB, "tab_saf_mca_b6i_vs_b7.csv"), row.names = FALSE)
write.csv(saf, file.path(DIR_TAB, "tab_saf_panel.csv"), row.names = FALSE)
cat("\n=== comparación ===\n"); print(comp, row.names = FALSE)
cat("\nOK -> tables/tab_saf_panel.csv, tables/tab_saf_mca_b6i_vs_b7.csv\n")
