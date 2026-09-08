# ─────────────────────────────────────────────────────────────────────────────
# 2026_3 — Opción 1: el campo burocrático del Estado comprador
# 00 — Panel de órganos (feasibility, no MCA todavía)
#
# Unidad = organismo (162 filas). Universo = adjudicaciones_tipo.parquet,
# ventana 2019-2025 con es_nuevo (igual que el espacio de proveedores, para
# que los dos espacios sean comparables). EXCEPCIÓN: la cohorte de entrada se
# calcula sobre el registro COMPLETO (2016+), porque si se usara la ventana
# todos los órganos previos a 2019 colapsarían en "macri" y la cohorte no
# distinguiría el arribo escalonado.
#
# Seis variables activas a nivel de organismo (espejo de las 6 del proveedor):
#   A_escala    terciles de la mediana del monto ARS deflactado (ventana)
#   A_directa   proporción de contratación directa (ventana, corte 3-niveles)
#   A_tipo      el six-type (tipo_organismo, hand-checked en 09_buyer_typology)
#   A_cohorte   primer año en la plataforma (registro completo)
#   A_anclaje   nº de localidades UOC distintas (flujos)  — variable física
#   A_alcance   mediana distancia gran-círculo (flujos)   — variable física
#
# Las dos variables físicas salen de flujos_adjudicacion.parquet (arrow/UTF-8),
# no del gazetteer CSV, para evitar el riesgo de encoding en la clave saf.
#
# Correr:  Rscript 00_organ_panel.R   (desde 2026_3/)
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(tidyr); library(readr)
})

source("../2026_2/theme_house.R", chdir = TRUE)
# theme_house.R fija DIR_* como rutas relativas a "data/processed"; al correr
# desde 2026_3 apuntan mal, así que se sobreescriben para leer los datos de
# 2026_2 y escribir las salidas en 2026_3.
DIR_PROC <- "../2026_2/data/processed"
DIR_RAW  <- "../2026_2/data/raw"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
dir.create(DIR_TAB, showWarnings = FALSE)
dir.create(DIR_FIG, showWarnings = FALSE)

adj_full <- read_parquet(file.path(DIR_PROC, "adjudicaciones_tipo.parquet"))
ipc <- read.csv(file.path(DIR_PROC, "ipc_anual.csv"))

# universo de la ventana (comparable al espacio de proveedores)
adj <- adj_full |> filter(es_nuevo, between(ejercicio, WIN0, WIN1))

# ── cohorte de entrada: primer award válido, registro completo ───────────────
cohorte <- adj_full |> filter(es_nuevo) |>
  group_by(organismo) |>
  summarise(min_ej = min(ejercicio, na.rm = TRUE), .groups = "drop")

# ── activas de ventana, a nivel de órgano ────────────────────────────────────
organ <- adj |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024), by = "ejercicio") |>
  mutate(monto_real = ifelse(moneda == "ARS", monto * deflactor_2024, NA_real_)) |>
  group_by(organismo) |>
  summarise(
    n_adj = n(),
    tipo = dplyr::first(tipo_organismo),
    anclaje_flag = dplyr::first(anclaje_territorial),
    med_real = median(monto_real, na.rm = TRUE),
    share_directa = mean(grepl("Directa", procedimiento, ignore.case = TRUE)),
    .groups = "drop")

# ── variables físicas desde flujos (parquet, sin riesgo de encoding) ─────────
fl <- read_parquet(file.path(DIR_PROC, "flujos_adjudicacion.parquet")) |>
  mutate(Ejercicio = as.numeric(Ejercicio),
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
  group_by(saf) |>
  summarise(n_loc = n_distinct(loc_uoc[!is.na(loc_uoc)]),
            med_km = median(km, na.rm = TRUE),
            n_geo = n(), .groups = "drop")

panel <- organ |>
  left_join(cohorte, by = "organismo") |>
  left_join(fisico, by = c("organismo" = "saf"))

# ── cortes a categorías activas ──────────────────────────────────────────────
tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  out[ok] <- mid
  out[ok & x <= q[1]] <- lo
  out[ok & x > q[2]] <- hi
  out
}

panel <- panel |> mutate(
  A_escala = ifelse(is.na(med_real), "sin_ars",
                    tercile_cut(med_real, "chica", "media", "grande")),
  A_directa = ifelse(share_directa <= 0.001, "sin_directa",
                     ifelse(share_directa <= 0.5, "mixto", "mayoria_directa")),
  A_tipo = tipo,
  A_cohorte = ifelse(min_ej < 2019, "pre2019",
                     ifelse(min_ej <= 2021, "2019_21", "2022plus")),
  A_anclaje = tercile_cut(n_loc, "bajo", "medio", "alto"),
  A_alcance = tercile_cut(med_km, "corto", "medio", "largo"))

# ── reporte de factibilidad ──────────────────────────────────────────────────
n_org <- nrow(panel)
actives <- c("A_escala", "A_directa", "A_tipo", "A_cohorte",
             "A_anclaje", "A_alcance")
cat(sprintf("\n=== panel de órganos: %d organismos ===\n", n_org))
cat(sprintf("n_adj por órgano: min %d | mediana %d | max %d | con <10 adj: %d\n",
            min(panel$n_adj), median(panel$n_adj), max(panel$n_adj),
            sum(panel$n_adj < 10)))

for (a in actives) {
  cat(sprintf("\n-- %s --\n", a))
  print(as.data.frame(table(panel[[a]], useNA = "ifany")))
}

thresh <- 0.05 * n_org
cat(sprintf("\numbral raro (<5%%): %.1f órganos\n", thresh))
cat(sprintf("A_anclaje NA: %d | A_alcance NA: %d | A_escala sin_ars: %d\n",
            sum(is.na(panel$A_anclaje)), sum(is.na(panel$A_alcance)),
            sum(panel$A_escala == "sin_ars", na.rm = TRUE)))
cat(sprintf("n_loc: %d valores distintos | min %d | mediana %d | max %d\n",
            n_distinct(panel$n_loc, na.rm = TRUE),
            min(panel$n_loc, na.rm = TRUE), median(panel$n_loc, na.rm = TRUE),
            max(panel$n_loc, na.rm = TRUE)))
cat(sprintf("med_km: %d órganos con km | mediana %.0f km\n",
            sum(!is.na(panel$med_km)), median(panel$med_km, na.rm = TRUE)))
cat(sprintf("cortes escala: %.1f / %.1f  |  alcance: %.0f / %.0f km\n",
            quantile(panel$med_real, 1 / 3, na.rm = TRUE),
            quantile(panel$med_real, 2 / 3, na.rm = TRUE),
            quantile(panel$med_km, 1 / 3, na.rm = TRUE),
            quantile(panel$med_km, 2 / 3, na.rm = TRUE)))

# ── salidas ──────────────────────────────────────────────────────────────────
write.csv(panel, file.path(DIR_TAB, "tab_organ_panel.csv"), row.names = FALSE)

freq <- do.call(rbind, lapply(actives, function(a) {
  t <- table(panel[[a]], useNA = "ifany")
  data.frame(variable = a, categoria = names(t), n = as.integer(t),
             row.names = NULL)
}))
freq <- freq |> mutate(share = round(n / n_org, 3), rara = n < thresh)
write.csv(freq, file.path(DIR_TAB, "tab_organ_actives_freq.csv"), row.names = FALSE)

cat("\nOK -> tables/tab_organ_panel.csv, tables/tab_organ_actives_freq.csv\n")
