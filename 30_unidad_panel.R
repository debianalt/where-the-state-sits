# 30 — The panel of buying units: one row per unit, seven coordinates, and the
# mean position of its suppliers.
#
# The unit of this paper is the buying unit (UOC) with its seat, not the organ
# (SAF): an organ mixes its ministry in the capital with its delegations, and
# the deployment argument is about where each unit sits. Seven coordinates
# mirror the organ battery B7f at the unit level — scale and procedure of the
# exchange (measured on the unit's own awards), functional type, seniority
# (ancienneté, the founding year of the parent organ: a temporal dimension of
# accumulation, not a species of capital), the jurisdiction of the seat,
# institutional rank and budget of the parent. The mean access and content positions of the unit's
# suppliers come from the supplier space C10, fitted once here and written to
# data/processed/ so that no later script refits it.
#
# Run: Rscript 30_unidad_panel.R   (from the repository root)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(GDAtools)})
dir.create(DIR_TAB, showWarnings = FALSE); dir.create(DIR_PROC3, showWarnings = FALSE,
                                                       recursive = TRUE)
N_MIN_SUP <- 20

adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC2, "supplier_master.parquet"))
ipc <- read.csv(file.path(DIR_PROC2, "ipc_anual.csv"))
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet"))

# ── the supplier space, once ─────────────────────────────────────────────────
m <- fit_supplier_space(adj, master)
# the two sides of the market are read on the same 24 jurisdictions: the
# province of a supplier's fiscal domicile and the jurisdiction of a unit's
# seat. The provincial stratum build_supplier_space() also computes is not
# written out, so that nothing downstream can read a class in its place.
write_parquet(m |> select(cuit, provincia, starts_with("A_"),
                          starts_with("S_"), n_adjudicaciones, sup_dim1, sup_dim2),
              file.path(DIR_PROC3, "proveedores_c10.parquet"))
cat(sprintf("espacio de proveedores: %d, Benzécri %.1f / %.1f\n",
            nrow(m), attr(m, "benzecri")[1], attr(m, "benzecri")[2]))

# ── awards in the window, with their unit and their supplier's position ──────
w <- adj |> filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1)) |>
  left_join(ipc |> select(ejercicio = anio, deflactor_2024), by = "ejercicio") |>
  mutate(monto_real = ifelse(moneda == "ARS", monto * deflactor_2024, NA_real_)) |>
  inner_join(unidad |> select(doc_contractual, saf_id, uoc_id, uoc,
                              sede_localidad, sede_provincia),
             by = "doc_contractual") |>
  left_join(m |> select(cuit, sup_dim1, sup_dim2), by = "cuit")

# ── parent properties (this project data layer) ────────────────────────────────────
fund <- read.csv(file.path(DIR_PROC3, "tab_saf_fundacion.csv"), encoding = "UTF-8") |>
  mutate(saf_id = as.character(saf_id)) |> select(saf_id, anio_fundacion)
pres <- read.csv(file.path(DIR_PROC3, "tab_saf_presupuesto.csv"), encoding = "UTF-8") |>
  mutate(saf_id = as.character(saf_id)) |> select(saf_id, credito_vigente) |>
  distinct(saf_id, .keep_all = TRUE)
rango <- read.csv(file.path(DIR_PROC3, "tab_organ_institucional_dotacion.csv"),
                  encoding = "UTF-8") |>
  mutate(saf_id = saf_of(organismo)) |> select(saf_id, estructura) |>
  distinct(saf_id, .keep_all = TRUE)

# ── one row per unit ─────────────────────────────────────────────────────────
u <- w |> group_by(uoc_id) |>
  summarise(uoc = dplyr::first(uoc), saf_id = dplyr::first(saf_id),
            organismo = dplyr::first(organismo),
            tipo = names(sort(table(tipo_organismo), decreasing = TRUE))[1],
            rubro_modal = names(sort(table(rubro_principal), decreasing = TRUE))[1],
            anclado = mean(anclaje_territorial) > 0.5,
            n_adj = n(), n_sup = n_distinct(cuit),
            escala_med = median(monto_real, na.rm = TRUE),
            share_directa = mean(grepl("Directa", procedimiento, ignore.case = TRUE)),
            acceso = mean(sup_dim2, na.rm = TRUE),
            contenido = mean(sup_dim1, na.rm = TRUE),
            sede_localidad = dplyr::first(sede_localidad),
            sede_provincia = dplyr::first(sede_provincia),
            .groups = "drop") |>
  left_join(fund, by = "saf_id") |> left_join(pres, by = "saf_id") |>
  left_join(rango, by = "saf_id")

# how many distinct seats the parent organ has among its active units
sedes <- u |> filter(!is.na(sede_provincia)) |> group_by(saf_id) |>
  summarise(n_sedes = n_distinct(paste(sede_localidad, sede_provincia)),
            .groups = "drop")
u <- u |> left_join(sedes, by = "saf_id") |>
  mutate(n_sedes = ifelse(is.na(n_sedes), 1L, n_sedes))

u <- u |> filter(n_sup >= N_MIN_SUP)

# Seniority and budget are properties of the parent organ, so their terciles
# are cut over the 113 parents and then carried to the units. Cutting them over
# units lets one distributed organ fill a tercile by itself: with 130 units the
# border force alone made "middle seniority" mean 1934-1938.
padres <- u |> distinct(saf_id, anio_fundacion, credito_vigente) |>
  mutate(P_antiguedad = ifelse(is.na(anio_fundacion), "sin_dato",
                               tercile_cut(anio_fundacion, "antiguo", "medio", "reciente")),
         P_presupuesto = ifelse(is.na(credito_vigente), "sin_presupuesto",
                                tercile_cut(credito_vigente, "bajo", "medio", "alto")))
# The budget is in millions of pesos (the national series: the 2007 total of
# 148,299 is 148 thousand million pesos). A peso amount is unreadable to a
# foreign reader, so each cut is also given in dollars at the annual average
# official rate of its own year, 2024 being the year of the credit.
tc <- read.csv(file.path(DIR_PROC2, "tipo_cambio_anual.csv"))
tc24 <- tc$tc_oficial_prom[tc$anio == 2024]
stopifnot(length(tc24) == 1, is.finite(tc24))
q_fund <- quantile(padres$anio_fundacion, c(1 / 3, 2 / 3), na.rm = TRUE)
q_cred <- quantile(padres$credito_vigente, c(1 / 3, 2 / 3), na.rm = TRUE)
cortes <- data.frame(
  variable = c("anio_fundacion", "anio_fundacion", "credito_vigente", "credito_vigente"),
  corte = c("tercil 1", "tercil 2", "tercil 1", "tercil 2"),
  valor = c(q_fund, q_cred),
  valor_usd = c(NA_real_, NA_real_, q_cred / tc24),
  tipo_cambio = c(NA_real_, NA_real_, tc24, tc24))
write.csv(cortes, file.path(DIR_TAB, "tab_unidad_cortes_padre.csv"), row.names = FALSE)
cat(sprintf("cortes de presupuesto: %.1f y %.1f millones de pesos = %.1f y %.1f millones de USD (tc 2024 = %.2f)
",
            q_cred[1], q_cred[2], q_cred[1] / tc24, q_cred[2] / tc24, tc24))

u <- u |> left_join(padres |> select(saf_id, P_antiguedad, P_presupuesto), by = "saf_id") |>
  mutate(
  A_escala = ifelse(is.na(escala_med), "sin_ars",
                    tercile_cut(escala_med, "chica", "media", "grande")),
  A_directa = as.character(cut(share_directa, c(-0.01, 0.001, 0.5, 1.0),
                               labels = c("sin_directa", "mixto", "mayoria_directa"))),
  A_tipo = tipo,
  A_antiguedad = P_antiguedad,
  A_juris = juris_of(sede_provincia),
  A_rango = dplyr::case_when(
    estructura == "administracion_centralizada" ~ "centralizada",
    estructura == "administracion_descentralizada" ~ "descentralizada",
    estructura == "administracion_desconcentrada" ~ "desconcentrada",
    TRUE ~ "otro"),
  A_presupuesto = P_presupuesto,
  N_distribuido = dplyr::case_when(n_sedes >= 3 ~ "distribuido",
                                   n_sedes == 2 ~ "dos_sedes",
                                   TRUE ~ "una_sede"),
  N_tamano = tercile_cut(n_adj, "chica", "media", "grande"))

write.csv(u, file.path(DIR_TAB, "tab_unidad_panel.csv"), row.names = FALSE)

cat(sprintf("unidades con >= %d proveedores: %d, en %d órganos; con sede %d\n",
            N_MIN_SUP, nrow(u), n_distinct(u$saf_id), sum(u$A_juris != "sin_sede")))
for (v in c("A_escala", "A_directa", "A_tipo", "A_antiguedad", "A_juris", "A_rango",
            "A_presupuesto", "N_distribuido")) {
  cat(sprintf("  %-14s", v)); print(table(u[[v]]))
}
cat("OK -> tables/tab_unidad_panel.csv, data/processed/proveedores_c10.parquet\n")
