# 42 — Quiénes son los proveedores que el Estado nombró.
#
# La sección 7 muestra dónde cae la nominación en el plano, y no dice nada de
# los nombrados. Son 1.224 empresas a las que el Estado argentino declaró
# únicas capaces de proveer, o cuya competencia declaró la requerida, y el
# registro permite preguntarles dos cosas: ante cuántos organismos lo son, y
# cuánto duran.
#
# La primera importa porque separa dos lecturas de la consagración. Si un
# nombrado lo es ante muchos compradores, el acto confiere una posición en el
# mercado. Si lo es ante uno solo, confiere una posición ante ese comprador y
# el mercado no se entera.
#
# Descriptivo: cuenta, no modela. Sin sorteos.
#
# Run: Rscript 42_consagrados.R   (desde 2026_3/, después de 32)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow)})

sup <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet")) |>
  select(cuit, S_fundamento, S_tenure, S_intensidad)
adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  select(doc_contractual, uoc_id, saf_id)
# familia: la clase del fundamento con que se dejó la competencia abierta
apart <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_apartado.parquet")) |>
  select(doc_contractual, familia, apartado_num)

w <- adj |> inner_join(unidad, by = "doc_contractual") |>
  left_join(apart, by = "doc_contractual") |>
  mutate(familia = ifelse(is.na(familia), "competitivo", familia)) |>
  filter(cuit %in% sup$cuit)

por_prov <- w |> group_by(cuit) |>
  summarise(organos = n_distinct(saf_id), unidades = n_distinct(uoc_id),
            .groups = "drop")
nombrado_por <- w |> filter(familia == "cualitativo") |> group_by(cuit) |>
  summarise(organos_que_nombran = n_distinct(saf_id), .groups = "drop")

d <- sup |> left_join(por_prov, by = "cuit") |>
  left_join(nombrado_por, by = "cuit") |>
  mutate(organos_que_nombran = ifelse(is.na(organos_que_nombran), 0L,
                                      organos_que_nombran))
cons <- d |> filter(S_fundamento == "consagrado")
# the same question for each of the two grounds (5 Sep 2026): before how many
# organs is a supplier named on exclusivity (apartado 3), and on speciality
# (apartado 2)? Counts must match script 44's tab_causales_perfil.csv.
por_causal <- w |> filter(apartado_num %in% c(2, 3)) |>
  mutate(ground = ifelse(apartado_num %in% 2, "esp", "exc")) |>
  group_by(cuit, ground) |> summarise(organos = n_distinct(saf_id), .groups = "drop")
exc <- por_causal |> filter(ground == "exc"); esp <- por_causal |> filter(ground == "esp")

pct <- function(x) round(100 * mean(x), 1)
res <- data.frame(
  medida = c("named suppliers",
             "named by one organ only, %", "named by two or three organs, %",
             "named by four organs or more, %",
             "median organs buying from a named supplier",
             "five years or more in the register, named, %",
             "five years or more, exempted on the amount alone, %",
             "five years or more, never exempted, %",
             "twenty awards or more, named, %",
             "twenty awards or more, not named, %",
             "named on exclusivity, suppliers",
             "named on exclusivity, by one organ only, %",
             "named on speciality, suppliers",
             "named on speciality, by one organ only, %"),
  valor = c(nrow(cons),
            pct(cons$organos_que_nombran == 1),
            pct(cons$organos_que_nombran %in% 2:3),
            pct(cons$organos_que_nombran >= 4),
            median(cons$organos),
            pct(cons$S_tenure == "5+anios"),
            pct(d$S_tenure[d$S_fundamento == "solo_aritmetico"] == "5+anios"),
            pct(d$S_tenure[d$S_fundamento == "sin_fundamento"] == "5+anios"),
            pct(cons$S_intensidad == "20+adj"),
            pct(d$S_intensidad[d$S_fundamento != "consagrado"] == "20+adj"),
            nrow(exc), pct(exc$organos == 1),
            nrow(esp), pct(esp$organos == 1)))
write.csv(res, file.path(DIR_TAB, "tab_consagrados.csv"), row.names = FALSE)
print(res, row.names = FALSE)
cat("\ndistribución de organismos que nombran a un consagrado:\n")
print(table(pmin(cons$organos_que_nombran, 5)))
cat("OK -> tables/tab_consagrados.csv\n")
