# 45 — Qué parte de cada nube corre entre órganos y qué parte dentro de ellos,
#      si la sede sigue diciendo algo cuando se sostiene el domicilio del
#      proveedor, y de dónde a dónde va el N del lado proveedor.
#
# Tres flancos que el artículo tenía abiertos y contesta con lo que ya calculó.
#
# 1. Cuatro de las siete coordenadas activas del espacio de unidades son del
#    órgano padre y se transmiten a sus unidades, así que las 457 unidades no
#    ocupan 457 posiciones: ocupan bastantes menos, y el primer eje corre casi
#    entero entre órganos. La tasa modificada de ese eje es alta en parte por
#    eso. El plano conjunto, en cambio, se construye desde las díadas, y ahí
#    cada unidad tiene posición propia. Esa diferencia es la defensa del
#    diseño y hasta ahora no estaba medida en ninguna tabla.
#
#    La descomposición se hace en la métrica de cada nube: pesos uniformes en
#    el ACM, que es como está construida, y masas en el AC del cuadro de
#    lazos. Se reportan las dos para que no quede escondida ninguna.
#
# 2. El límite que declara la sección 8 —que la sede podría estar leyendo la
#    composición de la oferta local y no el despliegue— se puede acotar:
#    sosteniendo el domicilio del proveedor, la brecha entre una sede en la
#    Capital y una sede en provincia se achica pero no desaparece.
#
# 3. El cuerpo nombra cuatro poblaciones de proveedores —10.602 en el registro,
#    10.580 en el espacio, 10.543 en el cuadro de lazos, 10.529 después del
#    apartado— y ninguna tabla decía qué se va en cada paso. La regla del árbol
#    pide total, cada exclusión con su conteo, N final. Los conteos se leen de
#    los artefactos que cada paso ya escribió, no se re-derivan los filtros.
#
# No refitea nada y no mueve ningún sorteo: lee las coordenadas que escribieron
# el 31 y el 32 y cuenta sobre los mismos parquets. Precedente: 39, 40, 42, 44.
#
# Outputs (tables/): tab_estructura_organo.csv     entre y dentro del órgano en
#                                                  las dos nubes
#                    tab_homologia_domicilio.csv   la brecha de sede con el
#                                                  domicilio sostenido
#                    tab_homologia_domicilio_intra.csv  la misma, por órgano
#                    tab_proveedor_cobertura.csv   la cadena de N del lado
#                                                  proveedor
#
# Run: Rscript 45_estructura_organo.R   (desde 2026_3/, después de 32)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(dplyr)})

# mínimo de proveedores del tipo considerado para que una unidad entre en una
# comparación restringida: por debajo la media de la unidad es ruido
N_MIN_TIPO <- 10

# ── 1. Entre órganos y dentro del órgano ─────────────────────────────────────

u <- read.csv(file.path(DIR_TAB, "tab_unidad_coords.csv"),
              encoding = "UTF-8", stringsAsFactors = FALSE) |>
  mutate(saf_id = as.character(saf_id))
j <- read.csv(file.path(DIR_TAB, "tab_conjunto_unidades.csv"),
              encoding = "UTF-8", stringsAsFactors = FALSE) |>
  mutate(saf_id = as.character(saf_id))

# razón de correlación de Le Roux y Rouanet: la parte de la varianza de un eje
# que separa a los grupos, aquí los órganos. w son los pesos de la nube.
eta2 <- function(x, g, w = rep(1, length(x))) {
  w <- w / sum(w)
  mu <- sum(w * x)
  entre <- tapply(seq_along(x), g, function(i) sum(w[i]) * (sum(w[i] * x[i]) / sum(w[i]) - mu)^2)
  sum(entre) / sum(w * (x - mu)^2)
}

# parte de la inercia del eje que aporta un órgano, en la métrica de la nube
ctr_organo <- function(x, g, w = rep(1, length(x))) {
  w <- w / sum(w); mu <- sum(w * x)
  tapply(seq_along(x), g, function(i) sum(w[i] * (x[i] - mu)^2)) / sum(w * (x - mu)^2)
}

fila <- function(nube, eje, x, g, w, pos) {
  ctr <- ctr_organo(x, g, w)
  n_org <- table(g)
  mayor <- names(which.max(n_org))
  data.frame(
    nube = nube, eje = eje, n = length(x),
    posiciones_distintas = pos,
    eta2_nube = round(eta2(x, g, w), 3),
    eta2_uniforme = round(eta2(x, g), 3),
    mayor_organo = mayor,
    mayor_organo_unidades = as.integer(n_org[[mayor]]),
    mayor_organo_unidades_pct = round(100 * n_org[[mayor]] / length(x), 1),
    mayor_organo_inercia_pct = round(100 * ctr[[mayor]], 1),
    row.names = NULL)
}

pos_u <- nrow(unique(u[, c("unidad_dim1", "unidad_dim2")]))
pos_j <- nrow(unique(j[, c("f1", "f2")]))
wj <- j$masa

est <- rbind(
  fila("Space of units", 1, u$unidad_dim1, u$saf_id, rep(1, nrow(u)), pos_u),
  fila("Space of units", 2, u$unidad_dim2, u$saf_id, rep(1, nrow(u)), pos_u),
  fila("Joint plane", 1, j$f1, j$saf_id, wj, pos_j),
  fila("Joint plane", 2, j$f2, j$saf_id, wj, pos_j),
  fila("Mean access position", NA, j$acceso, j$saf_id, rep(1, nrow(j)), nrow(j)))
write.csv(est, file.path(DIR_TAB, "tab_estructura_organo.csv"), row.names = FALSE)

cat(sprintf("espacio de unidades: %d posiciones distintas sobre %d unidades, eta2 eje 1 = %.3f\n",
            pos_u, nrow(u), est$eta2_nube[1]))
cat(sprintf("plano conjunto: %d posiciones distintas sobre %d unidades, eta2 eje 1 = %.3f (uniforme %.3f)\n",
            pos_j, nrow(j), est$eta2_nube[3], est$eta2_uniforme[3]))

# ── 2. La sede con el domicilio del proveedor sostenido ──────────────────────

adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1)) |>
  select(doc_contractual, cuit)
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  select(doc_contractual, uoc_id)
prov <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet")) |>
  select(cuit, domicilio = provincia, sup_dim2)

# las 455 unidades del plano con sede localizada: la comparación es entre una
# sede en la Capital y una sede en provincia, así que la unidad sin sede queda
# fuera por definición
sedes <- j |> filter(!is.na(sede_provincia)) |>
  select(uoc_id, saf_id, sede_provincia)

lazos <- adj |>
  inner_join(unidad, by = "doc_contractual") |>
  inner_join(prov, by = "cuit") |>
  inner_join(sedes, by = "uoc_id") |>
  distinct(uoc_id, saf_id, sede_provincia, cuit, domicilio, sup_dim2)

# de dónde vienen los proveedores de una unidad
origen <- lazos |>
  group_by(uoc_id, saf_id, sede_provincia) |>
  summarise(n_prov = n(),
            share_local = mean(domicilio == sede_provincia),
            share_capital = mean(domicilio == JURIS_REF),
            .groups = "drop") |>
  filter(n_prov >= 20) |>
  mutate(capital = sede_provincia == JURIS_REF)

cat(sprintf("origen: unidades de provincia con %.0f %% de sus proveedores en su propia jurisdicción y %.0f %% en la Capital\n",
            100 * mean(origen$share_local[!origen$capital]),
            100 * mean(origen$share_capital[!origen$capital])))

# la brecha de sede sobre subconjuntos de proveedores definidos por domicilio
brecha <- function(sub, etiqueta, minimo) {
  g <- sub |> group_by(uoc_id, saf_id, sede_provincia) |>
    summarise(n = n(), acceso = mean(sup_dim2), .groups = "drop") |>
    filter(n >= minimo) |>
    mutate(capital = sede_provincia == JURIS_REF)
  data.frame(
    conjunto = etiqueta,
    unidades_capital = sum(g$capital), unidades_provincia = sum(!g$capital),
    acceso_capital = round(mean(g$acceso[g$capital]), 3),
    acceso_provincias = round(mean(g$acceso[!g$capital]), 3),
    brecha = round(mean(g$acceso[g$capital]) - mean(g$acceso[!g$capital]), 3),
    row.names = NULL)
}

dom <- rbind(
  brecha(lazos, "All suppliers", 20),
  brecha(filter(lazos, domicilio == JURIS_REF), "Domiciled in the federal capital", N_MIN_TIPO),
  brecha(filter(lazos, domicilio == "Buenos Aires"), "Domiciled in Buenos Aires province", N_MIN_TIPO))
dom$brecha_relativa <- round(dom$brecha / dom$brecha[1], 2)

# el mismo corte dentro de los siete órganos de la prueba intra-órgano
siete <- read.csv(file.path(DIR_TAB, "tab_intraorgano_organos.csv"),
                  encoding = "UTF-8", stringsAsFactors = FALSE) |>
  mutate(saf_id = as.character(saf_id))
intra <- lazos |>
  filter(saf_id %in% siete$saf_id, domicilio == JURIS_REF) |>
  group_by(uoc_id, saf_id, sede_provincia) |>
  summarise(n = n(), acceso = mean(sup_dim2), .groups = "drop") |>
  filter(n >= N_MIN_TIPO) |>
  mutate(capital = sede_provincia == JURIS_REF) |>
  group_by(saf_id) |>
  filter(any(capital), any(!capital)) |>
  summarise(unidades = n(),
            acceso_capital = round(mean(acceso[capital]), 3),
            acceso_provincias = round(mean(acceso[!capital]), 3),
            .groups = "drop") |>
  mutate(brecha = round(acceso_capital - acceso_provincias, 3),
         capital_mas_alta = acceso_capital > acceso_provincias) |>
  left_join(select(siete, saf_id, organismo), by = "saf_id") |>
  relocate(organismo, .after = saf_id) |>
  arrange(desc(unidades))

write.csv(dom, file.path(DIR_TAB, "tab_homologia_domicilio.csv"), row.names = FALSE)
write.csv(intra, file.path(DIR_TAB, "tab_homologia_domicilio_intra.csv"), row.names = FALSE)

cat(sprintf("brecha de sede: todos %.3f | domiciliados en la Capital %.3f (%.0f %% de la anterior) | en Buenos Aires %.3f\n",
            dom$brecha[1], dom$brecha[2], 100 * dom$brecha_relativa[2], dom$brecha[3]))
cat(sprintf("dentro del órgano, sólo proveedores de la Capital: la capital es la más alta en %d de %d órganos\n",
            sum(intra$capital_mas_alta), nrow(intra)))

# ── 3. La cadena de N del lado proveedor ─────────────────────────────────────
# Cada paso se cuenta sobre el artefacto que ese paso escribió, para que la
# tabla no pueda quedar desfasada de la cadena que documenta.

c10 <- read_parquet(file.path(DIR_PROC3, "proveedores_c10.parquet")) |> select(cuit)
plano <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet")) |> select(cuit)
todas <- adj |> inner_join(unidad, by = "doc_contractual")
u457 <- unique(u$uoc_id)
u456 <- unique(j$uoc_id)

paso <- function(etiqueta, sup, adjs) {
  data.frame(paso = etiqueta, proveedores = length(sup),
             adjudicaciones = adjs, row.names = NULL)
}
t457 <- todas |> filter(uoc_id %in% u457, cuit %in% c10$cuit)
t456 <- todas |> filter(uoc_id %in% u456, cuit %in% plano$cuit)

cob <- rbind(
  paso("In the window, new awards, supplier carrying a province",
       unique(adj$cuit), nrow(adj)),
  paso("Less those with no record in the supplier register",
       c10$cuit, NA_integer_),
  paso("Suppliers of the 457 units holding twenty or more suppliers",
       unique(t457$cuit), nrow(t457)),
  paso("Less the unit set aside before the plane was read, and its suppliers tied to no other unit",
       plano$cuit, nrow(t456)))
write.csv(cob, file.path(DIR_TAB, "tab_proveedor_cobertura.csv"), row.names = FALSE)

cat(sprintf("cadena de proveedores: %s\n", paste(cob$proveedores, collapse = " -> ")))
cat(sprintf("cadena de adjudicaciones: %s\n",
            paste(cob$adjudicaciones[!is.na(cob$adjudicaciones)], collapse = " -> ")))
cat("OK -> tables/tab_estructura_organo.csv, tab_homologia_domicilio{,_intra}.csv, tab_proveedor_cobertura.csv\n")
