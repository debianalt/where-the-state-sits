# 43 — Figura 1: dónde está el Estado, y qué tipo de proveedor produce eso.
#
# El artículo argumenta que el despliegue territorial ordena el mercado y hasta
# ahora no mostraba el territorio. Las tres clases de sede son un colapso para
# construir el espacio; las 24 jurisdicciones son donde el hallazgo se ve, y en
# un mapa se ven sin intermediación de ninguna coordenada.
#
# (a) el despliegue: cada sede de compra en su lugar, con el símbolo
#     proporcional a cuántas unidades se asientan ahí. Son 137 sedes para 456
#     unidades. El centroide provincial, que este panel usaba antes, dibujaba
#     una marca donde no hay nada y el panel se titula "dónde está el Estado";
#     una coropleta tampoco sirve, porque CABA concentra 179 unidades en la
#     superficie más chica del país.
# (b) lo que produce: la posición media de acceso de los proveedores de las
#     unidades asentadas ahí, en relleno divergente, con el polígono de CABA
#     repetido y agrandado sobre el Río de la Plata, porque el suyo es
#     demasiado chico para verse.
#
# La geometría es la de 2026_2 y el patrón de mapa también (script 35): recorte,
# contorno del país por unión, y coord_sf(expand = FALSE, datum = NA), que fija
# la relación de aspecto por proyección — un mapa no lleva coord_fixed().
#
# Run: Rscript 43_fig_mapa.R   (desde 2026_3/, después de 30)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(sf); library(patchwork); library(arrow)})
DIR_RAW3 <- file.path("data", "raw")   # el crudo propio de 2026_3

# ── el dato, por jurisdicción ───────────────────────────────────────────────
p <- read.csv(file.path(DIR_TAB, "tab_unidad_panel.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE) |>
  filter(!is.na(sede_provincia)) |>
  group_by(sede_provincia) |>
  summarise(unidades = n(), acceso = mean(acceso), .groups = "drop")

# el gacetero nombra las jurisdicciones como el registro; el geojson, como el
# nomenclador oficial. Dos difieren y se resuelven acá, no a mano en el mapa.
GEO_A_PANEL <- c("Ciudad Autónoma de Buenos Aires" = "CABA",
                 "Tierra del Fuego, Antártida e Islas del Atlántico Sur" =
                   "Tierra del Fuego")

geo <- st_read(file.path(DIR_RAW, "georef_provincias.geojson"), quiet = TRUE) |>
  st_make_valid()
geo <- suppressWarnings(
  st_crop(geo, xmin = -74.1, ymin = -55.6, xmax = -53.3, ymax = -21.7))
# el recuadro de las islas del Atlántico sur, que el nomenclador incluye en
# Tierra del Fuego y estira el mapa sin aportar nada
islas <- st_as_sfc(st_bbox(c(xmin = -62.5, ymin = -53.6, xmax = -56.0,
                             ymax = -50.5), crs = st_crs(geo)))
geo <- suppressWarnings(st_difference(geo, islas))
geo$jurisdiccion <- ifelse(geo$nombre %in% names(GEO_A_PANEL),
                           GEO_A_PANEL[geo$nombre], geo$nombre)

geo <- geo |> left_join(p, by = c("jurisdiccion" = "sede_provincia"))
stopifnot(!any(is.na(geo$unidades)))   # las 24 tienen al menos una unidad

cen <- suppressWarnings(st_centroid(geo)) |> st_coordinates() |> as.data.frame()
names(cen) <- c("lon", "lat")
cen <- cbind(cen, st_drop_geometry(geo[, c("jurisdiccion", "unidades", "acceso")]))

# ── las sedes, una por localidad ─────────────────────────────────────────────
# El gacetero ubica la oficina compradora, no el punto de entrega, y lo hace a
# nivel de localidad: las 179 unidades de CABA comparten un punto.
un <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  distinct(uoc_id, .keep_all = TRUE) |>
  select(uoc_id, sede_localidad, sede_provincia, lat, lon)
u <- read.csv(file.path(DIR_TAB, "tab_unidad_panel.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE) |>
  select(uoc_id, acceso) |> left_join(un, by = "uoc_id") |> filter(!is.na(lat))
sedes <- u |> group_by(sede_localidad, sede_provincia, lat, lon) |>
  summarise(unidades = n(), acceso = mean(acceso), .groups = "drop") |>
  mutate(fuente = "gacetero")

# Correcciones de geocodificado, con su fuente citada: el topónimo de una sede
# puede resolver a un homónimo de otra provincia, y ninguna corrección se hace
# en el mapa a mano. Las que el gacetero nacional de localidades no puede
# resolver se dibujan en el centroide de su jurisdicción, marcadas como tales.
corr <- read.csv(file.path(DIR_RAW3, "sedes_correcciones.csv"), encoding = "UTF-8",
                 stringsAsFactors = FALSE)
for (i in seq_len(nrow(corr))) {
  j <- which(sedes$sede_localidad == corr$sede_localidad[i] &
               sedes$sede_provincia == corr$sede_provincia[i])
  stopifnot(length(j) == 1)
  sedes$fuente[j] <- corr$fuente[i]
  if (corr$fuente[i] == "centroide") {
    k <- match(corr$sede_provincia[i], cen$jurisdiccion)
    sedes$lat[j] <- cen$lat[k]; sedes$lon[j] <- cen$lon[k]
  } else {
    sedes$lat[j] <- corr$lat[i]; sedes$lon[j] <- corr$lon[i]
  }
}

# Guardia: cada sede cae dentro del polígono de su propia jurisdicción. Es lo
# que encontró las tres del archivo de correcciones y lo que impide que una
# cuarta entre sin verse.
pts <- st_as_sf(sedes, coords = c("lon", "lat"), crs = st_crs(geo), remove = FALSE)
dentro <- sapply(st_within(pts, geo),
                 \(i) if (length(i) == 0) NA_character_ else geo$jurisdiccion[i[1]])
mal <- which(is.na(dentro) | dentro != sedes$sede_provincia)
if (length(mal)) {
  print(sedes[mal, c("sede_localidad", "sede_provincia", "lat", "lon", "fuente")])
  stop(sprintf("%d sedes fuera de su jurisdicción", length(mal)))
}

# sin título de eje, la etiqueta del panel y el título se pisan: el título se
# corre a la derecha para dejarle sitio, y las leyendas van cortas porque dos
# leyendas anchas al pie de dos paneles angostos se superponen
mapa_theme <- function() {
  theme_panel() +
    theme(axis.title = element_blank(),
          plot.title = element_text(margin = margin(b = 4, l = 16)),
          legend.position = "bottom",
          legend.title = element_text(size = 8, colour = "grey20"),
          legend.box.spacing = unit(2, "pt"))
}

base <- function(g) {
  ggplot(g) +
    geom_sf(fill = "#f7f7f7", colour = "#e0e0e0", linewidth = 0.18) +
    geom_sf(data = st_union(g), fill = NA, colour = "#b8b8b8", linewidth = 0.3)
}

# ── (a) el despliegue ────────────────────────────────────────────────────────
pa <- base(geo) +
  geom_point(data = sedes[order(-sedes$unidades), ], aes(lon, lat, size = unidades),
             colour = MEAN_COL, alpha = 0.55) +
  scale_size_area(max_size = 8, name = "Buying units at the seat",
                  breaks = c(1, 5, 20, 179)) +
  guides(size = guide_legend(title.position = "top", title.hjust = 0, nrow = 1)) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(tag = "a", title = "Where the state is") +
  mapa_theme()

# ── (b) lo que produce ───────────────────────────────────────────────────────
# En una coropleta lo que se lee es el polígono, no un sustituto. Las 23
# jurisdicciones grandes se leen sin ayuda; CABA mide unos 0,2° de lado y a
# tamaño de impresión no se distingue del borde entre provincias. Se dibuja
# entonces su propio polígono escalado y llevado al plano vacío del Río de la
# Plata, con una guía a su posición real: el callout de siempre, con el relleno
# de la misma escala, y ninguna marca que no sea un polígono. `2026_2` resuelve
# lo mismo con un recuadro aparte en su Figura 2.
K_LUPA <- 9; TGT_LUPA <- c(-55.30, -34.45)
cb  <- geo[geo$jurisdiccion == "CABA", ]
stopifnot(nrow(cb) == 1)
ctr <- st_coordinates(suppressWarnings(st_centroid(st_geometry(cb))))[1, ]
lupa <- (st_geometry(cb) - ctr) * K_LUPA + TGT_LUPA
st_crs(lupa) <- st_crs(geo)
cb_lupa <- st_sf(st_drop_geometry(cb), geometry = lupa)
# la lupa vive en el mar: si un cambio del recorte o del geojson la dejara
# encima del continente o fuera del panel, la corrida para acá
bb <- st_bbox(lupa); marco <- st_bbox(geo)
stopifnot(bb$xmin > marco$xmin, bb$xmax < marco$xmax,
          bb$ymin > marco$ymin, bb$ymax < marco$ymax,
          !any(st_intersects(lupa, geo, sparse = FALSE)))
pb <- base(geo) +
  geom_sf(aes(fill = acceso), colour = "#e0e0e0", linewidth = 0.18) +
  geom_sf(data = st_union(geo), fill = NA, colour = "#b8b8b8", linewidth = 0.3) +
  annotate("segment", x = ctr[1], y = ctr[2],
           xend = TGT_LUPA[1] - 0.95, yend = TGT_LUPA[2] + 0.2,
           colour = "grey55", linewidth = 0.22) +
  geom_sf(data = cb_lupa, aes(fill = acceso), colour = "grey40", linewidth = 0.3) +
  annotate("text", x = TGT_LUPA[1], y = TGT_LUPA[2] - 1.55,
           label = sprintf("CABA ×%d", K_LUPA), size = 2.1, colour = "grey20") +
  scale_fill_gradient2(low = "#D55E00", mid = "#f2f2f2", high = "#3D405B",
                       midpoint = 0, name = "Mean access",
                       breaks = c(-0.2, 0, 0.2)) +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0,
                                barwidth = unit(78, "pt"),
                                barheight = unit(5, "pt"))) +
  coord_sf(expand = FALSE, datum = NA) +
  labs(tag = "b", title = "What its suppliers look like") +
  mapa_theme()

pub(pa | pb, "1", 5.2)

write.csv(cen[order(cen$acceso), c("jurisdiccion", "unidades", "acceso")] |>
            mutate(acceso = round(acceso, 3)),
          file.path(DIR_TAB, "tab_mapa_jurisdicciones.csv"), row.names = FALSE)
write.csv(sedes[order(-sedes$unidades), c("sede_localidad", "sede_provincia",
                                          "unidades", "acceso", "lat", "lon",
                                          "fuente")] |>
            mutate(acceso = round(acceso, 3)),
          file.path(DIR_TAB, "tab_mapa_sedes.csv"), row.names = FALSE)
cat(sprintf("Fig1: %d jurisdicciones | %d sedes, %d unidades | %d corregidas | acceso de %.3f a %.3f\n",
            nrow(cen), nrow(sedes), sum(sedes$unidades),
            sum(sedes$fuente != "gacetero"), min(cen$acceso), max(cen$acceso)))
cat("OK -> figures/Fig1.*, tables/tab_mapa_jurisdicciones.csv, tables/tab_mapa_sedes.csv\n")
