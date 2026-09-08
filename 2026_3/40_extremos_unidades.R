# 40 — Qué unidades hay en los dos extremos del primer eje conjunto.
#
# El eje 1 del plano se lee hasta acá por coordenadas de categorías. Un lector
# que no conoce la administración argentina no sabe qué cosa es una unidad
# anclada, y el registro sí lo dice: la unidad tiene nombre, sede y rubro
# modal. Esta tabla es descriptiva, no es otra prueba, y existe para que las
# viñetas del cuerpo se puedan verificar una por una.
#
# Lee las coordenadas que ya escribió el script 32 y el panel del 30; no
# reajusta nada y no toca ningún sorteo.
#
# Run: Rscript 40_extremos_unidades.R   (desde 2026_3/, después de 32)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)

N_EXTREMO <- 5

u <- read.csv(file.path(DIR_TAB, "tab_conjunto_unidades.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE)
panel <- read.csv(file.path(DIR_TAB, "tab_unidad_panel.csv"), encoding = "UTF-8",
                  stringsAsFactors = FALSE)

d <- u |>
  select(uoc_id, f1) |>
  inner_join(panel |> select(uoc_id, uoc, organismo, rubro_modal, sede_localidad,
                             sede_provincia, n_sup, anio_fundacion),
             by = "uoc_id")

# el nombre de la unidad sin el prefijo numérico, y el del órgano sin su SAF:
# el código ya está en la tabla de cobertura y acá sólo estorba la lectura
limpio <- function(x) trimws(sub("^[0-9/]+\\s*-\\s*", "", x))

# una descripción que termina repitiendo el nombre del órgano ensancha la
# tabla sin decir nada: "División Compras y Suministros - Superintendencia de
# Bienestar de la Policia Federal Argentina" contra ese mismo órgano
sin_organo <- function(unidad, organo) {
  suf <- paste0(" - ", organo)
  ifelse(endsWith(unidad, suf), substr(unidad, 1, nchar(unidad) - nchar(suf)), unidad)
}

tomar <- function(df, polo) {
  df |> mutate(polo = polo,
               organo = limpio(organismo),
               unidad = sin_organo(limpio(uoc), organo),
               sede = ifelse(is.na(sede_localidad), "no localizada",
                             ifelse(sede_localidad == sede_provincia, sede_localidad,
                                    paste0(sede_localidad, ", ", sede_provincia)))) |>
    select(polo, unidad, organo, anio_fundacion, sede, rubro_modal, n_sup, f1)
}

ext <- rbind(tomar(d |> slice_min(f1, n = N_EXTREMO), "deployed end"),
             tomar(d |> slice_max(f1, n = N_EXTREMO), "administrative end"))
ext$f1 <- round(ext$f1, 2)

write.csv(ext, file.path(DIR_TAB, "tab_extremos_unidades.csv"), row.names = FALSE)
print(ext, row.names = FALSE)
cat(sprintf("\nproveedores: extremo desplegado %d-%d, extremo administrativo %d-%d\n",
            min(ext$n_sup[ext$polo == "deployed end"]),
            max(ext$n_sup[ext$polo == "deployed end"]),
            min(ext$n_sup[ext$polo == "administrative end"]),
            max(ext$n_sup[ext$polo == "administrative end"])))
cat("OK -> tables/tab_extremos_unidades.csv\n")
