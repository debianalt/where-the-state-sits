# 39 — ¿El segundo eje del plano conjunto es un artefacto del primero?
#
# El efecto Guttman (arco, herradura) hace que el segundo eje de un análisis
# de correspondencias sea una función cuadrática del primero, y entonces la
# lectura sustantiva del segundo eje no dice nada que el primero no dijera.
# En la polémica que The Sociological Review alojó sobre la homología entre
# el espacio social y el espacio de la comida, el arco es una de las lecturas
# alternativas que el comentario propone para los datos discutidos.
#
# La prueba es directa: se regresa el eje 2 sobre el eje 1 y sobre su cuadrado,
# y se compara con la regresión lineal sola. Si el término cuadrático no suma,
# no hay arco. Se corre por separado en las dos nubes, porque el arco puede
# aparecer en una y no en la otra.
#
# Lee las coordenadas ya calculadas por el script 32; no vuelve a ajustar nada
# y no toca ningún sorteo.
#
# Run: Rscript 39_conjunto_arco.R   (desde 2026_3/, después de 32)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow)})

u <- read.csv(file.path(DIR_TAB, "tab_conjunto_unidades.csv"), encoding = "UTF-8")
p <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet"))

arco <- function(x, y) {
  z <- as.numeric(scale(x))
  lin <- summary(lm(y ~ z))$r.squared
  cua <- summary(lm(y ~ z + I(z^2)))$r.squared
  c(r_lineal = cor(z, y), r_cuadratico = cor(z^2, y),
    r2_lineal = lin, r2_cuadratico = cua, ganancia = cua - lin)
}

au <- arco(u$f1, u$f2)
ap <- arco(p$g1, p$g2)

res <- data.frame(
  nube = c(rep("units", 5), rep("suppliers", 5)),
  medida = rep(c("r(axis 1, axis 2)", "r(axis 1 squared, axis 2)",
                 "R2 of axis 2 on axis 1", "R2 of axis 2 on a quadratic of axis 1",
                 "gain of the quadratic term"), 2),
  valor = round(c(au, ap), 4), row.names = NULL)
write.csv(res, file.path(DIR_TAB, "tab_conjunto_arco.csv"), row.names = FALSE)
print(res, row.names = FALSE)
cat(sprintf("\nunidades: el cuadrado del eje 1 explica %.1f %% del eje 2 y suma %.1f puntos sobre el lineal\n",
            100 * au["r2_cuadratico"], 100 * au["ganancia"]))
cat(sprintf("proveedores: %.1f %% y %.1f puntos\n",
            100 * ap["r2_cuadratico"], 100 * ap["ganancia"]))
cat("OK -> tables/tab_conjunto_arco.csv\n")
