# 20 — ¿El anclaje separa el acceso de los proveedores dentro de cada nivel de
# escala? (test de la pierna débil del gozne bourdieuano).
#
# La homología más fuerte corre por el eje escala-procedimiento (r=0.52); el
# anclaje (la pierna "espacio físico") correlaciona 0.21. Para saber si el
# anclaje tiene efecto independiente o es un proxy de la escala, se compara el
# acceso medio de los proveedores (multi_sede vs una_sede) dentro de cada tercil
# del eje de escala.
#
# Correr:  Rscript 20_condicionar_anclaje.R   (desde la raíz del repositorio)
suppressPackageStartupMessages({library(dplyr); library(tidyr)})

hom <- read.csv("tables/tab_b7f_homologia.csv", encoding = "UTF-8")
hom$saf_id <- as.character(hom$saf_id)
hom$t2 <- cut(hom$org_dim2, quantile(hom$org_dim2, c(0, 1/3, 2/3, 1)),
              labels = c("bajo", "medio", "alto"), include.lowest = TRUE)

overall <- hom |> group_by(A_anclaje) |>
  summarise(n = n(), access = round(mean(mean_sup_dim2, na.rm = TRUE), 3),
            .groups = "drop")
cat("=== acceso medio por anclaje (total) ===\n")
print(as.data.frame(overall), row.names = FALSE)

by_t2 <- hom |> filter(A_anclaje %in% c("multi_sede", "una_sede")) |>
  group_by(t2, A_anclaje) |>
  summarise(n = n(), access = round(mean(mean_sup_dim2, na.rm = TRUE), 3),
            .groups = "drop")
cat("\n=== acceso medio por tercil de escala × anclaje ===\n")
print(as.data.frame(by_t2), row.names = FALSE)

dif <- by_t2 |> pivot_wider(names_from = A_anclaje, values_from = c(access, n)) |>
  mutate(dif = access_una_sede - access_multi_sede)
cat("\n=== diferencia una_sede - multi_sede dentro de cada tercil ===\n")
print(as.data.frame(dif), row.names = FALSE)

# correlación parcial: ¿el anclaje predice acceso controlando el eje 2?
fit <- lm(mean_sup_dim2 ~ org_dim2 + (A_anclaje == "multi_sede"), data = hom)
cat(sprintf("\nanclaje (multi_sede) controlando org_dim2: β = %.3f, p = %.3g\n",
            coef(fit)[3], summary(fit)$coefficients[3, 4]))
