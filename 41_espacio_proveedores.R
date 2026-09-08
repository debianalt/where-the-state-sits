# 41 — El espacio social de los proveedores, documentado en este artículo.
#
# El espacio de unidades tiene sus tasas modificadas y sus contribuciones en el
# suplemento (S3, S4) y el plano conjunto los suyos (S8, S8b, S8c). El de
# proveedores no tenía nada, y de él sale la variable dependiente de toda la
# sección 6: la posición de acceso de cada proveedor. Un lector no podía
# auditar la construcción de la que cuelga el resultado central.
#
# Este script ajusta ese espacio con la misma función que usan el 30 y el 32
# —así que no hay dos ajustes que puedan divergir— y escribe lo que el
# suplemento necesita: las activas con sus categorías y sus n, las tasas
# modificadas, y las coordenadas y contribuciones de cada categoría sobre los
# dos primeros ejes.
#
# Determinista y sin sorteos.
#
# Run: Rscript 41_espacio_proveedores.R   (desde la raíz del repositorio)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(GDAtools)})

adj    <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet"))
master <- read_parquet(file.path(DIR_PROC2, "supplier_master.parquet"))

m <- fit_supplier_space(adj, master)
mr <- attr(m, "benzecri")

# el mismo ajuste, para quedarnos con el objeto y poder leer var$coord y
# var$contrib: fit_supplier_space devuelve los individuos, no el modelo
X <- active_matrix(m)
raras <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, raras), ncp = 3)

# los signos con que el artículo lee los ejes, fijados en fit_supplier_space:
# eje 1 positivo hacia la adjudicación grande, eje 2 hacia la sociedad anónima
s1 <- sign(mean(mca$ind$coord[m$A_escala == "grande", 1]) -
           mean(mca$ind$coord[m$A_escala == "chica", 1]))
s2 <- sign(mean(mca$ind$coord[m$A_personeria == "SA", 2]) -
           mean(mca$ind$coord[m$A_personeria == "PersFisica", 2]))

VARS <- c(A_personeria = "Legal form", A_rubro = "Modal sector",
          A_cliente = "Modal client type", A_escala = "Scale of the exchange",
          A_directa = "Procedure profile", A_registro = "Registration cohort")

# ── las activas, sus categorías y las pasivadas ──────────────────────────────
vars <- do.call(rbind, lapply(names(VARS), function(v) {
  tb <- table(X[[v]])
  data.frame(variable = VARS[[v]], categoria = names(tb),
             n = as.integer(tb), pct = round(100 * as.numeric(tb) / nrow(X), 1),
             pasivada = ifelse(paste0(v, ".", names(tb)) %in% raras, "yes", "no"),
             row.names = NULL)
}))
write.csv(vars, file.path(DIR_TAB, "tab_proveedor_variables.csv"), row.names = FALSE)

# ── tasas modificadas ────────────────────────────────────────────────────────
benz <- data.frame(dim = 1:3, pct_benzecri = round(modif.rate(mca)$modif$mrate[1:3], 1))
write.csv(benz, file.path(DIR_TAB, "tab_proveedor_benzecri.csv"), row.names = FALSE)

# ── coordenadas y contribuciones de cada categoría ───────────────────────────
act <- !(rownames(mca$var$coord) %in% raras)
cats <- data.frame(
  categoria = rownames(mca$var$coord)[act],
  dim1 = round(mca$var$coord[act, 1] * s1, 3),
  dim2 = round(mca$var$coord[act, 2] * s2, 3),
  ctr1 = round(mca$var$contrib[act, 1], 1),
  ctr2 = round(mca$var$contrib[act, 2], 1), row.names = NULL)
cats <- cats[order(cats$dim2), ]
write.csv(cats, file.path(DIR_TAB, "tab_proveedor_categorias.csv"), row.names = FALSE)

cat(sprintf("proveedores: %d | activas 6 | categorías %d, pasivadas %d\n",
            nrow(X), nrow(vars), sum(vars$pasivada == "yes")))
cat(sprintf("tasas modificadas: %.1f / %.1f / %.1f\n", benz$pct_benzecri[1],
            benz$pct_benzecri[2], benz$pct_benzecri[3]))
cat(sprintf("eje 2 (acceso): persona física %.2f, SA %.2f\n",
            cats$dim2[cats$categoria == "A_personeria.PersFisica"],
            cats$dim2[cats$categoria == "A_personeria.SA"]))
cat("OK -> tables/tab_proveedor_{variables,benzecri,categorias}.csv\n")
