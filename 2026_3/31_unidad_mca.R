# 31 — The social space of buying units: specific MCA on the seven coordinates.
#
# Writes the canonical tables the text reads the axes from: modified rates,
# category coordinates with contributions and counts, contributions summed by
# variable (the table the external review asked for), the 24 jurisdictions of
# the seat with their test values, and the coordinates of every unit.
#
# The seventh coordinate is the jurisdiction of the seat, the register's own
# 24 categories. Under the 5 per cent rule only the federal capital and the
# province of Buenos Aires are active; the other twenty-two are passive, so
# they are located in the space without building it, and all 24 are read from
# tab_unidad_juris.csv with their test values (speMCA returns the coordinates
# of the active categories only). Axis 1 is oriented positive towards the
# capital; every other sign is re-read from tab_unidad_categorias.csv after a
# run.
#
# Run: Rscript 31_unidad_mca.R   (from 2026_3/, after 30)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools)})

u <- read.csv(file.path(DIR_TAB, "tab_unidad_panel.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE)
ACT <- c("A_escala", "A_directa", "A_tipo", "A_antiguedad", "A_juris", "A_rango",
         "A_presupuesto")
X <- u[, ACT] |> mutate(across(everything(), as.factor)) |> as.data.frame()
RARE <- rare_categories(X)
mca <- speMCA(X, excl = excl_index(X, RARE), ncp = 3)
mr <- modif.rate(mca)$modif$mrate
n <- nrow(X)

# orientation: the sign of an axis is arbitrary, and axis 1 is read from the
# capital, so it is made positive towards the capital's seats
s1 <- sign(mean(mca$ind$coord[u$A_juris == JURIS_REF, 1]) -
           mean(mca$ind$coord[u$A_juris != JURIS_REF, 1]))
if (s1 == 0) s1 <- 1
ind <- mca$ind$coord[, 1:3]; ind[, 1] <- ind[, 1] * s1
varc <- mca$var$coord[, 1:3]; varc[, 1] <- varc[, 1] * s1

write.csv(data.frame(dim = 1:3, pct_benzecri = round(mr[1:3], 1)),
          file.path(DIR_TAB, "tab_unidad_benzecri.csv"), row.names = FALSE)

conteos <- unlist(lapply(names(X), function(v)
  setNames(as.integer(table(X[[v]])), paste0(v, ".", levels(X[[v]])))))
cats <- data.frame(categoria = rownames(varc),
                   variable = sub("\\..*$", "", rownames(varc)),
                   n = conteos[rownames(varc)],
                   pct = round(100 * conteos[rownames(varc)] / n, 1),
                   round(varc, 3),
                   ctr = round(mca$var$contrib[, 1:3], 1),
                   check.names = FALSE, row.names = NULL)
write.csv(cats, file.path(DIR_TAB, "tab_unidad_categorias.csv"), row.names = FALSE)

vars <- aggregate(mca$var$contrib[, 1:3],
                  by = list(variable = sub("\\..*$", "", rownames(mca$var$contrib))),
                  FUN = sum)
names(vars)[2:4] <- c("ctr_dim1", "ctr_dim2", "ctr_dim3")
vars[, 2:4] <- round(vars[, 2:4], 1)
vars <- vars[order(-vars$ctr_dim1), ]
write.csv(vars, file.path(DIR_TAB, "tab_unidad_variables.csv"), row.names = FALSE)

# the 24 jurisdictions on the first two axes, active or passive alike: count,
# share, whether the 5 per cent rule keeps it active, mean coordinate of its
# units and test value. This is the table the text reads the seat from.
act_j <- sub("^A_juris\\.", "", grep("^A_juris\\.", rownames(varc), value = TRUE))
j1 <- vtest(ind[, 1], u$A_juris); j2 <- vtest(ind[, 2], u$A_juris)
juris <- data.frame(jurisdiccion = j1$categoria, n = as.integer(j1$n),
                    pct = round(100 * j1$n / n, 1),
                    activa = j1$categoria %in% act_j,
                    dim1 = round(j1$media, 3), vtest1 = round(j1$vtest, 1),
                    dim2 = round(j2$media, 3), vtest2 = round(j2$vtest, 1))
juris <- juris[order(-juris$dim1), ]
write.csv(juris, file.path(DIR_TAB, "tab_unidad_juris.csv"), row.names = FALSE)

coords <- data.frame(uoc_id = u$uoc_id, saf_id = u$saf_id, A_juris = u$A_juris,
                     n_sup = u$n_sup, acceso = u$acceso, contenido = u$contenido,
                     unidad_dim1 = ind[, 1], unidad_dim2 = ind[, 2],
                     unidad_dim3 = ind[, 3])
write.csv(coords, file.path(DIR_TAB, "tab_unidad_coords.csv"), row.names = FALSE)

# the homology on the full battery (shared coordinates included; the net
# version on the non-shared ones is script 27)
r1 <- cor(coords$unidad_dim1, coords$acceso); r2 <- cor(coords$unidad_dim2, coords$acceso)
fit <- lm(acceso ~ unidad_dim1 + unidad_dim2, coords)
write.csv(data.frame(medida = c("r(dim1, access)", "r(dim2, access)",
                                "R2 access ~ dim1 + dim2"),
                     valor = round(c(r1, r2, summary(fit)$r.squared), 3)),
          file.path(DIR_TAB, "tab_unidad_homologia.csv"), row.names = FALSE)

cat(sprintf("unidades: %d | Benzécri %.1f / %.1f / %.1f | pasivadas (%d): %s\n",
            n, mr[1], mr[2], mr[3], length(RARE), paste(RARE, collapse = ", ")))
cat("\ncontribución por variable:\n"); print(vars, row.names = FALSE)
for (d in 1:2) {
  o <- order(-mca$var$contrib[, d])[1:8]
  cat(sprintf("\nDim.%d, mayores contribuciones:\n", d))
  print(data.frame(cat = rownames(varc)[o], coord = round(varc[o, d], 2),
                   ctr = round(mca$var$contrib[o, d], 1)), row.names = FALSE)
}
cat("\nlas jurisdicciones sobre el eje 1 (activas: ", paste(act_j, collapse = ", "), "):\n", sep = "")
print(juris[, c("jurisdiccion", "n", "activa", "dim1", "vtest1")], row.names = FALSE)
cat(sprintf("\nhomología, batería completa: r(dim1, acceso) = %.3f | r(dim2, acceso) = %.3f | R2 = %.3f\n",
            r1, r2, summary(fit)$r.squared))
cat("OK -> tables/tab_unidad_{benzecri,categorias,variables,juris,coords,homologia}.csv\n")
