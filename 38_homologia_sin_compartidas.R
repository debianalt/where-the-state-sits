# 38 — The net homology against an access axis that never saw the shared
# coordinates.
#
# The scale and the procedure of the exchange are the same amounts seen from
# each side, and they are active in both the unit space and the supplier
# space. Controlling for them in the block regressions of script 27 nets them
# out of the gain, but a reader can still ask whether the outcome itself is
# contaminated. This script rebuilds the supplier space WITHOUT scale and
# procedure — four actives: legal form, modal sector, modal client type,
# registration cohort — and repeats the block regressions on the mean position
# of each unit's suppliers on the access axis of that reduced space. If the
# non-shared coordinates still add what they added, the homology is not
# circular.
#
# The axis is chosen by the opposition the access axis names, the corporate
# supplier against the personal one, and NOT by its correlation with the
# original axis: choosing it by that correlation would make the check circular
# in its own turn. On axis 1 the public limited company and the natural person
# are 1.53 SD apart, against 0.57 on the runner-up, so the choice is not close.
# The correlation with the original access axis is then a result, not a
# criterion.
#
# Run: Rscript 38_homologia_sin_compartidas.R   (from the repository root, after 30 and 27)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(GDAtools)})

sup <- read_parquet(file.path(DIR_PROC3, "proveedores_c10.parquet"))
X4 <- sup |> select(A_personeria, A_rubro, A_cliente, A_registro) |>
  mutate(across(everything(), as.factor)) |> as.data.frame()
mca4 <- speMCA(X4, excl = excl_index(X4, rare_categories(X4)), ncp = 3)
mr4 <- modif.rate(mca4)$modif$mrate
# on which axis are the public limited company and the natural person furthest
# apart, in standard deviations of the axis?
brecha <- sapply(1:3, function(j) {
  d <- mca4$ind$coord[, j]
  (mean(d[sup$A_personeria == "SA"]) - mean(d[sup$A_personeria == "PersFisica"])) / sd(d)
})
k <- which.max(abs(brecha))
acc4 <- mca4$ind$coord[, k] * sign(brecha[k])
co <- cor(acc4, sup$sup_dim2)
cat(sprintf("espacio de 4 activas: Benzécri %.1f / %.1f / %.1f\n", mr4[1], mr4[2], mr4[3]))
cat(sprintf("brecha SA - persona física por eje (SD): %s -> se toma el eje %d\n",
            paste(sprintf("%+.2f", brecha), collapse = " / "), k))
cat(sprintf("ese eje correlaciona %.3f con el eje de acceso original\n", abs(co)))
sup$acc4 <- acc4

adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1))
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  select(doc_contractual, uoc_id)
medias <- adj |> inner_join(unidad, by = "doc_contractual") |>
  inner_join(sup |> select(cuit, acc4), by = "cuit") |>
  group_by(uoc_id) |> summarise(acceso4 = mean(acc4), .groups = "drop")

ua <- read.csv(file.path(DIR_TAB, "tab_homologia_unidades.csv"), encoding = "UTF-8",
               stringsAsFactors = FALSE) |>
  inner_join(medias, by = "uoc_id") |>
  # the seat is the jurisdiction, with the capital as the reference (script 27)
  mutate(N_juris = relevel(factor(N_juris), ref = JURIS_REF))
r2 <- function(f, d) summary(lm(f, d))$r.squared
f_comp <- acceso4 ~ S_escala + S_directa
f_tipo <- acceso4 ~ S_escala + S_directa + tipo
f_nons <- acceso4 ~ N_juris + N_distribuido + N_fundacion + N_rango + N_presupuesto + N_tamano
f_full <- acceso4 ~ S_escala + S_directa + tipo + N_juris + N_distribuido + N_fundacion +
  N_rango + N_presupuesto + N_tamano
an <- anova(lm(f_tipo, ua), lm(f_full, ua))
multi <- ua |> group_by(saf_id) |> filter(n_distinct(N_juris) >= 2, n() >= 5) |> ungroup()
fe_sin <- lm(acceso4 ~ factor(saf_id) + S_escala + S_directa + N_tamano, multi)
fe_con <- lm(acceso4 ~ factor(saf_id) + S_escala + S_directa + N_juris + N_tamano, multi)
aj <- anova(fe_sin, fe_con)
cfj <- summary(fe_con)$coefficients
cfj <- cfj[grep("^N_juris", rownames(cfj)), , drop = FALSE]
res <- data.frame(
  medida = c("axis of the reduced supplier space taken as the access axis",
             "public limited company minus natural person on that axis, in SD",
             "the same gap on the runner-up axis, in SD",
             "correlation of that axis with the original access axis",
             "shared coordinates (scale, procedure)", "+ functional type",
             "non-shared coordinates alone", "shared + type + non-shared",
             "gain of the non-shared over shared", "gain of the non-shared over type",
             "F of the non-shared over type", "r(axis of non-shared, access)",
             "within-organ, F of the jurisdiction block", "within-organ, p of that F",
             "within-organ, provinces below the capital (of 23)",
             "within-organ, provinces below the capital with p < 0.05",
             "n units"),
  valor = round(c(k, brecha[k], sort(abs(brecha), decreasing = TRUE)[2],
                  abs(co), r2(f_comp, ua), r2(f_tipo, ua), r2(f_nons, ua),
                  r2(f_full, ua), r2(f_full, ua) - r2(f_comp, ua),
                  r2(f_full, ua) - r2(f_tipo, ua), an$F[2], cor(ua$uA1, ua$acceso4),
                  aj$F[2], aj$`Pr(>F)`[2], sum(cfj[, 1] < 0),
                  sum(cfj[, 1] < 0 & cfj[, 4] < 0.05), nrow(ua)), 4))
write.csv(res, file.path(DIR_TAB, "tab_homologia_sin_compartidas.csv"), row.names = FALSE)
print(res, row.names = FALSE)
cat("OK -> tables/tab_homologia_sin_compartidas.csv\n")
