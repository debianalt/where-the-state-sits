# 27 — The net homology at the level of the buying unit: what the coordinates
# NOT shared with the supplier space predict.
#
# The organ-level homology (scripts 18, 24) gives r = 0.21-0.24 on the axis
# that shares no coordinate with the supplier space, because an organ mixes
# its ministry in the capital with its delegations. At the level of the buying
# unit with its seat (script 30), the question is whether the properties the
# supplier space never sees — the jurisdiction of the seat, whether the parent
# is distributed, its seniority, rank and budget, the unit's size — predict
# the mean access position of the unit's suppliers, over and above the two
# coordinates the two spaces share (scale and procedure) and the functional
# type, which the supplier space carries as the modal client.
#
# The seat is the jurisdiction, a 24-level factor with the capital as the
# reference, so every coefficient reads "a seat in this province, against one
# in the capital", and the block is tested with an F.
#
# Reads the canonical panel of script 30; writes tab_homologia_unidades.csv
# for scripts 28 and 38, tab_homologia_unidad_resumen.csv for the text and
# tab_homologia_unidad_juris.csv with the 23 provincial coefficients.
#
# Run: Rscript 27_homologia_unidad.R   (from 2026_3/, after 30)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(GDAtools)})

u <- read.csv(file.path(DIR_TAB, "tab_unidad_panel.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE) |>
  filter(A_juris != "sin_sede", !is.na(acceso)) |>
  # the capital as the reference, so a coefficient reads "a seat in this
  # province, against one in the capital", which is the direction §6 argues
  mutate(N_juris = relevel(factor(A_juris), ref = JURIS_REF),
         N_fundacion = ifelse(A_antiguedad == "sin_dato", NA, A_antiguedad),
         N_rango = A_rango,
         N_presupuesto = ifelse(A_presupuesto == "sin_presupuesto", NA, A_presupuesto),
         S_escala = ifelse(A_escala == "sin_ars", NA, A_escala),
         S_directa = A_directa)
cat(sprintf("unidades con sede y >= 20 proveedores: %d (padres %d)\n",
            nrow(u), n_distinct(u$saf_id)))

# ── A. the unit space on the non-shared coordinates alone ────────────────────
ua <- u |> filter(!is.na(N_fundacion), !is.na(N_presupuesto), !is.na(S_escala))
XA <- ua |> select(N_juris, N_distribuido, N_fundacion, N_rango, N_presupuesto,
                   N_tamano) |> mutate(across(everything(), as.factor)) |>
  as.data.frame()
# speMCA rejects an empty exclusion vector, so none is passed when nothing is
# rare; with the jurisdiction in the battery twenty-two categories are
rareA <- rare_categories(XA)
exclA <- if (length(rareA)) excl_index(XA, rareA) else NULL
mca_A <- speMCA(XA, excl = exclA, ncp = 3)
mrA <- modif.rate(mca_A)$modif$mrate
ua$uA1 <- mca_A$ind$coord[, 1]; ua$uA2 <- mca_A$ind$coord[, 2]
# orient axis 1 so that a seat in the capital is positive
sA <- sign(mean(ua$uA1[ua$N_juris == JURIS_REF]) - mean(ua$uA1[ua$N_juris != JURIS_REF]))
if (sA == 0) sA <- 1
ua$uA1 <- ua$uA1 * sA
cat(sprintf("\nA. non-shared unit space: n = %d, Benzécri %.1f / %.1f, pasivadas %d\n",
            nrow(ua), mrA[1], mrA[2], length(rareA)))
cat(sprintf("   r(uA1, acceso) = %.3f | r(uA2, acceso) = %.3f | R2 on both = %.3f\n",
            cor(ua$uA1, ua$acceso), cor(ua$uA2, ua$acceso),
            summary(lm(acceso ~ uA1 + uA2, ua))$r.squared))
catA <- data.frame(categoria = rownames(mca_A$var$coord),
                   dim1 = round(mca_A$var$coord[, 1] * sA, 3),
                   ctr1 = round(mca_A$var$contrib[, 1], 1), row.names = NULL)
write.csv(catA[order(catA$dim1), ], file.path(DIR_TAB, "tab_homologia_unidad_ejeA.csv"),
          row.names = FALSE)

# ── B. regressions: shared, + type, + non-shared ────────────────────────────
f_comp <- acceso ~ S_escala + S_directa
f_tipo <- acceso ~ S_escala + S_directa + tipo
f_nons <- acceso ~ N_juris + N_distribuido + N_fundacion + N_rango + N_presupuesto + N_tamano
f_full <- acceso ~ S_escala + S_directa + tipo + N_juris + N_distribuido + N_fundacion +
  N_rango + N_presupuesto + N_tamano
f_sinj <- acceso ~ S_escala + S_directa + tipo + N_distribuido + N_fundacion +
  N_rango + N_presupuesto + N_tamano
r2 <- function(f, d) summary(lm(f, d))$r.squared
an <- anova(lm(f_tipo, ua), lm(f_full, ua))
anj <- anova(lm(f_sinj, ua), lm(f_full, ua))
# every number the text quotes comes from a table, never from the console: the
# correlation of the non-shared axis, the spread the within-organ coefficient
# is read against, the block F of the jurisdiction and the single-seat
# coefficient. The 23 provincial coefficients go to a table of their own.
cf <- summary(lm(f_full, ua))$coefficients
cfj <- cf[grep("^N_juris", rownames(cf)), , drop = FALSE]
nj <- table(ua$N_juris)
juris <- data.frame(jurisdiccion = sub("^N_juris", "", rownames(cfj)),
                    n = as.integer(nj[sub("^N_juris", "", rownames(cfj))]),
                    coef_vs_capital = round(cfj[, 1], 4), se = round(cfj[, 2], 4),
                    p = round(cfj[, 4], 4), row.names = NULL)
juris <- juris[order(juris$coef_vs_capital), ]
write.csv(juris, file.path(DIR_TAB, "tab_homologia_unidad_juris.csv"), row.names = FALSE)
res <- data.frame(
  modelo = c("shared coordinates (scale, procedure)", "+ functional type",
             "non-shared coordinates alone", "shared + type + non-shared",
             "gain of the non-shared over shared", "gain of the non-shared over type",
             "F of the non-shared over type", "p of that F", "n units", "n parent organs",
             "r(axis of the non-shared, access)",
             "SD of the mean access position across units",
             "F of the jurisdiction block, full model", "p of that F",
             "provinces below the capital, full model (of 23)",
             "provinces below the capital with p < 0.05, full model",
             "parent with a single seat, full model",
             "parent with a single seat, full model, p"),
  valor = round(c(r2(f_comp, ua), r2(f_tipo, ua), r2(f_nons, ua), r2(f_full, ua),
                  r2(f_full, ua) - r2(f_comp, ua), r2(f_full, ua) - r2(f_tipo, ua),
                  an$F[2], an$`Pr(>F)`[2], nrow(ua), n_distinct(ua$saf_id),
                  cor(ua$uA1, ua$acceso), sd(ua$acceso),
                  anj$F[2], anj$`Pr(>F)`[2],
                  sum(juris$coef_vs_capital < 0), sum(juris$coef_vs_capital < 0 & juris$p < 0.05),
                  cf["N_distribuidouna_sede", 1], cf["N_distribuidouna_sede", 4]), 4))
write.csv(res, file.path(DIR_TAB, "tab_homologia_unidad_resumen.csv"), row.names = FALSE)
cat("\nB. R2 by model:\n"); print(res, row.names = FALSE)
cat("\ncoefficients of the non-shared, full model (jurisdictions in their own table):\n")
print(round(cf[grep("^N_(distribuido|fundacion|rango|presupuesto|tamano)", rownames(cf)), c(1, 4)], 3))
cat("\nthe provinces against the capital, full model:\n"); print(juris, row.names = FALSE)

# ── C. the plainest reading: the capital against the provinces within scale ──
cat("\nmean access, capital against the provinces, within scale tercile:\n")
print(as.data.frame(ua |> mutate(sede = ifelse(N_juris == JURIS_REF, "capital", "provincias")) |>
  group_by(S_escala, sede) |>
  summarise(n = n(), acceso = round(mean(acceso), 3), .groups = "drop") |>
  tidyr::pivot_wider(names_from = sede, values_from = c(n, acceso))))

write.csv(ua |> select(uoc_id, saf_id, sede_provincia, tipo, rubro_modal, anclado, n_adj,
                       n_sup, acceso, contenido, uA1, uA2, starts_with("N_"),
                       starts_with("S_")),
          file.path(DIR_TAB, "tab_homologia_unidades.csv"), row.names = FALSE)
cat("\nOK -> tables/tab_homologia_unidades.csv, tab_homologia_unidad_resumen.csv, tab_homologia_unidad_juris.csv, tab_homologia_unidad_ejeA.csv\n")
