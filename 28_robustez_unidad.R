# 28 — Paso 1b: lo que el R2 del modelo completo tiene que sobrevivir antes de
# creerle.
#
# Las 443 unidades están anidadas en 103 SAF, así que (a) la inferencia tiene
# que agruparse por SAF, (b) la prueba limpia del despliegue es DENTRO del SAF:
# la unidad del Ejército en Chaco contra la del Ejército en CABA, y (c) el tipo
# funcional del órgano es una coordenada del proveedor (cliente modal), así que
# hay que ver qué queda de las no compartidas controlando el tipo.
#
# La sede es la jurisdicción, factor de 24 niveles con la Capital como
# referencia: dentro del órgano cada provincia se compara con la Capital, el
# bloque entero se prueba con F, y los 23 coeficientes van a una tabla propia
# (la que dibuja la Figura 3b).

suppressPackageStartupMessages({library(dplyr)})
set.seed(42)
ua <- read.csv("tables/tab_homologia_unidades.csv",
               encoding = "UTF-8", stringsAsFactors = FALSE)
ua$saf_id <- as.character(ua$saf_id)
# the reference level travels from script 27 through the CSV as plain text
ua$N_juris <- relevel(factor(ua$N_juris), ref = "CABA")
cat(sprintf("unidades %d | SAF %d | jurisdicciones %d | tipos: %s\n", nrow(ua),
            n_distinct(ua$saf_id), nlevels(ua$N_juris),
            paste(names(table(ua$tipo)), table(ua$tipo), sep = "=", collapse = " ")))

f_comp <- acceso ~ S_escala + S_directa
f_tipo <- acceso ~ S_escala + S_directa + tipo
f_full <- acceso ~ S_escala + S_directa + tipo + N_juris + N_distribuido +
  N_fundacion + N_rango + N_presupuesto + N_tamano
r2 <- function(f, d) summary(lm(f, d))$r.squared

# ── (c) el tipo funcional como control ───────────────────────────────────────
cat(sprintf("\nR2: compartidas %.3f | + tipo %.3f | + no compartidas %.3f  (ganancia neta de las no compartidas sobre tipo: %.3f)\n",
            r2(f_comp, ua), r2(f_tipo, ua), r2(f_full, ua),
            r2(f_full, ua) - r2(f_tipo, ua)))
print(anova(lm(f_tipo, ua), lm(f_full, ua)))

# ── (a) bootstrap por conglomerado de SAF ────────────────────────────────────
safs <- unique(ua$saf_id); idx <- split(seq_len(nrow(ua)), ua$saf_id)
B <- 500
boot <- t(replicate(B, {
  rows <- unlist(idx[sample(safs, length(safs), replace = TRUE)], use.names = FALSE)
  d <- ua[rows, ]
  # a draw that loses every unit of some level cannot fit the full model
  tryCatch(c(gain_over_shared = r2(f_full, d) - r2(f_comp, d),
             gain_over_tipo = r2(f_full, d) - r2(f_tipo, d),
             r_uA1 = cor(d$uA1, d$acceso)),
           error = function(e) c(gain_over_shared = NA_real_,
                                 gain_over_tipo = NA_real_, r_uA1 = NA_real_))
}))
ok <- complete.cases(boot)
q <- apply(boot[ok, ], 2, quantile, c(0.025, 0.5, 0.975))
cat(sprintf("\nbootstrap por SAF (B = %d, válidas %d), percentiles 2.5 / 50 / 97.5:\n",
            B, sum(ok))); print(round(q, 3))

# ── (b) dentro del SAF: efectos fijos por órgano ─────────────────────────────
# Two rules select the organs: units in more than one jurisdiction, and five
# or more units, since with fewer the organ's fixed effect absorbs the
# comparison. Nine organs meet the first rule, seven meet both; the seven are
# named in §6 and listed in Table S10d (tab_intraorgano_organos.csv).
multi <- ua |> group_by(saf_id) |> filter(n_distinct(N_juris) >= 2, n() >= 5) |> ungroup()
cat(sprintf("\nSAF con unidades en más de una jurisdicción y >= 5 unidades: %d (unidades %d)\n",
            n_distinct(multi$saf_id), nrow(multi)))
fe0 <- lm(acceso ~ factor(saf_id) + S_escala + S_directa, multi)
fe1 <- lm(acceso ~ factor(saf_id) + S_escala + S_directa + N_juris + N_tamano, multi)
# the jurisdiction block on its own: the same model without it
fe1_sin <- lm(acceso ~ factor(saf_id) + S_escala + S_directa + N_tamano, multi)
# what the unit buys could carry the seat effect: hold the unit's modal sector
fe2 <- lm(acceso ~ factor(saf_id) + S_escala + S_directa + factor(rubro_modal) +
            N_juris + N_tamano, multi)
fe2_sin <- lm(acceso ~ factor(saf_id) + S_escala + S_directa + factor(rubro_modal) +
                N_tamano, multi)
aj1 <- anova(fe1_sin, fe1); aj2 <- anova(fe2_sin, fe2)
cf <- summary(fe1)$coefficients
cfj <- cf[grep("^N_juris", rownames(cf)), , drop = FALSE]
n_test <- table(multi$N_juris)
intra <- data.frame(jurisdiccion = sub("^N_juris", "", rownames(cfj)),
                    n_en_test = as.integer(n_test[sub("^N_juris", "", rownames(cfj))]),
                    coef_vs_capital = round(cfj[, 1], 4), se = round(cfj[, 2], 4),
                    p = round(cfj[, 4], 4), row.names = NULL)
intra <- intra[order(intra$coef_vs_capital), ]
write.csv(intra, "tables/tab_homologia_unidad_juris_intra.csv", row.names = FALSE)
# in how many of the organs do the capital's units rank highest?
orden <- multi |> group_by(saf_id, N_juris) |>
  summarise(acceso = mean(acceso), n = n(), .groups = "drop") |>
  group_by(saf_id) |>
  summarise(tiene_capital = any(N_juris == "CABA"),
            capital_mas_alta = tiene_capital && N_juris[which.max(acceso)] == "CABA",
            .groups = "drop")
# who the seven are, so the reader knows the test rests on the security
# forces, the parks administration and the roads directorate (Table S10d)
nombres <- read.csv("tables/tab_unidad_panel.csv", encoding = "UTF-8",
                    stringsAsFactors = FALSE) |>
  mutate(saf_id = as.character(saf_id)) |> group_by(saf_id) |>
  summarise(organismo = first(organismo), .groups = "drop")
organos_test <- multi |> group_by(saf_id) |>
  summarise(tipo = first(tipo), unidades = n(), jurisdicciones = n_distinct(N_juris),
            unidades_capital = sum(N_juris == "CABA"),
            acceso_capital = round(mean(acceso[N_juris == "CABA"]), 3),
            acceso_provincias = round(mean(acceso[N_juris != "CABA"]), 3),
            .groups = "drop") |>
  left_join(nombres, by = "saf_id") |>
  left_join(orden |> select(saf_id, capital_mas_alta), by = "saf_id") |>
  arrange(desc(unidades)) |>
  select(saf_id, organismo, tipo, unidades, jurisdicciones, unidades_capital,
         acceso_capital, acceso_provincias, capital_mas_alta)
write.csv(organos_test, "tables/tab_intraorgano_organos.csv", row.names = FALSE)
print(organos_test, row.names = FALSE)
cat(sprintf("órganos con unidades en la Capital: %d; en los que ésas son las más altas: %d\n",
            sum(orden$tiene_capital), sum(orden$capital_mas_alta)))
cat(sprintf("bloque de jurisdicción dentro del órgano: F = %.2f (p = %.3g); controlando rubro modal: F = %.2f (p = %.3g)\n",
            aj1$F[2], aj1$`Pr(>F)`[2], aj2$F[2], aj2$`Pr(>F)`[2]))
cat(sprintf("provincias por debajo de la Capital: %d de %d; con p < 0.05: %d\n",
            sum(intra$coef_vs_capital < 0), nrow(intra),
            sum(intra$coef_vs_capital < 0 & intra$p < 0.05)))
cat(sprintf("efectos fijos por SAF: R2 %.3f -> %.3f con jurisdicción y tamaño de la unidad\n",
            summary(fe0)$r.squared, summary(fe1)$r.squared))
print(intra, row.names = FALSE)
print(anova(fe0, fe1))

# la lectura más directa: la Capital contra las provincias dentro de los cinco
# padres con más unidades
top <- ua |> count(saf_id, sort = TRUE) |> head(5) |> pull(saf_id)
cat("\nacceso medio, Capital contra provincias, DENTRO de los cinco SAF con más unidades:\n")
print(as.data.frame(ua |> filter(saf_id %in% top) |>
  mutate(sede = ifelse(N_juris == "CABA", "capital", "provincias")) |>
  group_by(saf_id, tipo, sede) |>
  summarise(n = n(), acceso = round(mean(acceso), 3), .groups = "drop") |>
  tidyr::pivot_wider(names_from = sede, values_from = c(n, acceso))))

# ── (d) ponderado por proveedores ────────────────────────────────────────────
fw <- lm(f_full, ua, weights = n_sup)
cat(sprintf("\nponderado por n de proveedores: R2 = %.3f (sin ponderar %.3f)\n",
            summary(fw)$r.squared, r2(f_full, ua)))

# every number the text quotes comes from a table, never from the console
rob <- data.frame(
  medida = c("gain over shared, bootstrap by organ, 2.5", "gain over shared, median",
             "gain over shared, 97.5", "gain over type, 2.5", "gain over type, median",
             "gain over type, 97.5", "r(axis of non-shared, access), 2.5",
             "r(axis of non-shared, access), median", "r(axis of non-shared, access), 97.5",
             "organs in the within-organ test", "units in the within-organ test",
             "within-organ, F of the jurisdiction block", "within-organ, p of that F",
             "within-organ, provinces below the capital (of 23)",
             "within-organ, provinces below the capital with p < 0.05",
             "within-organ, F of jurisdiction and size", "within-organ, p of that F",
             "within-organ, F of the jurisdiction block, holding the unit's modal sector",
             "within-organ, p of that F, holding the unit's modal sector",
             "organs with units in the capital",
             "organs in which the capital's units rank highest",
             "within-organ, R2 with the shared coordinates alone",
             "within-organ, R2 with jurisdiction and size added",
             "R2 weighted by suppliers", "R2 unweighted", "bootstrap draws valid"),
  valor = round(c(q[, "gain_over_shared"], q[, "gain_over_tipo"], q[, "r_uA1"],
                  n_distinct(multi$saf_id), nrow(multi),
                  aj1$F[2], aj1$`Pr(>F)`[2],
                  sum(intra$coef_vs_capital < 0),
                  sum(intra$coef_vs_capital < 0 & intra$p < 0.05),
                  anova(fe0, fe1)$F[2], anova(fe0, fe1)$`Pr(>F)`[2],
                  aj2$F[2], aj2$`Pr(>F)`[2],
                  sum(orden$tiene_capital), sum(orden$capital_mas_alta),
                  summary(fe0)$r.squared, summary(fe1)$r.squared,
                  summary(fw)$r.squared, r2(f_full, ua), sum(ok)), 4))
write.csv(rob, "tables/tab_homologia_unidad_robustez.csv", row.names = FALSE)
cat("OK -> tables/tab_homologia_unidad_robustez.csv, tab_homologia_unidad_juris_intra.csv\n")
