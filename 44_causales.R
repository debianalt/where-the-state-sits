# 44 — The two grounds of the naming, read apart: exclusivity and speciality.
#
# For every award that leaves open competition the register records the clause
# the organ invoked. Two clauses name a supplier rather than price the purchase.
# Exclusivity (Decree 1023/2001, art. 25 d) 3) covers a good or service that
# has one seller and no convenient substitute; speciality (art. 25 d) 2) covers
# a scientific, technical or artistic work that only a given firm, artist or
# specialist can carry out. S_fundamento folds both into "consagrado". The
# first certifies a market; the second recognises a competence. Read apart,
# they name different populations, and the plane places both.
#
# Refits nothing and moves no draw: reads the supplier coordinates script 32
# wrote and the award-level ground the data layer's 43_apartado.py wrote
# (apartado_num 2 = speciality, 3 = exclusivity). Precedent: 39, 40, 42.
#
# Outputs (tables/): tab_causales_deciles.csv    share by decile of each joint
#                                                axis, per ground, Wilson 95 %
#                    tab_causales_lectura.csv    the four categories on g1, g2:
#                                                n, mean, test value, R2
#                    tab_causales_perfil.csv     who each group is
#                    tab_causales_rubros.csv     leading sectors of the awards
#                    tab_causales_tipos.csv      organ types of the awards
#                    tab_causales_tendencia.csv  point-biserial and Spearman
#          data/processed/causales_proveedor.parquet  the flags per supplier,
#                                                read by 36
#
# Run: Rscript 44_causales.R   (from the repository root, after 32; before 36 and 42)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(dplyr); library(tidyr)})

s <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet")) |>
  select(cuit, g1, g2, S_fundamento, A_personeria, provincia)
adj <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_tipo.parquet")) |>
  filter(es_nuevo, !is.na(provincia), between(ejercicio, WIN0, WIN1)) |>
  select(doc_contractual, cuit, ejercicio, rubro_principal)
unidad <- read_parquet(file.path(DIR_PROC3, "adjudicaciones_unidad.parquet")) |>
  select(doc_contractual, uoc_id, saf_id)
apart <- read_parquet(file.path(DIR_PROC2, "adjudicaciones_apartado.parquet")) |>
  select(doc_contractual, apartado_num)
# one name and one type per organ (a renamed organ carries several names)
organos <- read.csv("tables/tab_unidad_panel.csv", encoding = "UTF-8",
                    stringsAsFactors = FALSE) |>
  mutate(saf_id = as.character(saf_id)) |>
  group_by(saf_id) |>
  summarise(organismo = first(organismo), tipo = first(tipo), .groups = "drop")

w <- adj |> inner_join(unidad, by = "doc_contractual") |>
  left_join(apart, by = "doc_contractual") |>
  filter(cuit %in% s$cuit) |>
  mutate(saf_id = as.character(saf_id),
         ground = case_when(apartado_num %in% 2 ~ "especialidad",
                            apartado_num %in% 3 ~ "exclusividad",
                            .default = NA_character_))
cat(sprintf("awards in the window with a unit and a supplier of the plane: %d; on speciality %d, on exclusivity %d\n",
            nrow(w), sum(w$ground %in% "especialidad"), sum(w$ground %in% "exclusividad")))

# ── the flags per supplier ───────────────────────────────────────────────────
flags <- w |> group_by(cuit) |>
  summarise(n_esp = sum(ground %in% "especialidad"),
            n_exc = sum(ground %in% "exclusividad"), .groups = "drop")
d <- s |> left_join(flags, by = "cuit") |>
  mutate(n_esp = coalesce(n_esp, 0L), n_exc = coalesce(n_exc, 0L),
         esp = n_esp > 0, exc = n_exc > 0,
         S_causal = case_when(esp & exc ~ "ambas", esp ~ "especialidad",
                              exc ~ "exclusividad", .default = "no_nombrado"))
# the split must partition S_fundamento's "consagrado" exactly: every named
# supplier carries one of the two grounds and no unnamed one does
stopifnot(all((d$S_causal != "no_nombrado") == (d$S_fundamento == "consagrado")))
write_parquet(d |> select(cuit, esp, exc, n_esp, n_exc, S_causal),
              file.path(DIR_PROC3, "causales_proveedor.parquet"))
cat("\nsuppliers by ground:\n"); print(table(d$S_causal))

# ── share by decile of each joint axis, Wilson 95 % ──────────────────────────
wilson <- function(share, n, z = 1.96) {
  centre <- (share + z^2 / (2 * n)) / (1 + z^2 / n)
  half <- z * sqrt(share * (1 - share) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  list(lo = centre - half, hi = centre + half)
}
deciles <- function(x, eje) {
  d |> mutate(decil = dplyr::ntile(.data[[x]], 10)) |>
    group_by(decil) |>
    summarise(n = n(), posicion = mean(.data[[x]]),
              exclusividad = mean(exc), especialidad = mean(esp), .groups = "drop") |>
    pivot_longer(c(exclusividad, especialidad), names_to = "ground",
                 values_to = "share") |>
    mutate(eje = eje, lo = wilson(share, n)$lo, hi = wilson(share, n)$hi)
}
dec <- bind_rows(deciles("g1", "axis1"), deciles("g2", "axis2")) |>
  mutate(across(c(share, lo, hi), ~round(100 * .x, 2)),
         posicion = round(posicion, 3)) |>
  select(eje, decil, n, posicion, ground, share, lo, hi)
write.csv(dec, file.path(DIR_TAB, "tab_causales_deciles.csv"), row.names = FALSE)
cat("\nshare named (%) by decile of axis 1 and axis 2:\n")
print(as.data.frame(dec |> select(eje, decil, ground, share) |>
                      pivot_wider(names_from = c(eje, ground), values_from = share)))

# ── the four categories on the two axes: test values, R2 ─────────────────────
lect <- bind_rows(lapply(c("g1", "g2"), function(a) {
  data.frame(eje = a, vtest(d[[a]], d$S_causal),
             r2_variable = round(summary(lm(d[[a]] ~ factor(d$S_causal)))$r.squared, 3))
})) |> mutate(media = round(media, 3), vtest = round(vtest, 1))
write.csv(lect, file.path(DIR_TAB, "tab_causales_lectura.csv"), row.names = FALSE)
cat("\ntest values on the joint axes:\n"); print(lect, row.names = FALSE)

# ── who each group is ────────────────────────────────────────────────────────
que_nombran <- w |> filter(!is.na(ground)) |> group_by(cuit) |>
  summarise(org_exc = n_distinct(saf_id[ground == "exclusividad"]),
            org_esp = n_distinct(saf_id[ground == "especialidad"]),
            org_any = n_distinct(saf_id), .groups = "drop")
dp <- d |> left_join(que_nombran, by = "cuit")
grupos <- list("named on exclusivity, any"  = dp$exc,
               "named on speciality, any"   = dp$esp,
               "named on exclusivity only"  = dp$S_causal == "exclusividad",
               "named on speciality only"   = dp$S_causal == "especialidad",
               "named on both"              = dp$S_causal == "ambas",
               "not named"                  = dp$S_causal == "no_nombrado")
pct <- function(x) round(100 * mean(x), 1)
perfil <- bind_rows(lapply(names(grupos), function(g) {
  x <- dp[grupos[[g]], ]
  org <- switch(g, "named on exclusivity, any" = x$org_exc,
                "named on speciality, any" = x$org_esp, x$org_any)
  data.frame(grupo = g, proveedores = nrow(x),
             pct_SA = pct(x$A_personeria == "SA"),
             pct_SRL = pct(x$A_personeria == "SRL"),
             pct_persona_fisica = pct(x$A_personeria == "PersFisica"),
             pct_CABA = pct(x$provincia == "CABA"),
             pct_un_organo = if (g == "not named") NA_real_ else pct(org == 1),
             mediana_organos = if (g == "not named") NA_real_ else median(org))
}))
write.csv(perfil, file.path(DIR_TAB, "tab_causales_perfil.csv"), row.names = FALSE)
cat("\nprofile of each group:\n"); print(perfil, row.names = FALSE)

# ── what the named awards buy, and from which organs ─────────────────────────
named_awards <- w |> filter(!is.na(ground)) |> left_join(organos, by = "saf_id")
rubros <- named_awards |> count(ground, rubro_principal, name = "adjudicaciones") |>
  group_by(ground) |>
  mutate(total = sum(adjudicaciones),
         pct = round(100 * adjudicaciones / total, 1)) |>
  arrange(desc(adjudicaciones), .by_group = TRUE) |> slice_head(n = 5) |> ungroup()
write.csv(rubros, file.path(DIR_TAB, "tab_causales_rubros.csv"), row.names = FALSE)
tipos <- named_awards |> count(ground, tipo, name = "adjudicaciones") |>
  group_by(ground) |> mutate(pct = round(100 * adjudicaciones / sum(adjudicaciones), 1)) |>
  ungroup() |> arrange(ground, desc(adjudicaciones))
write.csv(tipos, file.path(DIR_TAB, "tab_causales_tipos.csv"), row.names = FALSE)
cat("\nleading sectors of the named awards:\n"); print(as.data.frame(rubros))
cat("\norgan types of the named awards:\n"); print(as.data.frame(tipos))

# ── the trend, not only the picture ──────────────────────────────────────────
trend_row <- function(ground, ind, axis, eje) {
  dd <- dec[dec$eje == eje & dec$ground == ground, ]
  data.frame(ground = ground, eje = eje,
             point_biserial = round(cor(as.integer(ind), d[[axis]]), 4),
             p_point_biserial = signif(cor.test(as.integer(ind), d[[axis]])$p.value, 3),
             spearman_decil_share = round(cor(dd$decil, dd$share, method = "spearman"), 4),
             p_spearman = signif(cor.test(dd$decil, dd$share, method = "spearman",
                                          exact = FALSE)$p.value, 3))
}
tend <- bind_rows(trend_row("exclusividad", d$exc, "g1", "axis1"),
                  trend_row("especialidad", d$esp, "g1", "axis1"),
                  trend_row("exclusividad", d$exc, "g2", "axis2"),
                  trend_row("especialidad", d$esp, "g2", "axis2"))
write.csv(tend, file.path(DIR_TAB, "tab_causales_tendencia.csv"), row.names = FALSE)
cat("\ntrend:\n"); print(tend, row.names = FALSE)
cat("\nnamed awards by year and ground:\n"); print(table(named_awards$ejercicio, named_awards$ground))
cat("\nOK -> tables/tab_causales_{deciles,lectura,perfil,rubros,tipos,tendencia}.csv, data/processed/causales_proveedor.parquet\n")
