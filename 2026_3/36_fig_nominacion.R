# 36 — Figure 4: where the state names, in the plane of the ties.
#
# (a) by decile of the first axis of the joint plane, (b) by decile of the
# second: the share of suppliers with at least one award granted on
# exclusivity, the share with at least one granted on speciality, and the
# share exempted from competition on the amount alone. The two grounds of the
# naming are read apart since 5 Sep 2026 (script 44 derives the flags); the
# merged "named" series is no longer drawn but is still written to
# tab_nominacion_deciles.csv and tab_nominacion_tendencia.csv (Table S11), so
# those tables do not move.
#
# Run: Rscript 36_fig_nominacion.R   (from 2026_3/, after 32 and 44)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(patchwork)})
dir.create(DIR_FIG, showWarnings = FALSE)

s <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet")) |>
  filter(!is.na(S_fundamento)) |>
  left_join(read_parquet(file.path(DIR_PROC3, "causales_proveedor.parquet")) |>
              select(cuit, esp, exc), by = "cuit")
stopifnot(!anyNA(s$esp), !anyNA(s$exc))

# the series drawn, in legend order; hues from the validated set of
# ../2026_2/theme_house.R, colour doubled by shape and by line type
GR_LEVELS <- c("exclusividad", "especialidad", "solo_aritmetico")
GR_LABELS <- c(exclusividad = "Named on exclusivity",
               especialidad = "Named on speciality",
               solo_aritmetico = "Exempted on the amount alone")
GR_COLS   <- c(exclusividad = "#D55E00", especialidad = "#0072B2",
               solo_aritmetico = "#3D405B")
GR_SHAPES <- c(exclusividad = 16, especialidad = 17, solo_aritmetico = 15)
GR_LINES  <- c(exclusividad = "solid", especialidad = "solid",
               solo_aritmetico = "dashed")

# Wilson interval for a binomial share
wilson <- function(d) {
  d |> mutate(z = 1.96, centre = (share + z^2 / (2 * n)) / (1 + z^2 / n),
              half = z * sqrt(share * (1 - share) / n + z^2 / (4 * n^2)) / (1 + z^2 / n),
              lo = centre - half, hi = centre + half) |>
    select(-z, -centre, -half)
}
deciles <- function(x, g) {
  s |> mutate(decil = dplyr::ntile(.data[[x]], 10)) |>
    group_by(decil) |>
    summarise(n = n(), posicion = mean(.data[[x]]),
              consagrado = mean(S_fundamento == "consagrado"),
              solo_aritmetico = mean(S_fundamento == "solo_aritmetico"),
              exclusividad = mean(exc), especialidad = mean(esp),
              .groups = "drop") |>
    tidyr::pivot_longer(c(consagrado, solo_aritmetico, exclusividad, especialidad),
                        names_to = "ground", values_to = "share") |>
    mutate(eje = g) |>
    wilson()
}
d1 <- deciles("g1", "axis1"); d2 <- deciles("g2", "axis2")
dd <- bind_rows(d1, d2)

# Table S11, unchanged: the merged named series and the amount-only series
tab <- dd |> filter(ground %in% c("consagrado", "solo_aritmetico")) |>
  mutate(ground = factor(ground, c("consagrado", "solo_aritmetico"))) |>
  arrange(eje, decil, ground) |>
  mutate(across(c(share, lo, hi), ~round(100 * .x, 2)), posicion = round(posicion, 3)) |>
  select(decil, n, posicion, ground, share, eje, lo, hi)
write.csv(tab, file.path(DIR_TAB, "tab_nominacion_deciles.csv"), row.names = FALSE)
# the trend, not only the picture: the named indicator against the axis
named <- as.integer(s$S_fundamento == "consagrado")
share1 <- d1$share[d1$ground == "consagrado"]
tend <- data.frame(
  medida = c("point-biserial r, named against axis 1", "p",
             "Spearman rho, share named against decile of axis 1", "p",
             "point-biserial r, named against axis 2", "p"),
  valor = round(c(cor(named, s$g1), cor.test(named, s$g1)$p.value,
                  cor(1:10, share1, method = "spearman"),
                  cor.test(1:10, share1, method = "spearman", exact = FALSE)$p.value,
                  cor(named, s$g2), cor.test(named, s$g2)$p.value), 4))
write.csv(tend, file.path(DIR_TAB, "tab_nominacion_tendencia.csv"), row.names = FALSE)
print(tend, row.names = FALSE)

# the two grounds drawn here must be the ones script 44 tabulated (Table S11c)
cau <- read.csv(file.path(DIR_TAB, "tab_causales_deciles.csv"), stringsAsFactors = FALSE)
chk <- dd |> filter(ground %in% c("exclusividad", "especialidad")) |>
  transmute(eje, decil, ground, share = round(100 * share, 2)) |>
  inner_join(cau |> select(eje, decil, ground, share44 = share),
             by = c("eje", "decil", "ground"))
stopifnot(nrow(chk) == 40, all(abs(chk$share - chk$share44) < 1e-6))

fig <- dd |> filter(ground %in% GR_LEVELS) |> mutate(ground = factor(ground, GR_LEVELS))
panel <- function(d, xlab, tag, titulo) {
  ggplot(d, aes(decil, 100 * share, group = ground)) +
    geom_errorbar(aes(ymin = 100 * lo, ymax = 100 * hi), width = 0.25,
                  colour = "grey60", linewidth = 0.3) +
    geom_line(aes(linetype = ground, colour = ground), linewidth = 0.5) +
    geom_point(aes(shape = ground, colour = ground), size = 2.2) +
    scale_shape_manual(values = GR_SHAPES, labels = GR_LABELS, name = NULL) +
    scale_colour_manual(values = GR_COLS, labels = GR_LABELS, name = NULL) +
    scale_linetype_manual(values = GR_LINES, labels = GR_LABELS, name = NULL) +
    scale_x_continuous(breaks = 1:10) +
    labs(x = xlab, y = "Share of suppliers (%)", tag = tag, title = titulo) +
    theme_panel() + theme(legend.position = "bottom")
}
p <- panel(fig |> filter(eje == "axis1"),
           "Decile of the first axis\n(anchored, personal → central, corporate)", "a",
           "Along the axis of deployment") +
  panel(fig |> filter(eje == "axis2"),
        "Decile of the second axis\n(singular contracting → everyday supply)", "b",
        "Along the axis of content") +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")
pub(p, "4", 3.2)
cat("Fig4 ok; deciles en tables/tab_nominacion_deciles.csv (S11) y tab_causales_deciles.csv (S11c)\n")
print(as.data.frame(fig |> transmute(eje, decil, ground, pct = round(100 * share, 1)) |>
  tidyr::pivot_wider(names_from = c(eje, ground), values_from = pct)), row.names = FALSE)
