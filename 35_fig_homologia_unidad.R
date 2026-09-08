# 35 — Figure 3: the homology at the level of the buying unit.
#
# (a) each unit's position on the axis built from the coordinates the supplier
#     space never sees (script 27) against the mean access position of its
#     suppliers, in grey, with the 24 jurisdictions of the seat at the mean of
#     their units, named in a column at the left with a leader to each, in two
#     blocks according to the sign of the jurisdiction's mean;
# (b) within the organ, each province against the capital: the coefficient of
#     the jurisdiction of the seat in the fixed-effects model of script 28,
#     with its 95 per cent interval, the same organ in different places.
#
# Run: Rscript 35_fig_homologia_unidad.R   (from the repository root, after 27 and 28)

source("theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(patchwork); library(ggrepel)})
dir.create(DIR_FIG, showWarnings = FALSE)

ua <- read.csv(file.path(DIR_TAB, "tab_homologia_unidades.csv"), encoding = "UTF-8",
               stringsAsFactors = FALSE)
r <- cor(ua$uA1, ua$acceso)
rob <- read.csv(file.path(DIR_TAB, "tab_homologia_unidad_robustez.csv"))
ci <- rob$valor[grepl("^r\\(axis of non-shared, access\\), (2\\.5|97\\.5)$", rob$medida)]
r_lab <- sprintf("r = %.2f [%.2f, %.2f]", r, min(ci), max(ci))

mu <- juris_means(ua, "uA1", "acceso", juris = "N_juris")
KEY_A <- c(unit = "Buying unit", juris = "Jurisdiction of the seat, mean of its units")

# Las 24 medias caen en un pañuelo del cuadrante inferior izquierdo y con el
# repel libre se pisaban entre sí y contra la nube. Van a una columna de
# etiquetas en el plano vacío de la izquierda, con guía a cada media. CABA
# queda fuera de la columna y se etiqueta junto a su rombo: es la única media
# lejos del pañuelo y su guía habría cruzado la nube entera.
#
# La columna se parte en dos bloques a los lados de la línea del cero, según
# el acceso medio de la jurisdicción sea positivo o negativo. El corte NO es
# la recta de ajuste: esa recta está estimada sobre las unidades y la tira la
# Capital con 169 de ellas en el extremo alto, de modo que 20 de las 24 medias
# —sin ponderar y amontonadas en el extremo desplegado— caen por debajo. Ese
# 20 a 4 habla del nivel de la recta y no de las jurisdicciones. El cero, en
# cambio, es una cantidad definida, porque el eje de acceso está centrado por
# construcción.
#
# Las posiciones de las etiquetas se calculan acá y no con ggrepel: el repel
# libre no conserva el orden (Tucumán salía entre San Juan y Formosa) y sus
# guías se cruzaban, que es justo lo que la guía en bermellón busca evitar.
XR   <- range(ua$uA1)
XCOL <- XR[1] - 0.60
YBOT <- min(ua$acceso) - 0.02
GAP  <- 0.06                       # medio hueco a cada lado de la línea del cero
mu_caba <- mu[mu$juris == "CABA", ]
stopifnot(nrow(mu_caba) == 1)
arriba <- mu[mu$juris != "CABA" & mu$y >  0, ]
abajo  <- mu[mu$juris != "CABA" & mu$y <= 0, ]
PITCH  <- (-GAP - YBOT) / nrow(abajo)     # paso común a los dos bloques
arriba <- arriba[order(-arriba$y), ]
abajo  <- abajo[order(-abajo$y), ]
arriba$ylab <- GAP + PITCH * rev(seq_len(nrow(arriba)) - 1)
abajo$ylab  <- -GAP - PITCH * (seq_len(nrow(abajo)) - 1)
lab <- rbind(arriba, abajo)
stopifnot(nrow(lab) == 23, !is.unsorted(rev(lab$ylab)))
pa <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(data = ua, aes(uA1, acceso, colour = "unit"), size = 1.6, alpha = 0.75) +
  geom_smooth(data = ua, aes(uA1, acceso), method = "lm", se = FALSE, colour = "#4d4d4d",
              linewidth = 0.5, formula = y ~ x) +
  geom_segment(data = lab, aes(x = XCOL + 0.04, y = ylab, xend = x, yend = y),
               colour = MEAN_COL2, linewidth = 0.2, alpha = 0.7) +
  geom_point(data = mu, aes(x, y, colour = "juris"), shape = 18, size = 2.4) +
  geom_text(data = lab, aes(XCOL, ylab, label = juris), colour = MEAN_COL,
            size = 2.0, hjust = 1) +
  geom_text_repel(data = mu_caba, aes(x, y, label = juris), colour = MEAN_COL, size = 2.0,
                  seed = 42, nudge_x = 0.30, nudge_y = 0.10, hjust = 0,
                  box.padding = 0.10, point.padding = 0.15, min.segment.length = 0,
                  segment.colour = MEAN_COL2, segment.size = 0.2, segment.alpha = 0.7,
                  show.legend = FALSE) +
  scale_colour_manual(values = c(unit = CLOUD_COL, juris = MEAN_COL),
                      labels = KEY_A, breaks = names(KEY_A), name = NULL) +
  # el margen izquierdo tiene que dar para la etiqueta más larga, "Santiago del
  # Estero", que a 2,0 mide cerca de 0,78 pulgadas: con menos de 1,2 unidades
  # del eje se corta contra el borde del panel
  scale_x_continuous(limits = c(XCOL - 1.25, XR[2] + 0.05), breaks = c(0, 1, 2)) +
  guides(colour = guide_legend(override.aes = list(shape = c(16, 18), size = c(2, 2.6),
                                                   alpha = 1))) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.05, vjust = -0.8,
           label = r_lab, size = 2.6, colour = "#4d4d4d") +
  labs(x = paste0("Unit position on the axis of the non-shared coordinates\n",
                  "(deployed, distributed → central, single-seat)"),
       y = "Mean access position of the unit's suppliers", tag = "a",
       title = "Each unit against its own suppliers") +
  theme_panel() + theme(legend.position = "bottom")

# (b) the 23 provinces against the capital, within the organ: the coefficients
# of script 28's fixed-effects model, ordered, with 95 per cent intervals
b <- read.csv(file.path(DIR_TAB, "tab_homologia_unidad_juris_intra.csv"),
              encoding = "UTF-8", stringsAsFactors = FALSE) |>
  mutate(lo = coef_vs_capital - 1.96 * se, hi = coef_vs_capital + 1.96 * se,
         jurisdiccion = factor(jurisdiccion, levels = jurisdiccion[order(coef_vs_capital)]))
pb <- ggplot(b, aes(coef_vs_capital, jurisdiccion)) +
  geom_vline(xintercept = 0, colour = MEAN_COL, linewidth = 0.4) +
  annotate("text", x = 0, y = nlevels(b$jurisdiccion) + 0.9, label = "capital",
           size = 2.3, colour = MEAN_COL, hjust = -0.15) +
  geom_errorbar(aes(xmin = lo, xmax = hi), orientation = "y", width = 0,
                colour = "grey55", linewidth = 0.35) +
  geom_point(colour = MEAN_COL, size = 1.9) +
  scale_y_discrete(expand = expansion(add = c(0.6, 1.6))) +
  labs(x = "Difference from the capital, within the organ",
       y = NULL, tag = "b",
       title = "Within the organ, province against capital") +
  theme_panel() + theme(legend.position = "none",
                        axis.text.y = element_text(size = 6.5))

p <- pa + pb + plot_layout(widths = c(1.35, 1))
pub(p, "3", 4.3)
write.csv(b |> select(jurisdiccion, n_en_test, coef_vs_capital, se, p, lo, hi) |>
            mutate(across(c(lo, hi), \(x) round(x, 4))),
          file.path(DIR_TAB, "tab_fig3_intraorgano.csv"), row.names = FALSE)
cat(sprintf("Fig3: r = %.3f over %d units, %d jurisdictions labelled (%d sobre cero, %d bajo, CABA aparte); panel (b) on %d provinces\n",
            r, nrow(ua), nrow(mu), nrow(arriba), nrow(abajo), nrow(b)))
