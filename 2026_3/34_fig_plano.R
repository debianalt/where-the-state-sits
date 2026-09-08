# 34 — Figure 2: the market in one plane.
#
# (a) the 456 buying units on the first two axes of the joint correspondence
#     analysis, in grey and sized by their mass, with the 24 jurisdictions of
#     the seat at the mean position of their units, labelled;
# (b) the 10,529 suppliers, drawn as the lattice of distinct positions sized
#     by how many suppliers share each, shaded by legal form, with the 24
#     jurisdictions of the fiscal domicile at the mean position of their
#     suppliers, labelled;
# (c) the other supplementary categories of both sides in the same plane:
#     anchoring and rank of the units, legal forms of the suppliers. Every
#     panel is isometric and on one common scale.
#
# The jurisdictions are drawn as labelled means and never as a palette: 24
# hues are unreadable, and any grouping of them would put back the collapse
# the article dropped (unidad_helpers.R, 4 Sep 2026).
#
# Run: Rscript 34_fig_plano.R   (from 2026_3/, after 32)

source("../2026_2/theme_house.R", chdir = TRUE)
source("unidad_helpers.R", chdir = TRUE)
suppressPackageStartupMessages({library(arrow); library(patchwork); library(ggrepel)})
dir.create(DIR_FIG, showWarnings = FALSE)

u <- read.csv(file.path(DIR_TAB, "tab_conjunto_unidades.csv"), encoding = "UTF-8",
              stringsAsFactors = FALSE)
s <- read_parquet(file.path(DIR_PROC3, "conjunto_proveedores.parquet"))
lect <- read.csv(file.path(DIR_TAB, "tab_conjunto_lectura.csv"), encoding = "UTF-8",
                 stringsAsFactors = FALSE)
autov <- read.csv(file.path(DIR_TAB, "tab_conjunto_autovalores.csv"))

u <- u |> filter(A_juris != "sin_sede")
# A sparse table throws a few columns of tiny mass far out; drawing the whole
# range squashes the mass of the plane into a dot. The units set the window
# for (a) and (c); (b) is drawn at the window that holds 90 per cent of the
# suppliers, and the caption says how many lie beyond it. At the 99 per cent
# window the tails ran to -10 on axis 2 and the 24 jurisdiction means fell
# into one unreadable clump at the origin.
qwin <- function(x, y, lo = 0.01, hi = 0.99, pad = 0.08) {
  qx <- quantile(x, c(lo, hi)); qy <- quantile(y, c(lo, hi))
  list(x = qx + c(-1, 1) * pad * diff(qx), y = qy + c(-1, 1) * pad * diff(qy))
}
lim <- qwin(u$f1, u$f2)
limb <- qwin(s$g1, s$g2, lo = 0.05, hi = 0.95)
fuera_u <- sum(u$f1 < lim$x[1] | u$f1 > lim$x[2] | u$f2 < lim$y[1] | u$f2 > lim$y[2])
fuera <- sum(s$g1 < limb$x[1] | s$g1 > limb$x[2] | s$g2 < limb$y[1] | s$g2 > limb$y[2])
# the axis label carries its poles, as Fig 3 does: a reader should not have to
# hold the sign convention in mind. Signs read from tab_conjunto_lectura.csv:
# on axis 1 the distributed, provincial and deconcentrated sit negative and the
# single-seat, capital and centralised positive; on axis 2 health and culture,
# the large award and mostly-direct sit negative.
AX_POLES <- c("deployed, anchored → administrative, central",
              "singular contracting → everyday supply")
ax <- function(k, poles = TRUE) {
  lab <- sprintf("Axis %d (eigenvalue %.2f)", k, autov$autovalor[k])
  if (poles) paste0(lab, "\n", AX_POLES[k]) else lab
}
# only position and margin: the text and key sizes belong to theme_panel(),
# and setting them here after it silently shrank this legend back to 6 pt
leg <- theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0))
# one label style for the jurisdictions in (a) and (b)
juris_labels <- function(m, col) {
  geom_text_repel(data = m, aes(x, y, label = juris), colour = col, size = 2.0,
                  seed = 42, max.overlaps = Inf, box.padding = 0.3,
                  point.padding = 0.2, force = 2, force_pull = 0.8, max.iter = 60000,
                  min.segment.length = 0.15, segment.colour = "grey60",
                  segment.size = 0.2, show.legend = FALSE)
}

# ── (a) the units, with the jurisdictions of their seats ─────────────────────
mu <- juris_means(u, "f1", "f2")
KEY_A <- c(unit = "Buying unit", juris = "Jurisdiction of the seat, mean of its units")
pa <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(data = u, aes(f1, f2, size = masa, colour = "unit"), alpha = 0.7) +
  geom_point(data = mu, aes(x, y, colour = "juris"), shape = 18, size = 2.4) +
  juris_labels(mu, MEAN_COL) +
  scale_colour_manual(values = c(unit = CLOUD_COL, juris = MEAN_COL),
                      labels = KEY_A, breaks = names(KEY_A), name = NULL) +
  scale_size_area(max_size = 3.2, guide = "none") +
  guides(colour = guide_legend(override.aes = list(shape = c(16, 18), size = c(2, 2.6),
                                                   alpha = 1))) +
  coord_fixed(xlim = lim$x, ylim = lim$y) +
  labs(x = ax(1), y = ax(2), tag = "a",
       title = sprintf("The %d buying units with a located seat", nrow(u))) +
  theme_panel() + leg

# ── (b) the suppliers, with the jurisdictions of their domiciles ─────────────
lat <- s |> mutate(forma = dplyr::case_when(A_personeria == "PersFisica" ~ "Natural person",
                                            A_personeria == "SA" ~ "Public limited company",
                                            TRUE ~ "Other company"),
                   x = round(g1, 2), y = round(g2, 2)) |>
  count(x, y, forma, name = "n")
ms <- juris_means(s |> mutate(A_juris = provincia) |> as.data.frame(), "g1", "g2")
pb <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(data = lat, aes(x, y, size = n, colour = forma), alpha = 0.55, stroke = 0) +
  scale_colour_manual(values = c("Natural person" = "#9ecae1",
                                 "Other company" = "#4292c6",
                                 "Public limited company" = "#08306b"), name = NULL) +
  scale_size_area(max_size = 3.5, guide = "none") +
  geom_point(data = ms, aes(x, y), colour = MEAN_COL2, shape = 18, size = 2.4) +
  juris_labels(ms, MEAN_COL2) +
  coord_fixed(xlim = limb$x, ylim = limb$y) +
  labs(x = ax(1, poles = FALSE), y = ax(2, poles = FALSE), tag = "b",
       title = "The 10,529 suppliers") +
  theme_panel() + theme(legend.position = "none")

# ── (c) the other categories that read the plane ────────────────────────────
LAB <- c(
  "anclado.anclado" = "Anchored organ", "anclado.programatico" = "Programmatic organ",
  "A_rango.centralizada" = "Centralised", "A_rango.descentralizada" = "Decentralised",
  "A_rango.desconcentrada" = "Deconcentrated",
  "A_personeria.PersFisica" = "Natural person", "A_personeria.SA" = "Public limited co.",
  "A_personeria.SRL" = "Limited liability co.")
cat_u <- lect |> filter(lado == "unidad", eje %in% c("f1", "f2")) |>
  mutate(key = paste(variable, categoria, sep = ".")) |> filter(key %in% names(LAB)) |>
  select(key, eje, media) |> tidyr::pivot_wider(names_from = eje, values_from = media) |>
  rename(x = f1, y = f2) |> mutate(lado = "Units")
cat_s <- lect |> filter(lado == "proveedor", eje %in% c("g1", "g2")) |>
  mutate(key = paste(variable, categoria, sep = ".")) |> filter(key %in% names(LAB)) |>
  select(key, eje, media) |> tidyr::pivot_wider(names_from = eje, values_from = media) |>
  rename(x = g1, y = g2) |> mutate(lado = "Suppliers")
# Units first in the legend, as everywhere else in the article: bind_rows
# leaves the order to the alphabet, which puts the suppliers first
cats <- bind_rows(cat_u, cat_s) |>
  mutate(label = LAB[key], lado = factor(lado, c("Units", "Suppliers")))
limc <- list(x = range(cats$x) + c(-0.75, 0.75), y = range(cats$y) + c(-0.75, 0.75))
pc <- ggplot(cats, aes(x, y)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(aes(shape = lado, colour = lado), size = 2.6) +
  geom_text_repel(aes(label = label, colour = lado), size = 2.3, seed = 42,
                  max.overlaps = Inf, box.padding = 0.5, point.padding = 0.25,
                  force = 1.5, force_pull = 1.2, max.iter = 40000,
                  min.segment.length = 0, segment.colour = "grey70",
                  segment.size = 0.25, show.legend = FALSE) +
  scale_shape_manual(values = c(Units = 16, Suppliers = 18), name = NULL) +
  scale_colour_manual(values = c(Units = MEAN_COL, Suppliers = MEAN_COL2), name = NULL) +
  coord_fixed(xlim = limc$x, ylim = limc$y) +
  labs(x = ax(1), y = ax(2), tag = "c",
       title = "Anchoring, rank and legal form, both sides projected") +
  theme_panel() + theme(legend.position = "bottom")

# (c) needs the whole width for its labels: two rows, the plane on top
p <- (pa | pb) / pc + plot_layout(heights = c(1, 1.05))
pub(p, "2", 7.1)
write.csv(data.frame(unidades_fuera_ventana_a = fuera_u, proveedores_fuera_ventana_b = fuera),
          file.path(DIR_TAB, "tab_fig1_fuera.csv"), row.names = FALSE)
cat(sprintf("proveedores fuera de la ventana de (b): %d\n", fuera))
cat(sprintf("Fig2: %d unidades, %d jurisdicciones en (a) y %d en (b), %d posiciones distintas de proveedor, %d categorías en (c)\n",
            nrow(u), nrow(mu), nrow(ms), nrow(lat), nrow(cats)))
