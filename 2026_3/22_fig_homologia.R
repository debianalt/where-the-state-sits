# 22 — Figura 2: la homología en dos paneles.
# (a) dispersión: posición del órgano en el eje escala-procedimiento contra la
#     posición media de acceso de sus proveedores, con el r = 0.52 anotado;
# (b) el acceso medio por anclaje, total y dentro del tercil bajo de escala.
source("../2026_2/theme_house.R", chdir = TRUE)
DIR_TAB <- "tables"
DIR_FIG <- "figures"
dir.create(DIR_FIG, showWarnings = FALSE)
suppressPackageStartupMessages({library(dplyr); library(ggplot2); library(patchwork)})

hom <- read.csv(file.path(DIR_TAB, "tab_b7f_homologia.csv"), encoding = "UTF-8")
hom$anclaje <- factor(hom$A_anclaje, levels = c("una_sede", "multi_sede", "sin_geo"))
r2 <- round(cor(hom$org_dim2, hom$mean_sup_dim2, use = "complete.obs"), 2)

pa <- ggplot(hom |> filter(!is.na(org_dim2)), aes(org_dim2, mean_sup_dim2)) +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(aes(fill = anclaje, shape = anclaje), size = 2.0, stroke = 0.5) +
  geom_smooth(method = "lm", se = FALSE, colour = "#666666", linewidth = 0.5) +
  scale_shape_manual(values = c(una_sede = 21, multi_sede = 21, sin_geo = 23),
                     name = NULL) +
  scale_fill_manual(values = c(una_sede = "white", multi_sede = "#1a1a1a",
                               sin_geo = "#999999"), name = NULL) +
  annotate("text", x = Inf, y = -Inf, hjust = 1.1, vjust = -0.8,
           label = sprintf("r = %.2f", r2), size = 2.6, colour = "#4d4d4d") +
  labs(x = "Organ position on the scale-and-procedure axis",
       y = "Mean access position of suppliers", tag = "a") +
  theme_house()

# panel (b): acceso medio por anclaje, total y dentro del tercil bajo
hom$t2 <- cut(hom$org_dim2, quantile(hom$org_dim2, c(0, 1/3, 2/3, 1), na.rm = TRUE),
              labels = c("bajo", "medio", "alto"), include.lowest = TRUE)
b <- hom |> filter(A_anclaje %in% c("multi_sede", "una_sede")) |>
  group_by(t2, A_anclaje) |>
  summarise(access = mean(mean_sup_dim2, na.rm = TRUE), n = n(), .groups = "drop") |>
  mutate(anclaje = factor(A_anclaje, levels = c("una_sede", "multi_sede"))) |>
  filter(!is.na(t2))

pb <- ggplot(b, aes(access, t2, fill = anclaje)) +
  geom_hline(yintercept = seq_along(unique(b$t2)), colour = "grey90",
             linewidth = 0.2) +
  geom_point(shape = 21, size = 3.2, stroke = 0.5,
             position = position_dodge(width = 0.6)) +
  scale_fill_manual(values = c(una_sede = "white", multi_sede = "#1a1a1a"),
                    name = NULL) +
  labs(x = "Mean access position of suppliers", y = "Scale tercile of the organ",
       tag = "b") +
  theme_house() +
  theme(legend.position = "none")

pub(pa | pb, "2", 4.0)
