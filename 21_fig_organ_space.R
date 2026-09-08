# 21 — Figura 1: la nube del espacio de órganos (B7f).
source("theme_house.R", chdir = TRUE)
DIR_PROC <- "data/processed"
DIR_TAB  <- "tables"
DIR_FIG  <- "figures"
dir.create(DIR_FIG, showWarnings = FALSE)
suppressPackageStartupMessages({library(arrow); library(GDAtools); library(ggrepel)})

saf <- read.csv(file.path(DIR_TAB, "tab_saf_panel.csv"), encoding = "UTF-8")
fund <- read.csv(file.path("data", "processed", "tab_saf_fundacion.csv"),
                 encoding = "UTF-8")
saf$saf_id <- as.character(saf$saf_id); fund$saf_id <- as.character(fund$saf_id)
saf <- saf |> left_join(fund |> select(saf_id, anio_fundacion), by = "saf_id")

tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- mid; out[ok & x <= q[1]] <- lo; out[ok & x > q[2]] <- hi; out
}
saf <- saf |> mutate(
  A_escala = factor(A_escala), A_directa = factor(A_directa),
  A_tipo = factor(A_tipo), A_anclaje = factor(A_anclaje),
  A_institucional = factor(A_institucional), A_presupuesto = factor(A_presupuesto),
  A_fundacion = factor(tercile_cut(anio_fundacion, "antiguo", "medio", "reciente")))

X <- saf[, c("A_escala", "A_directa", "A_tipo", "A_fundacion", "A_anclaje",
             "A_institucional", "A_presupuesto")] |>
  mutate(across(everything(), as.factor))
mca <- speMCA(X, excl = excl_index(X, rare_categories(X)), ncp = 2)
mr <- modif.rate(mca)$modif$mrate

LAB <- c(
  "A_escala.chica" = "Small award", "A_escala.media" = "Mid award",
  "A_escala.grande" = "Large award",
  "A_directa.mixto" = "Mixed procedure", "A_directa.mayoria_directa" = "Mostly direct",
  "A_tipo.administracion" = "General administration",
  "A_tipo.ciencia_universidad" = "Science and universities",
  "A_tipo.cultura_educacion" = "Culture and education",
  "A_tipo.infraestructura" = "Infrastructure",
  "A_tipo.salud_social" = "Health and social",
  "A_tipo.seguridad_defensa" = "Security and defence",
  "A_fundacion.antiguo" = "Oldest cohort", "A_fundacion.medio" = "Middle cohort",
  "A_fundacion.reciente" = "Recent cohort",
  "A_anclaje.multi_sede" = "Several seats", "A_anclaje.una_sede" = "Single seat",
  "A_institucional.administracion_centralizada" = "Centralised",
  "A_institucional.administracion_descentralizada" = "Decentralised",
  "A_institucional.administracion_desconcentrada" = "Deconcentrated",
  "A_presupuesto.alto" = "High budget", "A_presupuesto.bajo" = "Low budget",
  "A_presupuesto.medio" = "Mid budget", "A_presupuesto.sin_presupuesto" = "No 2024 line")

cats <- data.frame(categoria = rownames(mca$var$coord),
                   dim1 = mca$var$coord[, 1], dim2 = mca$var$coord[, 2]) |>
  mutate(label = LAB[categoria])
org <- data.frame(dim1 = mca$ind$coord[, 1], dim2 = mca$ind$coord[, 2],
                  anclaje = saf$A_anclaje)

p <- ggplot() +
  geom_hline(yintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = 0, colour = "grey85", linewidth = 0.3) +
  geom_point(data = org, aes(dim1, dim2), colour = "#d9d9d9", size = 1.4) +
  geom_point(data = org |> filter(anclaje == "multi_sede"), aes(dim1, dim2),
             shape = 1, colour = "#1a1a1a", size = 2.2) +
  geom_text_repel(data = cats, aes(dim1, dim2, label = label),
                  size = 2.1, colour = "#4d4d4d", seed = 7,
                  max.overlaps = Inf, max.iter = 60000,
                  force = 4, force_pull = 0.4, min.segment.length = 0.15,
                  box.padding = 0.3, point.padding = 0.12,
                  segment.colour = "grey55", segment.size = 0.2) +
  coord_fixed() +
  labs(x = sprintf("Dim 1 (%.1f%%, Benzécri)", mr[1]),
       y = sprintf("Dim 2 (%.1f%%, Benzécri)", mr[2]),
       title = "The space of state organs") +
  theme_house()

pub(p, "1", 4.8)
