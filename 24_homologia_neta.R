# 24 — Homología neta: lo que sobrevive sin las coordenadas compartidas.
#
# La correlación del eje escala-procedimiento (r=0.52) es en parte mecánica,
# porque escala y procedimiento se comparten entre los dos espacios. Este script
# separa la parte no-mecánica: la correlación parcial de cada eje controlando el
# otro, y el efecto del anclaje (coordenada no compartida) controlando la escala.
#
# Correr:  Rscript 24_homologia_neta.R   (desde la raíz del repositorio)
suppressPackageStartupMessages({library(dplyr)})

hom <- read.csv("tables/tab_b7f_homologia.csv", encoding = "UTF-8")
hom$saf_id <- as.character(hom$saf_id)
hom <- hom[complete.cases(hom[c("org_dim1", "org_dim2", "mean_sup_dim2")]), ]

r1 <- cor(hom$org_dim1, hom$mean_sup_dim2)
r2 <- cor(hom$org_dim2, hom$mean_sup_dim2)

# correlación parcial: dim1 controlando dim2
e_a <- lm(mean_sup_dim2 ~ org_dim2, hom)$residuals
e_b <- lm(org_dim1 ~ org_dim2, hom)$residuals
r1_par <- cor(e_a, e_b)
# dim2 controlando dim1
e_a2 <- lm(mean_sup_dim2 ~ org_dim1, hom)$residuals
e_b2 <- lm(org_dim2 ~ org_dim1, hom)$residuals
r2_par <- cor(e_a2, e_b2)

# anclaje controlando escala
hom$multi <- as.integer(hom$A_anclaje == "multi_sede")
fit_anc <- lm(mean_sup_dim2 ~ org_dim2 + multi, hom)
b_anc <- coef(fit_anc)[3]
p_anc <- summary(fit_anc)$coefficients[3, 4]

# rango desconcentrado (no compartido) controlando escala
saf <- read.csv("tables/tab_saf_panel.csv", encoding = "UTF-8")
saf$saf_id <- as.character(saf$saf_id)
saf$descon <- as.integer(saf$A_institucional == "administracion_desconcentrada")
hom2 <- hom |> left_join(saf |> select(saf_id, descon), by = "saf_id")
fit_des <- lm(mean_sup_dim2 ~ org_dim2 + descon, hom2)
b_des <- coef(fit_des)[3]
p_des <- summary(fit_des)$coefficients[3, 4]

cat("Homología neta (n =", nrow(hom), "órganos):\n")
cat(sprintf("  r(dim1 territorial, acceso)            = %.3f\n", r1))
cat(sprintf("  r(dim2 escala-procedimiento, acceso)   = %.3f  (mecánica en parte)\n", r2))
cat(sprintf("  r parcial (dim1 | dim2)               = %.3f\n", r1_par))
cat(sprintf("  r parcial (dim2 | dim1)               = %.3f\n", r2_par))
cat(sprintf("  anclaje multi_sede | escala: β = %.3f, p = %.3g\n", b_anc, p_anc))
cat(sprintf("  rango desconcentrado | escala: β = %.3f, p = %.3g\n", b_des, p_des))

res <- data.frame(
  medida = c("Correlation, first axis", "Correlation, second axis",
             "Partial r, first axis (controlling the second)",
             "Partial r, second axis (controlling the first)",
             "Anchoring (several seats), controlling scale",
             "Deconcentrated rank, controlling scale"),
  valor = round(c(r1, r2, r1_par, r2_par, b_anc, b_des), 3),
  p = c(NA, NA, NA, NA, p_anc, p_des))
write.csv(res, "tables/tab_homologia_neta.csv", row.names = FALSE)
cat("\nOK -> tables/tab_homologia_neta.csv\n")
