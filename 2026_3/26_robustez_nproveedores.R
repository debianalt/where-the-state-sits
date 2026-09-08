# 26 — Robustez de la homología al número de proveedores por órgano.
#
# Un órgano con un solo proveedor tiene una media poco fiable. Se reporta la
# distribución del número de proveedores por órgano y se re-estima la correlación
# ponderando por el log del número de proveedores, para ver si la homología
# depende de los órganos con pocos proveedores.
#
# Correr:  Rscript 26_robustez_nproveedores.R   (desde 2026_3/)
suppressPackageStartupMessages({library(dplyr)})

hom <- read.csv("tables/tab_b7f_homologia.csv", encoding = "UTF-8")
hom <- hom[complete.cases(hom[c("org_dim1", "org_dim2", "mean_sup_dim2")]), ]

cat("nº de proveedores por órgano:\n")
print(summary(hom$n_sup))
cat(sprintf("  órganos con 1 proveedor: %d\n", sum(hom$n_sup == 1)))
cat(sprintf("  órganos con <5 proveedores: %d\n", sum(hom$n_sup < 5)))
cat(sprintf("  órganos con >=50: %d\n", sum(hom$n_sup >= 50)))

r1 <- cor(hom$org_dim1, hom$mean_sup_dim2)
r2 <- cor(hom$org_dim2, hom$mean_sup_dim2)
w <- log(hom$n_sup)
r1_w <- cov.wt(cbind(hom$org_dim1, hom$mean_sup_dim2), wt = w, cor = TRUE)$cor[1, 2]
r2_w <- cov.wt(cbind(hom$org_dim2, hom$mean_sup_dim2), wt = w, cor = TRUE)$cor[1, 2]

cat(sprintf("\nr(dim1 territorial) sin ponderar = %.3f | ponderado (log n) = %.3f\n",
            r1, r1_w))
cat(sprintf("r(dim2 escala-proc)   sin ponderar = %.3f | ponderado (log n) = %.3f\n",
            r2, r2_w))

res <- data.frame(
  medida = c("Correlation, first axis", "Correlation, second axis",
             "Weighted correlation, first axis", "Weighted correlation, second axis"),
  valor = round(c(r1, r2, r1_w, r2_w), 3))
write.csv(res, "tables/tab_robustez_nproveedores.csv", row.names = FALSE)
cat("\nOK -> tables/tab_robustez_nproveedores.csv\n")
