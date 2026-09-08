# unidad_helpers.R — helpers shared by the unit-level scripts (27, 28, 30-33).
#
# Sourced after ../2026_2/theme_house.R, which supplies the window (WIN0/WIN1),
# the province groups (METRO, NEA, NOA), the supplier-space builder and the
# passivation helpers. Everything here is small and used in more than one
# script; tercile_cut used to live in five copies.

D2 <- normalizePath(file.path(dirname(sys.frame(1)$ofile %||% "."), "..", "2026_2"),
                    mustWork = FALSE)
DIR_PROC2 <- file.path(D2, "data", "processed")
DIR_RAW2  <- file.path(D2, "data", "raw")
DIR_PROC3 <- "data/processed"
DIR_TAB   <- "tables"
DIR_FIG   <- "figures"
# theme_house.R's own functions (build_supplier_space reads ipc_anual.csv)
# resolve DIR_PROC and DIR_RAW as globals, and its defaults are relative to
# 2026_2; run from 2026_3 they must point back at 2026_2's data layer
DIR_PROC <- DIR_PROC2
DIR_RAW  <- DIR_RAW2

saf_of <- function(x) sub("^([0-9]+).*", "\\1", as.character(x))

tercile_cut <- function(x, lo, mid, hi) {
  q <- quantile(x, c(1 / 3, 2 / 3), na.rm = TRUE)
  out <- rep(NA_character_, length(x)); ok <- !is.na(x)
  out[ok] <- mid; out[ok & x <= q[1]] <- lo; out[ok & x > q[2]] <- hi
  out
}

# The seat of a unit is its jurisdiction, the register's own 24 categories,
# and nothing coarser (4 Sep 2026). A three-class collapse — core, middle,
# outer — was used for one day and dropped: on the first joint axis the
# capital stands at +0.26 and every other jurisdiction, Buenos Aires included,
# is negative, so the "core" class mixed what the plane separates. The
# jurisdiction is active in the unit space under the 5 per cent rule (only the
# capital and the province of Buenos Aires clear it; the other twenty-two are
# passive and located as supplementary), is a 24-level factor with the capital
# as reference in the regressions, and is read as supplementary in the joint
# plane, from both ends of a tie.
JURIS_REF <- "CABA"
juris_of <- function(provincia) ifelse(is.na(provincia), "sin_sede", provincia)

# Figures draw the cloud in grey and the jurisdictions as labelled mean points,
# the canonical drawing of a supplementary variable with many categories; no
# palette of classes, since 24 hues are unreadable and any grouping of them
# would reintroduce the collapse just dropped. Colour stays where it carries a
# meaning of its own (units against suppliers, legal forms). The two hues come
# from the validated set of ../2026_2/theme_house.R.
CLOUD_COL <- "grey74"
MEAN_COL  <- "#3D405B"
MEAN_COL2 <- "#D55E00"

# mean position of each jurisdiction in a plane, with its count; the unit not
# located by the gazetteer is left out
juris_means <- function(df, x, y, juris = "A_juris") {
  d <- df[df[[juris]] != "sin_sede", ]
  out <- aggregate(cbind(x = d[[x]], y = d[[y]]), by = list(juris = d[[juris]]),
                   FUN = mean)
  out$n <- as.integer(table(d[[juris]])[out$juris])
  out
}

# Le Roux and Rouanet's test value of a category mean against the whole cloud:
# the categories that read an axis are reported with it, never from where
# they happen to fall. One definition, used by scripts 31 and 32.
vtest <- function(x, g) {
  N <- length(x); mu <- mean(x); v <- var(x) * (N - 1) / N
  out <- lapply(split(seq_along(x), g), function(i) {
    n <- length(i); m <- mean(x[i])
    c(n = n, media = m, vtest = (m - mu) / sqrt(v / n * (N - n) / (N - 1)))
  })
  data.frame(categoria = names(out), do.call(rbind, out), row.names = NULL)
}

# The house legend runs one point under the body size, which is right for a
# legend the reader glances at and wrong for one that carries the categories
# the axis is read by. These three figures put the legend back to body size and
# give each panel a title, so a reader knows what a panel is before reading the
# caption.
theme_panel <- function(...) {
  theme_house() +
    theme(legend.text = element_text(size = 8.5, colour = "grey20"),
          legend.key.height = unit(12, "pt"),
          legend.key.width = unit(16, "pt"),
          plot.title = element_text(size = 8.5, colour = "grey20",
                                    margin = margin(b = 4)),
          ...)
}

# Correspondence analysis of a contingency table by SVD of the standardised
# residuals: eigenvalues, principal coordinates of rows and columns, masses.
# Base R only; a 457 x 10,500 table takes a few seconds.
ca_fit_svd <- function(M, k = 3) {
  P <- M / sum(M); r <- rowSums(P); c <- colSums(P)
  S <- (P - outer(r, c)) / sqrt(outer(r, c))
  sv <- svd(S, nu = k, nv = k)
  d <- sv$d[1:k]
  list(ev = d^2, total = sum(sv$d^2),
       F = sweep(sv$u[, 1:k, drop = FALSE], 1, sqrt(r), "/") %*% diag(d, k),
       G = sweep(sv$v[, 1:k, drop = FALSE], 1, sqrt(c), "/") %*% diag(d, k),
       r = r, c = c)
}

# the dyadic table units x suppliers for a set of awards
dyadic_table <- function(ties) {
  units <- sort(unique(ties$uoc_id)); sups <- sort(unique(ties$cuit))
  M <- matrix(0, length(units), length(sups), dimnames = list(units, sups))
  M[cbind(match(ties$uoc_id, units), match(ties$cuit, sups))] <- ties$n
  M
}

# The supplier space C10, fitted once and oriented so that the signs of the
# axes are stable across scripts: axis 2 positive towards the public limited
# company (the corporate pole), axis 1 positive towards the large award (the
# singular, large, infrequent transaction). speMCA signs are arbitrary.
fit_supplier_space <- function(adj, master) {
  m <- build_supplier_space(adj, master)
  X <- active_matrix(m)
  mca <- speMCA(X, excl = excl_index(X, rare_categories(X)), ncp = 2)
  mr <- modif.rate(mca)$modif$mrate
  stopifnot(abs(mr[1] - 43.6) < 0.15, abs(mr[2] - 33.0) < 0.15)
  d1 <- mca$ind$coord[, 1]; d2 <- mca$ind$coord[, 2]
  s1 <- sign(mean(d1[m$A_escala == "grande"]) - mean(d1[m$A_escala == "chica"]))
  s2 <- sign(mean(d2[m$A_personeria == "SA"]) - mean(d2[m$A_personeria == "PersFisica"]))
  m$sup_dim1 <- d1 * s1
  m$sup_dim2 <- d2 * s2
  attr(m, "benzecri") <- mr
  m
}
