# 46 — De-identified interchange files for the replication repository.
#
# Six of the files the analysis reads carry the taxpayer identifier of the
# supplier, and two of them carry the registered name as well. Roughly two in
# five suppliers in this population are natural persons, so redistributing
# those columns is a different act from citing the public source they came
# from. The repository ships this instead: the same six files, with the same
# schema, a surrogate key in place of the identifier and no name in any column.
#
# The schema is preserved on purpose. Every analysis script joins, groups and
# filters on the column named `cuit`, so the column keeps its name and its
# string type and only its values change. Nothing in the analysis reads the
# digits of an identifier, and nothing reads a supplier name, so every table
# and every figure of the paper reproduces from these files unchanged.
#
# One key over all six files, so a join across them still holds.
#
# The key is the RANK of the identifier, not a random permutation, and that is
# a deliberate choice with a cost. A random key reorders the individuals of the
# geometric analysis, and reordering flips the sign of an axis: measured here,
# a permuted key returned every coordinate of axis 2 negated, so a replicator
# reading the published tables would meet the mirror image of the space. The
# rank key makes every table reproduce to the last decimal, signs included.
#
# What it costs: the key preserves the lexical order of the identifiers. The
# prefix that distinguishes a natural person from a company is already a column
# of the release (`personeria`), and within natural persons the order tracks the
# underlying national identity number, so it carries a rough ordering by age.
# Against that: the sources are public downloads, a reader who holds them can
# re-link the rows whatever the key does, and an order without the source maps
# to no one. The reproduction of the published space is worth more than an
# ordering that identifies nobody on its own.
#
# What this does not claim: de-identified is not anonymous in the strict sense.
# The sources are public downloads, so a reader who holds them can re-link the
# rows. The key defends the file, not the population.
#
# Run: python 46_release_anonimizada.py   (from the repository root)
import sys
from pathlib import Path

import pandas as pd

PROJ = Path(__file__).resolve().parent
SRC2 = PROJ / "data" / "processed"
SRC3 = PROJ / "data" / "processed"
OUT = PROJ / "data" / "anonymised"

# the three that come from the data layer, then the three built here
FILES = [
    (SRC2, "adjudicaciones_tipo.parquet"),
    (SRC2, "flujos_adjudicacion.parquet"),
    (SRC2, "supplier_master.parquet"),
    (SRC3, "causales_proveedor.parquet"),
    (SRC3, "conjunto_proveedores.parquet"),
    (SRC3, "proveedores_c10.parquet"),
]
DROP = {"proveedor", "razon_social_sipro"}

OUT.mkdir(parents=True, exist_ok=True)
frames = {}
for src, name in FILES:
    p = src / name
    if not p.exists():
        sys.exit(f"falta {p}")
    frames[name] = pd.read_parquet(p)

# one key over the union of the six files, so any join across them still holds
universe = sorted(set().union(*(set(d["cuit"].dropna()) for d in frames.values())))
key = {c: f"S{i + 1:05d}" for i, c in enumerate(universe)}
print(f"clave sustituta: {len(key)} identificadores")

for name, d in frames.items():
    dropped = [c for c in d.columns if c in DROP]
    d = d.drop(columns=dropped)
    d["cuit"] = d["cuit"].map(key)
    d.to_parquet(OUT / name, index=False)
    print(f"  {name:34} {len(d):>7} filas | fuera: {dropped or 'nada'}")

# ── safety net: no original identifier may survive ───────────────────────────
bad = []
for name in frames:
    d = pd.read_parquet(OUT / name)
    for col in d.columns:
        if d[col].dtype != object:
            continue
        s = d[col].dropna().astype(str)
        if s.str.fullmatch(r"(?:20|23|24|27|30|33|34)\d{9}").any():
            bad.append(f"{name}:{col}")
    if d["cuit"].isna().any():
        bad.append(f"{name}: cuit quedó con nulos")
print("columnas con un identificador fiscal:", bad if bad else "ninguna")
if bad:
    sys.exit("no se escribe un release con identificadores")
