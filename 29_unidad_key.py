"""
29 — The buying unit behind every award
=======================================
Project: the market in one plane (this project)

Every analysis of this paper runs at the level of the buying unit (UOC), not
the organ (SAF). Until now the unit came from re-reading the 88 MB raw CSV
inside R (script 27) or from flujos_adjudicacion.parquet, which only carries
the 85 per cent of awards whose two ends were located. This script builds the
key once, for every award, and joins the seat of the unit from the gazetteer
where it exists. Seat class (metropolitan / interior / north) is derived in R
from the province, with the constants of theme_house.R, so the two layers
cannot drift.

Input:  data/raw/comprar_adjudicaciones_2016_2026.csv
        data/raw/uoc_gazetteer.csv
        data/processed/adjudicaciones_tipo.parquet   (audit only)
Output: data/processed/adjudicaciones_unidad.parquet
        tables/tab_unidad_cobertura.csv

Usage:  python 29_unidad_key.py
"""
import sys
from pathlib import Path

import pandas as pd

PROJECT = Path(__file__).resolve().parent
RAW2 = PROJECT / "data" / "raw"
PROC2 = PROJECT / "data" / "processed"
OUT = PROJECT / "data" / "processed"
TAB = PROJECT / "tables"
OUT.mkdir(parents=True, exist_ok=True)
TAB.mkdir(parents=True, exist_ok=True)

WIN0, WIN1 = 2019, 2025

raw = pd.read_csv(RAW2 / "comprar_adjudicaciones_2016_2026.csv", dtype=str,
                  usecols=["Documento_Contractual", "Nro_SAF", "Descripcion_SAF",
                           "Nro_UOC", "Descripcion_UOC"])
raw = raw.dropna(subset=["Documento_Contractual"])
key = (raw.drop_duplicates("Documento_Contractual")
          .rename(columns={"Documento_Contractual": "doc_contractual"}))
key["saf_id"] = key["Nro_SAF"].str.strip()
key["uoc"] = key["Descripcion_UOC"].fillna("").str.strip()
key["organismo"] = key["Descripcion_SAF"].str.strip()
# Nro_UOC is the organ-level purchasing office; the unit itself lives in the
# "NN/NNN" prefix of the description (an army brigade is 374/017, not 374).
# Keying on Nro_UOC collapses every delegation of an organ into one row.
# The unit is therefore the description, scoped by its organ so that two
# organs' identically named offices stay apart.
key["uoc_codigo"] = key["uoc"].str.extract(r"^\s*(\d+/\d+)", expand=False)
key["uoc_id"] = key["saf_id"] + "|" + key["uoc"]

# one contractual document must name one unit
mixed = raw.groupby("Documento_Contractual")["Descripcion_UOC"].nunique()
if (mixed > 1).any():
    sys.exit(f"{int((mixed > 1).sum())} documentos con más de una unidad")

# ── the seat of the unit ─────────────────────────────────────────────────────
try:
    gaz = pd.read_csv(RAW2 / "uoc_gazetteer.csv", encoding="utf-8")
except UnicodeDecodeError:
    gaz = pd.read_csv(RAW2 / "uoc_gazetteer.csv", encoding="latin-1")
gaz["uoc"] = gaz["uoc"].astype(str).str.strip()
dups = int(gaz["uoc"].duplicated().sum())
gaz = (gaz.drop_duplicates("uoc")
          .rename(columns={"localidad": "sede_localidad",
                           "provincia": "sede_provincia",
                           "verificado": "sede_verificada"})
          [["uoc", "sede_localidad", "sede_provincia", "lat", "lon",
            "sede_verificada"]])

unidad = key[["doc_contractual", "saf_id", "organismo", "uoc_id", "uoc"]].merge(
    gaz, on="uoc", how="left")
unidad.to_parquet(OUT / "adjudicaciones_unidad.parquet", index=False)

# ── audit against the canonical window ───────────────────────────────────────
tipo = pd.read_parquet(PROC2 / "adjudicaciones_tipo.parquet")
w = tipo[tipo["ejercicio"].between(WIN0, WIN1) & tipo["es_nuevo"]
         & tipo["provincia"].notna()]
m = w.merge(unidad, on="doc_contractual", how="left")
con_doc = m[m["doc_contractual"].notna()]
match = con_doc["uoc"].notna().mean()
sin_doc = int(w["doc_contractual"].isna().sum())
con_sede = m["sede_provincia"].notna().mean()
# counted on uoc_id, the organ-scoped key every analysis uses: one
# description ("81/100 - Direccion General de Administracion") belongs to two
# organs, and counting on the description alone loses a unit
por_unidad = (m.dropna(subset=["uoc_id"]).groupby("uoc_id")
                .agg(n_adj=("cuit", "size"), n_sup=("cuit", "nunique"),
                     con_sede=("sede_provincia", lambda s: s.notna().any())))
print(f"documentos únicos: {len(unidad):,} | gacetero: {len(gaz)} unidades "
      f"({dups} descripciones repetidas colapsadas)")
print(f"ventana canónica: {len(w):,} adjudicaciones | sin documento: {sin_doc} "
      f"| match por documento: {match:.4f}")
print(f"adjudicaciones con sede localizada: {con_sede:.3f}")
print(f"unidades con adjudicaciones: {len(por_unidad)} | con >= 20 proveedores: "
      f"{int((por_unidad.n_sup >= 20).sum())} | de ellas con sede: "
      f"{int(((por_unidad.n_sup >= 20) & por_unidad.con_sede).sum())}")
if match < 1.0:
    sys.exit("la clave de unidad no cubre la ventana")

cob = pd.DataFrame({
    "medida": ["awards in the window", "awards with a unit", "awards with a located seat",
               "units with awards", "units with 20 or more suppliers",
               "of which with a located seat"],
    "valor": [len(w), int(con_doc["uoc_id"].notna().sum()),
              int(m["sede_provincia"].notna().sum()), len(por_unidad),
              int((por_unidad.n_sup >= 20).sum()),
              int(((por_unidad.n_sup >= 20) & por_unidad.con_sede).sum())]})
cob.to_csv(TAB / "tab_unidad_cobertura.csv", index=False)
print("OK -> data/processed/adjudicaciones_unidad.parquet, tables/tab_unidad_cobertura.csv")
