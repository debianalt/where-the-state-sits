# 10 — Extrae entidades + estructura + dotación del Excel INDEC.
#
# Lee "serie cuadro 2 y 3" de dotacion_total.xlsx (una fila por entidad,
# columnas mensuales jul-2022 a jul-2026). Limpia marcadores de nota al pie
# "(22)" de los nombres, descarta filas de total/footnote, y guarda dos salidas:
#   data/processed/tab_dotacion_entidades.csv  — serie completa por entidad
#   data/processed/entidades_indec.csv         — nombre + estructura (para el link)
#
# NOTA de fondo (footnote 1 del INDEC): la administración centralizada NO se
# desagrega a partir de ene-2024 por la reforma ministerial. Sólo hay
# entidades descentralizadas / desconcentradas / otros entes / empresas. La
# dotación de los ministerios centrales queda como "total", no por organismo.
#
# Run: python 10_extract_dotacion.py  (desde 2026_3/)
import re
from pathlib import Path

import pandas as pd

PROJ = Path(__file__).resolve().parent
RAW = PROJ / "data" / "raw"
PROC = PROJ / "data" / "processed"
PROC.mkdir(exist_ok=True)

ESTRUCTURAS = {
    "Administración centralizada",
    "Administración descentralizada",
    "Administración desconcentrada",
    "Otros entes",
    "Empresas y sociedades",
}

xl = pd.ExcelFile(RAW / "dotacion_total.xlsx")
hoja = [s for s in xl.sheet_names if s.strip().startswith("serie cuadro 2")]
df = xl.parse(hoja[0], header=2)
df = df.rename(columns={df.columns[0]: "estructura", df.columns[1]: "entidad"})

df["estructura"] = df["estructura"].astype(str).str.strip()
df["entidad"] = df["entidad"].astype(str).str.strip()
# sacar marcadores de nota "(22)" / "(15) (20)" del nombre
df["entidad"] = df["entidad"].map(lambda s: re.sub(r"\s*\(\d+\)", "", s).strip())

es = df["estructura"].isin(ESTRUCTURAS)
ent = ~df["entidad"].isin(["", "nan", "NaN"]) & ~df["entidad"].str.startswith("Total")
ents = df[es & ent].copy()

print("estructura (n entidades):")
print(ents["estructura"].value_counts().to_string())
print("total entidades:", len(ents))

ents.to_csv(PROC / "tab_dotacion_entidades.csv", index=False, encoding="utf-8")
ents[["estructura", "entidad"]].to_csv(PROC / "entidades_indec.csv",
                                        index=False, encoding="utf-8")

# órganos del panel (para el linkage)
panel = pd.read_csv(PROJ / "tables" / "tab_organ_panel.csv", encoding="utf-8")
panel["organismo"].dropna().to_csv(PROC / "organos_162.csv",
                                    index=False, header=["organismo"],
                                    encoding="utf-8")
print("órganos del panel:", panel["organismo"].nunique())
print("OK -> data/processed/tab_dotacion_entidades.csv, entidades_indec.csv, organos_162.csv")
