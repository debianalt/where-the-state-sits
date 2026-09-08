# 13 — Presupuesto (capital económico) por SAF + consolidación de órganos.
#
# Hallazgo clave: el prefijo numérico del nombre del órgano en COMPR.AR
# ("101 - Fundación Miguel Lillo") ES el código SAF (servicio_id). Esto (a)
# permite el join directo al presupuesto por código, sin fuzzy matching, y (b)
# consolida las 152 cadenas de nombre en 122 SAFs (el mismo SAF aparece con
# nombres distintos según el gobierno; p. ej. SAF 310 = "Ministerio de Salud" /
# "Secretaría de Gobierno de Salud").
#
# Agrega credito-anual-2024 por servicio_id y une a los órganos por SAF.
# Run: python 13_build_presupuesto.py  (desde la raíz del repositorio)
import re
from pathlib import Path

import pandas as pd

PROJ = Path(__file__).resolve().parent
RAW = PROJ / "data" / "raw"
PROC = PROJ / "data" / "processed"


def num(s):
    return pd.to_numeric(s.astype(str).str.replace(",", ".", regex=False),
                         errors="coerce")


cred = pd.read_csv(RAW / "credito-anual-2024.csv", dtype=str, encoding="utf-8")
for c in ["credito_presupuestado", "credito_vigente", "credito_devengado"]:
    cred[c] = num(cred[c])

saf_budget = (cred.groupby("servicio_id")
              .agg(credito_presupuestado=("credito_presupuestado", "sum"),
                   credito_vigente=("credito_vigente", "sum"),
                   credito_devengado=("credito_devengado", "sum"),
                   servicio_desc=("servicio_desc", "first"))
              .reset_index())
print("SAFs en el presupuesto 2024:", len(saf_budget))

organos = pd.read_csv(PROC / "organos_162.csv", encoding="utf-8")["organismo"]
organos = organos.dropna().tolist()
odf = pd.DataFrame({
    "organismo": organos,
    "saf_id": [re.sub(r"^(\d+).*", r"\1", str(o)).strip() for o in organos],
})
odf = odf.merge(saf_budget, left_on="saf_id", right_on="servicio_id", how="left")
odf = odf.drop(columns=["servicio_id"])

print("name-strings:", len(odf), "| SAFs distintos:", odf["saf_id"].nunique())
print("con presupuesto:", int(odf["credito_presupuestado"].notna().sum()),
      "/", len(odf))
print("\nsin presupuesto (SAFs fuera del 2024):")
sinp = odf[odf["credito_presupuestado"].isna()][["saf_id", "organismo"]]
print(sinp.drop_duplicates("saf_id").to_string(index=False))

odf.to_csv(PROC / "tab_saf_presupuesto.csv", index=False, encoding="utf-8")
print("\nOK -> data/processed/tab_saf_presupuesto.csv")
