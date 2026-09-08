# 15 — Gastos en personal (inciso 1) por SAF: proxy completo de capital humano.
#
# El credito-anual trae el objeto del gasto (inciso). Inciso 1 = "Gastos en
# personal" (masa salarial). Sirve para completar la brecha de la dotación del
# INDEC (que no desagrega la administración centralizada): la masa salarial sí
# está disponible para TODOS los SAF, incluidos los ministerios.
#
# Run: python 15_gastos_personal.py  (desde 2026_3/)
import pandas as pd
from pathlib import Path

PROJ = Path(__file__).resolve().parent
RAW = PROJ / "data" / "raw"
PROC = PROJ / "data" / "processed"


def num(s):
    return pd.to_numeric(s.astype(str).str.replace(",", ".", regex=False),
                         errors="coerce")


cred = pd.read_csv(RAW / "credito-anual-2024.csv", dtype=str, encoding="utf-8")
cred["credito_presupuestado"] = num(cred["credito_presupuestado"])

print("incisos disponibles:", sorted(cred["inciso_id"].dropna().unique())[:12])
print("inciso 1 (Gastos en personal) presente:",
      (cred["inciso_id"] == "1").sum(), "filas")

per = (cred[cred["inciso_id"] == "1"]
       .groupby("servicio_id")["credito_presupuestado"].sum()
       .rename("gastos_personal"))
tot = (cred.groupby("servicio_id")["credito_presupuestado"].sum()
       .rename("credito_total"))

out = pd.DataFrame({"gastos_personal": per, "credito_total": tot}).reset_index()
out["share_personal"] = out["gastos_personal"] / out["credito_total"]
out = out.rename(columns={"servicio_id": "saf_id"})
out["saf_id"] = out["saf_id"].astype(str)

out.to_csv(PROC / "tab_saf_gastos_personal.csv", index=False, encoding="utf-8")
print("\nSAFs con gastos en personal:", len(out))
print("share_personal: min %.3f | med %.3f | max %.3f" % (
    out["share_personal"].min(), out["share_personal"].median(),
    out["share_personal"].max()))
print("OK -> data/processed/tab_saf_gastos_personal.csv")
