# 12 — une el link DeepSeek con la serie de dotación (mes de referencia dic-2024).
# Produce data/processed/tab_organ_institucional_dotacion.csv (órgano →
# estructura + entidad_indec + dotación).
import re
import unicodedata

import pandas as pd
from pathlib import Path

PROC = Path(__file__).resolve().parent / "data" / "processed"

link = pd.read_csv(PROC / "link_organ_entidad.csv", encoding="utf-8")
ent = pd.read_csv(PROC / "tab_dotacion_entidades.csv", encoding="utf-8")

date_cols = [c for c in ent.columns if c not in ("estructura", "entidad")]


def to_ts(c):
    try:
        return pd.Timestamp(c)
    except Exception:
        return None


target = [c for c in date_cols if to_ts(c) == pd.Timestamp("2024-12-01")]
ref_col = target[0] if target else date_cols[-1]
print("mes de referencia:", str(ref_col)[:7])

dot = ent[["estructura", "entidad", ref_col]].rename(
    columns={ref_col: "dotacion_ref"})
dot["dotacion_ref"] = pd.to_numeric(dot["dotacion_ref"], errors="coerce")

def norm(s):
    s = unicodedata.normalize("NFD", str(s))
    s = "".join(c for c in s if unicodedata.category(c) != "Mn")
    return re.sub(r"\s+", " ", s).lower().strip()


dot["ent_norm"] = dot["entidad"].map(norm)
link["ent_norm"] = link["entidad_indec"].map(norm)
assert dot["ent_norm"].is_unique, "clave normalizada duplicada en entidades"

out = link.merge(dot[["ent_norm", "dotacion_ref"]], on="ent_norm", how="left")
out = out.drop(columns=["ent_norm"])

out.to_csv(PROC / "tab_organ_institucional_dotacion.csv",
           index=False, encoding="utf-8")
print("\nestructura (n órganos):")
print(out["estructura"].value_counts().to_string())
print("\ndotación no nula:", int(out["dotacion_ref"].notna().sum()), "de",
      len(out), "(match entidad)")
print("match rate del entidad_indec:",
      round(100 * out["dotacion_ref"].notna().mean(), 1), "%")
print("OK -> data/processed/tab_organ_institucional_dotacion.csv")
