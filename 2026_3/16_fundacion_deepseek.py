# 16 — Año de fundación de cada SAF (DeepSeek, temperatura 0).
#
# La variable temporal bourdieuana real (ancienneté) es el año de creación del
# organismo como institución, no la entrada a la plataforma. Cache en JSON.
# Run: python 16_fundacion_deepseek.py  (desde 2026_3/)
import json
import os
import re
import sys
import urllib.request
import winreg
from pathlib import Path

import pandas as pd

PROJ = Path(__file__).resolve().parent
PROC = PROJ / "data" / "processed"


def get_key():
    k = os.environ.get("DEEPSEEK_API_KEY")
    if k:
        return k
    try:
        with winreg.OpenKey(winreg.HKEY_CURRENT_USER, "Environment") as h:
            return winreg.QueryValueEx(h, "DEEPSEEK_API_KEY")[0]
    except OSError:
        pass
    f = Path.home() / ".deepseek_key"
    if f.exists():
        return f.read_text(encoding="utf-8").strip()
    env = Path(r"C:\Users\ant\OneDrive\articles_2_\2026_19_positions\.env")
    for line in env.read_text(encoding="utf-8").splitlines():
        if line.startswith("DEEPSEEK_API_KEY="):
            return line.split("=", 1)[1].strip()
    sys.exit("DEEPSEEK_API_KEY no encontrada")


def saf_of(s):
    return re.sub(r"^(\d+).*", r"\1", str(s)).strip()


def strip_pref(s):
    return re.sub(r"^\d+\s*-\s*", "", str(s)).strip()


# 122 SAFs: dedupe de los órganos por código
organos = (pd.read_csv(PROC / "organos_162.csv", encoding="utf-8")["organismo"]
           .dropna().tolist())
safs = {}
for o in organos:
    c = saf_of(o)
    if c not in safs:
        safs[c] = strip_pref(o)
saf_ids = list(safs.keys())
print("SAFs:", len(saf_ids))

SYSTEM = (
    "Sos un experto en la administración pública argentina y su historia "
    "institucional. Para cada organismo devolvés el año de creación/origen como "
    "institución: el año del decreto o ley que lo creó, o el año aproximado de "
    "origen del organismo o del área ministerial que lo antecede. Para los "
    "ministerios usá el año de creación del ministerio en su forma actual (o su "
    "origen directo si es conocido). Respondés SOLO un array JSON, un objeto por "
    "organismo: {\"i\": numero, \"anio\": int}. Si no lo sabés con razonable "
    "certeza, \"anio\": null.")

CACHE = PROC / "fundacion_saf_cache.json"
cache = (json.loads(CACHE.read_text(encoding="utf-8"))
         if CACHE.exists() else [])

B = 40
res = list(cache)
done = {r["i"] for r in cache}
for lo in range(0, len(saf_ids), B):
    chunk = [(i + 1, safs[saf_ids[i]]) for i in range(lo, min(lo + B, len(saf_ids)))]
    pend = [(i, n) for i, n in chunk if i not in done]
    if not pend:
        continue
    listado = "\n".join(f"{i}. [{n}]" for i, n in pend)
    payload = {"model": "deepseek-chat", "temperature": 0, "max_tokens": 4000,
               "messages": [
                   {"role": "system", "content": SYSTEM},
                   {"role": "user", "content":
                    "Organismos (número = orden):\n\n" + listado +
                    "\n\nDevolvé el array JSON."}]}
    req = urllib.request.Request(
        "https://api.deepseek.com/chat/completions",
        data=json.dumps(payload).encode("utf-8"),
        headers={"Content-Type": "application/json",
                 "Authorization": f"Bearer {get_key()}"})
    print(f"tanda {lo // B + 1}: {len(pend)} organismos...", flush=True)
    with urllib.request.urlopen(req, timeout=900) as r:
        resp = json.load(r)
    txt = resp["choices"][0]["message"]["content"].strip().strip("`")
    if txt.startswith("json"):
        txt = txt[4:]
    arr = json.loads(txt)
    res.extend(arr)
    CACHE.write_text(json.dumps(res, ensure_ascii=False), encoding="utf-8")

out = pd.DataFrame({
    "saf_id": [saf_ids[r["i"] - 1] for r in res],
    "organismo": [safs[saf_ids[r["i"] - 1]] for r in res],
    "anio_fundacion": [r.get("anio") for r in res],
})
out.to_csv(PROC / "tab_saf_fundacion.csv", index=False, encoding="utf-8")
print("\nanio_fundacion nulos:", int(out["anio_fundacion"].isna().sum()),
      "/", len(out))
print("rango:", int(out["anio_fundacion"].min()), "-",
      int(out["anio_fundacion"].max()))
print("OK -> data/processed/tab_saf_fundacion.csv")
