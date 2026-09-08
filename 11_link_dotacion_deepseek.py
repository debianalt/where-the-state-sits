# 11 — Link órganos COMPR.AR → entidades INDEC (DeepSeek, temperatura 0).
#
# Para cada órgano comprador devuelve: (1) estructura organizativa
# (centralizada/descentralizada/desconcentrada/otros_entes/empresas/otro) y
# (2) la entidad INDEC asociada (o vacío). El join de dotación se hace después,
# determinístico, por nombre exacto de entidad. Cache en JSON para no re-llamar.
#
# Run: python 11_link_dotacion_deepseek.py  (desde la raíz del repositorio)
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
    # a .env beside the scripts, as a last resort
    env = Path(".env")
    if env.exists():
        for line in env.read_text(encoding="utf-8").splitlines():
            if line.startswith("DEEPSEEK_API_KEY="):
                return line.split("=", 1)[1].strip()
    sys.exit("DEEPSEEK_API_KEY no encontrada")


def strip_pref(s):
    return re.sub(r"^\d+\s*-\s*", "", str(s)).strip()


organos = (pd.read_csv(PROC / "organos_162.csv", encoding="utf-8")["organismo"]
           .dropna().tolist())
organos_limpios = [strip_pref(o) for o in organos]
entidades = pd.read_csv(PROC / "entidades_indec.csv", encoding="utf-8")

SYSTEM = (
    "Sos un experto en la administración pública nacional argentina y su "
    "estructura organizativa. Recibís (a) una lista numerada de organismos que "
    "compraron a través del Estado nacional (COMPR.AR) y (b) una lista de "
    "entidades del INDEC con su estructura organizativa. Para cada organismo "
    "determinás: (1) estructura, una de [administracion_centralizada, "
    "administracion_descentralizada, administracion_desconcentrada, "
    "otros_entes, empresas_y_sociedades, otro]; (2) entidad_indec: el nombre "
    "exacto de la entidad INDEC de la lista a la que pertenece, o cadena vacía "
    "si no pertenece a ninguna (un ministerio central o un órgano interno de uno "
    "va a administracion_centralizada con entidad_indec vacía). Universidades "
    "nacionales o entes ajenos a la APN: estructura \"otro\". Respondés SOLO un "
    "array JSON, un objeto por organismo: {\"i\": numero, \"estructura\": str, "
    "\"entidad_indec\": str}. No inventes: si dudás entre una entidad y ninguna, "
    "elegí ninguna y dejá entidad_indec vacía.")

ents_txt = "\n".join(f"{i + 1}. [{r.estructura}] {r.entidad}"
                     for i, r in entidades.iterrows())

CACHE = PROC / "link_organ_entidad_cache.json"
cache = (json.loads(CACHE.read_text(encoding="utf-8"))
         if CACHE.exists() else [])

B = 40
resultado = list(cache)
done = {r["i"] for r in cache}
for lo in range(0, len(organos), B):
    chunk = [(i + 1, organos_limpios[i])
             for i in range(lo, min(lo + B, len(organos)))]
    pend = [(i, n) for i, n in chunk if i not in done]
    if not pend:
        continue
    listado = "\n".join(f"{i}. {n}" for i, n in pend)
    payload = {"model": "deepseek-chat", "temperature": 0, "max_tokens": 8000,
               "messages": [
                   {"role": "system", "content": SYSTEM},
                   {"role": "user", "content":
                    "Organismos (número = orden):\n\n" + listado +
                    "\n\nEntidades INDEC (estructura | nombre):\n\n" + ents_txt +
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
    resultado.extend(arr)
    CACHE.write_text(json.dumps(resultado, ensure_ascii=False),
                     encoding="utf-8")

mapa = {}
for r in resultado:
    i = r["i"]
    mapa[organos[i - 1]] = {
        "organismo": organos[i - 1],
        "organismo_limpio": organos_limpios[i - 1],
        "estructura": r["estructura"],
        "entidad_indec": r.get("entidad_indec", "") or ""}

df = pd.DataFrame([mapa[o] for o in organos])
df.to_csv(PROC / "link_organ_entidad.csv", index=False, encoding="utf-8")
print("\nestructura (n órganos):")
print(df["estructura"].value_counts().to_string())
print("sin entidad_indec:", int((df["entidad_indec"] == "").sum()))
print("OK -> data/processed/link_organ_entidad.csv")
