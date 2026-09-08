# 37 — Adversarial read of the rewritten manuscript (the joint plane).
#
# Copy-adapted from 23_deepseek_review.py: same jury, same two blocks, three
# figures instead of two. The prompt does not say the manuscript was rewritten
# or why; the read is of the text as it stands. What matters is the content of
# the charges, never the score (round 33 of 2026_2 showed the score series
# moves its own anchors between runs).
#
# Run: python 37_deepseek_review_conjunto.py   (from 2026_3/)
import json
import os
import sys
from datetime import date
import urllib.request
import winreg
from pathlib import Path

sys.stdout.reconfigure(encoding="utf-8")

PROJ = Path(__file__).resolve().parent
SECTIONS = PROJ / "sections"
OUT = PROJ / "validation"
OUT.mkdir(exist_ok=True)


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


order = ["00_abstract.md", "01_introduction.md", "02_framework.md",
         "03_methods.md", "04_results.md", "05_discussion.md",
         "06_conclusions.md"]
manuscript = "\n\n".join((SECTIONS / f).read_text(encoding="utf-8").strip()
                         for f in order)

SYSTEM = (
    "Sos un revisor adversativo de The British Journal of Sociology, experto en "
    "la tradición del espacio social bourdieuano (Bourdieu, Distinction; On the "
    "State) y en análisis geométrico de datos (Le Roux & Rouanet; Greenacre; "
    "homologías entre espacios, Hovden 2023, Krause 2018). Revisás el "
    "manuscrito que sigue. Respondés en dos bloques, en español: "
    "(1) MEJORAS CRUCIALES — las que hay que hacer para que el artículo sea "
    "aceptable, ordenadas por severidad, cada una con el problema concreto y la "
    "corrección sugerida; no diplomacia, no relleno. Prestá atención especial a: "
    "si el análisis de correspondencias de la tabla de lazos está bien "
    "justificado como objeto geométrico y bien calibrado (nulo por permutación, "
    "fila apartada, estabilidad); si la homología a nivel de unidad es circular "
    "o no; si la teoría del capital que promete la sección 2 se entrega; si lo "
    "que se afirma sobre el despliegue del Estado como principio generador está "
    "sostenido por la comparación intra-órgano; y si hay sobreafirmación. "
    "(2) FIGURAS — evaluá las tres figuras descritas abajo: ¿son claras? ¿qué "
    "les falta? ¿transmiten el hallazgo? "
    "Contexto de figuras: Figura 1, el plano conjunto en tres paneles, todos "
    "isométricos: (a) las 456 unidades sobre los ejes 1 y 2 del AC diádico, "
    "marcadas por clase de sede y dimensionadas por masa, ventana de cuantiles "
    "1-99; (b) los proveedores como retícula de posiciones distintas "
    "dimensionadas por cuántos comparten cada una y sombreadas por forma "
    "jurídica; (c) las categorías suplementarias de los dos lados en el mismo "
    "plano, en la media de sus miembros. Figura 2, dos paneles: (a) posición de "
    "cada unidad sobre el eje de las coordenadas no compartidas contra la "
    "posición media de acceso de sus proveedores, marcada por clase de sede, "
    "con recta de ajuste; (b) dentro de los cinco órganos más distribuidos, "
    "acceso medio por clase de sede. Figura 3, dos paneles: participación de "
    "proveedores nombrados por exclusividad o especialidad y de exentos sólo por "
    "monto, por decil de cada eje del plano.")

payload = {
    "model": "deepseek-chat", "temperature": 0, "max_tokens": 8000,
    "messages": [
        {"role": "system", "content": SYSTEM},
        {"role": "user", "content": "MANUSCRITO:\n\n" + manuscript},
    ],
}

req = urllib.request.Request(
    "https://api.deepseek.com/chat/completions",
    data=json.dumps(payload).encode("utf-8"),
    headers={"Content-Type": "application/json",
             "Authorization": f"Bearer {get_key()}"})
print(f"manuscrito: {len(manuscript):,} caracteres; consultando DeepSeek...", flush=True)
with urllib.request.urlopen(req, timeout=900) as r:
    resp = json.load(r)
txt = resp["choices"][0]["message"]["content"]

# date-stamped: a fixed name overwrote the previous round's report on 3 Sep 2026
out = OUT / f"deepseek_review_{date.today():%Y-%m-%d}_conjunto.md"
out.write_text(txt, encoding="utf-8")
print(f"OK -> {out}")
print("=" * 70)
print(txt)
