# 23 — Revisión adversarial con DeepSeek: mejoras cruciales + calidad de figuras.
#
# Manda el manuscrito completo (secciones 00-06) a DeepSeek y pide una revisión
# crítica: mejoras cruciales ordenadas por severidad + evaluación de las dos
# figuras (nube del espacio y homología). Guarda la respuesta en validation/.
#
# Run: python 23_deepseek_review.py  (desde la raíz del repositorio)
import json
import os
import sys
import urllib.request
import winreg
from pathlib import Path

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
    # a .env beside the scripts, as a last resort
    env = Path(".env")
    if env.exists():
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
    "la tradición del espacio social bourdieuano (Bourdieu, Distinction) y en "
    "análisis geométrico de datos (Le Roux & Rouanet; homólogías). Revisás el "
    "manuscrito que sigue. Respondés en dos bloques, en español: "
    "(1) MEJORAS CRUCIALES — las que hay que hacer para que el artículo sea "
    "aceptable, ordenadas por severidad, cada una con el problema concreto y la "
    "corrección sugerida; no diplomacia, no relleno. "
    "(2) FIGURAS — evaluá la calidad de las dos figuras descritas abajo: ¿son "
    "claras? ¿qué les falta? ¿transmiten el hallazgo? "
    "Contexto de figuras: Figura 1 es la nube del espacio de órganos (122 "
    "puntos grises, categorías activas etiquetadas, los órganos con varias sedes "
    "marcados en círculos abiertos, ejes isométricos coord_fixed). Figura 2 es "
    "un diagrama de dispersión: posición del órgano en el eje "
    "escala-procedimiento (horizontal) contra la posición media de acceso de sus "
    "proveedores (vertical), con la forma del punto por anclaje y una recta de "
    "ajuste.")

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
print("consultando DeepSeek...", flush=True)
with urllib.request.urlopen(req, timeout=900) as r:
    resp = json.load(r)
txt = resp["choices"][0]["message"]["content"]

out = OUT / "deepseek_review_2026-08-31.md"
out.write_text(txt, encoding="utf-8")
print(f"OK -> {out}")
print("=" * 70)
print(txt)
