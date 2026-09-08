# Build the replication folder for GitHub.
#
# The analysis layer of this project reads a sibling data-layer tree through
# relative paths (`../2026_2/theme_house.R`, `../2026_2/data/...`), so the
# repository mirrors the pair of directories rather than this one alone. That
# way `cd 2026_3 && Rscript 01_organ_mca.R` runs verbatim and the scripts
# published are the scripts that produced the numbers, with no edit to a path
# standing between them.
#
# Run: python build_replication_folder.py   (from 2026_3/)
import pathlib
import re
import shutil
import sys

SRC = pathlib.Path(__file__).resolve().parent          # .../2026_3
SIB = SRC.parent / "2026_2"                            # the data-layer tree
DST = SRC.parent / "where-the-state-sits"
A = SRC / "data" / "anonymised"                        # written by script 46


def _wipe(root):
    # OneDrive holds a handle on a folder it has just synced, so rmtree can
    # fail on the directory even when every file inside it is gone. Delete the
    # files, then remove what directories will go, and carry on.
    for f in sorted(root.rglob("*"), key=lambda q: -len(q.parts)):
        if ".git" in f.parts:          # never touch the git metadata
            continue
        try:
            f.unlink() if f.is_file() else f.rmdir()
        except OSError:
            pass


KEEP = ("README.md", "DATA.md", "LICENSE", "LICENSE-DATA.md", ".gitignore",
        "requirements.txt", "R-packages.txt")
if DST.exists():
    saved = {k: (DST / k).read_bytes() for k in KEEP if (DST / k).exists()}
    _wipe(DST)
else:
    saved = {}
for d in ("2026_3/tables", "2026_3/figures", "2026_3/data/raw",
          "2026_3/data/processed", "2026_2/data/raw", "2026_2/data/processed"):
    (DST / d).mkdir(parents=True, exist_ok=True)

# ── code ─────────────────────────────────────────────────────────────────────
# generate_docx.py stays out: it holds the authors' names, ORCID, email and
# postal address in constants and cannot run without the manuscript sources.
SKIP_CODE = {"generate_docx.py", "update_anon_link.py"}
n_code = 0
for p in sorted(SRC.glob("*.R")) + sorted(SRC.glob("*.py")):
    if p.name in SKIP_CODE:
        continue
    shutil.copy2(p, DST / "2026_3" / p.name)
    n_code += 1
shutil.copy2(SIB / "theme_house.R", DST / "2026_2" / "theme_house.R")
n_code += 1

# ── raw inputs: public, small and not personal ───────────────────────────────
# credito-anual-2024.csv is 108 MB and is the extraction of the zip beside it,
# so the zip travels and DATA.md says to unzip it in place. The COMPR.AR award
# extract the sibling tree holds is 84 MB and is a public download; it is only
# read by 29_unidad_key.py, whose output is shipped, so DATA.md gives its URL.
RAW3 = ["credito-anual-2024.zip", "d-servicio-2024.csv", "d-servicio-2024.zip",
        "d-unidad-ejecutora.csv", "d-unidad-ejecutora.zip",
        "dotacion_entidades.txt", "dotacion_por_organismo.xlsx",
        "dotacion_total.xlsx", "sedes_correcciones.csv",
        "totales-de-presupuesto.csv", "totales-de-presupuesto.zip"]
RAW2 = ["georef_provincias.geojson", "uoc_gazetteer.csv"]
n_raw = 0
for names, src, side in ((RAW3, SRC, "2026_3"), (RAW2, SIB, "2026_2")):
    for name in names:
        p = src / "data" / "raw" / name
        if p.exists():
            shutil.copy2(p, DST / side / "data" / "raw" / name)
            n_raw += 1
        else:
            print("MISSING raw:", side, name)

# ── interchange files ────────────────────────────────────────────────────────
# The de-identified parquets replace the originals under the same names, so no
# script needs editing. Everything else in data/processed is organ-level or
# unit-level and carries no identifier.
CLEAN3 = ["adjudicaciones_unidad.parquet", "entidades_indec.csv",
          "fundacion_saf_cache.json", "link_organ_entidad.csv",
          "link_organ_entidad_cache.json", "organos_162.csv",
          "tab_dotacion_entidades.csv", "tab_organ_institucional_dotacion.csv",
          "tab_saf_fundacion.csv", "tab_saf_gastos_personal.csv",
          "tab_saf_presupuesto.csv"]
n_proc = 0
for name in CLEAN3:
    p = SRC / "data" / "processed" / name
    if p.exists():
        shutil.copy2(p, DST / "2026_3" / "data" / "processed" / name)
        n_proc += 1
    else:
        print("MISSING processed:", name)
# theme_house.R reads these two itself, and neither carries an identifier
CLEAN2 = ["ipc_anual.csv", "adjudicaciones_apartado.parquet"]
for name in CLEAN2:
    p = SIB / "data" / "processed" / name
    if p.exists():
        shutil.copy2(p, DST / "2026_2" / "data" / "processed" / name)
        n_proc += 1
    else:
        print("MISSING sibling processed:", name)
n_anon = 0
if not A.exists():
    sys.exit("faltan los archivos desidentificados: correr 46_release_anonimizada.py")
for side in ("2026_2", "2026_3"):
    for p in sorted((A / side).glob("*.parquet")):
        shutil.copy2(p, DST / side / "data" / "processed" / p.name)
        n_anon += 1
if n_anon != 6:
    sys.exit(f"se esperaban 6 parquets desidentificados y hay {n_anon}")

# ── canonical outputs ────────────────────────────────────────────────────────
n_tab = 0
for p in sorted((SRC / "tables").glob("*.csv")):
    shutil.copy2(p, DST / "2026_3" / "tables" / p.name)
    n_tab += 1
n_fig = 0
for p in sorted((SRC / "figures").glob("*.png")) + [SRC / "figures" / "captions.md"]:
    if p.exists():
        shutil.copy2(p, DST / "2026_3" / "figures" / p.name)
        n_fig += 1

for k, v in saved.items():
    p = DST / k
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_bytes(v)
print("hand-written docs restored:", len(saved))
print(f"code {n_code} | raw {n_raw} | processed {n_proc} | de-identified {n_anon} "
      f"| tables {n_tab} | figures {n_fig}")

# ── safety net 1: nothing personal may reach the repo ────────────────────────
# An identifier is eleven digits standing alone: not touching another digit and
# not touching a decimal point, or the decimal expansion of a latitude reads as
# one. Only quoted cells in text files, since R and pandas quote a character
# column, whereas an award amount of eleven digits is an unquoted number.
cuit_txt = re.compile(r"\"(?:20|23|24|27|30|33|34)\d{9}\"")
cuit_val = re.compile(r"^(?:20|23|24|27|30|33|34)\d{9}$")
bad = []
for p in DST.rglob("*"):
    if not p.is_file() or ".git" in p.parts:
        continue
    if p.suffix.lower() == ".parquet":
        import pandas as pd
        d = pd.read_parquet(p)
        for col in d.columns:
            if d[col].dtype == object and d[col].dropna().astype(str).str.match(cuit_val).any():
                bad.append(f"{p.relative_to(DST)}:{col}")
        continue
    if p.suffix.lower() in (".png", ".xlsx", ".zip", ".gz", ".tiff", ".eps"):
        continue
    try:
        if cuit_txt.search(p.read_text(encoding="utf-8", errors="replace")):
            bad.append(str(p.relative_to(DST)))
    except OSError:
        continue
print("files carrying a taxpayer identifier:", bad if bad else "none")

# ── safety net 2: no credential, and no name of an author ────────────────────
# copia.env holds live API keys. It is not matched by any copy rule above, and
# this is the check that says so out loud on every build.
secret = re.compile(r"sk-[A-Za-z0-9-]{16,}")
leaks = [str(p.relative_to(DST)) for p in DST.rglob("*")
         if p.is_file() and ".git" not in p.parts
         and (p.suffix == ".env" or p.name == "copia.env"
              or (p.suffix in (".py", ".R", ".md", ".txt", ".json", ".csv")
                  and secret.search(p.read_text(encoding="utf-8", errors="replace"))))]
print("files carrying a credential:", leaks if leaks else "none")

if bad or leaks:
    sys.exit("refusing to leave personal identifiers or credentials in the repository")
size = sum(f.stat().st_size for f in DST.rglob("*") if f.is_file() and ".git" not in f.parts)
print(f"total {size/1048576:.1f} MB, "
      f"{sum(1 for f in DST.rglob('*') if f.is_file() and '.git' not in f.parts)} files")
