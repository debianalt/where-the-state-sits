"""build_si_tables.py — curate the canonical tables into S_supplementary.md.

Reads the canonical CSVs in tables/ and writes each curated markdown table
between <!-- TABLE:Sx --> markers, placed after the first paragraph of the
supplement that names the table. The prose outside the markers is preserved,
so the supplement's text can be edited by hand. Same pattern as 2026_2.

Run: python build_si_tables.py
"""
import csv
import re
from pathlib import Path

BASE = Path(__file__).resolve().parent
TABLES = BASE / "tables"
SI = BASE / "sections" / "S_supplementary.md"

# English labels for the register's categories, on both sides
CAT = {
    "seguridad_defensa": "Security and defence", "administracion": "General administration",
    "infraestructura": "Infrastructure", "salud_social": "Health and social",
    "ciencia_universidad": "Science and universities",
    "cultura_educacion": "Culture and education",
    "centralizada": "Centralised", "descentralizada": "Decentralised",
    "desconcentrada": "Deconcentrated", "otro": "Other",
    "sin_sede": "Not located",
    "antiguo": "Before 1949", "reciente": "After 1996",
    "sin_dato": "No founding year",
    "alto": "High", "bajo": "Low", "medio": "Middle", "sin_presupuesto": "No 2024 line",
    "chica": "Small", "media": "Middle", "grande": "Large", "sin_ars": "No peso award",
    "sin_directa": "No direct contracting", "mixto": "Mixed",
    "mayoria_directa": "Mostly direct",
    "distribuido": "Three seats or more", "dos_sedes": "Two seats", "una_sede": "One seat",
    "anclado": "Anchored", "programatico": "Programmatic",
    "PersFisica": "Natural person", "SA": "Public limited company",
    "SRL": "Limited liability company", "OtraForma": "Other legal form",
    "reg16_18": "Registered 2016–18", "reg19_21": "Registered 2019–21",
    "reg22plus": "Registered from 2022",
    "consagrado": "Named on exclusivity or speciality",
    "solo_aritmetico": "Exempted on the amount alone",
    "sin_fundamento": "Never exempted from competition",
    "macri": "Macri (2019)", "fernandez": "Fernández (2020–23)", "milei": "Milei (2024–25)",
}
VAR = {
    "A_escala": "Scale of the exchange", "A_directa": "Procedure profile",
    "A_tipo": "Functional type", "A_antiguedad": "Seniority",
    "A_juris": "Jurisdiction of the seat",
    "A_rango": "Institutional rank", "A_presupuesto": "Budget",
    "N_tamano": "Size of the unit", "N_distribuido": "Seats of the organ",
    "anclado": "Anchoring", "sede_provincia": "Jurisdiction of the seat",
    "A_personeria": "Legal form",
    "provincia": "Jurisdiction of the domicile", "A_rubro": "Modal sector",
    "A_registro": "Registration cohort", "A_cliente": "Modal client type",
    "S_fundamento": "Ground of the exception",
    "N_juris": "Jurisdiction of the seat", "N_fundacion": "Seniority",
    "N_rango": "Institutional rank",
    "N_presupuesto": "Budget",
}
# the two grounds of the naming, read apart by script 44 (Table S11c)
GROUND = {"exclusividad": "Exclusivity", "especialidad": "Speciality"}
CAUSAL = {"exclusividad": "Named on exclusivity only",
          "especialidad": "Named on speciality only",
          "ambas": "Named on both", "no_nombrado": "Not named"}
GRUPO = {"named on exclusivity, any": "Named on exclusivity (at least one award)",
         "named on speciality, any": "Named on speciality (at least one award)",
         "named on exclusivity only": "Named on exclusivity only",
         "named on speciality only": "Named on speciality only",
         "named on both": "Named on both", "not named": "Not named"}
# the register's sector names, translated for display
RUBRO_EN = {"SERV. PROFESIONAL Y COMERCIAL": "Professional and commercial services",
            "MANT. REPARACION Y LIMPIEZA": "Maintenance, repair and cleaning",
            "SERVICIO DE NOTICIAS": "News services",
            "LIBRERIA PAP. Y UTILES OFICINA": "Stationery and office supplies",
            "EQUIPOS": "Equipment", "REPUESTOS": "Spare parts",
            "PROD. MEDICO/FARMACEUTICOS/LAB": "Medical, pharmaceutical and laboratory products",
            "INFORMATICA": "Information technology",
            "ELECTRICIDAD Y TELEFONIA": "Electricity and telephony",
            "ALIMENTOS": "Food", "ALQUILER": "Rentals"}
# the seven organs of the within-organ test (Table S10d): the record's names
# without their numeric codes and with spelling normalised, no word dropped
ORGAN_NAMES = {
    "375 - Gendarmeria Nacional": "Gendarmería Nacional",
    "374 - Estado Mayor General del Ejercito": "Estado Mayor General del Ejército",
    "107 - Administración de Parques Nacionales": "Administración de Parques Nacionales",
    "379 - Estado Mayor General de la Armada": "Estado Mayor General de la Armada",
    "381 - Estado Mayor General de La Fuerza Aérea": "Estado Mayor General de la Fuerza Aérea",
    "604 - Dirección Nacional de Vialidad": "Dirección Nacional de Vialidad",
    "371 - Estado Mayor Conjunto (EMCO)": "Estado Mayor Conjunto (EMCO)",
}


def _fmt(value, digits=None, pct=False, year=False):
    s = "" if value is None else str(value).strip()
    if s in ("", "NA", "NaN", "None", "nan"):
        return "—"
    if digits is None and not pct:
        # a unit id is "organ|description": the bar would split the cell
        return CAT.get(s, VAR.get(s, s)).replace("|", " – ")
    try:
        x = float(s)
    except ValueError:
        return CAT.get(s, VAR.get(s, s))
    if pct:
        x *= 100
    d = 1 if digits is None else digits
    # a year is not a quantity: 1949, never 1,949
    if year and d == 0 and 1500 <= x <= 2100:
        return f"{x:.0f}"
    return f"{x:,.{d}f}"


def md_table(csv_name, cols=None, renames=None, digits=None, pct=(), maps=None,
             sort=None, where=None, limit=None, years=(), apply=None):
    with open(TABLES / csv_name, encoding="utf-8-sig", newline="") as fh:
        rows = list(csv.DictReader(fh))
    if where:
        rows = [r for r in rows if where(r)]
    if sort:
        rows.sort(key=sort)
    if limit:
        rows = rows[:limit]
    if not rows:
        return "_No rows._"
    cols = cols or list(rows[0].keys())
    renames, digits, maps = renames or {}, digits or {}, maps or {}
    apply = apply or {}
    head = [renames.get(c, c) for c in cols]
    out = ["| " + " | ".join(head) + " |", "|" + "|".join(["---"] * len(head)) + "|"]
    for r in rows:
        cells = []
        for c in cols:
            v = r.get(c, "")
            if c in apply:
                v = apply[c](v)
            if c in maps:
                v = maps[c].get(str(v).strip(), v)
            cells.append(_fmt(v, digits.get(c), c in pct, c in years))
        out.append("| " + " | ".join(cells) + " |")
    return "\n".join(out)


# Administrative names reach the register with data-entry artefacts: whole
# names in capitals, missing accents, semicolons where the official name has
# commas, and tokens run together ("Agrupación VIFormosa"). Reproducing them
# untouched looks like carelessness rather than fidelity, so spelling and
# capitalisation are normalised for display and the tables say so. Nothing but
# typography is changed: no word is added, dropped or reordered.
# only the particles that are almost never part of a proper name: "La" is
# capitalised in La Rioja and La Pampa, "De" mid-name is a data artefact
ACCENTS = {"Gendarmeria": "Gendarmería", "Policia": "Policía",
           "Movil": "Móvil", "Cordoba": "Córdoba", "Angel": "Ángel",
           "Formacion": "Formación"}
RUN_ON = {"VIFormosa": "VI Formosa"}


def display_name(raw):
    t = str(raw).strip()
    if t.isupper():                       # a whole name in capitals
        t = t.title()
    out = []
    for k, w in enumerate(t.split()):
        w = RUN_ON.get(w, w)
        for a, b in ACCENTS.items():
            if w.rstrip(",;.") == a:
                w = b + w[len(a):]
        if k and w in ("De", "Del", "Y", "E"):
            w = w.lower()
        out.append(w)
    return " ".join(out).replace(";", ",")


def block(key, title, body, note=None):
    parts = [f"<!-- TABLE:{key} -->", "", f"**Table {key}.** {title}", "", body]
    if note:
        parts += ["", f"*Note*: {note}"]
    parts += ["", f"<!-- /TABLE:{key} -->"]
    return "\n".join(parts)


SENIORITY = {"antiguo": "Before 1949", "medio": "1949–1996", "reciente": "After 1996",
             "sin_dato": "No founding year"}


def label_cat(v, c):
    """the middle tercile of seniority is a span of years; every other 'medio'
    is just the middle"""
    if v in ("A_antiguedad", "N_fundacion"):
        return SENIORITY.get(c, c)
    return CAT.get(c, c)


def _cat(r):
    """split 'A_tipo.seguridad_defensa' into a labelled variable and category"""
    v, _, c = r["categoria"].partition(".")
    return VAR.get(v, v), label_cat(v, c)


SENIORITY_BY_ROW = {}


def _prelabel_lectura():
    """write a helper copy of the lectura table whose seniority categories carry
    their own key, so one column can hold both 'medio' of budget and of seniority"""
    src = TABLES / "tab_conjunto_lectura.csv"
    dst = TABLES / "tab_conjunto_lectura_si.csv"
    with open(src, encoding="utf-8-sig", newline="") as fh:
        rows = list(csv.DictReader(fh))
    for r in rows:
        if r["variable"] in ("A_antiguedad", "N_fundacion"):
            key = "seniority_" + r["categoria"]
            SENIORITY_BY_ROW[key] = SENIORITY.get(r["categoria"], r["categoria"])
            r["categoria"] = key
    with open(dst, "w", encoding="utf-8", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=rows[0].keys()); w.writeheader(); w.writerows(rows)


def build():
    B = {}
    _prelabel_lectura()
    B["S1"] = block(
        "S1", "Coverage of the unit key and of the gazetteer, 2019–2025.",
        md_table("tab_unidad_cobertura.csv",
                 renames={"medida": "Measure", "valor": "Value"},
                 digits={"valor": 0}),
        "Awards in the window are those made through COMPR.AR from 2019 to 2025, "
        "new awards only, whose supplier carries a province. A unit is the "
        "buying office named on the award, scoped by its organ.")

    with open(TABLES / "tab_unidad_categorias.csv", encoding="utf-8-sig") as fh:
        cats = list(csv.DictReader(fh))
    freq = "\n".join(
        ["| Coordinate | Category | Units | % |", "|---|---|---|---|"]
        + [f"| {_cat(r)[0]} | {_cat(r)[1]} | {r['n']} | {float(r['pct']):.1f} |"
           for r in cats])
    B["S2"] = block(
        "S2", "The seven coordinates of the units: categories, counts, and the "
              "tercile cuts of the organ-level ones.",
        freq + "\n\n" + md_table("tab_unidad_cortes_padre.csv",
                                 cols=["variable", "corte", "valor", "valor_usd"],
                                 renames={"variable": "Organ-level variable",
                                          "corte": "Cut", "valor": "Value",
                                          "valor_usd": "Value (million USD)"},
                                 digits={"valor": 0, "valor_usd": 1},
                                 years=["valor"],
                                 maps={"variable": {"anio_fundacion": "Founding year",
                                                    "credito_vigente": "2024 credit (million pesos)"}}),
        "Active categories only; the jurisdiction of the seat shows the two that "
        "clear the 5 per cent threshold, and all 24 are in Table S5b. Seniority "
        "and budget are properties of the parent organ, cut in terciles over the "
        "113 organs and carried to their units. The budget is in millions of "
        "pesos; the two cuts are also given in United States dollars at the "
        "annual average official exchange rate of 2024, the year of the credit.")

    B["S3"] = block(
        "S3", "The space of units: modified rates.",
        md_table("tab_unidad_benzecri.csv",
                 renames={"dim": "Axis", "pct_benzecri": "Modified rate (%)"},
                 digits={"dim": 0, "pct_benzecri": 1}))

    B["S4"] = block(
        "S4", "The space of units: contributions by variable and category "
              "coordinates on the first two axes.",
        md_table("tab_unidad_variables.csv",
                 renames={"variable": "Coordinate", "ctr_dim1": "Axis 1 (%)",
                          "ctr_dim2": "Axis 2 (%)", "ctr_dim3": "Axis 3 (%)"},
                 digits={"ctr_dim1": 1, "ctr_dim2": 1, "ctr_dim3": 1},
                 maps={"variable": VAR})
        + "\n\n" + "\n".join(
            ["| Coordinate | Category | Units | Axis 1 | Axis 2 | Ctr 1 (%) | Ctr 2 (%) |",
             "|---|---|---|---|---|---|---|"]
            + [f"| {_cat(r)[0]} | {_cat(r)[1]} | {r['n']} | {float(r['dim.1']):.2f} | "
               f"{float(r['dim.2']):.2f} | {float(r['ctr.dim.1']):.1f} | {float(r['ctr.dim.2']):.1f} |"
               for r in sorted(cats, key=lambda r: float(r["dim.1"]))]),
        "Active categories only: a passivated category carries no contribution and "
        "is not listed. The 24 jurisdictions of the seat, active and passive, are "
        "in Table S5b.")

    B["S4b"] = block(
        "S4b", "The space of suppliers: active coordinates, their categories "
               "and the modified rates.",
        md_table("tab_proveedor_variables.csv",
                 renames={"variable": "Coordinate", "categoria": "Category",
                          "n": "Suppliers", "pct": "%", "pasivada": "Passivated"},
                 digits={"n": 0, "pct": 1},
                 maps={"categoria": CAT})
        + '\n\n' + md_table("tab_proveedor_benzecri.csv",
                            renames={"dim": "Axis",
                                     "pct_benzecri": "Modified rate (%)"},
                            digits={"dim": 0, "pct_benzecri": 1}),
        "The same passivation rule as the unit space: a category carried by "
        "fewer than 5 per cent of suppliers takes no part in the construction "
        "and is located afterwards.")

    B["S4c"] = block(
        "S4c", "The space of suppliers: category coordinates and contributions "
               "on the first two axes.",
        md_table("tab_proveedor_categorias.csv",
                 renames={"categoria": "Category", "dim1": "Axis 1",
                          "dim2": "Axis 2", "ctr1": "Ctr 1 (%)",
                          "ctr2": "Ctr 2 (%)"},
                 digits={"dim1": 2, "dim2": 2, "ctr1": 1, "ctr2": 1},
                 maps={"categoria": CAT}),
        "Rows are ordered by the second axis, the axis of access, which runs "
        "from the natural person selling on direct contracts to the public "
        "limited company selling at scale through competition. Axis 1 "
        "separates the large and infrequent award from the small and repeated "
        "one. Signs are fixed by rule, axis 1 towards the large award and axis "
        "2 towards the public limited company.")

    B["S4d"] = block(
        "S4d", "From the register to the plane: the supplier and award counts "
               "of each step.",
        md_table("tab_proveedor_cobertura.csv",
                 renames={"paso": "Step", "proveedores": "Suppliers",
                          "adjudicaciones": "Awards"},
                 digits={"proveedores": 0, "adjudicaciones": 0}),
        "Each count is read from the file that step wrote. The 22 suppliers "
        "lost at the second step hold awards in the window but carry no record "
        "in SIPRO, so the register gives them neither a legal form nor a "
        "domicile and they cannot be placed. The third step keeps the "
        "suppliers of the units that hold twenty or more. The fourth removes "
        "the unit set aside before the plane was read (Table S6) together with "
        "the fourteen suppliers that were tied to no other unit.")

    B["S5b"] = block(
        "S5b", "The 24 jurisdictions of the seat in the space of units: count, "
               "role and position on the first two axes.",
        md_table("tab_unidad_juris.csv",
                 renames={"jurisdiccion": "Jurisdiction", "n": "Units", "pct": "%",
                          "activa": "Active", "dim1": "Axis 1",
                          "vtest1": "Test value 1", "dim2": "Axis 2",
                          "vtest2": "Test value 2"},
                 digits={"n": 0, "pct": 1, "dim1": 3, "vtest1": 1, "dim2": 3,
                         "vtest2": 1},
                 maps={"activa": {"TRUE": "yes", "FALSE": "no"},
                       "jurisdiccion": CAT}),
        "A jurisdiction is active when it holds 5 per cent or more of the 457 "
        "units, following Le Roux and Rouanet (2010); the others are passive, "
        "located in the space without building it. The position is the mean "
        "coordinate of the jurisdiction's units, with its test value; axis 1 is "
        "oriented towards the capital. CABA is the Ciudad Autónoma de Buenos "
        "Aires, the federal capital.")

    B["S5"] = block(
        "S5", "The unit space against the mean access position of the units' "
              "suppliers, full battery.",
        md_table("tab_unidad_homologia.csv",
                 renames={"medida": "Measure", "valor": "Value"}, digits={"valor": 3}),
        "The full battery includes the two coordinates shared with the supplier "
        "space; the net comparison on the non-shared ones is Table S9.")

    B["S6"] = block(
        "S6", "Stability of the joint plane.",
        md_table("tab_conjunto_estabilidad.csv",
                 renames={"prueba": "Check", "unidades": "Units",
                          "proveedores": "Suppliers", "cor_eje1": "Axis 1",
                          "cor_eje2": "Axis 2"},
                 digits={"unidades": 0, "proveedores": 0})
        + "\n\n" + md_table("tab_conjunto_excluidas.csv",
                            renames={"uoc_id": "Unit set aside", "n_sup": "Suppliers",
                                     "masa": "Mass", "coordenada_eje1": "Coordinate",
                                     "contribucion_eje1": "Contribution to the axis"},
                            digits={"n_sup": 0, "masa": 5, "coordenada_eje1": 2,
                                    "contribucion_eje1": 3},
                            apply={"uoc_id": display_name})
        + "\n\n" + md_table("tab_conjunto_arco.csv",
                            renames={"nube": "Cloud", "medida": "Measure",
                                     "valor": "Value"},
                            digits={"valor": 3},
                            maps={"nube": {"units": "Units",
                                           "suppliers": "Suppliers"}}),
        "Correlations between the axes of a refitted plane, aligned to the full "
        "solution by Procrustes rotation on the suppliers both hold, and the "
        "axes of the full solution. The second panel lists any unit set aside "
        "for carrying more than half of an axis on its own. The third asks "
        "whether the second axis is the arch a strong first axis can "
        "manufacture, by regressing it on a quadratic of the first: on the "
        "units the quadratic term adds nothing, and on the suppliers it adds "
        "0.08, leaving more than three quarters of the second axis unexplained "
        "by any function of the first.")

    B["S6b"] = block(
        "S6b", "Between organs and within organs: the two clouds of units.",
        md_table("tab_estructura_organo.csv",
                 cols=["nube", "eje", "n", "posiciones_distintas", "eta2_nube",
                       "eta2_uniforme", "mayor_organo_unidades_pct",
                       "mayor_organo_inercia_pct"],
                 renames={"nube": "Cloud", "eje": "Axis", "n": "Units",
                          "posiciones_distintas": "Distinct positions",
                          "eta2_nube": "Between organs",
                          "eta2_uniforme": "Between organs, equal weights",
                          "mayor_organo_unidades_pct": "Largest organ, % of units",
                          "mayor_organo_inercia_pct": "Largest organ, % of the axis"},
                 digits={"eje": 0, "n": 0, "posiciones_distintas": 0,
                         "eta2_nube": 3, "eta2_uniforme": 3,
                         "mayor_organo_unidades_pct": 1,
                         "mayor_organo_inercia_pct": 1}),
        "The correlation ratio of the organ on each axis: the share of the "
        "variance that separates organs, the rest running within them. Four of "
        "the seven coordinates of the unit space belong to the parent organ, so "
        "units of one organ that share a scale and a procedure profile occupy "
        "one point, and the 457 units occupy 149 positions. The joint plane is "
        "built from the ties, so every unit has a position of its own. Each "
        "cloud is decomposed in its own metric, equal weights for the "
        "correspondence analysis of the categories and masses for the "
        "correspondence analysis of the table of ties; the equal-weight value "
        "is given beside it. The largest organ is the border force. The last "
        "row is the mean access position of a unit's suppliers, the outcome of "
        "Tables S9 and S10, which is not a coordinate of either cloud.")

    B["S7"] = block(
        "S7", "The joint plane fitted on each presidency alone.",
        md_table("tab_conjunto_eras.csv",
                 renames={"presidencia": "Presidency", "unidades": "Units",
                          "proveedores": "Suppliers", "autovalor_1": "Eigenvalue 1",
                          "autovalor_2": "Eigenvalue 2",
                          "proveedores_comunes": "Suppliers in common",
                          "cor_eje1": "Axis 1", "cor_eje2": "Axis 2", "rv": "RV"},
                 digits={"unidades": 0, "proveedores": 0, "autovalor_1": 3,
                         "autovalor_2": 3, "proveedores_comunes": 0, "cor_eje1": 3,
                         "cor_eje2": 3, "rv": 3}),
        "Each presidency's table holds the units with twenty or more suppliers in "
        "that period; its solution is aligned to the full-window one on the "
        "suppliers both hold.")

    B["S8"] = block(
        "S8", "The joint plane: eigenvalues against a margin-preserving null.",
        md_table("tab_conjunto_autovalores.csv",
                 renames={"dim": "Axis", "autovalor": "Eigenvalue",
                          "share_inercia_pct": "Share of inertia (%)",
                          "nulo_media": "Null, mean", "nulo_sd": "Null, SD",
                          "nulo_mediana": "Null, median", "nulo_p95": "Null, 95th",
                          "nulo_p99": "Null, 99th", "nulo_max": "Null, maximum",
                          "B": "Draws"},
                 digits={"dim": 0, "autovalor": 4, "share_inercia_pct": 2,
                         "nulo_media": 4, "nulo_sd": 4, "nulo_mediana": 4,
                         "nulo_p95": 4, "nulo_p99": 4, "nulo_max": 4, "B": 0}),
        "The null draws tables with the same row and column totals under "
        "independence and decomposes each; only the two largest eigenvalues are "
        "kept per draw.")

    B["S8b"] = block(
        "S8b", "The supplementary categories on the first two axes of the joint "
               "plane: mean coordinate and test value.",
        md_table("tab_conjunto_lectura_si.csv",
                 cols=["lado", "variable", "categoria", "n", "eje", "media", "vtest"],
                 renames={"lado": "Side", "variable": "Variable",
                          "categoria": "Category", "n": "n", "eje": "Axis",
                          "media": "Mean", "vtest": "Test value"},
                 digits={"n": 0, "media": 3, "vtest": 1},
                 maps={"lado": {"unidad": "Units", "proveedor": "Suppliers"},
                       "variable": VAR, "categoria": {**CAT, **SENIORITY_BY_ROW},
                       "eje": {"f1": "1", "f2": "2", "g1": "1", "g2": "2"}},
                 where=lambda r: r["eje"] in ("f1", "f2", "g1", "g2")
                 and r["variable"] != "A_rubro",
                 sort=lambda r: (r["lado"] != "unidad", r["variable"], r["eje"],
                                 float(r["media"]))),
        "Test values follow Le Roux and Rouanet (2010). The jurisdiction is read "
        "from both ends of a tie: the seat of the unit and the fiscal domicile "
        "of the supplier. Sectors are in the replication tables. CABA is the "
        "Ciudad Autónoma de Buenos Aires, the federal capital.")

    B["S8c"] = block(
        "S8c", "Share of the variance of each axis accounted for by each "
               "supplementary variable.",
        md_table("tab_conjunto_r2.csv",
                 renames={"lado": "Side", "variable": "Variable", "f1": "Units, axis 1",
                          "f2": "Units, axis 2", "f3": "Units, axis 3",
                          "g1": "Suppliers, axis 1", "g2": "Suppliers, axis 2",
                          "g3": "Suppliers, axis 3"},
                 digits={"f1": 3, "f2": 3, "f3": 3, "g1": 3, "g2": 3, "g3": 3},
                 maps={"lado": {"unidad": "Units", "proveedor": "Suppliers"},
                       "variable": VAR})
        + "\n\n" + md_table("tab_conjunto_puente.csv",
                            renames={"medida": "Bridge", "valor": "Value"},
                            digits={"valor": 3}),
        "R² of a regression of the coordinate on the variable's categories. The "
        "second panel relates the joint axes to the unit space and to the mean "
        "access position of each unit's suppliers.")

    B["S9"] = block(
        "S9", "The mean access position of a unit's suppliers regressed in blocks.",
        md_table("tab_homologia_unidad_resumen.csv",
                 renames={"modelo": "Block", "valor": "Value"}, digits={"valor": 3})
        + "\n\n" + md_table("tab_homologia_unidad_ejeA.csv",
                            renames={"categoria": "Category", "dim1": "Axis 1",
                                     "ctr1": "Contribution (%)"},
                            digits={"dim1": 3, "ctr1": 1},
                            maps={"categoria": {k: f"{VAR.get(k.split('.')[0], k)}: "
                                                   f"{label_cat(k.split('.')[0], k.split('.')[1])}"
                                                for k in [
                                                    r["categoria"] for r in csv.DictReader(
                                                        open(TABLES / "tab_homologia_unidad_ejeA.csv",
                                                             encoding="utf-8-sig"))]}}),
        "Shared coordinates are the scale and the procedure of the exchange. The "
        "jurisdiction of the seat enters as a factor of 24 levels with the "
        "capital as the reference; its block F, the count of provinces below "
        "the capital and the single-seat coefficient are from the full model, "
        "at equal scale, procedure and type, and the 23 provincial coefficients "
        "are in Table S9b. The standard deviation is of the mean access "
        "position across the 443 units, and is the spread the within-organ "
        "coefficients of Table S10b are read against. The second panel gives "
        "the categories of the axis built from the non-shared coordinates "
        "alone; the passive jurisdictions are located in that space but do "
        "not appear here.")

    JURIS_COLS = {"jurisdiccion": "Province", "n": "Units", "n_en_test": "Units",
                  "coef_vs_capital": "Difference from the capital", "se": "SE",
                  "p": "p"}
    B["S9b"] = block(
        "S9b", "Each province against the capital: the coefficients of the "
               "jurisdiction of the seat in the full model.",
        md_table("tab_homologia_unidad_juris.csv", renames=JURIS_COLS,
                 digits={"n": 0, "coef_vs_capital": 3, "se": 3, "p": 3}),
        "Coefficients of the full model of Table S9, each the difference in "
        "the mean access position of a unit's suppliers between a seat in that "
        "province and a seat in the capital, at equal scale, procedure, type, "
        "distribution, seniority, rank, budget and size. Ordered from the "
        "largest difference.")

    B["S10"] = block(
        "S10", "Robustness of the net homology.",
        md_table("tab_homologia_unidad_robustez.csv",
                 renames={"medida": "Measure", "valor": "Value"}, digits={"valor": 3}),
        "Intervals from 500 bootstrap draws that resample organs. The within-organ "
        "test holds a fixed effect for each organ that holds five or more units "
        "and seats them in more than one jurisdiction (Table S10d); the modal "
        "sector is the sector in which the unit made "
        "most of its awards. The two within-organ R2 values are of the model "
        "with the shared coordinates alone and of the same model with the "
        "jurisdiction and the size of the unit added. The 23 within-organ "
        "coefficients are in Table S10b.")

    B["S10b"] = block(
        "S10b", "Each province against the capital, within the organ.",
        md_table("tab_homologia_unidad_juris_intra.csv", renames=JURIS_COLS,
                 digits={"n_en_test": 0, "coef_vs_capital": 3, "se": 3, "p": 3}),
        "Coefficients of the jurisdiction of the seat in the within-organ model "
        "of Table S10, a fixed effect for each of the seven organs that hold five "
        "or more units and seat them in more than one jurisdiction (Table S10d), "
        "with scale, procedure and the size "
        "of the unit held: the difference in the mean access position of a "
        "unit's suppliers between a seat in that province and a seat in the "
        "capital, inside the same organ. Units are those in the within-organ "
        "test. Ordered from the largest difference; drawn in Figure 3b.")

    B["S10c"] = block(
        "S10c", "The net homology against an access axis built without the "
                "shared coordinates.",
        md_table("tab_homologia_sin_compartidas.csv",
                 renames={"medida": "Measure", "valor": "Value"}, digits={"valor": 3}),
        "The reduced supplier space is a specific multiple correspondence "
        "analysis on legal form, modal sector, modal client type and registration "
        "cohort, with the same passivation rule as the full one. Its access axis "
        "is the one on which the public limited company and the natural person "
        "stand furthest apart, which is the opposition the original axis of "
        "access names; taking the axis by its correlation with that original "
        "would make the check circular in its turn, and the correlation is "
        "reported as a result. The axis is oriented towards the public limited "
        "company and averaged over each unit's suppliers. R2 values are for the "
        "same block regressions as Table S9 on that mean.")

    B["S10d"] = block(
        "S10d", "The seven organs of the within-organ test.",
        md_table("tab_intraorgano_organos.csv",
                 cols=["organismo", "tipo", "unidades", "jurisdicciones",
                       "unidades_capital", "acceso_capital", "acceso_provincias",
                       "capital_mas_alta"],
                 renames={"organismo": "Organ", "tipo": "Functional type",
                          "unidades": "Units", "jurisdicciones": "Jurisdictions",
                          "unidades_capital": "Units in the capital",
                          "acceso_capital": "Mean access, capital",
                          "acceso_provincias": "Mean access, provinces",
                          "capital_mas_alta": "Capital highest"},
                 digits={"unidades": 0, "jurisdicciones": 0, "unidades_capital": 0,
                         "acceso_capital": 3, "acceso_provincias": 3},
                 maps={"organismo": ORGAN_NAMES, "tipo": CAT,
                       "capital_mas_alta": {"TRUE": "yes", "FALSE": "no"}}),
        "The organs that hold five or more units and seat them in more than one "
        "jurisdiction, which the within-organ test of Tables S10 and S10b rests "
        "on; units are those in the test. Mean access is the mean access position "
        "of a unit's suppliers, averaged over the organ's units seated in the "
        "capital and over those seated in the provinces. Names are the record's, "
        "without their numeric codes and with spelling normalised. Two further "
        "organs seat units in more than one jurisdiction but hold four each and "
        "are not in the test.")

    B["S10e"] = block(
        "S10e", "The seat with the supplier's domicile held.",
        md_table("tab_homologia_domicilio.csv",
                 renames={"conjunto": "Suppliers considered",
                          "unidades_capital": "Units in the capital",
                          "unidades_provincia": "Units in the provinces",
                          "acceso_capital": "Mean access, capital",
                          "acceso_provincias": "Mean access, provinces",
                          "brecha": "Difference",
                          "brecha_relativa": "Share of the first row"},
                 digits={"unidades_capital": 0, "unidades_provincia": 0,
                         "acceso_capital": 3, "acceso_provincias": 3,
                         "brecha": 3, "brecha_relativa": 2})
        + "\n\n" + md_table("tab_homologia_domicilio_intra.csv",
                            cols=["organismo", "unidades", "acceso_capital",
                                  "acceso_provincias", "brecha", "capital_mas_alta"],
                            renames={"organismo": "Organ", "unidades": "Units",
                                     "acceso_capital": "Mean access, capital",
                                     "acceso_provincias": "Mean access, provinces",
                                     "brecha": "Difference",
                                     "capital_mas_alta": "Capital highest"},
                            digits={"unidades": 0, "acceso_capital": 3,
                                    "acceso_provincias": 3, "brecha": 3},
                            maps={"organismo": ORGAN_NAMES,
                                  "capital_mas_alta": {"TRUE": "yes",
                                                       "FALSE": "no"}}),
        "A unit's mean access position recomputed over the suppliers that share "
        "a domicile, so that the comparison between a seat in the capital and a "
        "seat in a province no longer runs over different supplier populations. "
        "A unit enters a restricted row when it holds ten or more suppliers of "
        "that domicile, and twenty in the first row. The second panel repeats "
        "the restriction to suppliers domiciled in the capital inside the seven "
        "organs of Table S10d. Rows are unweighted means over units, not the "
        "fitted coefficients of Table S10b.")

    B["S11"] = block(
        "S11", "The ground of the exception by decile of the joint axes.",
        md_table("tab_nominacion_deciles.csv",
                 cols=["eje", "decil", "n", "posicion", "ground", "share", "lo", "hi"],
                 renames={"eje": "Axis", "decil": "Decile", "n": "Suppliers",
                          "posicion": "Mean coordinate", "ground": "Ground",
                          "share": "Share (%)", "lo": "Lower 95%", "hi": "Upper 95%"},
                 digits={"decil": 0, "n": 0, "posicion": 3, "share": 1, "lo": 1,
                         "hi": 1},
                 maps={"ground": CAT, "eje": {"axis1": "1", "axis2": "2"}}),
        "A supplier is named if at least one of its awards in the window was "
        "granted on exclusivity or speciality; exempted on the amount alone if it "
        "was exempted from competition only on that ground.")

    B["S11b"] = block(
        "S11b", "The suppliers the state has named: before how many organs, "
                "and for how long.",
        md_table("tab_consagrados.csv",
                 renames={"medida": "Measure", "valor": "Value"},
                 digits={"valor": 1}),
        "A supplier counts as named if at least one of its awards in the "
        "window was granted on exclusivity or speciality. The organs that name "
        "it are those that granted it such an award; the organs that buy from "
        "it are all those that awarded it anything. Duration is the span "
        "between a supplier's first and last year in the register.")

    B["S11c"] = block(
        "S11c", "The two grounds of the naming: exclusivity and speciality read "
                "apart.",
        md_table("tab_causales_deciles.csv",
                 cols=["eje", "decil", "n", "posicion", "ground", "share", "lo", "hi"],
                 renames={"eje": "Axis", "decil": "Decile", "n": "Suppliers",
                          "posicion": "Mean coordinate", "ground": "Ground",
                          "share": "Share (%)", "lo": "Lower 95%", "hi": "Upper 95%"},
                 digits={"decil": 0, "n": 0, "posicion": 3, "share": 1, "lo": 1,
                         "hi": 1},
                 maps={"ground": GROUND, "eje": {"axis1": "1", "axis2": "2"}})
        + "\n\n" + md_table("tab_causales_lectura.csv",
                            cols=["eje", "categoria", "n", "media", "vtest",
                                  "r2_variable"],
                            renames={"eje": "Axis", "categoria": "Category",
                                     "n": "Suppliers", "media": "Mean coordinate",
                                     "vtest": "Test value",
                                     "r2_variable": "R2 of the axis on the variable"},
                            digits={"n": 0, "media": 3, "vtest": 1, "r2_variable": 3},
                            maps={"eje": {"g1": "1", "g2": "2"}, "categoria": CAUSAL})
        + "\n\n" + md_table("tab_causales_perfil.csv",
                            renames={"grupo": "Group", "proveedores": "Suppliers",
                                     "pct_SA": "Public limited company (%)",
                                     "pct_SRL": "Limited liability company (%)",
                                     "pct_persona_fisica": "Natural person (%)",
                                     "pct_CABA": "Domiciled in the capital (%)",
                                     "pct_un_organo": "Named by one organ (%)",
                                     "mediana_organos": "Median organs naming"},
                            digits={"proveedores": 0, "pct_SA": 1, "pct_SRL": 1,
                                    "pct_persona_fisica": 1, "pct_CABA": 1,
                                    "pct_un_organo": 1, "mediana_organos": 0},
                            maps={"grupo": GRUPO})
        + "\n\n" + md_table("tab_causales_rubros.csv",
                            cols=["ground", "rubro_principal", "adjudicaciones", "pct"],
                            renames={"ground": "Ground",
                                     "rubro_principal": "Sector of the award",
                                     "adjudicaciones": "Awards",
                                     "pct": "Share of the ground's awards (%)"},
                            digits={"adjudicaciones": 0, "pct": 1},
                            maps={"ground": GROUND, "rubro_principal": RUBRO_EN}),
        "Exclusivity is the ground of Decree 1023/2001, art. 25 d) 3, a good or "
        "service with one seller and no convenient substitute; speciality is "
        "art. 25 d) 2, a scientific, technical or artistic work that only a given "
        "firm, artist or specialist can carry out. The first panel counts a "
        "supplier under a ground if at least one of its awards in the window was "
        "granted on it, so a supplier named on both counts twice; intervals are "
        "Wilson 95 per cent intervals. The second panel's four categories are "
        "exclusive, and its test values follow Le Roux and Rouanet (2010). The "
        "third panel describes each group; the organs that name a supplier are "
        "those that granted it an award on that ground. The fourth panel gives "
        "the five leading sectors of the awards granted on each ground. Tables "
        "S8b and S8c read the three named categories as one.")

    B["S12"] = block(
        "S12", "The buying units at the two ends of the first joint axis.",
        md_table("tab_extremos_unidades.csv",
                 cols=["polo", "unidad", "organo", "anio_fundacion", "sede",
                       "rubro_modal", "n_sup", "f1"],
                 renames={"polo": "End", "unidad": "Unit", "organo": "Organ",
                          "anio_fundacion": "Organ founded", "sede": "Seat",
                          "rubro_modal": "Modal sector", "n_sup": "Suppliers",
                          "f1": "Axis 1"},
                 digits={"anio_fundacion": 0, "n_sup": 0, "f1": 2},
                 years=["anio_fundacion"],
                 apply={"unidad": display_name, "organo": display_name},
                 maps={"rubro_modal": {
                           "REPUESTOS": "Spare parts",
                           "ELECTRICIDAD Y TELEFONIA": "Electricity and telephony",
                           "PROD. MEDICO/FARMACEUTICOS/LAB": "Medical, pharmaceutical and laboratory products",
                           "ALIMENTOS": "Food"},
                       "sede": {"no localizada": "not located"}}),
        "The five units furthest out at each end of the first axis of the joint "
        "plane, with the properties the register carries for them. Names are "
        "the record's, without their numeric codes and with spelling and "
        "capitalisation normalised; no word is added, dropped or reordered. The "
        "modal sector is the sector in which the unit made most of its awards, "
        "and its name is translated from the register's own nomenclature.")
    return B


BLOCK_RE = re.compile(r"\n*<!-- TABLE:[^>]*? -->.*?<!-- /TABLE:[^>]*? -->\n*", re.S)
PARA_SEP = "\n\n"


def _natural(key):
    m = re.fullmatch(r"S(\d+)([a-z]*)", key)
    return (int(m.group(1)), m.group(2)) if m else (999, key)


def inject(text, blocks):
    text = BLOCK_RE.sub(PARA_SEP, text)
    paras = text.split(PARA_SEP)
    anchors, missing = {}, []
    for key in sorted(blocks, key=lambda k: (-len(k), k)):
        pat = re.compile(r"\bTables?\s+" + key + r"\b")
        hit = None
        for i, para in enumerate(paras):
            s = para.lstrip()
            if s.startswith("<!-- TABLE:") or s.startswith("|") or s.startswith("**Table"):
                continue
            if pat.search(para):
                hit = i
                break
        (missing.append(key) if hit is None else anchors.__setitem__(key, hit))
    out, placed = [], set()
    pending = sorted(anchors.items(), key=lambda kv: (kv[1], _natural(kv[0])))
    for i, para in enumerate(paras):
        out.append(para)
        for key, idx in pending:
            if idx == i:
                out.append(blocks[key])
                placed.add(key)
    return PARA_SEP.join(out), placed, missing


if __name__ == "__main__":
    blocks = build()
    text = SI.read_text(encoding="utf-8")
    new, placed, missing = inject(text, blocks)
    SI.write_text(new.rstrip("\n") + "\n", encoding="utf-8")
    print(f"tablas escritas: {len(placed)} de {len(blocks)}")
    if missing:
        print("SIN ANCLA en la prosa del SI:", ", ".join(sorted(missing, key=_natural)))
