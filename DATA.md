# Data

Every source is a public administrative record of the Argentine national
state. Nothing here was collected from people, and no supplier is named in any
file of this repository.

## Shipped

| File | Source | What it is for |
|---|---|---|
| `data/raw/georef_provincias.geojson` | georef distribution, infra.datos.gob.ar | the base map of the figures |
| `data/raw/uoc_gazetteer.csv` | own construction over the unit description field, with hand verification | the seat of each buying unit |
| `data/processed/ipc_anual.csv` | national consumer price index, annual | deflating amounts to constant pesos |
| `data/processed/adjudicaciones_apartado.parquet` | derived from the award records | the legal ground of each exemption |
| `data/raw/credito-anual-2024.zip` | national open budget, annual appropriation by service | the budget of each administrative service |
| `data/raw/d-servicio-2024.*`, `d-unidad-ejecutora.*`, `totales-de-presupuesto.*` | the same source, dimension tables | names and codes of services and executing units |
| `data/raw/dotacion_total.xlsx`, `dotacion_por_organismo.xlsx`, `dotacion_entidades.txt` | INDEC, public-employment series by entity | staffing of each entity |
| `data/raw/sedes_correcciones.csv` | hand-coded corrections to the gazetteer | seats the automatic layers placed wrongly |

`credito-anual-2024.zip` travels instead of the CSV inside it, which is 108 MB.
Unzip it in place before running `13_build_presupuesto.py`:

```
cd data/raw && unzip credito-anual-2024.zip
```

## Not shipped, and where to get it

**`data/raw/comprar_adjudicaciones_2016_2026.csv`**, the COMPR.AR award
records, 84 MB. Published as open data by the Oficina Nacional de
Contrataciones on datos.gob.ar, dataset 4, distribution 4.22. Only
`29_unidad_key.py` reads it, and the file that script produces
(`adjudicaciones_unidad.parquet`) is shipped, so the analysis runs without it.
Download it into that folder to rebuild the unit key from scratch.

**The supplier microdata.** The award file, the supplier register and the
taxpayer register carry taxpayer identifiers and names, and roughly two in five
suppliers in this population are natural persons. They are public downloads and
are not redistributed here. What the analysis needs from them ships instead as
the six de-identified interchange files described in the README.

## The de-identified files

`46_release_anonimizada.py` writes them. It replaces the column `cuit` with a
surrogate key, keeping the column name and its string type so that every join,
grouping and filter in the analysis still works, and it drops the two columns
that carry a registered name. It refuses to write a file in which a taxpayer
identifier survives.

The key is the rank of the identifier, one key over all six files. A random key
would reorder the individuals of the geometric analysis, and that reorders the
singular vectors: measured on this data, a permuted key returned every
coordinate of the second axis negated, so a reader comparing the output with
the published tables would meet a mirror image of the space. The cost of the
rank key is that it preserves the lexical order of the identifiers, which
carries a rough ordering by age among natural persons. The prefix that
separates natural persons from companies is already a column of the release,
the sources are public, and an order without the sources maps to nobody.

## Reproducibility note

Running the analysis against the shipped files reproduces every canonical table
in `tables/` byte for byte. The de-identification changes no aggregate: the
surrogate key preserves the order of the identifiers, so the individuals of the
geometric analysis keep the order the published solution was fitted on and the
signs of the axes come out as published.
