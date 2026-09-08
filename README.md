# Where the state sits

Replication material for a study of the demand side of a state's market: the
space of the organs and buying units of the Argentine national administration,
built from the award records of the COMPR.AR procurement platform, 2019–2025,
and set against the space of the suppliers they buy from.

The analysis builds three geometric spaces by multiple correspondence analysis
— of buying organs, of buying units and of the ties between units and
suppliers — and asks whether the coordinates the supplier space never sees, the
ones that say where the state physically sits, predict the access position of
the suppliers a unit buys from.

## Layout

The analysis layer reads its interchange files through relative paths into a
sibling directory, so the repository ships the pair rather than one folder
alone. Every script runs from `2026_3/` with no edit:

```
2026_3/    the analysis: 45 scripts, 69 canonical tables, 4 figures
2026_2/    the data layer it reads: theme_house.R, the public raw inputs and
           the three interchange files, de-identified
```

`theme_house.R` holds the window, the palettes, the labels and the shared
constructors; `unidad_helpers.R` holds the unit-level ones.

```
cd 2026_3
Rscript 00_organ_panel.R      # the organ panel
Rscript 01_organ_mca.R        # specification search
Rscript 18_homologia.R        # the homology between the two spaces
```

## What is here, and what is not

| Included | |
|---|---|
| `2026_3/*.R`, `*.py` | every script of the pipeline, 45 files |
| `2026_3/tables/` | the 69 canonical outputs every number in the paper is quoted from |
| `2026_3/figures/` | the four submission figures as PNG, with their captions |
| `2026_3/data/raw/` | the public inputs that are small and carry no personal data |
| `2026_3/data/processed/` | the organ-level and unit-level interchange files |
| `2026_2/` | only what the analysis reads: the shared theme, two public raw inputs and the interchange files |

The folder is assembled by `2026_3/build_replication_folder.py`, which lives
with the project sources and refuses to write a file carrying a taxpayer
identifier or a credential.

## The de-identified interchange files

Six of the files the analysis reads carry the taxpayer identifier of the
supplier, and two of them carry the registered name as well. Roughly two in
five suppliers in this population are natural persons. Those six ship with a
surrogate key in place of the identifier and no name in any column, under the
same file names and the same schema, so no script needs editing:

| file | rows |
|---|---|
| `2026_2/data/processed/adjudicaciones_tipo.parquet` | 204,362 |
| `2026_2/data/processed/flujos_adjudicacion.parquet` | 139,194 |
| `2026_2/data/processed/supplier_master.parquet` | 73,096 |
| `2026_3/data/processed/causales_proveedor.parquet` | 10,529 |
| `2026_3/data/processed/conjunto_proveedores.parquet` | 10,529 |
| `2026_3/data/processed/proveedores_c10.parquet` | 10,580 |

One key over all six, so a join across them still holds.
`2026_3/46_release_anonimizada.py` builds them and asserts the absence of an
identifier before writing. The key is the rank of the identifier rather than a
random permutation, because a permuted key reorders the individuals of the
geometric analysis and reordering flips the sign of an axis: the script header
gives the measurement and what the choice costs.

This is de-identified, not anonymous in the strict sense. The sources are
public downloads, so a reader who holds them can re-link the rows. The key
defends the file, not the population.

**Not included: the supplier microdata**, which carries taxpayer identifiers
and names, and the two large public downloads the data layer reads. `DATA.md`
says where each one comes from.

## Reproducing

R 4.5.1 with tidyverse, arrow, FactoMineR, GDAtools, ggplot2, patchwork,
ggrepel, sf and survival; Python 3 with pandas, pyarrow, numpy and openpyxl for
the data layer. Statistical analysis and every published figure are R;
downloading, cleaning, record linkage and geocoding are Python.

Running the analysis scripts against the shipped interchange files reproduces
the canonical tables byte for byte. One cell is a known exception:
`tab_organ_panel.csv` carries a median distance for one organ that was written
before the flow file was last rebuilt. It changes no category and no result
reported in the paper.

## Licence

Code under the MIT licence (`LICENSE`). Tables, figures and derived data under
Creative Commons Attribution 4.0 (`LICENSE-DATA.md`). The public sources keep
their own terms; `DATA.md` names them.
