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

The scripts sit at the root, as they do in the project folder, and everything
they read is inside the repository. `theme_house.R` holds the window, the
palettes, the labels and the shared constructors; `unidad_helpers.R` holds the
unit-level ones.

```
cd where-the-state-sits
Rscript 00_organ_panel.R      # the organ panel
Rscript 01_organ_mca.R        # specification search
Rscript 18_homologia.R        # the homology between the two spaces
```

In the working tree the analysis reads its interchange files from a separate
data-layer directory through relative paths. The copies published here have
those path constants rewritten to the repository's own `data/`, which is the
only difference between these files and the ones that produced the results.
The rewrite is textual and it is checked the only way that means anything: by
re-running the analysis in a copy of this folder and diffing every table.

## What is here, and what is not

| Included | |
|---|---|
| `*.R`, `*.py` | every script of the pipeline, 44 files |
| `tables/` | the 69 canonical outputs every number in the paper is quoted from |
| `figures/` | the four submission figures as PNG, with their captions |
| `data/raw/` | the public inputs that are small and carry no personal data |
| `data/processed/` | the interchange files the analysis reads, six of them de-identified |

The folder is assembled by a builder that lives with the project sources and
refuses to write a file carrying a taxpayer identifier, a credential or a
path out of the repository.

## The de-identified interchange files

Six of the files the analysis reads carry the taxpayer identifier of the
supplier, and two of them carry the registered name as well. Roughly two in
five suppliers in this population are natural persons. Those six ship with a
surrogate key in place of the identifier and no name in any column, under the
same file names and the same schema, so no script needs editing:

| file | rows |
|---|---|
| `data/processed/adjudicaciones_tipo.parquet` | 204,362 |
| `data/processed/flujos_adjudicacion.parquet` | 139,194 |
| `data/processed/supplier_master.parquet` | 73,096 |
| `data/processed/causales_proveedor.parquet` | 10,529 |
| `data/processed/conjunto_proveedores.parquet` | 10,529 |
| `data/processed/proveedores_c10.parquet` | 10,580 |

One key over all six, so a join across them still holds.
`46_release_anonimizada.py` builds them and asserts the absence of an
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
