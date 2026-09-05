> **Moved to academia:** This project is now maintained at
> [papers/causal-forest-ohie](https://github.com/denismaciel/academia/tree/main/papers/causal-forest-ohie).
> Its code, manuscripts, data, and Git history were preserved.
> This repository is retained as a historical archive.

# Application of Causal Forest to the Oregon Health Insurance Experiment Data

This repository contains the OHIE causal forest replication code and the paper
text in one reproducible project.

## Layout

- `R/`: reusable analysis functions.
- `scripts/`: command entrypoints for data preparation, analysis, checks, and
  notebook rendering.
- `notebooks/`: exploratory R Markdown notebooks kept as source only.
- `paper/`: LaTeX source.
- `figs/`: generated paper figures.
- `artifacts/`: generated outputs, including the compiled PDF.

## Rebuild

Regenerate analysis outputs:

```sh
nix run .#data
nix run .#analysis
```

Run analysis checks:

```sh
nix run .#check-analysis
```

Build the paper PDF:

```sh
nix run .#paper
```

Render exploratory notebooks:

```sh
nix run .#notebooks
```

Raw Stata files are kept under `data/OHIE_Public_Use_Files/OHIE_Data/`.
`nix run .#data` converts them into Parquet under `data/interim/parquet/` and
builds `data/analysis/model-data.parquet`, which is the analysis input.

The paper is written under `paper/`, figures are generated under `figs/`, and
the compiled PDF is written to `artifacts/causal-forest-ohie-paper.pdf`.
