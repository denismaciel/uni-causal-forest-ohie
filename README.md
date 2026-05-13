# Application of Causal Forest to the Oregon Health Insurance Experiment Data

This repository contains the OHIE causal forest replication code and the paper
text in one reproducible project.

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

Raw Stata files are kept under `data/OHIE_Public_Use_Files/OHIE_Data/`.
`nix run .#data` converts them into Parquet under `data/interim/parquet/` and
builds `data/analysis/model-data.parquet`, which is the analysis input.

The paper is written under `paper/`, figures are generated under `figs/`, and
the compiled PDF is written to `artifacts/causal-forest-ohie-paper.pdf`.
