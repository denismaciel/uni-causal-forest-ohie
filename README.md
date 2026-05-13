# Application of Causal Forest to the Oregon Health Insurance Experiment Data

This repository contains the OHIE causal forest replication code and the paper
text in one reproducible project.

## Rebuild

Regenerate analysis outputs:

```sh
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

The paper is written under `paper/`, figures are generated under `figs/`, and
the compiled PDF is written to `artifacts/causal-forest-ohie-paper.pdf`.
