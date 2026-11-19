# RAS / NRAS mutation analysis: acral (AM) vs cutaneous (CM) melanoma

Meta-analysis pipeline comparing the frequency and spectrum of RAS mutations between acral and cutaneous melanoma across published cohorts.

## Repository structure

```
ras_pipeline/
├── src/
│   └── ras_pipeline.R
├── data/
├── results/
├── renv/
├── renv.lock
├── .Rprofile
├── .gitignore
├── LICENSE
└── README.md
```

## Input

A tab-, comma-, semicolon- or pipe-separated table (separator detected automatically), one row per sample:

| Column | Content |
| --- | --- |
| `Study_cohort` | Cohort name, e.g. `Hayward et al., 2017` |
| `Melanoma_Type` | `Acral` or `Cutaneous` (other types are dropped) |
| `Genetic_Classification` | Genomic subtype; `RAS` marks a RAS-mutant sample |
| `RAS_specific` | `NRAS`, `KRAS` or `HRAS` |
| `NRAS` | NRAS protein change(s), e.g. `Q61R` |

## Usage

From the project root, restore the package environment once:

```bash
R -e 'renv::restore()'
```

Run with the default paths (`data/ras_acral_cutaneous_cohorts_review.tsv` and `results/`) or with your own:

```bash
Rscript src/ras_pipeline.R
Rscript src/ras_pipeline.R <input> <output_dir>
```

## Contact

Marcos Magalhães — marcos.magalhaes@ensino.inca.gov.br
Patricia A. Possik Group, Instituto Nacional de Câncer (INCA)
