# RAS mutations in acral and cutaneous melanoma: an emerging picture with therapeutic relevance

Analysis pipeline comparing the frequency and spectrum of RAS mutations between acral (AM) and cutaneous (CM) melanoma across published cohorts.

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

A tab-, comma-, semicolon- or pipe-separated table, one row per sample:

| Column | Content |
| --- | --- |
| `Study_cohort` | Cohort name, e.g. `Hayward et al., 2017` |
| `Melanoma_Type` | `Acral` or `Cutaneous` (other types are dropped) |
| `Genetic_Classification` | Genomic subtype; `RAS` marks a RAS-mutant sample |
| `RAS_specific` | `NRAS`, `KRAS` or `HRAS` |
| `NRAS` | NRAS protein change(s), e.g. `Q61R` |

## Output

Each figure (`<name>.pdf`) is written to `results/` with its source data (`Table_<name>.csv`).

Article figures:

| File | Content | Cohorts |
| --- | --- | --- |
| `Figure_1` | Between-cohort heterogeneity of the RAS mutation rate | All |
| `Figure_2` | NRAS, KRAS and HRAS composition of RAS-mutant samples per cohort | All |
| `Figure_3a` | RAS-mutant and RAS wild-type pooled proportions | Paired |
| `Figure_3b` | NRAS, KRAS and HRAS among RAS-mutant samples | Paired |
| `Figure_3c` | NRAS codons (G12, G13, Q61) among NRAS-mutant samples | Paired |
| `Figure_3d` | NRAS Q61 substitutions among NRAS Q61-mutant samples | Paired |
| `Figure_4` | Odds ratios of RAS alterations, AM vs CM | Paired |

Additional figures, not in the article (sensitivity analysis with all cohorts):

| File | Content |
| --- | --- |
| `Additional_Figure_3a`–`3d` | Same analyses as `Figure_3a`–`3d` |
| `Additional_Figure_4` | Same analysis as `Figure_4` |

Paired cohorts are those with both AM and CM samples. The article figures were edited in Affinity (colours, labels and layout); the plotted values are those produced by this pipeline.

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
