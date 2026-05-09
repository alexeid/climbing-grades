# climbing-grades

A Bayesian analysis of the climbing grade scale.

The accompanying manuscript (`manuscript/ms-jqas-r1.tex`) fits a Bayesian
dynamic Bradley-Terry model to ascent data from
[thecrag.com](https://www.thecrag.com) for sport, trad and bouldering grades
in Australia, New Zealand and Germany. The central inferential target is the
slope parameter `m`, which describes the multiplicative increase in difficulty
(`d = exp(m)`) per unit of grade.

## Repository layout

```
data/processed/        Pre-cleaned ascent and route RDS files (per country)
R/                     Analysis pipeline (data prep, Stan inference, plotting)
results/{aus,nz,germany}/   Per-analysis outputs: posterior draws, diagnostics, figures, processing tables
manuscript/            LaTeX sources for the manuscript, supplement and response document
logs/                  Stdout/stderr from the run-*.R scripts (created on first run)
```

## R dependencies

The inference scripts require: `rstan`, `lubridate`, `stringr`, `xtable`,
`knitr`, `tidyr`. The package `tidyjson` is referenced by
`R/thecrag-json.R` for the original JSON-to-RDS conversion of raw thecrag.com
exports, but is loaded lazily so the inference pipeline runs without it once
the RDS files in `data/processed/` are in place.

## Reproducing the analyses

The Stan models are defined in `R/climbing-stan.R`. Two variants are provided:

- **Fixed-`w` (default)** — the Wiener-process step SD is fixed at `w = 0.5`
  grade units per month. This was used for all 20 headline analyses.
- **Joint-`w`** — `w` is estimated jointly with `m` and the climber-grade
  trajectories under a HalfNormal(0, 1) prior. Selected with
  `run.stan.climbing.model(d, estimate.w=TRUE)`.

The 20 headline analyses are driven by `R/create-all-notebooks.R`, which
iterates over rows of `R/config.csv` and renders
`R/bayesian-climbing-analysis.Rmd` for each configuration.

For the JQAS revision we added several focused, reproducible scripts that run
a single configuration and write outputs (posterior draws, convergence
diagnostics, posterior figure) to the corresponding `results/` directory.

| Script | What it does | Reviewer comment addressed |
|---|---|---|
| `R/run-aus-sport-diagnostics.R`   | Headline AUS Sport per-session run with R-hat / ESS / divergence diagnostics captured                              | #2 (MCMC diagnostics)   |
| `R/run-aus-sport-thresholds.R`    | Threshold sensitivity: AUS Sport per-session at min.ascents = 50 and 100 (≥30 already done)                        | #4 (threshold sensitivity) |
| `R/run-aus-sport-every-attempt.R` | AUS Sport per-session restricted to climbers who appear to log every attempt (≤5% ticks, ≤5% lonely redpoints)     | #9 (every-attempt logger subset) |
| `R/run-aus-sport-estimate-w.R`    | AUS Sport per-session with joint estimation of the Wiener-process step SD `w` (Gamma(2, 4) prior)                  | #3 (Wiener variance treatment) |
| `R/run-aus-sport-prior-sensitivity.R` | AUS Sport per-session re-fit under three alternative priors on `m` (tighter / wider lognormal, Gamma(2, 4))    | #2 (prior justification / sensitivity) |

Each script is self-contained and writes its log to `logs/`. To run any of
them from the repository root:

```bash
mkdir -p logs
Rscript R/run-aus-sport-diagnostics.R   > logs/aus-sport-diagnostics.log   2>&1 &
Rscript R/run-aus-sport-thresholds.R    > logs/aus-sport-thresholds.log    2>&1 &
Rscript R/run-aus-sport-every-attempt.R > logs/aus-sport-every-attempt.log 2>&1 &
Rscript R/run-aus-sport-estimate-w.R    > logs/aus-sport-estimate-w.log    2>&1 &
```

A single AUS Sport per-session run takes roughly 80-150 minutes on a 4-core
machine with 4 parallel chains; smaller datasets (NZ Trad, Germany Sport-FR)
finish in under a minute. Each run also writes per-chain Stan CSVs to
`logs/stan-<run-name>-chain_<i>.csv` so that partial draws can be inspected
while the run is in flight.

## Output naming convention

Each run writes to `results/<country>/` with a file stem of the form

```
ascents-from-<startDate>-to-<endDate>-minAscents<n>-minFails<k>-<gear>-<grade.type>[-tl][-session][-estimateW]
```

where `-tl` indicates the tickprop / lonely-redpoint logging-quality filter
was applied, `-session` indicates session-aggregated data, and `-estimateW`
indicates the joint-`w` model. The four file suffixes that follow this stem
are: `.rds` (posterior draws), `-data.rds` (preprocessed data), `-diagnostics.rds`
(convergence summary), and `-posterior.png` (figure).

## Building the manuscript

```bash
cd manuscript
pdflatex ms-jqas-r1
bibtex   ms-jqas-r1
makeglossaries ms-jqas-r1
pdflatex ms-jqas-r1
pdflatex ms-jqas-r1

pdflatex supplement-jqas
bibtex   supplement-jqas
pdflatex supplement-jqas
pdflatex supplement-jqas

pdflatex response-to-reviewers
pdflatex response-to-reviewers
```
