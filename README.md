# NNPAS 2011-12

Code for working with the **Australian Health Survey: National Nutrition and Physical Activity Survey 2011-12** Basic CURF (ABS cat. no. 4324.0.55.002).

The CURF data itself is **not** included in this repo and must be sourced from the ABS via the Microdata DataLab. Scripts here are written to be run against the standard Basic CURF file layout.

## Contents

| File | Purpose |
|---|---|
| `hot_deck_impute.R` | Hot-deck imputation of measured weight (`PHDKGWBC`) and height (`PHDCMHBC`) on the person-level Basic CURF (`npa11bp.csv`). Adds flag columns and refreshes BMI fields. |

## `hot_deck_impute.R`

Adapts the [ABS National Health Survey 2022 physical-measures imputation method](https://www.abs.gov.au/methodologies/national-health-survey-methodology/2022) to NNPAS 2011-12.

### What gets imputed

`PHDKGWBC` and `PHDCMHBC` wherever they take a missing code (`997`, `998`, `999`), **except** self-reported-pregnant women (`SABDYMS == 4`), who are left unchanged per the source method.

### Matching variables

Donors must equal the recipient on all variables in the active tier:

| Life stage | Matching variables |
|---|---|
| Adults 18+ | 5-year age band, `SEX`, `AREA2`, `SABDYMS`, `EXLEVELN` |
| Ages 15-17 | single-year `AGE`, `SEX`, `AREA2`, `SABDYMS`, `EXLEVELN` |
| Ages 2-14  | single-year `AGE`, `SEX`, `AREA2` |

`AREA2` is `ARIABC` recoded to `1` = Major cities, `2` = Inner regional + Other.

If no donor exists at the most specific tier, the script drops conditioning variables one at a time until a donor is found (final fallback: `SEX` only). The donor pool excludes pregnant women.

### Differences from the NHS 2022 method as published

- **High cholesterol** dropped as a matching variable.
- **"Part of state"** approximated via the recoded `ARIABC`.
- **Self-reported BMI category** omitted — NNPAS 2011-12 has no self-reported height/weight.
- **Trans/Gender-Diverse carve-out** not applicable — that variable was not collected in 2011-12.

### Outputs (overwritten in place on `npa11bp.csv`)

- `PHDKGWBC`, `PHDCMHBC` — replaced with donor values on imputed rows.
- `BMISC`, `BMICATHY`, `BMIMCATW` — copied from the same donor so the BMI fields stay internally consistent with the new height/weight.
- `MEASWEIG`, `MEASHEIG` — new flag columns appended:
  - `0` = Not applicable (pregnant — not imputed)
  - `1` = Measurement taken
  - `2` = Measurement imputed

### Performance

Per-tier donor lookups are precomputed via `split()` into hash-keyed indices, and writeback uses column-wise `data.table::set()`. End-to-end runtime (read + impute + write) is ~1 second on 12,153 person records.

### Reproducibility

`set.seed(20260529)` is fixed at the top of the script, so re-running picks the same donors.

### Usage

1. Edit `bp_path` if your `npa11bp.csv` is not at `C:/Users/atyeo/OneDrive/R data/NNPAS_2011_12/CURF/`.
2. Keep a backup of `npa11bp.csv` — the script overwrites in place.
3. Run:
   ```r
   source("hot_deck_impute.R")
   ```

Dependencies: `data.table`, `dplyr`.

## Licence

MIT.
