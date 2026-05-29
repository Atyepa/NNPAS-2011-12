# Hot-deck imputation of measured weight (PHDKGWBC) and measured height (PHDCMHBC)
# in NNPAS 2011-12 CURF file npa11bp.csv. Adapted from the ABS NHS 2022
# physical-measures imputation method (see Hot deck imputation.docx).
#
# What is imputed:
#   PHDKGWBC and PHDCMHBC wherever they take a missing code (997, 998, 999),
#   except for self-reported-pregnant women (SABDYMS == 4) who are left as-is.
#
# Matching variables (donor must equal recipient on all):
#   Adults 18+:    5-year AGE band, SEX, AREA2 (= recoded ARIABC), SABDYMS, EXLEVELN
#   Ages 15-17:    AGE (single year), SEX, AREA2, SABDYMS, EXLEVELN
#   Ages 2-14:     AGE (single year), SEX, AREA2
# Fallback: drop matching vars one at a time (right-to-left) until a donor is
# available. Final fallback is SEX-only.
#
# Notes vs the ABS NHS 2022 method as written in the source brief:
#   - High cholesterol is excluded as a matching variable (user direction).
#   - "Part of state" is implemented via ARIABC recoded to 1 = Major cities,
#     2 = Inner regional + Other (user direction).
#   - "Self-reported BMI category" matching var is not available on this CURF
#     (no self-reported height/weight in NNPAS 2011-12) and is omitted.
#   - Trans/Gender Diverse carve-out has no corresponding 2011-12 variable.
#
# Outputs:
#   - PHDKGWBC, PHDCMHBC overwritten where imputed
#   - BMISC, BMICATHY, BMIMCATW overwritten (copied from the donor record so they
#     are internally consistent with the imputed height/weight)
#   - New flag columns MEASWEIG, MEASHEIG (0 = N/A, 1 = measurement taken,
#     2 = measurement imputed). Appended at the end of the data frame.
#   - npa11bp.csv is overwritten in place.

suppressPackageStartupMessages({
  library(data.table)
})

library(dplyr)
set.seed(20260529)

bp_path <- "Your path/npa11bp.csv"

dat <- fread(bp_path, na.strings = c(""))
orig_cols <- copy(names(dat))
n <- nrow(dat)

names<- names(dat) %>% as_tibble()

stopifnot(all(c("PHDKGWBC", "PHDCMHBC", "BMISC", "BMICATHY", "BMIMCATW",
                "AGE", "SEX", "ARIABC", "SABDYMS", "EXLEVELN") %in% names(dat)))

missing_codes <- c(997, 998, 999)

# Recoded geography: 1 = Major cities, 2 = All other regions (Inner regional + Other)
dat[, AREA2 := fifelse(ARIABC == 1L, 1L, 2L)]

# 5-year adult age bands (capped at 75+)
dat[, AGEGRP5 := fcase(
  AGE >= 18 & AGE <= 24, 18L,
  AGE >= 25 & AGE <= 29, 25L,
  AGE >= 30 & AGE <= 34, 30L,
  AGE >= 35 & AGE <= 39, 35L,
  AGE >= 40 & AGE <= 44, 40L,
  AGE >= 45 & AGE <= 49, 45L,
  AGE >= 50 & AGE <= 54, 50L,
  AGE >= 55 & AGE <= 59, 55L,
  AGE >= 60 & AGE <= 64, 60L,
  AGE >= 65 & AGE <= 69, 65L,
  AGE >= 70 & AGE <= 74, 70L,
  AGE >= 75,             75L,
  default = NA_integer_
)]

dat[, w_miss := PHDKGWBC %in% missing_codes]
dat[, h_miss := PHDCMHBC %in% missing_codes]
dat[, pregnant := SABDYMS == 4L]
dat[, is_donor := !w_miss & !h_miss & !pregnant]
dat[, is_recip := (w_miss | h_miss) & !pregnant]

donors <- dat[is_donor == TRUE,
              .(donor_row = .I, AGE, SEX, AREA2, SABDYMS, EXLEVELN, AGEGRP5,
                PHDKGWBC, PHDCMHBC, BMISC, BMICATHY, BMIMCATW)]
donors[, donor_row := .I]

# Auxiliary coarsened keys for fallback
dat[, AGEGRP5_TEEN := fcase(AGE %in% 15:17, 15L, default = NA_integer_)]
dat[, AGEGRP3_CHILD := AGE %/% 3L * 3L]
donors[, AGEGRP5_TEEN := fcase(AGE %in% 15:17, 15L, default = NA_integer_)]
donors[, AGEGRP3_CHILD := AGE %/% 3L * 3L]

# Tier lists (highest-to-lowest specificity) by life stage
adult_tiers <- list(
  c("AGEGRP5", "SEX", "AREA2", "SABDYMS", "EXLEVELN"),
  c("AGEGRP5", "SEX", "AREA2", "EXLEVELN"),
  c("AGEGRP5", "SEX", "AREA2", "SABDYMS"),
  c("AGEGRP5", "SEX", "AREA2"),
  c("AGEGRP5", "SEX"),
  c("SEX")
)
teen_tiers <- list(
  c("AGE", "SEX", "AREA2", "SABDYMS", "EXLEVELN"),
  c("AGE", "SEX", "AREA2", "EXLEVELN"),
  c("AGE", "SEX", "AREA2", "SABDYMS"),
  c("AGE", "SEX", "AREA2"),
  c("AGE", "SEX"),
  c("AGEGRP5_TEEN", "SEX"),
  c("SEX")
)
child_tiers <- list(
  c("AGE", "SEX", "AREA2"),
  c("AGE", "SEX"),
  c("AGEGRP3_CHILD", "SEX"),
  c("SEX")
)

pick_tier_list <- function(age) {
  if (is.na(age))         return(adult_tiers)
  if (age >= 18)          return(adult_tiers)
  if (age >= 15)          return(teen_tiers)
  return(child_tiers)
}

# Build per-tier hash lookups: key = "v1|v2|..." -> integer vector of donor rows.
# Each unique tier (variable combination) gets indexed exactly once, so recipient
# matching is then a single named-list hit instead of repeatedly filtering donors.
make_key <- function(dt, vars) {
  do.call(paste, c(lapply(vars, function(v) as.character(dt[[v]])), sep = "|"))
}

all_tiers <- unique(c(adult_tiers, teen_tiers, child_tiers))
tier_index <- vector("list", length(all_tiers))
names(tier_index) <- vapply(all_tiers, paste, "", collapse = ",")
for (k in seq_along(all_tiers)) {
  vars <- all_tiers[[k]]
  keys <- make_key(donors, vars)
  tier_index[[k]] <- split(seq_len(nrow(donors)), keys)
}
tier_key <- function(vars) paste(vars, collapse = ",")

# Imputation loop — O(1) lookup per (recipient, tier) attempt
recip_idx <- which(dat$is_recip)
donor_for <- integer(n)
tier_used <- integer(n)
unmatched <- integer(0)

# Cache recipient keys per tier (vectorised) so the inner loop only does
# named-list lookup + a sample.
recip_keys <- vector("list", length(all_tiers))
for (k in seq_along(all_tiers)) {
  recip_keys[[k]] <- make_key(dat[recip_idx], all_tiers[[k]])
}

for (ri in seq_along(recip_idx)) {
  i <- recip_idx[ri]
  tiers <- pick_tier_list(dat$AGE[i])
  matched <- FALSE
  for (t in seq_along(tiers)) {
    vars <- tiers[[t]]
    k <- match(tier_key(vars), names(tier_index))
    key <- recip_keys[[k]][ri]
    if (grepl("NA", key, fixed = TRUE)) next   # skip if recipient missing any var
    pool <- tier_index[[k]][[key]]
    if (!is.null(pool) && length(pool) > 0L) {
      donor_for[i] <- if (length(pool) == 1L) pool else sample(pool, 1L)
      tier_used[i] <- t
      matched <- TRUE
      break
    }
  }
  if (!matched) unmatched <- c(unmatched, i)
}

# Apply imputation using set() for fast in-place assignment.
dat[, MEASWEIG := fifelse(w_miss, 0L, 1L)]
dat[, MEASHEIG := fifelse(h_miss, 0L, 1L)]

did_impute <- donor_for > 0L
imp_rows <- which(did_impute)
donor_rows <- donor_for[imp_rows]

# Vectorised donor pulls (no per-row data.table filter).
d_kg  <- donors$PHDKGWBC[donor_rows]
d_cm  <- donors$PHDCMHBC[donor_rows]
d_bmi <- donors$BMISC[donor_rows]
d_bhy <- donors$BMICATHY[donor_rows]
d_bmw <- donors$BMIMCATW[donor_rows]

w_apply <- imp_rows[dat$w_miss[imp_rows]]
h_apply <- imp_rows[dat$h_miss[imp_rows]]
set(dat, w_apply, "PHDKGWBC", donors$PHDKGWBC[donor_for[w_apply]])
set(dat, w_apply, "MEASWEIG", 2L)
set(dat, h_apply, "PHDCMHBC", donors$PHDCMHBC[donor_for[h_apply]])
set(dat, h_apply, "MEASHEIG", 2L)
set(dat, imp_rows, "BMISC",    d_bmi)
set(dat, imp_rows, "BMICATHY", d_bhy)
set(dat, imp_rows, "BMIMCATW", d_bmw)

# For pregnant women: explicitly mark flag = 0 (not applicable)
dat[pregnant == TRUE, MEASWEIG := 0L]
dat[pregnant == TRUE, MEASHEIG := 0L]

# Report
cat("Total persons:        ", n, "\n")
cat("Recipients (weight):  ", sum(dat$w_miss & !dat$pregnant), "\n")
cat("Recipients (height):  ", sum(dat$h_miss & !dat$pregnant), "\n")
cat("Pregnant (skipped):   ", sum(dat$pregnant), "\n")
cat("Donor pool size:      ", nrow(donors), "\n")
cat("Imputed records:      ", length(imp_rows), "\n")
cat("Unmatched (none):     ", length(unmatched), "\n")
cat("Tier usage:\n"); print(table(tier_used[did_impute]))
cat("MEASWEIG distribution:\n"); print(table(dat$MEASWEIG, useNA = "ifany"))
cat("MEASHEIG distribution:\n"); print(table(dat$MEASHEIG, useNA = "ifany"))

# Drop helper columns and write back, with new flags at the end
helper <- c("AREA2", "AGEGRP5", "AGEGRP5_TEEN", "AGEGRP3_CHILD",
            "w_miss", "h_miss", "pregnant", "is_donor", "is_recip")
dat[, (helper) := NULL]

# Keep original column order, then append MEASWEIG / MEASHEIG
final_order <- c(orig_cols, "MEASWEIG", "MEASHEIG")
setcolorder(dat, final_order)

fwrite(dat, bp_path)
cat("Wrote:", bp_path, "\n")
