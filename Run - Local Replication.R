# =============================================================================
# Local, laptop-scale replication of Jensen, Kelly, Malamud & Pedersen (2024),
# "Machine Learning and the Implementable Efficient Frontier"
#
# The authors' original pipeline needs a SLURM HPC cluster (40-150GB RAM per
# job, dozens of jobs, days of wall time). This script runs the SAME
# functions/methodology from the "0 - *.R" files (essentially unmodified - see
# below) on a personal machine, using the paper's ACTUAL base-case settings:
# yearly return-model refits, the full 9-value/512-feature RFF grid, the
# paper's own NYSE/above-median-cap percentile screen, and the full stock
# pool (no subsampling). This is the result of five rounds of iteration -
# earlier rounds tried a random stock subsample, a decade-refit/6-value grid
# (to cut cost), and a "topN by market cap" screen as approximations; each was
# progressively reverted once measurement showed the full/paper-accurate
# version was affordable on a laptop after all. What's below is the settings
# block that produced the final, definitive Table 2 comparison.
#
# Code changes vs. the authors' unmodified files (see the four "0 -"/"1 -"/
# "3 -" files for details in-line):
#   - Three defensive fixes (diag() length-1 gotcha in "0 - General
#     functions.R"; hp-lookup fallback in "0 - Portfolio choice functions.R";
#     seed typo in "1 - Prepare Data.R") that were needed during earlier,
#     smaller-scale experimentation. Verified empirically against this run's
#     own output: none of the three is actually exercised at the paper's real
#     scale (0/480 OOS dates hit the hp fallback; realized universe never
#     drops below 163 stocks, so the diag() branch never fires either). Left
#     in as harmless, provably-equivalent-at-n>1 insurance, since a couple of
#     the still-pending optional analyses (e.g. performance-by-size-bucket)
#     could plausibly re-trigger them.
#   - One substantive, still-active fix: "3 - Estimate Covariance Matrix.R"
#     drops any covariance factor with an NA-producing regression column on a
#     per-calc-date basis, rather than assuming all 25 FF12+cluster factors
#     are always estimable. Needed because the "Utils" FF12 industry has zero
#     NYSE-listed stocks from 1954-12-01 to 1959-05-29 (genuine data
#     thinness, confirmed against real listing history - not a bug in our
#     WRDS pull). Affects 56 of 674 calc_dates, all before 1969-07; 100% of
#     the analysis period (1971+ hp validation, 1981+ OOS) is unaffected.
#
# Data-pipeline files "0 - Download WRDS Data.R" and "0 - Derive World and
# Market Returns.R" are new (the original repo assumes these are already on
# disk, sourced from the authors' separate SAS/Dropbox pipeline). Ours pulls
# usa.csv/usa_dsf.csv from the exact same WRDS tables the paper cites, and
# derives world_ret_monthly.csv/market_returns.csv from usa.csv's ret_ld1
# (an exact reconstruction, not an approximation - verified no non-CRSP ids
# leak into the market-return value-weighting).
#
# MEASURED COST (this machine): ~25.5 hours for Step 1 (yearly refits, full
# grid, all 12 horizons - the expensive one, but reusable across every
# portfolio-construction run via `reuse_models_from` below) + ~11 hours for
# Step 2 (data prep + covariance + Static-ML + Portfolio-ML + Multiperiod-ML,
# all at the paper's full grids). Budget ~36 hours end to end from a cold
# start; effectively free to re-run Step 2 alone once Step 1's models exist.
#
# SCOPE: reproduces Table 2 - Portfolio-ML, Static-ML*/Static-ML, Multiperiod-
# ML*/Multiperiod-ML, and the classic benchmarks (Market, 1/N, Min-Var,
# Factor-ML, Rank-ML, Markowitz-ML). Does NOT (yet) reproduce the
# implementable-efficient-frontier sweep, feature importance, performance-by-
# size, short-selling fees, the RF illustrative example, or the Appendix E
# simulations - each needs its own multi-hour run(s) on top of this one.
#
# PREREQUISITES (run once, in order):
#   1. "0 - Download WRDS Data.R"              -> Data/usa.csv, Data/usa_dsf.csv
#   2. "0 - Derive World and Market Returns.R"  -> Data/world_ret_monthly.csv, Data/market_returns.csv
# =============================================================================

if (!file.exists("Main.R")) {
  stop("Run this from the replication project's root folder (where Main.R lives) - ",
       "in RStudio, open the .Rproj file first; from a terminal, cd into this folder before Rscript.")
}
source("Main.R")

run_sub <- FALSE   # Use the full stock pool - the paper's percentile screen bounds monthly cost, no subsampling needed

# If a previous run already fit the 12 return models at these settings
# (Data/Generated/Models/<timestamp> containing model_1.RDS ... model_12.RDS),
# point this at that folder to skip the ~25.5-hour refit. Set to NULL to
# always refit from scratch.
reuse_models_from <- "Data/Generated/Models/20260713-0221-local-yearly"

# =============================================================================
# STEP 1: Fit expected-return models (all 12 horizons, sequentially)
# Paper's actual settings: yearly model updates, full 9-value RFF grid
# (up to 512 features), full 100-point ridge penalty grid.
# =============================================================================
if (!is.null(reuse_models_from) && all(file.exists(paste0(reuse_models_from, "/model_", 1:12, ".RDS")))) {
  cat("Reusing previously fit models from", reuse_models_from, "\n")
  model_output_path <- reuse_models_from
} else {
  settings$split$model_update_freq <- "yearly"
  settings$rff$p_vec  <- 2^(1:9)
  settings$rff$l_vec  <- c(0, exp(seq(-10, 10, length.out = 100)))

  settings$screens$size_screen <- "all"
  settings$screens$nyse_stocks <- FALSE

  search_grid <- tibble(name = paste0("m", 1:12), horizon = as.list(1:12))

  model_output_path <- paste0("Data/Generated/Models/", format(Sys.time(), "%Y%m%d-%H%M"), "-local")
  dir.create(model_output_path, recursive = TRUE, showWarnings = FALSE)
  output_path <- model_output_path
  file.copy("Main.R", paste0(output_path, "/Main.R"), overwrite = TRUE)
  settings |> saveRDS(paste0(output_path, "/settings.RDS"))

  tic("Prepare data (for model fitting)")
  source("1 - Prepare Data.R", echo = TRUE)
  toc()

  tic("Fit expected-return models (all horizons, yearly, full grid)")
  source("2 - Fit Models.R", echo = TRUE)
  toc()
}

# =============================================================================
# STEP 2: Build portfolios (Portfolio-ML, Static-ML, Multiperiod-ML, benchmarks)
# Paper's actual settings: perc_low50_high100_min50 screen (NYSE, above-median
# cap), full Portfolio-ML grid, industry-factor covariance.
# =============================================================================
get_from_path_model <- model_output_path
config_params <- list(
  size_screen   = "perc_low50_high100_min50",  # the paper's actual base-case screen
  wealth        = 1e10,
  gamma_rel     = 10,
  industry_cov  = TRUE,   # per-calc-date Utils handling in "3 - Estimate Covariance Matrix.R" makes this safe across the full 1954-2020 sample
  update_mp     = TRUE,   # Multiperiod-ML/Multiperiod-ML* - the most expensive method (~7h at this scale), but "5 - Base case.R" builds it in the same pass as everything else
  update_base   = TRUE,
  update_fi_base = FALSE,
  update_fi_ief  = FALSE,
  update_fi_ret  = FALSE
)
settings$screens$size_screen <- config_params$size_screen
settings$screens$nyse_stocks <- TRUE
settings$cov_set$industries  <- config_params$industry_cov
pf_set$wealth     <- config_params$wealth
pf_set$gamma_rel  <- config_params$gamma_rel

# Portfolio-ML's own grid at the paper's full values
settings$pf_ml$p_vec <- 2^(6:9)
settings$pf_ml$l_vec <- c(0, exp(seq(-10, 10, length.out = 100)))

pf_output_path <- paste0("Data/Generated/Portfolios/", format(Sys.time(), "%Y%m%d-%H%M"), "_local_final")
dir.create(pf_output_path, recursive = TRUE, showWarnings = FALSE)
output_path <- pf_output_path
file.copy("Main.R", paste0(output_path, "/Main.R"), overwrite = TRUE)
settings |> saveRDS(paste0(output_path, "/settings.RDS"))
pf_set |> saveRDS(paste0(output_path, "/pf_set.RDS"))

tic("Prepare data (for portfolio construction)")
source("1 - Prepare Data.R", echo = TRUE)
toc()

# Report realized monthly universe size for comparison against the paper's own
# reported min/median/max (184 / 646 / 805)
n_by_month <- chars[valid == TRUE, .N, by = eom]
cat("\n--- Realized monthly universe (perc_low50_high100_min50) ---\n")
print(summary(n_by_month$N))
cat("Paper's own reported figures: min 184, median 646, max 805\n\n")

tic("Estimate Barra-style covariance matrix")
source("3 - Estimate Covariance Matrix.R", echo = TRUE)
toc()

tic("Prepare portfolio data (merge model predictions)")
source("4 - Prepare Portfolio Data.R", echo = TRUE)
toc()

tic("Build portfolios: benchmarks + Static-ML* + Portfolio-ML + Multiperiod-ML*")
source("5 - Base case.R", echo = TRUE)
toc()

# =============================================================================
# STEP 3: Summarize performance (condensed version of "6 - Base analysis.R",
# skipping the pieces that depend on the not-yet-generated IEF/feature-
# importance/size-distribution runs). For the full official tables/figures,
# use "6 - Base analysis.R" / "7 - Tables.R" / "7 - Figures.R" instead.
# =============================================================================
# Static-ML and Multiperiod-ML ("one tuning layer") are the fixed-hp
# (k=1, g=0, u=1) slice of each method's own hp-search grid - same idiom the
# authors use in "6 - Base analysis.R" to derive them from the *-starred
# ("two tuning layer", fully hp-validated) result.
static_raw <- static$hps[eom_ret %in% static$pf$eom_ret & k == 1 & g == 0 & u == 1,
                          .(eom_ret = as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Static-ML"]
pf_list <- list(pfml$pf, static$pf, static_raw, bm_pfs)
if (config_params$update_mp) {
  mp_raw <- mp$hps[eom_ret %in% mp$pf$eom_ret & k == 1 & g == 0 & u == 1,
                    .(eom_ret = as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Multiperiod-ML"]
  pf_list <- c(pf_list, list(mp$pf, mp_raw))
}
pfs <- rbindlist(pf_list, use.names = TRUE, fill = TRUE)
pfs[, type := factor(type, levels = pf_order)]
pfs |> setorder(type, eom_ret)
pfs[, e_var_adj := (r - mean(r, na.rm = TRUE))^2, by = type]
pfs[, utility_t := r - tc - 0.5 * e_var_adj * pf_set$gamma_rel]

pf_summary <- pfs[, .(
  n = sum(!is.na(r)),
  inv = mean(inv, na.rm = TRUE),
  shorting = mean(shorting, na.rm = TRUE),
  turnover_notional = mean(turnover, na.rm = TRUE),
  r = mean(r, na.rm = TRUE) * 12,
  sd = sd(r, na.rm = TRUE) * sqrt(12),
  sr_gross = mean(r, na.rm = TRUE) / sd(r, na.rm = TRUE) * sqrt(12),
  tc = mean(tc, na.rm = TRUE) * 12,
  r_tc = mean(r - tc, na.rm = TRUE) * 12,
  sr = mean(r - tc, na.rm = TRUE) / sd(r, na.rm = TRUE) * sqrt(12),
  obj = (mean(r, na.rm = TRUE) - 0.5 * var(r, na.rm = TRUE) * pf_set$gamma_rel - mean(tc, na.rm = TRUE)) * 12
), by = type][order(type)]

print(pf_summary)
fwrite(pf_summary, paste0(pf_output_path, "/pf_summary.csv"))

# Cumulative gross-return plot
pfs[, cumret := cumsum(r), by = type]
p <- pfs |> ggplot(aes(eom_ret, cumret, colour = type)) +
  geom_line() +
  labs(y = "Cumulative gross return", x = NULL, colour = "Method:")
dir.create("Figures", showWarnings = FALSE)
ggsave(paste0("Figures/local_cumret_", format(Sys.time(), "%Y%m%d-%H%M"), ".pdf"), p, width = fig_w, height = fig_h, units = "in")

cat("\n\nDone. Summary table saved to '", pf_output_path, "/pf_summary.csv'.\n", sep = "")
cat("Cumulative return plot saved to Figures/.\n")
