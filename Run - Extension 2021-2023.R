# =============================================================================
# Extension of the local replication to the most recent data available
# (usa.csv/usa_dsf.csv cap out at 2023-11-30/2023-12-29 - JKP's contributed
# Global Factor Data has an inherent ~2.5yr lag, this is not a limitation of
# our pull). The paper's own sample stops 2020-12-31, so this adds three
# genuinely out-of-sample years (2021-2023) the original authors never saw.
#
# SCOPE: only the methods identified as dissertation-central - Portfolio-ML,
# Static-ML/Static-ML*, and the benchmark bundle (Market, 1/N, Minimum
# Variance, Factor-ML, Rank-ML, Markowitz-ML). Multiperiod-ML/Multiperiod-ML*
# are deliberately EXCLUDED here (they were flagged as "keep but caveat" -
# our replication diverges from the paper by ~4x on that method, not a
# result to build the extension around) - this also removes the single most
# expensive step (~7h) from the portfolio-construction phase.
#
# NON-DESTRUCTIVE: this script never reads from or writes to
#   Data/Generated/Models/20260713-0221-local-yearly (paper-matched-period models)
#   Data/Generated/Portfolios/20260715-0909_percentile_yearly_grid (paper-matched-period results)
# Every output goes to its own fresh, timestamped folder. "Run - Local
# Replication.R" and its outputs are completely untouched by this script.
#
# STEP 1 (model refit) is INCREMENTAL, not a from-scratch redo. Verified
# directly against the saved model files before writing this:
#   - Each yearly fold is fit independently (data_split()'s train/val/
#     train_full windows depend only on that fold's val_end, never on other
#     folds or on test_end - confirmed by reading "0 - Return prediction
#     functions.R"). So folds 1-50 (val_end 1970-12-31 ... 2019-12-31) can be
#     reused verbatim from the existing model_*.RDS files.
#   - Fold 51 (val_end=2020-12-31) currently has ZERO test predictions - its
#     test window collapsed to empty under the old test_end (confirmed:
#     nrow(pred)==0 in the saved model). It needs recomputing (cheap - same
#     training data, just a non-empty test window this time).
#   - Folds 52-53 (val_end=2021-12-31, 2022-12-31) are genuinely new.
#   Total: 3 folds x 12 horizons x ~290 sec/fold (measured from the original
#   run's per-fold timings, which plateaued rather than kept climbing) =~ 2.9h,
#   instead of a ~28-32h full redo of all 53 folds.
#
# PREREQUISITE: the paper-matched-period model folder must already exist at
# OLD_MODEL_PATH below (used as the source for folds 1-50).
# =============================================================================

if (!file.exists("Main.R")) {
  stop("Run this from the replication project's root folder (where Main.R lives).")
}
source("Main.R")

OLD_MODEL_PATH <- "Data/Generated/Models/20260713-0221-local-yearly"
NEW_TEST_END   <- as.Date("2023-11-30")   # matches usa.csv's actual max eom
NEW_END_YR     <- 2023

run_sub <- FALSE

# =============================================================================
# STEP 1: Incremental model refit (folds 51-53 only; 1-50 reused verbatim)
# =============================================================================
model_output_path <- paste0("Data/Generated/Models/", format(Sys.time(), "%Y%m%d-%H%M"), "-local-yearly-ext2023")

if (all(file.exists(paste0(model_output_path, "/model_", 1:12, ".RDS")))) {
  cat("Reusing previously extended models from", model_output_path, "\n")
} else {
  stopifnot(all(file.exists(paste0(OLD_MODEL_PATH, "/model_", 1:12, ".RDS"))))
  dir.create(model_output_path, recursive = TRUE, showWarnings = FALSE)

  settings$split$model_update_freq <- "yearly"
  settings$rff$p_vec  <- 2^(1:9)
  settings$rff$l_vec  <- c(0, exp(seq(-10, 10, length.out = 100)))
  settings$split$test_end <- NEW_TEST_END
  settings$screens$end    <- NEW_TEST_END   # separate hardcoded field in Main.R that independently truncates chars - must be overridden too, not just test_end

  settings$screens$size_screen <- "all"
  settings$screens$nyse_stocks <- FALSE

  search_grid <- tibble(name = paste0("m", 1:12), horizon = as.list(1:12))
  file.copy("Main.R", paste0(model_output_path, "/Main.R"), overwrite = TRUE)
  settings |> saveRDS(paste0(model_output_path, "/settings.RDS"))

  tic("Prepare data (for model fitting, extended through 2023-11-30)")
  source("1 - Prepare Data.R", echo = TRUE)
  toc()

  # Full extended val_ends sequence and the (verified, see header) old subset
  val_ends     <- seq.Date(from = settings$split$train_end, to = settings$split$test_end, by = "1 year")
  old_val_ends <- seq.Date(from = settings$split$train_end, to = as.Date("2020-12-31"), by = "1 year")
  stopifnot(length(old_val_ends) == 51, length(val_ends) >= length(old_val_ends))
  redo_from_idx <- length(old_val_ends)   # recompute the last old fold (empty test window) + everything after

  tic("Incremental model refit (folds 51+ only, all 12 horizons)")
  for (i in 1:nrow(search_grid)) {
    h <- search_grid[i, ]$horizon %>% unlist()
    print(paste0("horizons: ", list(h)))

    old_op <- readRDS(paste0(OLD_MODEL_PATH, "/model_", h, ".RDS"))
    stopifnot(length(old_op) == length(old_val_ends))

    pred_y <- data_ret[, paste0("ret_ld", h), with = F] %>% rowMeans()
    pred_y <- data_ret[, .(id, eom, eom_pred_last = eom + 1 + months(max(h)) - 1, ret_pred = pred_y)]
    data_pred <- pred_y[chars[valid == T], on = .(id, eom)]

    val_ends_to_compute <- val_ends[redo_from_idx:length(val_ends)]
    new_folds <- val_ends_to_compute %>% sapply(simplify = F, USE.NAMES = F, function(val_end) {
      print(val_end)
      train_test_val <- data_pred %>% data_split(
        type = settings$split$model_update_freq, val_end = val_end, val_years = settings$split$val_years,
        train_start = settings$screens$start, train_lookback = settings$split$train_lookback,
        retrain_lookback = settings$split$retrain_lookback, test_inc = 1, test_end = settings$split$test_end
      )
      print(system.time(model_op <- train_test_val %>% rff_hp_search(
        feat = features, p_vec = settings$rff$p_vec, g_vec = settings$rff$g_vec,
        l_vec = settings$rff$l, seed = settings$seed_no
      )))
      return(model_op)
    })

    op <- c(old_op[1:(redo_from_idx - 1)], new_folds)
    stopifnot(length(op) == length(val_ends))
    op %>% saveRDS(paste0(model_output_path, "/model_", h, ".RDS"))
  }
  toc()
}

# =============================================================================
# STEP 2: Build portfolios - benchmarks + Static-ML* + Portfolio-ML only
# (Multiperiod-ML deliberately excluded - see header)
# =============================================================================
get_from_path_model <- model_output_path
config_params <- list(
  size_screen   = "perc_low50_high100_min50",
  wealth        = 1e10,
  gamma_rel     = 10,
  industry_cov  = TRUE,
  update_mp     = FALSE,   # <- the scope decision for this extension
  update_base   = TRUE,
  update_fi_base = FALSE,
  update_fi_ief  = FALSE,
  update_fi_ret  = FALSE
)
settings$screens$size_screen <- config_params$size_screen
settings$screens$nyse_stocks <- TRUE
settings$cov_set$industries  <- config_params$industry_cov
settings$split$test_end      <- NEW_TEST_END
settings$screens$end         <- NEW_TEST_END   # separate hardcoded field in Main.R - must be overridden too
settings$pf$dates$end_yr     <- NEW_END_YR
pf_set$wealth     <- config_params$wealth
pf_set$gamma_rel  <- config_params$gamma_rel

settings$pf_ml$p_vec <- 2^(6:9)
settings$pf_ml$l_vec <- c(0, exp(seq(-10, 10, length.out = 100)))

pf_output_path <- paste0("Data/Generated/Portfolios/", format(Sys.time(), "%Y%m%d-%H%M"), "_ext2023")
dir.create(pf_output_path, recursive = TRUE, showWarnings = FALSE)
output_path <- pf_output_path
file.copy("Main.R", paste0(output_path, "/Main.R"), overwrite = TRUE)
settings |> saveRDS(paste0(output_path, "/settings.RDS"))
pf_set |> saveRDS(paste0(output_path, "/pf_set.RDS"))

tic("Prepare data (for portfolio construction, extended through 2023-11-30)")
source("1 - Prepare Data.R", echo = TRUE)
toc()

n_by_month <- chars[valid == TRUE, .N, by = eom]
cat("\n--- Realized monthly universe (perc_low50_high100_min50), extended sample ---\n")
print(summary(n_by_month$N))

tic("Estimate Barra-style covariance matrix")
source("3 - Estimate Covariance Matrix.R", echo = TRUE)
toc()

tic("Prepare portfolio data (merge model predictions)")
source("4 - Prepare Portfolio Data.R", echo = TRUE)
toc()

tic("Build portfolios: benchmarks + Static-ML* + Portfolio-ML (no Multiperiod-ML)")
source("5 - Base case.R", echo = TRUE)
toc()

# =============================================================================
# STEP 3: Summarize - full extended sample (1981-2023) AND the 2021-2023
# out-of-sample slice in isolation (the genuinely new evidence)
# =============================================================================
static_raw <- static$hps[eom_ret %in% static$pf$eom_ret & k == 1 & g == 0 & u == 1,
                          .(eom_ret = as.Date(eom_ret), inv, shorting, turnover, r, tc)][, type := "Static-ML"]
pfs <- rbindlist(list(pfml$pf, static$pf, static_raw, bm_pfs), use.names = TRUE, fill = TRUE)
pfs[, type := factor(type, levels = pf_order)]
pfs |> setorder(type, eom_ret)

summarize_pfs <- function(data) {
  data[, e_var_adj := (r - mean(r, na.rm = TRUE))^2, by = type]
  data[, .(
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
}

pf_summary_full <- summarize_pfs(copy(pfs))
cat("\n--- Full extended sample (1981-2023) ---\n")
print(pf_summary_full)
fwrite(pf_summary_full, paste0(pf_output_path, "/pf_summary_full_1981_2023.csv"))

pf_summary_2021_2023 <- summarize_pfs(copy(pfs)[eom_ret >= as.Date("2021-01-01")])
cat("\n--- 2021-2023 slice only (genuine out-of-sample vs. the paper) ---\n")
print(pf_summary_2021_2023)
fwrite(pf_summary_2021_2023, paste0(pf_output_path, "/pf_summary_2021_2023_only.csv"))

cat("\n\nDone. Full-sample summary:", paste0(pf_output_path, "/pf_summary_full_1981_2023.csv"), "\n")
cat("2021-2023-only summary:", paste0(pf_output_path, "/pf_summary_2021_2023_only.csv"), "\n")
