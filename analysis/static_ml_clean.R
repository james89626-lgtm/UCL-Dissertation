suppressMessages(library(data.table))

# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")

main_dir <- file.path(base, "Data/Generated/Portfolios/20260715-0909_percentile_yearly_grid")

sml   <- readRDS(file.path(main_dir, "static-ml.RDS"))
hps   <- as.data.table(sml$hps)
pf_st <- as.data.table(sml$pf)      # Static-ML* (correct OOS window reference)

cat("=== Reference: Static-ML* OOS window ===\n")
cat("n =", nrow(pf_st), " range:", format(min(pf_st$eom_ret)), "to", format(max(pf_st$eom_ret)), "\n\n")

cat("=== hps object: full date range and hp grid ===\n")
hps[, eom_ret := as.Date(eom_ret)]
cat("hps rows:", nrow(hps), " date range:", format(min(hps$eom_ret)), "to", format(max(hps$eom_ret)), "\n")
cat("unique (k,g,u) combos:", nrow(unique(hps[, .(k,g,u)])), "\n")
cat("unique k:", paste(sort(unique(hps$k)), collapse=", "), "\n")
cat("unique g:", paste(sort(unique(hps$g)), collapse=", "), "\n")
cat("unique u:", paste(sort(unique(hps$u)), collapse=", "), "\n\n")

# Plain Static-ML = no shrinkage/no scaling variant: k=1, g=0, u=1
raw <- hps[k == 1 & g == 0 & u == 1][order(eom_ret)]
cat("=== Plain Static-ML (k=1, g=0, u=1), UNRESTRICTED ===\n")
cat("n =", nrow(raw), " range:", format(min(raw$eom_ret)), "to", format(max(raw$eom_ret)), "\n")

# Restrict to exactly the same OOS months as Static-ML*
oos <- pf_st$eom_ret
raw_oos <- raw[eom_ret %in% oos][order(eom_ret)]
cat("\n=== Plain Static-ML, RESTRICTED to Static-ML*'s OOS months ===\n")
cat("n =", nrow(raw_oos), " range:", format(min(raw_oos$eom_ret)), "to", format(max(raw_oos$eom_ret)), "\n\n")

summ <- function(dt, label) {
  r <- dt$r; tc <- dt$tc
  cat(sprintf("%-28s n=%3d  SRg=%6.3f  SRn=%8.3f  TC=%7.4f  Turn=%6.3f  Lev=%6.3f  Util=%8.4f\n",
      label, nrow(dt),
      mean(r)/sd(r)*sqrt(12),
      mean(r-tc)/sd(r)*sqrt(12),
      mean(tc)*12,
      mean(dt$turnover),
      mean(dt$inv),
      (mean(r) - 0.5*var(r)*10 - mean(tc))*12))
}

cat("=== COMPARISON ===\n")
summ(raw,     "Static-ML UNRESTRICTED")
summ(raw_oos, "Static-ML restricted (OOS)")
summ(pf_st,   "Static-ML* (reference)")
