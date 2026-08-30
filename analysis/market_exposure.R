# Market exposure of the cost-aware strategies, for Section 5.2 and the Table 6 note.
#
# Table 6 reports PARTIAL coefficients from the six-factor regression, which is the
# correct thing to report alongside the alpha in Table 5, since the two come from the
# same equation. The partial market coefficient is not, however, the strategy's
# directional market exposure. Value, profitability, investment and momentum are each
# short the market, so conditioning on them raises the market coefficient. This script
# produces the unconditional figures quoted in the text and shows that the two
# reconcile exactly.
#
# Figures produced:
#   Section 5.2  - Portfolio-ML market beta 0.08, correlation 0.11, t-stat 2.56,
#                  1.3% of return variance, 0.66 of 13.93 percentage points of return
#   Table 6 note - unconditional market beta 0.08 (Portfolio-ML), 0.17 (Static-ML*)
#
# Uses the cached factor vintage in Data/ff5_mom_monthly.csv. See alpha_regression.R.
suppressMessages({library(data.table); library(lubridate)})

# Resolve the project root, so this runs from the root or from analysis/.
BASE <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
EXT  <- file.path(BASE, "Data/Generated/Portfolios/20260718-1828_ext2023")
LO   <- as.Date("1981-01-01"); HI <- as.Date("2023-11-30")

ff <- fread(file.path(BASE, "Data/ff5_mom_monthly.csv")); ff[, eom := as.Date(eom)]
pfml <- as.data.table(readRDS(file.path(EXT, "portfolio-ml.RDS"))$pf)
sml  <- as.data.table(readRDS(file.path(EXT, "static-ml.RDS"))$pf)

strategies <- list(
  "Portfolio-ML" = pfml[, .(eom = eom_ret, y = r - tc)],
  "Static-ML*"   = sml [, .(eom = eom_ret, y = r - tc)]
)
OTHERS <- c("smb", "hml", "rmw", "cma", "mom")
SIX    <- c("mktrf", OTHERS)

# ------------------------------------------------------- unconditional exposure
cat("=== market exposure measured on its own, 1981-2023 ===\n")
for (nm in names(strategies)) {
  d <- merge(strategies[[nm]], ff, by = "eom")[eom >= LO & eom <= HI]
  capm <- lm(y ~ mktrf, data = d)
  b <- coef(capm)[["mktrf"]]; r2 <- summary(capm)$r.squared
  mu <- mean(d$y)*12*100; prem <- mean(d$mktrf)*12*100
  cat(sprintf("\n%s  (n = %d)\n", nm, nrow(d)))
  cat(sprintf("  net excess return                  %6.2f%% p.a.\n", mu))
  cat(sprintf("  volatility                         %6.2f%% p.a.\n", sd(d$y)*sqrt(12)*100))
  cat(sprintf("  correlation with the market factor %6.3f\n", cor(d$y, d$mktrf)))
  cat(sprintf("  CAPM beta                          %6.3f  (t = %.2f)\n",
              b, summary(capm)$coefficients["mktrf", 3]))
  cat(sprintf("  share of return variance           %6.1f%%\n", r2*100))
  cat(sprintf("  return from market exposure        %6.2f%% p.a. of %.2f%%\n", b*prem, mu))
}

# -------------------------------------- why the partial coefficient differs
cat("\n\n=== reconciling the partial and unconditional market betas ===\n")
for (nm in names(strategies)) {
  d <- merge(strategies[[nm]], ff, by = "eom")[eom >= LO & eom <= HI]
  six <- lm(reformulate(SIX, "y"), data = d)
  L <- coef(six)[OTHERS]
  bmkt <- sapply(OTHERS, function(f) coef(lm(reformulate("mktrf", f), data = d))[["mktrf"]])
  cat(sprintf("\n%s\n", nm))
  cat(sprintf("  partial market coefficient                %+.3f\n", coef(six)[["mktrf"]]))
  for (f in OTHERS)
    cat(sprintf("    loading %+.3f on %-4s, whose own market beta is %+.3f  ->  %+.3f\n",
                L[[f]], toupper(f), bmkt[[f]], L[[f]]*bmkt[[f]]))
  cat(sprintf("  implied unconditional beta                %+.3f\n",
              coef(six)[["mktrf"]] + sum(L*bmkt)))
  cat(sprintf("  actual CAPM beta                          %+.3f\n",
              coef(lm(y ~ mktrf, data = d))[["mktrf"]]))
}

# ------------------------------------------------ what the partial coefficient means
# Frisch-Waugh-Lovell: it is the sensitivity to the component of the market factor
# that the other five factors do not explain.
cat("\n\n=== Frisch-Waugh-Lovell check, Portfolio-ML ===\n")
d <- merge(strategies[["Portfolio-ML"]], ff, by = "eom")[eom >= LO & eom <= HI]
m_orth <- residuals(lm(reformulate(OTHERS, "mktrf"), data = d))
cat(sprintf("  six-factor market loading                  %.4f\n",
            coef(lm(reformulate(SIX, "y"), data = d))[["mktrf"]]))
cat(sprintf("  regression on the orthogonalised market    %.4f\n",
            coef(lm(d$y ~ m_orth))[[2]]))
cat(sprintf("  share of market variance orthogonal to the other five: %.1f%%\n",
            var(m_orth)/var(d$mktrf)*100))

# ------------------------------------- only partial coefficients decompose the return
cat("\n\n=== return attribution, Portfolio-ML ===\n")
prem <- sapply(SIX, function(f) mean(d[[f]])*12*100)
six  <- lm(reformulate(SIX, "y"), data = d)
Lp   <- coef(six)[SIX]
Lu   <- sapply(SIX, function(f) coef(lm(reformulate(f, "y"), data = d))[[2]])
cat(sprintf("%-6s %9s %10s %13s %11s %13s\n",
            "factor", "premium", "partial b", "contribution", "univar. b", "contribution"))
for (f in SIX)
  cat(sprintf("%-6s %8.2f%% %10.3f %12.2f%% %11.3f %12.2f%%\n",
              toupper(f), prem[[f]], Lp[[f]], Lp[[f]]*prem[[f]], Lu[[f]], Lu[[f]]*prem[[f]]))
cat(sprintf("\n  partial contributions %.2f%% + alpha %.2f%% = %.2f%%, actual %.2f%%\n",
            sum(Lp*prem), coef(six)[[1]]*12*100,
            sum(Lp*prem) + coef(six)[[1]]*12*100, mean(d$y)*12*100))
cat(sprintf("  univariate contributions sum to %.2f%%, which pairs with no single alpha\n",
            sum(Lu*prem)))
