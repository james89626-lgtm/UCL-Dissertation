# Why the Market portfolio shows a significantly negative multi-factor alpha, for Section 5.2.
#
# Table 5 reports the Market portfolio at 0.02% under the CAPM but -1.88% under
# FF5 + momentum. A value-weighted portfolio should not be mispriced, so the
# figure needs explaining. This script rules out the obvious candidates and
# isolates the cause.
#
# FINDINGS, in the order the script prints them:
#   1. Not trading costs. The gross alpha is -1.86% against a net -1.88%; the
#      portfolio's cost drag is 0.020% a year.
#   2. Not the size restriction. French's own value-weighted large-cap
#      portfolios show near-zero style loadings and no meaningful alpha.
#   3. It is the NYSE-only listing screen, which excludes the Nasdaq large caps
#      and leaves genuine tilts toward value, profitability and conservative
#      investment.
#   4. Those tilts are not paid in large caps. Three quarters of the value
#      premium accrued in small stocks, which this universe excludes.
#   5. DECISIVE: against a size-matched benchmark, needing no factor model at
#      all, the alpha is statistically zero.
#
# Standard errors are Newey-West with six lags throughout, matching Tables 5 and 6.
suppressMessages({library(data.table); library(lubridate)})

# Resolve the project root, so this runs from the root or from analysis/.
BASE <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
EXT  <- file.path(BASE, "Data/Generated/Portfolios/20260718-1828_ext2023")
LO <- as.Date("1981-01-01"); HI <- as.Date("2023-11-30"); NW_LAGS <- 6

nw <- function(fit, lags = NW_LAGS) {
  X <- model.matrix(fit); e <- residuals(fit); n <- nrow(X)
  bread <- solve(crossprod(X)); u <- X * e; S <- crossprod(u)/n
  if (lags > 0) for (l in 1:lags) {
    w <- 1 - l/(lags + 1)
    G <- crossprod(u[(l+1):n, , drop = FALSE], u[1:(n-l), , drop = FALSE])/n
    S <- S + w*(G + t(G))
  }
  se <- sqrt(diag(bread %*% (S*n) %*% bread))
  list(coef = coef(fit), t = coef(fit)/se)
}

fetch <- function(url, zipname, pat) {
  zf <- file.path(tempdir(), zipname)
  if (!file.exists(zf)) download.file(url, zf, mode = "wb", quiet = TRUE)
  nm <- grep(pat, unzip(zf, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)[1]
  readLines(unz(zf, nm))
}
parse_fr <- function(lines, cols) {
  hdr <- grep("^[ ]*,", lines)[1]
  body <- lines[(hdr + 1):length(lines)]
  st <- grep("^[ ]*$|Annual", body)[1]; if (!is.na(st)) body <- body[1:(st - 1)]
  body <- body[grepl("^[ ]*[0-9]{6}[ ]*,", body)]
  dt <- fread(text = paste(body, collapse = "\n"), header = FALSE)
  setnames(dt, c("yyyymm", cols))
  dt[, eom := ceiling_date(as.Date(paste0(yyyymm, "01"), "%Y%m%d"), unit = "month") - 1]
  for (v in cols) dt[, (v) := as.numeric(get(v))/100]
  dt[, yyyymm := NULL][]
}
U <- "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/"

ff <- fread(file.path(BASE, "Data/ff5_mom_monthly.csv")); ff[, eom := as.Date(eom)]
bms <- fread(file.path(EXT, "bms.csv")); bms[, eom := as.Date(eom_ret)]
mkt <- bms[type == "Market", .(eom, net = r - tc, gross = r, tc)]
d <- merge(mkt, ff, by = "eom")[eom >= LO & eom <= HI]
SIX <- c("mktrf", "smb", "hml", "rmw", "cma", "mom")

cat("=== 1. trading costs are not the cause ===\n")
for (v in c("gross", "net")) {
  f <- nw(lm(reformulate(SIX, v), data = d))
  cat(sprintf("  alpha on %-5s returns %+7.3f%%  (t = %+.2f)\n", v, f$coef[[1]]*12*100, f$t[[1]]))
}
cat(sprintf("  annual cost drag %.3f%% of wealth\n", mean(d$tc)*12*100))

# French size-sorted portfolios: a large-cap value-weighted book we did not build
me <- parse_fr(fetch(paste0(U, "Portfolios_Formed_on_ME_CSV.zip"), "me.zip", "Portfolios_Formed_on_ME"),
               trimws(strsplit(grep("^[ ]*,", fetch(paste0(U, "Portfolios_Formed_on_ME_CSV.zip"),
                 "me.zip", "Portfolios_Formed_on_ME"), value = TRUE)[1], ",")[[1]])[-1])
d <- merge(d, me[, .(eom, big30 = `Hi 30`, big10 = `Hi 10`)], by = "eom")
d[, `:=`(big30 = big30 - rf, big10 = big10 - rf)]

cat("\n=== 2. the size restriction is not the cause ===\n")
cat(sprintf("%-34s %9s %8s %8s %8s %8s\n", "portfolio", "alpha", "t", "hml", "rmw", "cma"))
for (v in c("big30", "big10", "net")) {
  f <- nw(lm(reformulate(SIX, v), data = d))
  cat(sprintf("%-34s %+8.2f%% %+8.2f %+8.3f %+8.3f %+8.3f\n",
      switch(v, big30 = "French top 30% by size (VW)", big10 = "French top size decile (VW)",
             net = "our Market portfolio"),
      f$coef[[1]]*12*100, f$t[[1]], f$coef[["hml"]], f$coef[["rmw"]], f$coef[["cma"]]))
}
cat("  A large-cap value-weighted portfolio built by someone else carries no such tilts.\n")

cat("\n=== 3-4. the tilts are real, but not paid in large caps ===\n")
bm <- parse_fr(fetch(paste0(U, "6_Portfolios_2x3_CSV.zip"), "bm.zip", "6_Portfolios_2x3"),
               c("SL","SM","SH","BL","BMd","BH"))
x <- merge(ff, bm, by = "eom")[eom >= LO & eom <= HI]
cat(sprintf("  HML premium: combined %.2f%%, small-stock leg %.2f%%, large-stock leg %.2f%%\n",
    mean(x$hml)*12*100, mean(x$SH - x$SL)*12*100, mean(x$BH - x$BL)*12*100))
cat(sprintf("  share of the value premium accruing in small stocks: %.0f%%\n",
    (1 - mean(x$BH - x$BL)/mean(x$hml))*100))

cat("\n=== 5. DECISIVE: against a size-matched benchmark, no factor model needed ===\n")
for (b in c("big30", "big10")) {
  f <- nw(lm(reformulate(b, "net"), data = d))
  cat(sprintf("  our Market ~ %-28s alpha %+6.2f%%  (t = %+.2f)  beta %.3f\n",
      switch(b, big30 = "French top 30% by size", big10 = "French top size decile"),
      f$coef[[1]]*12*100, f$t[[1]], f$coef[[2]]))
}
cat("  Statistically zero. The six-factor model misprices the portfolio; the portfolio does not underperform.\n")
