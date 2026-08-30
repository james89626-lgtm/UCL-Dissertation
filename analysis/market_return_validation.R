# Validates the derived value-weighted market return against the series the
# original authors distribute, for Section 3.4.
#
# BACKGROUND. "0 - Derive World and Market Returns.R" rebuilds two files the
# replication code expects, world_ret_monthly.csv and market_returns.csv, from
# Data/usa.csv. Jensen et al. (2026) did not derive these; they took them from
# the Jensen et al. (2023) distribution. The WRDS table used here
# (contrib_global_factor.ctff_chars) supplies only ret_exc_lead1m and me, with
# no contemporaneous ret_exc, so the files were rebuilt to keep every input on
# one documented source.
#
# The market return matters more than its role as a benchmark suggests, because
# the investor's wealth path is compounded backwards along it (Section 4.4) and
# that path scales the transaction cost of every strategy. This script checks
# the derived series against the authors' own.
#
# INPUT. Data/market_returns_jkp.csv, downloaded manually from the Dropbox link
# in the authors' README (hhag022-replication/README.md). It covers all
# countries, 1926-2024. Do NOT save it over Data/market_returns.csv, which is
# the derived file the pipeline actually consumes.
#
# Figures produced, all quoted in Section 3.4:
#   correlation 0.999 over 1981-2023, mean absolute difference 11.5 bp/month,
#   annualised 8.19% derived against 7.99% published, and a wealth path at most
#   8.8% apart at the start of the out-of-sample period.
suppressMessages({library(data.table); library(lubridate)})

# Resolve the project root, so this runs from the root or from analysis/.
BASE <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
D <- file.path(BASE, "Data")

jkp <- fread(file.path(D, "market_returns_jkp.csv"))[excntry == "USA"]
jkp[, eom := as.Date(eom)]
drv <- fread(file.path(D, "market_returns.csv"), colClasses = c(eom = "character"))[excntry == "USA"]
drv[, eom := as.Date(eom, format = "%Y%m%d")]

cat("authors' series :", format(min(jkp$eom)), "to", format(max(jkp$eom)),
    sprintf("(%d months)\n", nrow(jkp)))
cat("derived series  :", format(min(drv$eom)), "to", format(max(drv$eom)),
    sprintf("(%d months)\n\n", nrow(drv)))

m <- merge(jkp[, .(eom, jkp = mkt_vw_exc, stocks)],
           drv[, .(eom, derived = mkt_vw_exc)], by = "eom")

cat("=== agreement by window ===\n")
for (w in list(c("1952-01-31", "2023-12-31"), c("1981-01-31", "2023-12-31"),
               c("1981-01-31", "2020-12-31"), c("2021-01-31", "2023-12-31"))) {
  x <- m[eom >= as.Date(w[1]) & eom <= as.Date(w[2])]
  cat(sprintf("%s to %s (n=%3d)  corr %.5f | mean abs diff %5.2f bp | ann. %5.2f%% pub vs %5.2f%% derived\n",
              substr(w[1],1,7), substr(w[2],1,7), nrow(x), cor(x$jkp, x$derived),
              mean(abs(x$jkp - x$derived))*10000, mean(x$jkp)*12*100, mean(x$derived)*12*100))
}

# Wealth is anchored at the sample end and compounded backwards, so at each date
# what matters is the cumulative return from there forward to the anchor.
cat("\n=== implied wealth path, derived relative to the authors', anchored 2023-12 ===\n")
x <- m[eom <= as.Date("2023-12-31")][order(-eom)]
x[, ratio := cumprod(1 + jkp) / cumprod(1 + derived)]
sel <- as.Date(c("2023-12-31","2020-12-31","2010-12-31","2000-12-31",
                 "1990-12-31","1981-01-31","1952-01-31"))
print(x[eom %in% sel][order(-eom), .(eom, wealth_ratio = round(ratio, 4),
                                     pct = round((ratio-1)*100, 2))], row.names = FALSE)
cat("\nFor scale, the sample-end change in Section 4.5 was a uniform 24.4% rescaling.\n")

cat("\n=== median absolute difference by decade, bp ===\n")
m[, dec := paste0(substr(year(eom), 1, 3), "0s")]
print(m[, .(months = .N, med_abs_bp = round(median(abs(derived - jkp))*10000, 2)),
        by = dec][order(dec)], row.names = FALSE)

cat("\n=== stocks behind the authors' USA market return ===\n")
print(m[, .(min = min(stocks), median = as.numeric(median(stocks)), max = max(stocks))],
      row.names = FALSE)
