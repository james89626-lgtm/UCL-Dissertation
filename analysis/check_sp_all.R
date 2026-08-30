suppressMessages({library(data.table); library(lubridate)})
# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
sp <- fread(file.path(base, "Data/sp500_index_returns.csv"))
sp[, eom_ret := ceiling_date(as.Date(caldt), unit="month")-1]
rf <- fread(file.path(base, "Data/ff3_m.csv"), select=c("yyyymm","RF"))
rf[, rf := RF/100][, eom_ret := ceiling_date(as.Date(paste0(yyyymm,"01"),"%Y%m%d"), unit="month")-1]
sp <- rf[, .(eom_ret, rf)][sp, on="eom_ret"]
sp <- sp[!is.na(vwretd_sp500) & !is.na(rf)][, x := vwretd_sp500 - rf]

# align to the portfolio series months exactly
pf <- as.data.table(readRDS(file.path(base,
  "Data/Generated/Portfolios/20260718-1828_ext2023/portfolio-ml.RDS"))$pf)
sp <- sp[eom_ret %in% pf$eom_ret]

f <- function(lo, hi, lab) {
  d <- sp[eom_ret >= as.Date(lo) & eom_ret <= as.Date(hi)]
  cat(sprintf("%-22s n=%4d  SR = %.4f  -> rounds to %.2f\n",
      lab, nrow(d), mean(d$x)/sd(d$x)*sqrt(12), round(mean(d$x)/sd(d$x)*sqrt(12),2)))
}
cat("S&P 500 TR net Sharpe, aligned to portfolio months:\n")
f("1981-01-01","2023-11-30","full 1981-2023")
f("1981-01-01","2020-12-31","original 1981-2020")
f("2021-01-01","2023-11-30","2021-2023 only")
cat("\nTable 2 currently shows: 0.54 / 0.55 / 0.45\n")
