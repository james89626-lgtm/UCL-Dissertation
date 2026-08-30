suppressMessages({library(data.table); library(lubridate)})
# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
sp <- fread(file.path(base, "Data/sp500_index_returns.csv"))
sp[, caldt := as.Date(caldt)]
sp[, eom := ceiling_date(caldt, unit="month")-1]

rf <- fread(file.path(base, "Data/ff3_m.csv"), select=c("yyyymm","RF"))
rf[, rf := RF/100]
rf[, eom := ceiling_date(as.Date(paste0(yyyymm,"01"), "%Y%m%d"), unit="month")-1]
rf <- rf[, .(eom, rf)]

sp <- rf[sp, on="eom"]
sp[, exc := vwretd_sp500 - rf]

ext <- sp[eom >= as.Date("2021-01-01") & eom <= as.Date("2023-11-30") & !is.na(exc)]
cat("S&P 500 TR, 2021-01..2023-11: n =", nrow(ext), "\n")
sp_sr <- mean(ext$exc)/sd(ext$exc)*sqrt(12)
cat("  precise annualised net SR =", sp_sr, "\n\n")

pfml <- as.data.table(readRDS(file.path(base,
  "Data/Generated/Portfolios/20260718-1828_ext2023/portfolio-ml.RDS"))$pf)
pe <- pfml[eom_ret >= as.Date("2021-01-01")]
pf_sr <- mean(pe$r - pe$tc)/sd(pe$r)*sqrt(12)
cat("Portfolio-ML 2021-2023: n =", nrow(pe), " precise net SR =", pf_sr, "\n\n")

cat("ratio (precise)        =", pf_sr/sp_sr, "\n")
cat("ratio (rounded 1.05/0.45) =", 1.05/0.45, "\n")

# Sharpe standard errors (Lo 2002 iid approximation), annualised
se_ann <- function(x, n) { srm <- mean(x)/sd(x); sqrt((1 + 0.5*srm^2)/n)*sqrt(12) }
cat("\napprox annualised SE, Portfolio-ML =", se_ann(pe$r - pe$tc, nrow(pe)), "\n")
cat("approx annualised SE, S&P 500      =", se_ann(ext$exc, nrow(ext)), "\n")
