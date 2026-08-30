# HAC-robust test of the difference between two Sharpe ratios (Ledoit & Wolf 2008,
# analytical HAC variant), versus Jobson-Korkie/Memmel which assumes no serial correlation.
suppressMessages({library(data.table); library(lubridate)})
# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
ext  <- file.path(base, "Data/Generated/Portfolios/20260718-1828_ext2023")

pf <- as.data.table(readRDS(file.path(ext, "portfolio-ml.RDS"))$pf)[, .(eom_ret, x1 = r - tc)]
sp <- fread(file.path(base, "Data/sp500_index_returns.csv"))
sp[, eom_ret := ceiling_date(as.Date(caldt), unit="month")-1]
rf <- fread(file.path(base, "Data/ff3_m.csv"), select=c("yyyymm","RF"))
rf[, rf := RF/100][, eom_ret := ceiling_date(as.Date(paste0(yyyymm,"01"),"%Y%m%d"), unit="month")-1]
sp <- rf[, .(eom_ret, rf)][sp, on="eom_ret"][!is.na(vwretd_sp500) & !is.na(rf)][, .(eom_ret, x2 = vwretd_sp500 - rf)]
d <- merge(pf, sp, by="eom_ret")

diff_test <- function(r1, r2, lags=NULL) {
  T <- length(r1)
  m1 <- mean(r1); m2 <- mean(r2)
  v1 <- mean((r1-m1)^2); v2 <- mean((r2-m2)^2)
  s1 <- sqrt(v1); s2 <- sqrt(v2)
  delta <- m1/s1 - m2/s2
  g <- cbind(r1-m1, (r1-m1)^2-v1, r2-m2, (r2-m2)^2-v2)
  if (is.null(lags)) lags <- floor(4*(T/100)^(2/9))
  S <- crossprod(g)/T
  if (lags > 0) for (l in 1:lags) {
    w <- 1 - l/(lags+1)
    G <- crossprod(g[(l+1):T,,drop=FALSE], g[1:(T-l),,drop=FALSE])/T
    S <- S + w*(G + t(G))
  }
  grad <- c(1/s1, -m1/(2*s1^3), -1/s2, m2/(2*s2^3))
  se <- sqrt(drop(t(grad) %*% S %*% grad)/T)
  z <- delta/se
  list(sr1=m1/s1*sqrt(12), sr2=m2/s2*sqrt(12), z=z, p=2*(1-pnorm(abs(z))), lags=lags)
}

jk <- function(r1, r2) {
  T <- length(r1); s1 <- sd(r1); s2 <- sd(r2); s12 <- cov(r1,r2)
  sr1 <- mean(r1)/s1; sr2 <- mean(r2)/s2; rho <- s12/(s1*s2)
  th <- (1/T)*(2 - 2*rho + 0.5*(sr1^2 + sr2^2 - 2*sr1*sr2*rho^2))
  z <- (sr1-sr2)/sqrt(th); list(z=z, p=2*(1-pnorm(abs(z))))
}

for (w in list(c("1981-01-01","2023-11-30","full 1981-2023"),
               c("2021-01-01","2023-11-30","extension 2021-2023"))) {
  sub <- d[eom_ret >= as.Date(w[1]) & eom_ret <= as.Date(w[2])]
  h <- diff_test(sub$x1, sub$x2); j <- jk(sub$x1, sub$x2)
  cat(sprintf("\n%s  (n=%d, NW lags=%d)\n", w[3], nrow(sub), h$lags))
  cat(sprintf("  Portfolio-ML ann SR = %.3f | S&P 500 ann SR = %.3f\n", h$sr1, h$sr2))
  cat(sprintf("  HAC-robust (Ledoit-Wolf style) : z = %6.3f, p = %.4f\n", h$z, h$p))
  cat(sprintf("  Jobson-Korkie / Memmel         : z = %6.3f, p = %.4f\n", j$z, j$p))
}
