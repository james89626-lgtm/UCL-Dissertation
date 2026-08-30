# Lo (2002), "The Statistics of Sharpe Ratios", applied to this dissertation's results.
#   Eq 9  : SE(SR_monthly) = sqrt((1 + SR^2/2)/T)              [IID]
#   Eq 15 : SE(SR_monthly) = sqrt(V_GMM/T)                     [robust, HAC]
#   Eq 20 : SR(q) = eta(q)*SR,  eta(q) = q / sqrt(q + 2*sum_{k=1}^{q-1}(q-k)*rho_k)
suppressMessages({library(data.table); library(lubridate)})
# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
ext  <- file.path(base, "Data/Generated/Portfolios/20260718-1828_ext2023")

# ---------- assemble monthly NET excess return series ----------
pfml   <- as.data.table(readRDS(file.path(ext, "portfolio-ml.RDS"))$pf)
sml    <- readRDS(file.path(ext, "static-ml.RDS"))
static <- as.data.table(sml$pf)
bms    <- fread(file.path(ext, "bms.csv")); bms[, eom_ret := as.Date(eom_ret)]

series <- list(
  "Portfolio-ML" = pfml[, .(eom_ret, x = r - tc)],
  "Static-ML*"   = static[, .(eom_ret, x = r - tc)],
  "Market"       = bms[type == "Market",   .(eom_ret, x = r - tc)],
  "1/N"          = bms[type == "1/N",      .(eom_ret, x = r - tc)]
)

sp <- fread(file.path(base, "Data/sp500_index_returns.csv"))
sp[, eom_ret := ceiling_date(as.Date(caldt), unit = "month") - 1]
rf <- fread(file.path(base, "Data/ff3_m.csv"), select = c("yyyymm","RF"))
rf[, rf := RF/100][, eom_ret := ceiling_date(as.Date(paste0(yyyymm,"01"), "%Y%m%d"), unit="month")-1]
sp <- rf[, .(eom_ret, rf)][sp, on = "eom_ret"]
series[["S&P 500 TR"]] <- sp[!is.na(vwretd_sp500) & !is.na(rf), .(eom_ret, x = vwretd_sp500 - rf)]

# ---------- Lo's machinery ----------
eta_q <- function(x, q = 12) {                       # Eq 20
  k <- 1:(q-1)
  rho <- sapply(k, function(kk) { a <- acf(x, lag.max = kk, plot = FALSE); a$acf[kk+1] })
  denom <- q + 2*sum((q - k) * rho)
  list(eta = q/sqrt(denom), rho1 = rho[1], rho = rho)
}

se_iid <- function(x) { srm <- mean(x)/sd(x); sqrt((1 + 0.5*srm^2)/length(x)) }   # Eq 9

se_gmm <- function(x, lags = NULL) {                 # Eq 14/15, Newey-West HAC
  T <- length(x); mu <- mean(x); s2 <- mean((x - mu)^2)
  if (is.null(lags)) lags <- floor(4*(T/100)^(2/9))
  m <- cbind(x - mu, (x - mu)^2 - s2)
  S <- crossprod(m)/T
  if (lags > 0) for (l in 1:lags) {
    w <- 1 - l/(lags + 1)
    G <- crossprod(m[(l+1):T, , drop=FALSE], m[1:(T-l), , drop=FALSE])/T
    S <- S + w*(G + t(G))
  }
  s <- sqrt(s2)
  d <- c(1/s, -mu/(2*s^3))                           # dg/dmu , dg/dsigma^2
  sqrt(drop(t(d) %*% S %*% d)/T)
}

analyse <- function(x, label, window) {
  T <- length(x); srm <- mean(x)/sd(x)
  e <- eta_q(x, 12)
  sr_iid <- sqrt(12)*srm
  sr_lo  <- e$eta*srm
  se_m_i <- se_iid(x); se_m_g <- se_gmm(x)
  data.table(window, method = label, T,
             sr_ann_sqrt12 = sr_iid,
             eta12 = e$eta, rho1 = e$rho1,
             sr_ann_lo = sr_lo,
             se_ann_iid = sqrt(12)*se_m_i,
             se_ann_gmm = sqrt(12)*se_m_g,
             ci_lo = sr_iid - 1.96*sqrt(12)*se_m_i,
             ci_hi = sr_iid + 1.96*sqrt(12)*se_m_i)
}

out <- rbindlist(lapply(names(series), function(nm) {
  d <- series[[nm]]
  a <- analyse(d[eom_ret >= as.Date("2021-01-01") & eom_ret <= as.Date("2023-11-30")]$x, nm, "2021-2023")
  b <- analyse(d[eom_ret >= as.Date("1981-01-01") & eom_ret <= as.Date("2023-11-30")]$x, nm, "full 1981-2023")
  rbind(a, b)
}))

cat("=== Lo (2002) analysis of annualised net Sharpe ratios ===\n")
cat("sqrt(12) =", sqrt(12), "\n\n")
print(out[window == "2021-2023"], digits = 3)
cat("\n")
print(out[window == "full 1981-2023"], digits = 3)

# ---------- difference test: Portfolio-ML vs S&P 500, 2021-2023 ----------
cat("\n=== Paired difference in Sharpe ratios, 2021-2023 ===\n")
a <- series[["Portfolio-ML"]][eom_ret >= as.Date("2021-01-01") & eom_ret <= as.Date("2023-11-30")]
b <- series[["S&P 500 TR"]][eom_ret >= as.Date("2021-01-01") & eom_ret <= as.Date("2023-11-30")]
m <- merge(a, b, by = "eom_ret", suffixes = c("_pf","_sp"))
cat("matched months:", nrow(m), " correlation:", cor(m$x_pf, m$x_sp), "\n")

# Jobson-Korkie / Memmel (2003) test of equal Sharpe ratios
jk <- function(r1, r2) {
  T <- length(r1); m1 <- mean(r1); m2 <- mean(r2)
  s1 <- sd(r1); s2 <- sd(r2); s12 <- cov(r1, r2)
  sr1 <- m1/s1; sr2 <- m2/s2
  th <- (1/T)*(2 - 2*s12/(s1*s2) + 0.5*(sr1^2 + sr2^2 - 2*sr1*sr2*(s12/(s1*s2))^2))
  z <- (sr1 - sr2)/sqrt(th)
  list(sr1_ann = sr1*sqrt(12), sr2_ann = sr2*sqrt(12), z = z, p = 2*(1 - pnorm(abs(z))))
}
r <- jk(m$x_pf, m$x_sp)
cat(sprintf("Portfolio-ML ann SR = %.3f | S&P 500 ann SR = %.3f\n", r$sr1_ann, r$sr2_ann))
cat(sprintf("Memmel-corrected Jobson-Korkie z = %.3f, p = %.3f\n", r$z, r$p))

cat("\n=== autocorrelations rho_1..rho_11, full sample 1981-2023 ===\n")
for (nm in names(series)) {
  d <- series[[nm]][eom_ret >= as.Date("1981-01-01") & eom_ret <= as.Date("2023-11-30")]$x
  e <- eta_q(d, 12)
  cat(sprintf("%-14s eta=%.2f  rho: %s\n", nm, e$eta, paste(sprintf("%+.3f", e$rho), collapse=" ")))
}

cat("\n=== Paired difference test, FULL sample 1981-2023 ===\n")
a2 <- series[["Portfolio-ML"]][eom_ret >= as.Date("1981-01-01") & eom_ret <= as.Date("2023-11-30")]
b2 <- series[["S&P 500 TR"]][eom_ret >= as.Date("1981-01-01") & eom_ret <= as.Date("2023-11-30")]
m2 <- merge(a2, b2, by="eom_ret", suffixes=c("_pf","_sp"))
r2 <- jk(m2$x_pf, m2$x_sp)
cat("matched months:", nrow(m2), " correlation:", round(cor(m2$x_pf, m2$x_sp),3), "\n")
cat(sprintf("Portfolio-ML ann SR = %.3f | S&P 500 ann SR = %.3f\n", r2$sr1_ann, r2$sr2_ann))
cat(sprintf("Memmel-corrected Jobson-Korkie z = %.3f, p = %.4f\n", r2$z, r2$p))
