suppressMessages({library(data.table); library(lubridate); library(dplyr)})

# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
risk_free <- fread(file.path(base, "Data/ff3_m.csv"), select = c("yyyymm", "RF")) %>%
  mutate(rf = RF/100, eom = paste0(yyyymm, "01") %>% as.Date("%Y%m%d") %>% ceiling_date(unit="month")-1) %>%
  select(eom, rf) %>% setDT()

market <- fread(file.path(base, "Data/market_returns.csv"), colClasses = c("eom"="character"))
market <- market[excntry == "USA", .(eom_ret = as.Date(eom, format="%Y%m%d"), mkt_vw_exc)]

wealth_func <- function(wealth_end, end, market, risk_free) {
  w <- risk_free[, .("eom_ret"=eom, rf)][market, on = .(eom_ret)][, tret := mkt_vw_exc+rf]
  w <- w[eom_ret <= end]
  w <- w[order(-eom_ret)][, wealth := cumprod(1-tret)*wealth_end]
  w[, .(eom = floor_date(eom_ret, unit = "month")-1, wealth, mu_ld1 = tret)] %>%
    rbind(data.table(eom=end, wealth = wealth_end, mu_ld1 = NA_real_)) %>% arrange(eom) %>% setDT()
}

W_MAIN_END <- as.Date("2020-12-31")   # main replication run's test_end
W_EXT_END  <- as.Date("2023-11-30")   # extension run's test_end

w_main <- wealth_func(1e10, W_MAIN_END, market, risk_free)
w_ext  <- wealth_func(1e10, W_EXT_END,  market, risk_free)

cmp <- merge(w_main[, .(eom, wealth_main = wealth)],
             w_ext [, .(eom, wealth_ext  = wealth)], by = "eom")
cmp[, ratio := wealth_ext / wealth_main]

cat("=== Investor wealth at selected historical dates ===\n")
show <- c("1980-12-31","1981-01-31","1990-12-31","2000-12-31","2010-12-31","2019-12-31","2020-11-30")
print(cmp[eom %in% as.Date(show)])

cat("\n=== Ratio (ext / main) summary across all common months ===\n")
print(summary(cmp$ratio))
cat("\nIs the ratio constant across time? sd =", sd(cmp$ratio), "\n")

cat("\n=== Sanity: cumulative market total return, 2020-12 .. 2023-11 ===\n")
seg <- risk_free[, .(eom_ret=eom, rf)][market, on=.(eom_ret)][, tret := mkt_vw_exc+rf][
  eom_ret > W_MAIN_END & eom_ret <= W_EXT_END]
cat("months:", nrow(seg), " cumulative gross return:", prod(1+seg$tret), "\n")
cat("1/cumprod(1-tret) over that segment:", 1/prod(1-seg$tret), "\n")
