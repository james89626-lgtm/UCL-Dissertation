# Screen-attrition diagnostic for Discrepancy 1 (v2: data.table syntax throughout
# to avoid MASS::select masking dplyr::select).
# Run from the project root; tolerate being run from analysis/.
if (!file.exists("Main.R") && file.exists("../Main.R")) setwd("..")
stopifnot(file.exists("Main.R"))
suppressMessages({library(data.table); library(lubridate); library(dplyr); library(stringr)})

source("Main.R")
settings$screens$size_screen <- "perc_low50_high100_min50"   # set by the Run scripts, not Main.R
settings$screens$nyse_stocks <- TRUE

cat("\n\n########## SCREEN ATTRITION DIAGNOSTIC ##########\n")
cat("features:", length(features), " feat_pct:", settings$screens$feat_pct,
    " min_feat:", floor(length(features)*settings$screens$feat_pct), "\n")
cat("size_screen:", settings$screens$size_screen,
    " addition_n:", settings$addition_n, " deletion_n:", settings$deletion_n, "\n\n")

report <- function(dt, label, flag = NULL) {
  d <- if (is.null(flag)) dt else dt[get(flag) == TRUE]
  o <- d[eom >= as.Date("1981-01-01") & eom <= as.Date("2020-12-31")]
  f <- d
  po <- o[, .N, by=eom]; pf <- f[, .N, by=eom]
  cat(sprintf("%-44s | OOS 81-20: min=%4d med=%6.1f max=%4d | FULL 52-20: min=%4d med=%6.1f max=%4d\n",
              label, min(po$N), median(po$N), max(po$N),
                     min(pf$N), median(pf$N), max(pf$N)))
  invisible(po)
}

chars <- fread("Data/usa.csv",
  select = unique(c("id","eom","sic","size_grp","me","crsp_exchcd","rvol_252d","dolvol_126d", features)),
  colClasses = c("eom"="character","sic"="character"))
chars[, eom := lubridate::fast_strptime(eom, format="%Y%m%d") %>% as.Date()]
cat("RAW rows:", nrow(chars), "\n\n")

report(chars, "0. raw usa.csv (USA)")
chars <- chars[id <= 99999];        report(chars, "1. CRSP ids only")
chars <- chars[crsp_exchcd == 1];   report(chars, "2. + NYSE only")
chars <- chars[eom >= settings$screens$start & eom <= settings$screens$end]
                                     report(chars, "3. + date screen")
chars <- chars[!is.na(me)];         report(chars, "4. + non-missing me")

monthly <- fread("Data/world_ret_monthly.csv", select=c("excntry","id","eom","ret_exc"), colClasses=c("eom"="character"))
monthly <- monthly[excntry=="USA" & id<=99999]
monthly[, eom := fast_strptime(eom, format="%Y%m%d") %>% as.Date()]
data_ret <- long_horizon_ret(monthly, h = settings$pf$hps$m1$K, impute = "zero")

rfdt <- fread("Data/ff3_m.csv", select=c("yyyymm","RF"))
rfdt[, rf := RF/100]
rfdt[, eom := ceiling_date(as.Date(paste0(yyyymm,"01"), "%Y%m%d"), unit="month")-1]
rfdt <- rfdt[, .(eom, rf)]

d1 <- data_ret[, .(id, eom, eom_ret = eom+1+months(1)-1, ret_ld1)]
d1 <- rfdt[d1, on="eom"]
d1[, tr_ld1 := ret_ld1 + rf][, rf := NULL]
d1 <- d1[, .(id, eom = eom+1+months(1)-1, "tr_ld0" = tr_ld1)][d1, on=.(id, eom)]
chars <- d1[chars, on=.(id, eom)]
rm(monthly, data_ret, d1)

chars <- chars[!is.na(tr_ld0) & !is.na(tr_ld1)]; report(chars, "5. + non-missing returns t-1, t+1")
chars[, dolvol := dolvol_126d]
chars <- chars[!is.na(dolvol) & dolvol > 0];     report(chars, "6. + dolvol present & > 0")
chars <- chars[!is.na(sic)];                     report(chars, "7. + valid SIC")

fa <- rowSums(!is.na(as.matrix(chars[, ..features])))
min_feat <- floor(length(features)*settings$screens$feat_pct)
cat(sprintf("\n   >> features non-missing: mean=%.1f / %d ; failing 50%% screen = %.2f%%\n\n",
            mean(fa), length(features), mean(fa < min_feat)*100))
chars <- chars[fa >= min_feat];                  report(chars, "8. + >=50% features non-missing")

chars[, valid_data := TRUE]
setorder(chars, id, eom)
lb <- pf_set$lb_hor + 1
chars[, eom_lag := shift(eom, n=lb, type="lag"), by=id]
chars[, month_diff := interval(eom_lag, eom) %/% months(1)]
chars[, valid_data := (valid_data==TRUE & month_diff==lb & !is.na(month_diff))]
chars[, c("eom_lag","month_diff") := NULL]
report(chars, "9. + 12-month lookback contiguity", flag="valid_data")

size_screen_fun(chars, type = settings$screens$size_screen)
chars[, valid_sz := (valid_data==TRUE & valid_size==TRUE)]; report(chars, "10. + above-median-cap screen", flag="valid_sz")
addition_deletion_fun(chars, addition_n=settings$addition_n, deletion_n=settings$deletion_n)
pm <- report(chars, "11. + 12-month addition/deletion", flag="valid")

cat("\n=== FINAL (OOS 1981-2020) vs PAPER ===\n")
cat(sprintf("ours : min=%d  median=%.1f  max=%d\n", min(pm$N), median(pm$N), max(pm$N)))
cat( "paper: min=184  median=646.0  max=805\n")
cat( "paper (all NYSE, no size screen): min=401  median=1270  max=1579\n")
