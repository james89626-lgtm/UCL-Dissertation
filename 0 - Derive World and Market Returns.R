# Builds the two data files the original authors sourced separately, but which
# can be derived directly from usa.csv:
#   Data/world_ret_monthly.csv  - excntry, id, eom, ret_exc
#   Data/market_returns.csv     - excntry, eom, mkt_vw_exc (value-weighted market excess return)
#
# Our WRDS pull (contrib_global_factor.ctff_chars) doesn't include a raw
# contemporaneous "ret_exc" column - only "ret_ld1" (next month's excess
# return) and "me" (market equity) at each (id, eom). But that's all we need:
# a stock's return realized *in* month t is exactly its ret_ld1 value recorded
# as of month t-1. So we shift ret_ld1 forward one month to build the monthly
# return panel, and value-weight it by me as of the start of the return period
# (t-1) to build the market return series - the standard construction used for
# a CRSP-style value-weighted index.
#
# Run this after "0 - Download WRDS Data.R" has produced Data/usa.csv (and
# after that file's "eom" column has been reformatted to YYYYMMDD, which the
# download script now does automatically for fresh downloads).

library(data.table)
library(lubridate)

stopifnot(file.exists("Data/usa.csv"))

cat("Reading id/eom/excntry/ret_ld1/me from Data/usa.csv...\n")
cols <- fread("Data/usa.csv", nrows = 1) |> names()
need <- c("id", "eom", "excntry", "ret_ld1", "me")
missing <- setdiff(need, cols)
if (length(missing) > 0) {
  stop("Data/usa.csv is missing expected column(s): ", paste(missing, collapse = ", "))
}

usa <- fread("Data/usa.csv", select = need, colClasses = c(eom = "character"))
usa <- usa[!is.na(ret_ld1)]
usa[, eom := as.Date(eom, format = "%Y%m%d")]
usa[, eom_ret := ceiling_date(eom + 1, unit = "month") - 1]  # month in which ret_ld1 is realized

# world_ret_monthly.csv --------------------------------------------------
out_world <- "Data/world_ret_monthly.csv"
world <- usa[, .(excntry, id, eom = format(eom_ret, "%Y%m%d"), ret_exc = ret_ld1)]
fwrite(world, out_world)
cat("Saved", nrow(world), "rows ->", out_world, "\n")

# market_returns.csv ------------------------------------------------------
out_mkt <- "Data/market_returns.csv"
mkt <- usa[!is.na(me) & me > 0,
           .(mkt_vw_exc = sum(me * ret_ld1) / sum(me)),
           by = .(excntry, eom = format(eom_ret, "%Y%m%d"))]
fwrite(mkt, out_mkt)
cat("Saved", nrow(mkt), "rows ->", out_mkt, "\n")

cat("\nDerived files ready. Next: run 'Run - Local Replication.R'.\n")
