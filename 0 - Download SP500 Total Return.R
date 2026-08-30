# Run this script yourself (interactively, e.g. in RStudio or `Rscript "0 - Download SP500 Total Return.R"`)
# It pulls the S&P 500 index return series from CRSP's monthly index files on
# WRDS, for comparison against our "Market"/"Portfolio-ML"/etc. results.
#
# HISTORY: an earlier version of this script pulled crsp_a_indexes.msix,
# whose vwretd column is CRSP's broad value-weighted return across ALL
# NYSE/AMEX/NASDAQ common stocks (thousands of names) - not literally the
# S&P 500. Checking WRDS's table catalog directly (crsp_a_indexes.msp500)
# turned up a separate, dedicated file where totcnt == 505 on every row -
# confirming vwretd there is computed specifically over the S&P 500's own
# constituents, dividends included. That's the correct total-return series;
# this version pulls BOTH tables so we can compare them directly.
#
# COLUMNS:
#   msix   (broad market, all NYSE/AMEX/NASDAQ common stocks):
#     vwretd_broad, sprtrn (S&P 500 price return, confirmed dividend-free
#     by direct comparison against spindx level changes), spindx
#   msp500 (S&P 500 constituents specifically, ~505 names):
#     vwretd_sp500, vwretx_sp500 (price-only, for cross-checking),
#     totcnt (constituent count, sanity check)

if (!requireNamespace("RPostgres", quietly = TRUE)) install.packages("RPostgres")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("getPass", quietly = TRUE)) install.packages("getPass")

library(DBI)

wrds_user <- Sys.getenv("WRDS_USERNAME")
if (wrds_user == "") {
  cat("WRDS username: ")
  wrds_user <- readLines(con = "stdin", n = 1)
}
wrds_pass <- Sys.getenv("WRDS_PASSWORD")
if (wrds_pass == "") {
  wrds_pass <- getPass::getPass("WRDS password: ")
}

cat("Connecting to WRDS...\n")
wrds <- dbConnect(
  RPostgres::Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  dbname = "wrds",
  sslmode = "require",
  user = wrds_user,
  password = wrds_pass
)
rm(wrds_pass)

dir.create("Data", showWarnings = FALSE)

# Broad market (msix) - kept for comparison, no longer treated as "the S&P 500"
broad <- dbGetQuery(wrds, "
  SELECT caldt, spindx, sprtrn, vwretd AS vwretd_broad, vwretx AS vwretx_broad, totval
  FROM crsp_a_indexes.msix
  ORDER BY caldt
")

# S&P 500 specifically (msp500) - vwretd here is the real S&P 500 total return
sp500 <- dbGetQuery(wrds, "
  SELECT caldt, vwretd AS vwretd_sp500, vwretx AS vwretx_sp500, totcnt, spindx AS spindx_check, sprtrn AS sprtrn_check
  FROM crsp_a_indexes.msp500
  ORDER BY caldt
")

dbDisconnect(wrds)

merged <- merge(broad, sp500[, c("caldt", "vwretd_sp500", "vwretx_sp500", "totcnt")], by = "caldt", all = TRUE)

out_file <- "Data/sp500_index_returns.csv"
data.table::fwrite(merged, out_file)
cat("\nSaved", nrow(merged), "rows ->", out_file, "\n")
cat("Date range:", as.character(range(as.Date(merged$caldt))), "\n\n")

cat("Last few rows (sanity check on totcnt ~500-505 and vwretd_sp500 vs vwretd_broad):\n")
print(tail(merged[, c("caldt", "sprtrn", "vwretd_broad", "vwretd_sp500", "totcnt")]))
