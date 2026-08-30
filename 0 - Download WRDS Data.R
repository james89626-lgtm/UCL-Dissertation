# Run this script yourself (interactively, e.g. in RStudio or `Rscript "0 - Download WRDS Data.R"`)
# It downloads the two large WRDS-gated files this replication needs:
#   Data/usa.csv       - JKP monthly firm characteristics (USA)
#   Data/usa_dsf.csv   - JKP daily excess returns (USA)
#
# You need a WRDS account with access to "Contributed Global Factor Data"
# (the JKP dataset behind Jensen/Kelly/Pedersen (2023), "Is There a Replication
# Crisis in Finance?"). It's the same dataset your Python replication used.
#
# This will take a while: the SQL queries pull the full USA characteristics
# panel (~150 columns x ~5,000 stocks x monthly, 1952-2020) and the daily
# excess-return panel. Expect it to take anywhere from ~10 minutes to over an
# hour depending on your connection and WRDS server load.

if (!requireNamespace("RPostgres", quietly = TRUE)) install.packages("RPostgres")
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("getPass", quietly = TRUE)) install.packages("getPass")

library(DBI)

wrds_user <- Sys.getenv("WRDS_USERNAME")
if (wrds_user == "") {
  # readline() doesn't block under plain `Rscript` (non-interactive), so read stdin directly instead
  cat("WRDS username: ")
  wrds_user <- readLines(con = "stdin", n = 1)
}
wrds_pass <- getPass::getPass("WRDS password: ")

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

# usa.csv --------------------------------------------------------------
out_chars <- "Data/usa.csv"
if (file.exists(out_chars)) {
  cat("[skip]", out_chars, "already exists\n")
} else {
  cat("Querying monthly characteristics (this may take several minutes)...\n")
  chars <- dbGetQuery(wrds, "
    SELECT
        c.*,
        g.crsp_exchcd,
        g.crsp_shrcd
    FROM contrib_global_factor.ctff_chars AS c
    LEFT JOIN contrib_global_factor.global_factor AS g
        ON c.id = g.id AND c.eom = g.eom
    WHERE c.excntry = 'USA'
  ")
  # Add pipeline aliases used by the original R code, without dropping originals
  chars$me <- chars$market_equity
  if ("ret_exc_lead1m" %in% names(chars)) names(chars)[names(chars) == "ret_exc_lead1m"] <- "ret_ld1"
  # WRDS returns "eom" as a native date; the original scripts expect it as a YYYYMMDD string
  chars$eom <- format(as.Date(chars$eom), "%Y%m%d")
  data.table::fwrite(chars, out_chars)
  cat("Saved", nrow(chars), "rows ->", out_chars, "\n")
  rm(chars); gc()
}

# usa_dsf.csv ------------------------------------------------------------
out_daily <- "Data/usa_dsf.csv"
if (file.exists(out_daily)) {
  cat("[skip]", out_daily, "already exists\n")
} else {
  cat("Querying daily excess returns...\n")
  daily <- dbGetQuery(wrds, "
    SELECT id, date, ret_exc
    FROM contrib_global_factor.ctff_daily_ret
    WHERE id <= 99999
  ")
  daily$date <- format(as.Date(daily$date), "%Y%m%d")
  data.table::fwrite(daily, out_daily)
  cat("Saved", nrow(daily), "rows ->", out_daily, "\n")
  rm(daily); gc()
}

dbDisconnect(wrds)
cat("\nAll WRDS downloads complete.\n")
cat("Next: run '0 - Derive World and Market Returns.R' to build the two remaining\n")
cat("derived files (world_ret_monthly.csv, market_returns.csv) from usa.csv.\n")
