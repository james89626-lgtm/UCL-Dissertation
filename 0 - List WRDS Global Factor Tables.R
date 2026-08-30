# Read-only metadata query against WRDS.
#
# Purpose: find out whether the contrib_global_factor schema exposes a market
# return series directly. If it does, market_returns.csv can come from the same
# source Jensen et al. (2026) used rather than being derived from usa.csv by
# "0 - Derive World and Market Returns.R", and it would be the same vintage as
# our characteristics pull, so it would also cover the 2021-2023 extension.
#
# Authentication: uses WRDS_USERNAME from the environment and lets libpq read
# the password from pgpass.conf. Falls back to an interactive prompt only if
# that is not configured, in which case run this yourself rather than in a
# non-interactive shell.

library(DBI)

wrds_user <- Sys.getenv("WRDS_USERNAME")
if (wrds_user == "") stop("WRDS_USERNAME is not set.")

args <- list(
  RPostgres::Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  dbname = "wrds",
  sslmode = "require",
  user = wrds_user
)

# Only prompt if libpq has no stored credential to fall back on.
pgpass <- c(file.path(Sys.getenv("APPDATA"), "postgresql", "pgpass.conf"),
            file.path(Sys.getenv("HOME"), ".pgpass"))
if (!any(file.exists(pgpass))) {
  args$password <- getPass::getPass("WRDS password: ")
}

wrds <- do.call(dbConnect, args)
on.exit(try(dbDisconnect(wrds), silent = TRUE))

cat("\n=== tables in contrib_global_factor ===\n")
tabs <- dbGetQuery(wrds, "
  SELECT table_name
  FROM information_schema.tables
  WHERE table_schema = 'contrib_global_factor'
  ORDER BY table_name
")
print(tabs, row.names = FALSE)

hits <- grep("mkt|market|index|ret", tabs$table_name, value = TRUE, ignore.case = TRUE)
if (length(hits) > 0) {
  cat("\n=== columns of candidate tables ===\n")
  for (tb in hits) {
    cols <- dbGetQuery(wrds, sprintf("
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_schema = 'contrib_global_factor' AND table_name = '%s'
      ORDER BY ordinal_position", tb))
    cat("\n--", tb, "  (", nrow(cols), "columns )\n")
    print(head(cols, 40), row.names = FALSE)
  }
} else {
  cat("\nNo table name matched mkt/market/index/ret.\n")
}
