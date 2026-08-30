# Factor-alpha regressions for Section 6.2.
#
# Regresses each strategy's monthly NET EXCESS return on four factor models:
#   CAPM, Fama-French 3-factor, Fama-French 5-factor, and FF5 + momentum.
# Standard errors are Newey-West (1987) HAC with 6 lags.
#
# Factor data is downloaded once from Kenneth French's data library and cached
# in Data/. NOTE: French revises these series periodically, so a fresh download
# may not reproduce figures computed from an earlier vintage. The cached copy in
# Data/ is the vintage actually used for the reported results.
suppressMessages({library(data.table); library(lubridate)})

# Resolve the project root, so this runs from the root or from analysis/.
BASE <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")
EXT  <- file.path(BASE, "Data/Generated/Portfolios/20260718-1828_ext2023")
DATA <- file.path(BASE, "Data")
NW_LAGS <- 6

# ---------------------------------------------------------------- factor data
fetch_french <- function(url, zipname, csvpat) {
  zf <- file.path(tempdir(), zipname)
  if (!file.exists(zf)) download.file(url, zf, mode = "wb", quiet = TRUE)
  nm <- grep(csvpat, unzip(zf, list = TRUE)$Name, value = TRUE, ignore.case = TRUE)[1]
  readLines(unz(zf, nm))
}

# Monthly block runs from the header row to the first blank/annual marker.
parse_french <- function(lines, valcols) {
  hdr <- grep("^\\s*,", lines)[1]
  body <- lines[(hdr + 1):length(lines)]
  stop_at <- grep("^\\s*$|Annual", body)[1]
  if (!is.na(stop_at)) body <- body[1:(stop_at - 1)]
  body <- body[grepl("^\\s*[0-9]{6}\\s*,", body)]
  dt <- fread(text = paste(body, collapse = "\n"), header = FALSE)
  setnames(dt, c("yyyymm", valcols))
  dt[, eom := ceiling_date(as.Date(paste0(yyyymm, "01"), "%Y%m%d"), unit = "month") - 1]
  for (v in valcols) dt[, (v) := get(v)/100]     # French reports percent
  dt[, yyyymm := NULL][]
}

ff_cache <- file.path(DATA, "ff5_mom_monthly.csv")
if (file.exists(ff_cache)) {
  cat("Using cached factor data:", ff_cache, "\n")
  ff <- fread(ff_cache); ff[, eom := as.Date(eom)]
} else {
  cat("Downloading factor data from Kenneth French's data library...\n")
  ff5 <- parse_french(fetch_french(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_5_Factors_2x3_CSV.zip",
    "ff5.zip", "5_Factors"), c("mktrf","smb","hml","rmw","cma","rf"))
  mom <- parse_french(fetch_french(
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Momentum_Factor_CSV.zip",
    "mom.zip", "Momentum"), c("mom"))
  ff <- merge(ff5, mom, by = "eom")
  fwrite(ff, ff_cache)
  cat("Cached to:", ff_cache, "\n")
}
cat("Factor coverage:", format(min(ff$eom)), "to", format(max(ff$eom)), "\n\n")

# ------------------------------------------------------------ strategy returns
pfml   <- as.data.table(readRDS(file.path(EXT, "portfolio-ml.RDS"))$pf)
static <- as.data.table(readRDS(file.path(EXT, "static-ml.RDS"))$pf)
bms    <- fread(file.path(EXT, "bms.csv")); bms[, eom_ret := as.Date(eom_ret)]

strategies <- list(
  "Portfolio-ML" = pfml  [, .(eom = eom_ret, y = r - tc)],
  "Static-ML*"   = static[, .(eom = eom_ret, y = r - tc)],
  "Market"       = bms[type == "Market", .(eom = eom_ret, y = r - tc)]
)

# --------------------------------------------------------- Newey-West test
nw_coeftest <- function(fit, lags = NW_LAGS) {
  X <- model.matrix(fit); e <- residuals(fit); n <- nrow(X); k <- ncol(X)
  bread <- solve(crossprod(X))
  u <- X * e
  S <- crossprod(u)/n
  if (lags > 0) for (l in 1:lags) {
    w <- 1 - l/(lags + 1)
    G <- crossprod(u[(l+1):n, , drop = FALSE], u[1:(n-l), , drop = FALSE])/n
    S <- S + w*(G + t(G))
  }
  V <- bread %*% (S*n) %*% bread
  se <- sqrt(diag(V))
  data.table(term = colnames(X), coef = coef(fit), se = se, t = coef(fit)/se)
}

MODELS <- list(
  "CAPM"                 = "mktrf",
  "Fama-French 3-factor" = c("mktrf","smb","hml"),
  "Fama-French 5-factor" = c("mktrf","smb","hml","rmw","cma"),
  "FF5 + Momentum"       = c("mktrf","smb","hml","rmw","cma","mom")
)

run <- function(y_dt, rhs, lo, hi) {
  d <- merge(y_dt, ff, by = "eom")[eom >= as.Date(lo) & eom <= as.Date(hi)]
  fit <- lm(reformulate(rhs, response = "y"), data = d)
  ct <- nw_coeftest(fit)
  list(n = nrow(d), ct = ct, adjr2 = summary(fit)$adj.r.squared,
       alpha_ann = coef(fit)[["(Intercept)"]]*12,
       alpha_t   = ct[term == "(Intercept)", t])
}

report_window <- function(lo, hi, label) {
  cat("\n=====================================================================\n")
  cat(label, " (Newey-West, ", NW_LAGS, " lags)\n", sep = "")
  cat("=====================================================================\n")
  tab <- rbindlist(lapply(names(MODELS), function(m) {
    row <- data.table(model = m)
    for (s in names(strategies)) {
      r <- run(strategies[[s]], MODELS[[m]], lo, hi)
      row[, paste0(s, "_a") := sprintf("%.2f%%", r$alpha_ann*100)]
      row[, paste0(s, "_t") := sprintf("%.2f", r$alpha_t)]
      row[, n := r$n]
    }
    row
  }))
  print(tab, row.names = FALSE)

  cat("\n-- Full FF5+Momentum loadings --\n")
  for (s in names(strategies)) {
    r <- run(strategies[[s]], MODELS[["FF5 + Momentum"]], lo, hi)
    cat(sprintf("\n%s   (n=%d, adj R2 = %.3f)\n", s, r$n, r$adjr2))
    print(r$ct[, .(term, coef = round(coef,4), t = round(t,2))], row.names = FALSE)
  }
}

report_window("1981-01-01", "2023-11-30", "1981-2023")
report_window("2021-01-01", "2023-11-30", "2021-2023 extension window only")
