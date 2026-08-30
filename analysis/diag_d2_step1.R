suppressMessages(library(data.table))

# Resolve the project root, so this runs from the root or from analysis/.
base <- if (dir.exists("Data")) "." else if (dir.exists("../Data")) ".." else
  stop("Run from the project root, e.g. Rscript analysis/<script>.R")

ext_dir  <- file.path(base, "Data/Generated/Portfolios/20260718-1828_ext2023")
main_dir <- file.path(base, "Data/Generated/Portfolios/20260715-0909_percentile_yearly_grid")

w_main <- as.data.table(readRDS(file.path(main_dir, "static-ml.RDS"))$w)
w_ext  <- as.data.table(readRDS(file.path(ext_dir,  "static-ml.RDS"))$w)

cat("=== First eom in each run ===\n")
cat("main:", format(min(w_main$eom)), " ext:", format(min(w_ext$eom)), "\n\n")

d0 <- min(w_main$eom)
a <- w_main[eom == d0][order(id)]
b <- w_ext [eom == d0][order(id)]
cat("=== At FIRST eom (", format(d0), ") ===\n")
cat("n main:", nrow(a), " n ext:", nrow(b), " identical ids:", identical(a$id, b$id), "\n")
for (col in c("mu_ld1","tr_ld1","pred_ld1","w_start","w")) {
  cat(sprintf("  max|diff| %-9s : %.10g\n", col, max(abs(a[[col]] - b[[col]]), na.rm=TRUE)))
}

cat("\n=== Universe size per month, all common months ===\n")
n_main <- w_main[, .(n_main = .N), by = eom]
n_ext  <- w_ext [, .(n_ext  = .N), by = eom]
nn <- merge(n_main, n_ext, by = "eom")
nn[, same := n_main == n_ext]
cat("common months:", nrow(nn), " | months where universe size differs:", nn[same == FALSE, .N], "\n")
if (nn[same == FALSE, .N] > 0) print(head(nn[same == FALSE], 20))

cat("\n=== Exact id-set equality per month (first 60 common months) ===\n")
common_eoms <- sort(intersect(w_main$eom, w_ext$eom))
common_eoms <- as.Date(common_eoms, origin = "1970-01-01")
mismatch <- 0L
for (d in as.character(head(common_eoms, 60))) {
  ia <- sort(w_main[eom == as.Date(d), id]); ib <- sort(w_ext[eom == as.Date(d), id])
  if (!identical(ia, ib)) mismatch <- mismatch + 1L
}
cat("id-set mismatches in first 60 months:", mismatch, "\n")

cat("\n=== Where does w_start first diverge? ===\n")
for (d in as.character(head(common_eoms, 6))) {
  a <- w_main[eom == as.Date(d)][order(id)]; b <- w_ext[eom == as.Date(d)][order(id)]
  if (!identical(a$id, b$id)) { cat(d, ": id sets differ\n"); next }
  cat(sprintf("%s  max|w_start diff| = %.10g   max|w diff| = %.10g   n=%d\n",
              d, max(abs(a$w_start - b$w_start), na.rm=TRUE),
                 max(abs(a$w - b$w), na.rm=TRUE), nrow(a)))
}
