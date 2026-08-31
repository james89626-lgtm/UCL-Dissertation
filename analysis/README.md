# Analysis scripts

Supporting scripts for "Can Machine Learning Beat the Market After Considering
Trading Costs?". Each produces a figure that appears in the dissertation text.
Written against the outputs of the replication pipeline in this project, which
builds on the replication code released by Jensen, Kelly, Malamud and Pedersen
(2026).

Run from the project root, for example `Rscript analysis/lo_sharpe.R`. Running
from inside `analysis/` also works; anywhere else stops with a message. Requires
`data.table`, `lubridate`, `dplyr`, `stringr`.

## Sharpe ratio statistics (Section 5.1)

| Script | Produces |
|---|---|
| `lo_sharpe.R` | Table 3. Lo (2002) annualisation multipliers η(12), monthly autocorrelations ρ₁ to ρ₁₁, IID standard errors (Lo eq. 9), HAC standard errors (Lo eq. 15), and 95% confidence intervals, for Portfolio-ML, Static-ML\*, Market, 1/N and the S&P 500, over both the full 1981–2023 sample and the 2021–2023 extension window. |
| `sharpe_diff_hac.R` | The p-values of 0.0016 (full sample) and 0.47 (extension window) for the difference between Portfolio-ML's and the S&P 500's Sharpe ratios. Delta method with Newey-West standard errors. Also reports the Jobson-Korkie/Memmel statistic for comparison, which is *not* used in the dissertation because it assumes serially uncorrelated returns. |
| `check_sp_all.R` | Verifies the three S&P 500 Sharpe ratios in Table 2, with months aligned exactly to the portfolio series. Source of the correction from 0.45 to 0.44 in the 2021–2023 column. |
| `check_sp500_ratio.R` | Earlier, narrower version of the same check. Superseded by `check_sp_all.R`. |

## Factor regressions (Section 5.2)

| Script | Produces |
|---|---|
| `alpha_regression.R` | Tables 5 and 6. Regresses each strategy's monthly net excess return on the CAPM, Fama-French three- and five-factor models, and FF5 plus momentum, over 1981–2023 and over the 2021–2023 extension window alone. Standard errors are Newey-West with six lags and no small-sample correction, computed by hand rather than via `sandwich`. Downloads the factor series on first run and caches them (see below). |
| `market_exposure.R` | The unconditional market-exposure figures quoted in Section 5.2 and in the Table 6 note. Portfolio-ML's market beta of 0.08, correlation of 0.11, t-statistic of 2.56, 1.3% share of return variance and 0.66 of 13.93 percentage points of return, plus 0.17 for Static-ML\*. Also shows why these differ from the partial coefficients in Table 6, since every other factor is itself short the market, and confirms the reconciliation and the Frisch-Waugh-Lovell equivalence. |
| `market_portfolio_alpha.R` | Why the Market portfolio carries a significantly negative multi-factor alpha, $-$1.88% under FF5 plus momentum against 0.02% under the CAPM. Rules out trading costs (a drag of 0.020% a year) and the size restriction, then isolates the NYSE-only listing screen, which excludes the Nasdaq large caps and leaves real tilts toward value, profitability and conservative investment that are not paid in large stocks. Decisively, against a size-matched benchmark the alpha is statistically zero, which needs no factor model at all. |

## Market return series (Section 3.4)

| Script | Produces |
|---|---|
| `market_return_validation.R` | Validates the derived value-weighted market return against the series the original authors distribute. Correlation 0.999 over 1981–2023, mean absolute difference 11.5 bp per month, 8.19% annualised against 7.99% published, and a wealth path at most 8.8% apart at the start of the out-of-sample period. This matters more than a benchmark comparison would suggest, because the investor's wealth path is compounded backwards along the market return and that path scales every strategy's transaction cost. Reads `Data/market_returns_jkp.csv`; see below. |

## Universe construction (Section 3.2)

| Script | Produces |
|---|---|
| `diag_d1_screens2.R` | The screen-attrition cascade over 1952–2020. Counts the investable universe surviving each screen in turn, establishing that the shortfall against the paper's reported universe originates in the source data rather than in the screening pipeline. |
| `diag_d1_2023.R` | The same cascade over 1952–2023. Source of the figures quoted in Section 3.2 (median 490, and the pre-screen NYSE pool of 1,121). |

## Wealth anchoring (Section 4.5)

| Script | Produces |
|---|---|
| `diag_d2_wealth.R` | Demonstrates that the investor's wealth path is anchored at the sample end and computed backwards, so extending the sample rescales the historical investor by a constant factor of 0.756094. Explains why figures for the same calendar window differ between the original and extended specifications. |
| `diag_d2_step1.R` | Supporting comparison of portfolio weights and universe composition between the two specifications, confirming that predictions and stock membership are identical while realised weights are not. |

## Other

| Script | Produces |
|---|---|
| `static_ml_clean.R` | Re-derives the plain Static-ML figures on the correct out-of-sample window (480 months), confirming the values reported in Tables 7 and 8. |
| `docx_to_md2.py` | Converts the working `.docx` draft to Markdown. Requires `python-docx`. The document must be closed in Word first, otherwise the file is locked. |

## Note on factor data

The five-factor and momentum series are downloaded from Kenneth French's data
library by `alpha_regression.R` on first run and cached to
`Data/ff5_mom_monthly.csv`. Both `alpha_regression.R` and `market_exposure.R`
read that cached file if it exists.

Keep it. French revises these series periodically, so a fresh download will not
necessarily reproduce the figures reported in the dissertation. The cached copy
is the vintage the reported results were computed from. Deleting it and
re-running is not a neutral operation.

## Note on the two market return files

`Data/market_returns.csv` and `Data/market_returns_jkp.csv` are different files
and must not be confused.

- `market_returns.csv` is **derived**, rebuilt from `Data/usa.csv` by
  `0 - Derive World and Market Returns.R`. This is the file the pipeline reads.
- `market_returns_jkp.csv` is the authors' own series, **downloaded by hand**
  from the Dropbox link in `hhag022-replication/README.md`. It covers all
  countries, 1926–2024. Nothing in the pipeline reads it; it exists only so that
  `market_return_validation.R` can check the derived series against it.

Do not save the downloaded file over `market_returns.csv`. Because the wealth
path is compounded backwards along this series, substituting it would change
the transaction cost of every strategy and so every figure in Tables 2 to 8.
