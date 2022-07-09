Inverse Covariance Clustering on Implied Stochastic Volatility Models
================
Saef, Danial; Wang, Yuanrong
4/27/2022

This library serves as a companion to the publication “Clustering the
implied volatility surface of Bitcoin options over time”. However it can
also be used independently for clustering high dimensional datasets and
fitting an implied stochastic volatility model.

# 1 Methodology

The increasing adoption of Digital Assets (DAs), such as Bitcoin (BTC),
rises the need for accurate option pricing models. Yet, existing
methodologies fail to cope with the volatile nature of the emerging DAs.
Many models have been proposed to address the unorthodox market dynamics
and frequent disruptions in the microstructure caused by the
non-stationarity, and peculiar statistics, in DA markets. However, they
are either prone to the curse of dimensionality, as additional
complexity is required to employ traditional theories, or they overfit
historical patterns that may never repeat.

Instead, we leverage recent advances in market regime (MR) clustering
with the Implied Stochastic Volatility Model (ISVM). Time-regime
clustering is a temporal clustering method, that clusters the historic
evolution of a market into different volatility periods accounting for
non-stationarity. ISVM can incorporate investor expectations in each of
the sentiment-driven periods by using implied volatility (IV) data.

In this publication, we applied this integrated time-regime clustering
and ISVM method (termed MR-ISVM) to high-frequency data on BTC options
at the popular trading platform Deribit. We demonstrate that MR-ISVM
contributes to overcome the burden of complex adaption to jumps in
higher order characteristics of option pricing models. This allows us to
price the market based on the expectations of its participants in an
adaptive fashion.

For the exact methodology we refer to the methodology section of this
paper, as well as @massara_learning_2019 @procacci_forecasting_2019,
@wang_dynamic_2022 for ICC, as well as @ait-sahalia_implied_2021 for the
ISVM methodology. Similar implementations can be found in those original
works, however not for tbe MR-ISVM approach.

# 2 Usage

## 2.1 Installing

The usage is pretty simple. First, install the package with `devtools`.

``` r
library(devtools)
install_github("YalDan/icc.isvm")
library(icc.isvm)
```

## 2.2 Running the model

Now we can just load a suitable dataset and run the `fit_ICC_ISVM`
function. Note that as of now, the bootstrapping estimation of ISVM
requires the function `mclapply` from the `parallel` package. This
unfortunately won’t work on Windows machines. A Windows friendly
implementation is however planned in the future, as well as the option
to deactivate parallel computing. ICC can also be calculated in parallel
setting `parallel_ICC = TRUE`, however this is still experimental and it
is recommended to leave it at default. Inside the function
`fit_ICC_ISVM` is also a minimalistic example on how to run ICC and ISVM
individually.

``` r
DT_BTC_deribit <- fread("DT_sample.csv")
DT_BTC_deribit_sample <- fit_ICC_ISVM(DT_full = DT_BTC_deribit,
                              last_date = as.Date("2022-01-28"),
                              K = 2,
                              gamma = 0.5)
```

If needed, the resulting plots from the model can be saved by using the
make_plots function.

``` r
make_plots(ICC_ISVM_list_K2, SAVE_PLOTS = FALSE)
```

# 3 References