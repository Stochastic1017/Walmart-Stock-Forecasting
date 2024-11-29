# Modeling and Forecasting Walmart Stock Prices: A Comparative Analysis of ARMA and ARCH Approaches

## Introduction

Understanding stock price dynamics is crucial for informed investment decisions. This study models and forecasts Walmart Inc.'s (WMT) daily adjusted closing prices from *2020-01-01* to *2023-12-06* with the following objectives: 

 1. Compute log returns
 2. Find the optimal **ARIMA** and **GARCH** models to capture volatility clustering and conditional heteroskedasticity
 3. Fit an ensemble Validate models using AIC, BIC, and residual diagnostics
 4. Generate a 10-step ahead forecast to provide actionable insights for investors and risk managers by modeling and forecasting stock price movements.

Let $P_t$ be the price of an asset at time $t$, then the log returns is defined as:

$$r_t = \text{log} P_t - \text{log} P_{t-1}$$

From Figure 1, there seems to exist some volatility clusters that need to be addressed. Furthermore, the log return distribution seems to be Leptokurtic in nature, illustrating heavy tails and deviations from normality. We now proceed with rigorously testing for normality and stationarity.

## Fitting ARIMA Model

The `tseries` package in R allows us to use `auto.arima` which automatically selects the best model based on AIC/BIC.

| **Model**  | **ARIMA(0,0,1) with zero mean** |
|------------|----------------------------------|
| **MA1 Coefficient** | -0.0666                 |
| **Standard Error**  | 0.0312                  |
| **$\sigma^2$**      | 0.0002266               |
| **Log-Likelihood**  | 2694.43                 |
| **AIC**             | -5384.85                |
| **AICc**            | -5384.84                |
| **BIC**             | -5375.1                 |

**Table 1**: [Summary of fitted ARMA model.](https://github.com/Stochastic1017/Walmart-Stock-Forecasting/blob/main/R/Fit_ARMA.R)

The fitted `ARIMA(0,0,1)` model for Walmart Inc.'s log returns is summarized in Table 1, with a moving average coefficient of $-0.0666$ (standard error = $0.0312$), indicating weak short-term autocorrelation. The residual variance is estimated as $\sigma^2 = 0.0002266$, and model selection criteria, including AIC ($-5384.85$) and BIC ($-5375.1$), confirm its suitability.

In mathematical terms, we can write the **ARIMA** model as:

$$r_t = a_t - (-0.0666) \cdot a_{t-1}$$

where $a_t \sim N(0, 0.0312)$ and $\mathbb{E}[r_t] = 0$.

## Fitting **ARCH/GARCH** Models

The `fGarch` library in R allows us to fit various `GARCH` models and compare performances between the them. For the purposes of keeping the models simple and explainable, the focus is on `GARCH(1,0)` and `GARCH(1,1)`, while generalizing conditional distributions.

#### ARCH Model, GARCH(0,1)
| **Model**  | **GARCH(1,0), cond.dist="std"** |
|------------|----------------------------------|
| **mu**              | 6.090e-04              |
| **omega**           | 1.505e-04              |
| **alpha1**          | 2.988e-01              |
| **shape**           | 3.436e+00              |
| **Log-Likelihood**  | 2908.097               |
| **AIC**             | -5.987829              |
| **BIC**             | -5.967716              |
| **SIC**             | -5.987863              |
| **HQIC**            | -5.980173              |

#### GARCH Model, GARCH(1,1)
| **Model**  | **GARCH(1,1), cond.dist="std"** |
|------------|----------------------------------|
| **mu**              | 5.220e-04              |
| **omega**           | 1.560e-05              |
| **alpha1**          | 1.291e-01              |
| **beta1**           | 7.905e-01              |
| **shape**           | 4.062e+00              |
| **Log-Likelihood**  | 2929.534               |
| **AIC**             | -6.029967              |
| **BIC**             | -6.004826              |
| **SIC**             | -6.030020              |
| **HQIC**            | -6.020398              |

**Table 2**: [Summary of fitted ARCH and GARCH model.](https://github.com/Stochastic1017/Walmart-Stock-Forecasting/blob/main/R/Fit_ARCH_GARCH.R)


Table 2 summarizes the fitted `GARCH(1,0)` and `GARCH(1,1)` models using a Student-t conditional distribution. Although `GARCH(1,0)` achieves slightly lower AIC (-5.99 vs. -6.03) and BIC (-5.97 vs. -6.00), indicating a marginally better balance between model fit and complexity, we find that `GARCH(1,1)` achieves a higher log-likelihood (2929.534 vs. 2908.097) and captures long-term volatility persistence through the additional $\beta_1$ parameter, which is crucial for financial time series exhibiting volatility clustering. Therefore, we prefer `GARCH(1,1)` over `GARCH(1,0)`.

In mathematical terms, we can write the `GARCH(1,1)` model as:

$$a_t = \sigma_t \epsilon_t, \quad \sigma^2_t = (\text{1.560e-05}) + (\text{1.291e-01})a^2_{t-1} + (\text{7.905e-01})\sigma^2_{t-1}$$

where $\epsilon_t \sim \text{std}(0,1)$ with shape parameter 4.0624. The relatively large value of $\beta_1$ (0.7905) relative to $\alpha_1$ (0.1291) reflects the long memory in volatility, which is consistent with financial time series exhibiting volatility clustering.

## Fitting Ensemble `ARIMA(0,0,1) + GARCH(1,1)` Model

A preliminary `ARIMA(0,0,1) + GARCH(1,1)` model with a Student-t conditional distribution is fitted to the log returns to capture volatility clustering and heavy tails. We iteratively detect and remove influential points (standardized residuals exceeding a threshold of $\pm 3$) by fitting until the process continues until no new influential points are identified, or the changes in residual diagnostics become negligible.

| **Model**  | **ARIMA(0,0,1) + GARCH(1,1)**    |
|------------|-----------------------------------|
| **mu**              | 4.9198e-04              |
| **omega**           | 6.3505e-06              |
| **alpha1**          | 1.0224e-01              |
| **beta1**           | 8.5001e-01              |
| **shape**           | 1.0000e+01              |
| **Log-Likelihood**  | 2972.232                |
| **AIC**             | -6.246803               |
| **BIC**             | -6.221243               |
| **SIC**             | -6.246858               |
| **HQIC**            | -6.237065               |

**Table 3**: [Summary of fitted ARMA(0,0,1)+GARCH(1,1) model.](https://github.com/Stochastic1017/Walmart-Stock-Forecasting/blob/main/R/Fit_ARMA_and_GARCH.R)

There is a substantial increase in log-likelihood compared to individual `GARCH(1,1)` or `ARIMA(0,0,1)` models, with only slight increase in AIC and BIC. Furthermore, the statistically significant $\beta_1$ value (0.8501) indicates strong long-term persistence in volatility, while the moderate statistically significant $\alpha_1$ value (0.1022) captures short-term shocks effectively.

## Residual Diagnostics

Figure 2 show that the fitted ensemble model is satisfactory. The residuals over time are random around zero, the histogram aligns with a normal distribution, the Q-Q plot shows minimal deviation from normality, and the ACF plots confirm no significant autocorrelation in residuals, squared residuals, or absolute residuals, indicating no remaining structure or volatility clustering.

## 10-Day Forecast

Figure 3 allows us to compare our 10-day forecast with the true log returns and adjusted closing price. It is crucial to note that the model only fits data from *2020-01-01* to *2023-12-05*. The actual log returns from *2023-12-06* to *2023-12-15* is used to compare model performance on unseen data.

The forecasted log returns align well with the actual values, with all observations falling within the 95\% confidence interval. This indicates that the `ARIMA(0,0,1) + GARCH(1,1)` model effectively captures the short-term dynamics of the stock. However, the increasing width of the confidence interval for closing prices reflects the compounding effect of forecast uncertainty over time, a common challenge in financial modeling. 

Given the residual diagnostics affirmation that the `ARIMA(0,0,1) + GARCH(1,1)` model effectively captures the underlying structure of Walmart's stock returns, we can conclude that this is a good and effective model for forecasting.
