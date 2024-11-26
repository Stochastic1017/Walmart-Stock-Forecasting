
# Clean the environment
rm(list = ls()) 

# Load required libraries
if (!require("fGarch")) install.packages("fGarch", dependencies = TRUE)
if (!require("knitr")) install.packages("knitr", dependencies = TRUE)
if (!require("kableExtra")) install.packages("kableExtra", dependencies = TRUE)

library(fGarch)
library(knitr)
library(kableExtra)

# Load csv
WMT_df <- na.omit(read.csv("Walmart_AdjPrice.csv"))

# Convert Date column to Date type
WMT_df$Date <- as.Date(WMT_df$Date, format = "%Y-%m-%d")

# Fit garch(1, 0) model to log returns
fit_arch <- garchFit(~ garch(1, 0), data = WMT_df$LogReturns, cond.dist = "norm")
print(summary(fit_arch))

# Title:
#   GARCH Modelling 
# 
# Call:
#   garchFit(formula = ~garch(1, 0), data = WMT_df$LogReturns, cond.dist = "std") 
# 
# Mean and Variance Equation:
#   data ~ garch(1, 0)
# <environment: 0x5faa9d9eb748>
#   [data = WMT_df$LogReturns]
# 
# Conditional Distribution:
#   std 
# 
# Coefficient(s):
#   mu       omega      alpha1       shape  
# 0.00060905  0.00015052  0.29883056  3.43559886  
# 
# Std. Errors:
#   based on Hessian 
# 
# Error Analysis:
#   Estimate  Std. Error  t value Pr(>|t|)    
# mu     6.090e-04   3.326e-04    1.831 0.067090 .  
# omega  1.505e-04   2.019e-05    7.454 9.06e-14 ***
#   alpha1 2.988e-01   8.543e-02    3.498 0.000469 ***
#   shape  3.436e+00   3.925e-01    8.753  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:
#   2908.097    normalized:  2.998038 
# 
# Description:
#   Tue Nov 26 15:27:34 2024 by user:  
#   
#   
#   Standardised Residuals Tests:
#   Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  8428.0097469 0.0000000
# Shapiro-Wilk Test  R    W         0.8835441 0.0000000
# Ljung-Box Test     R    Q(10)    10.6330501 0.3868118
# Ljung-Box Test     R    Q(15)    14.9891482 0.4521992
# Ljung-Box Test     R    Q(20)    17.3231831 0.6318993
# Ljung-Box Test     R^2  Q(10)     6.0589807 0.8102838
# Ljung-Box Test     R^2  Q(15)     9.2123320 0.8661560
# Ljung-Box Test     R^2  Q(20)    10.1141536 0.9660545
# LM Arch Test       R    TR^2      6.2949686 0.9004887
# 
# Information Criterion Statistics:
#   AIC       BIC       SIC      HQIC 
# -5.987829 -5.967716 -5.987863 -5.980173 

# Fit garch(1, 1) model to log returns
fit_garch <- garchFit(~ garch(1, 1), data = WMT_df$LogReturns, cond.dist = "norm")
print(summary(fit_garch))

# Title:
#   GARCH Modelling 
# 
# Call:
#   garchFit(formula = ~garch(1, 1), data = WMT_df$LogReturns, cond.dist = "std") 
# 
# Mean and Variance Equation:
#   data ~ garch(1, 1)
# <environment: 0x5faaa45fe850>
#   [data = WMT_df$LogReturns]
# 
# Conditional Distribution:
#   std 
# 
# Coefficient(s):
#   mu       omega      alpha1       beta1       shape  
# 0.00052202  0.00001560  0.12912833  0.79048538  4.06182415  
# 
# Std. Errors:
#   based on Hessian 
# 
# Error Analysis:
#   Estimate  Std. Error  t value Pr(>|t|)    
# mu     5.220e-04   3.236e-04    1.613  0.10672    
# omega  1.560e-05   6.808e-06    2.292  0.02193 *  
#   alpha1 1.291e-01   4.705e-02    2.744  0.00606 ** 
#   beta1  7.905e-01   6.870e-02   11.507  < 2e-16 ***
#   shape  4.062e+00   5.057e-01    8.032 8.88e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Log Likelihood:
#   2929.534    normalized:  3.020138 
# 
# Description:
#   Tue Nov 26 15:25:52 2024 by user:  
#   
#   
#   Standardised Residuals Tests:
#   Statistic   p-Value
# Jarque-Bera Test   R    Chi^2  1.332671e+04 0.0000000
# Shapiro-Wilk Test  R    W      8.693276e-01 0.0000000
# Ljung-Box Test     R    Q(10)  6.444710e+00 0.7766203
# Ljung-Box Test     R    Q(15)  7.411283e+00 0.9452166
# Ljung-Box Test     R    Q(20)  1.119994e+01 0.9408718
# Ljung-Box Test     R^2  Q(10)  1.153811e+00 0.9996696
# Ljung-Box Test     R^2  Q(15)  1.830576e+00 0.9999836
# Ljung-Box Test     R^2  Q(20)  2.085511e+00 0.9999998
# LM Arch Test       R    TR^2   1.372330e+00 0.9999192
# 
# Information Criterion Statistics:
#   AIC       BIC       SIC      HQIC 
# -6.029967 -6.004826 -6.030020 -6.020398 
