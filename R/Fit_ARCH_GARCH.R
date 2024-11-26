
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
fit_arch <- garchFit(~ garch(1, 0), data = WMT_df$LogReturns, cond.dist = "std")
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
# 
# 
# Series Initialization:
#   ARMA Model:                arma
# Formula Mean:              ~ arma(0, 0)
# GARCH Model:               garch
# Formula Variance:          ~ garch(1, 1)
# ARMA Order:                0 0
# Max ARMA Order:            0
# GARCH Order:               1 1
# Max GARCH Order:           1
# Maximum Order:             1
# Conditional Dist:          std
# h.start:                   2
# llh.start:                 1
# Length of Series:          970
# Recursion Init:            mci
# Series Scale:              0.01508381
# 
# Parameter Initialization:
#   Initial Parameters:          $params
# Limits of Transformations:   $U, $V
# Which Parameters are Fixed?  $includes
# Parameter Matrix:
#   U           V     params includes
# mu     -0.24252318   0.2425232 0.02425232     TRUE
# omega   0.00000100 100.0000000 0.10000000     TRUE
# alpha1  0.00000001   1.0000000 0.10000000     TRUE
# gamma1 -0.99999999   1.0000000 0.10000000    FALSE
# beta1   0.00000001   1.0000000 0.80000000     TRUE
# delta   0.00000000   2.0000000 2.00000000    FALSE
# skew    0.10000000  10.0000000 1.00000000    FALSE
# shape   1.00000000  10.0000000 4.00000000     TRUE
# Index List of Parameters to be Optimized:
#   mu  omega alpha1  beta1  shape 
# 1      2      3      5      8 
# Persistence:                  0.9 
# 
# 
# --- START OF TRACE ---
#   Selected Algorithm: nlminb 
# 
# R coded nlminb Solver: 
#   
#   0:     1144.0365: 0.0242523 0.100000 0.100000 0.800000  4.00000
# 1:     1140.8189: 0.0242527 0.0907284 0.0976184 0.793654  3.99981
# 2:     1139.9958: 0.0242539 0.0801877 0.100110 0.789829  3.99972
# 3:     1139.4862: 0.0242560 0.0801669 0.110852 0.793895  3.99989
# 4:     1139.1930: 0.0242596 0.0704237 0.116546 0.791757  3.99993
# 5:     1138.9625: 0.0242735 0.0694686 0.127116 0.796087  4.00041
# 6:     1138.9471: 0.0243531 0.0662979 0.134805 0.789117  4.00226
# 7:     1138.9036: 0.0244474 0.0722051 0.138167 0.780925  4.00413
# 8:     1138.8905: 0.0246555 0.0706010 0.133614 0.785691  4.00765
# 9:     1138.8901: 0.0246560 0.0707493 0.133643 0.785784  4.00766
# 10:     1138.8899: 0.0246617 0.0707840 0.133276 0.785777  4.00776
# 11:     1138.8895: 0.0246705 0.0708291 0.133249 0.785972  4.00792
# 12:     1138.8814: 0.0251670 0.0671673 0.129505 0.793530  4.01699
# 13:     1138.8050: 0.0315006 0.0681307 0.132208 0.786055  4.12987
# 14:     1138.7985: 0.0372908 0.0698496 0.131679 0.789030  3.97496
# 15:     1138.7771: 0.0346322 0.0686342 0.127097 0.791620  4.07268
# 16:     1138.7760: 0.0343408 0.0685777 0.130443 0.789571  4.06513
# 17:     1138.7751: 0.0346158 0.0685543 0.129147 0.790478  4.06150
# 18:     1138.7751: 0.0346092 0.0685688 0.129127 0.790487  4.06182
# 19:     1138.7751: 0.0346079 0.0685661 0.129128 0.790485  4.06182
# 
# Final Estimate of the Negative LLH:
#   LLH:  -2929.534    norm LLH:  -3.020138 
# mu        omega       alpha1        beta1        shape 
# 5.220192e-04 1.560025e-05 1.291283e-01 7.904854e-01 4.061824e+00 
# 
# R-optimhess Difference Approximated Hessian Matrix:
#   mu         omega        alpha1         beta1         shape
# mu     -9605288.786       2119893 -3.380175e+03  1.421115e+03 -1.798020e+02
# omega   2119893.004 -347391089679 -2.992451e+07 -4.841374e+07 -1.371940e+06
# alpha1    -3380.175     -29924505 -4.599349e+03 -5.342390e+03 -1.564956e+02
# beta1      1421.115     -48413741 -5.342390e+03 -7.640047e+03 -2.119294e+02
# shape      -179.802      -1371940 -1.564956e+02 -2.119294e+02 -1.006576e+01
# attr(,"time")
# Time difference of 0.03225803 secs
# 
# --- END OF TRACE ---
#   
#   
#   Time to Estimate Parameters:
#   Time difference of 0.126307 secs

# Fit garch(1, 1) model to log returns
fit_garch <- garchFit(~ garch(1, 1), data = WMT_df$LogReturns, cond.dist = "std")
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
