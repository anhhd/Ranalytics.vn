# Mô hình ARIMA

## Chuỗi thời gian dừng

Chuỗi thời gian dừng là yêu cầu bắt buộc để xây dựng mô hình ARIMA. 

Định nghĩa chuối thời gian dừng:
  
  - Giá trị trung bình không đổi theo thời gian 

![](Images/61-ts1.png)

- Phương sai không đổi theo thời gian (homoskedaticity)

![](Images/61-ts2.png)

- Covariance của chuỗi thời gian thứ i và (i+m) không đổi

![](Images/61-ts3.png)

## Mô hình ARIMA

ARIMA là viết tắt của `Auto-Regressive Integrated Moving Average`

$$ARIMA(p,d,q) = AR(p) + I(d) + MA(q)$$

Trong đó:

- p: Số bậc trong mô hình Auto-Regressive
- d: Số bậc trong mô hình Itegrated (số lần lấy $\delta$ để có chuỗi thời gian dừng)
- q: Số bậc trong mô hình Moving Agerage

**Integrated model**
  
  Gọi $Y_1,...,Y_t$ là chuỗi thời gian gốc. Ta có:
  
  - d=0: $y_t=Y_t$ 
  - d=1: $y_t=Y_t-Y_{t-1}$
  - d=2: $y_t=(Y_t-Y_{t-1})-(Y_{t-1}-Y_{t-2})$
  
  Với d=2, còn được gọi là "the first difference of the first difference". Sau khi được chuỗi dừng $y_t$, ta có thể dự báo mô hình ARIMA như sau:
  
  $$\hat{y}_t=\mu + \phi_1y_{t-1}+...+\phi_py_{t-p} + \theta_1\epsilon_{t-1}+...+\theta_q\epsilon_{t-q}$$
  
  **ACF vs. PACF**
  
  - ACF: Autocorrelation - đo correlation giữa các quan sát trong chuỗi
- PACF: Partial Auto Correlation - đo correlation giữa biến $Y_t$ và $Y_{t-k}$, loại bỏ các biến ở giữa chúng

**Kỹ thuật tạo chuỗi dừng**:
  
  - Với biến có var biến đổi: log
- Với biến có mean biến đổi: Sử dụng "Difference"

**Test chuỗi dừng**: Test ADF (Augmented Dickey-Fuller)

- $H_0$: Chuỗi không dừng (non-stationary)
- $H_1$: Chuỗi dừng (stationary)

**Lựa chọn tham số trong ARIMA**


+----------------+-----------------------------------------+---------------------+
|     Model      |                   ACF                   |        PACF         |
+================+=========================================+=====================+
|   ARIMA(p,d,0) | Giảm dần đều về 0                       | Giảm về 0 sau lag p |
+----------------+-----------------------------------------+---------------------+
| ARIMA(0,d,q)   | Giảm về 0 sau lag q   Giảm dần đều về 0 |                     |
+----------------+-----------------------------------------+---------------------+
| ARIMA(p,d,q)   | Giảm dần đều về 0                       | Giảm dần đều về 0   |
+----------------+-----------------------------------------+---------------------+



Xem xét ACF & PACF trong các chuỗi sau


```r
library(forecast)
library(tseries)
library(dplyr)
library(ggfortify)
ar1 <- arima.sim(list(ar=c(0.89)), n = 100)
ar2 <- arima.sim(list(ar=c(0.89, -0.4858)), n = 100)
ma1 <- arima.sim(n = 100, list(ma = c(-0.2279)))
ma2 <- arima.sim(n = 100, list(ma = c(-0.2279, 0.2488)))
arma <- arima.sim(n = 100, list(ar = c(0.8897, -0.4858), 
                                ma = c(-0.2279, 0.2488)))
#Tạo function
tseries.plot <- function(x){
  par(mfrow=c(1,2));
  Acf(x, col = "blue", main = paste(c("ACF plot")));
  Pacf(x, col = "red", main = paste(c("PACF plot")));
}
purrr::map(list(ar1, ar2, ma1, ma2, arma), tseries.plot)
```

![](61-arima_files/figure-html/unnamed-chunk-1-1.png)

![](61-arima_files/figure-html/unnamed-chunk-1-2.png)

![](61-arima_files/figure-html/unnamed-chunk-1-3.png)

![](61-arima_files/figure-html/unnamed-chunk-1-4.png)

![](61-arima_files/figure-html/unnamed-chunk-1-5.png)



```
## [[1]]
## 
## Partial autocorrelations of series 'x', by lag
## 
##      1      2      3      4      5      6      7      8      9     10 
##  0.840  0.051 -0.199 -0.014  0.078  0.057 -0.234 -0.004  0.011  0.014 
##     11     12     13     14     15     16     17     18     19     20 
##  0.032  0.084 -0.200 -0.138 -0.087 -0.004  0.097 -0.049  0.075  0.011 
## 
## [[2]]
## 
## Partial autocorrelations of series 'x', by lag
## 
##      1      2      3      4      5      6      7      8      9     10 
##  0.643 -0.498 -0.022  0.039 -0.060 -0.171  0.053  0.019 -0.072 -0.192 
##     11     12     13     14     15     16     17     18     19     20 
##  0.062 -0.021  0.086 -0.145 -0.057 -0.017  0.013 -0.039  0.040 -0.163 
## 
## [[3]]
## 
## Partial autocorrelations of series 'x', by lag
## 
##      1      2      3      4      5      6      7      8      9     10 
## -0.351 -0.237 -0.191 -0.118  0.078  0.210  0.004 -0.029 -0.110  0.013 
##     11     12     13     14     15     16     17     18     19     20 
## -0.040 -0.160 -0.032 -0.071 -0.086  0.066 -0.031 -0.112 -0.026  0.105 
## 
## [[4]]
## 
## Partial autocorrelations of series 'x', by lag
## 
##      1      2      3      4      5      6      7      8      9     10 
## -0.356  0.098  0.252 -0.143 -0.060  0.020  0.056  0.046 -0.006  0.088 
##     11     12     13     14     15     16     17     18     19     20 
##  0.028 -0.083 -0.075  0.062 -0.093  0.029 -0.037  0.039 -0.030 -0.089 
## 
## [[5]]
## 
## Partial autocorrelations of series 'x', by lag
## 
##      1      2      3      4      5      6      7      8      9     10 
##  0.602 -0.264 -0.008 -0.187  0.176  0.097 -0.198 -0.141  0.046 -0.086 
##     11     12     13     14     15     16     17     18     19     20 
##  0.005 -0.158 -0.021  0.034  0.010  0.016 -0.158  0.125  0.104 -0.034
```

## Ví dụ với R


```r
library(forecast)
library(tseries)
plot(Nile)
```

![](61-arima_files/figure-html/unnamed-chunk-2-1.png)



```r
#Tìm giá trị tối ưu của d để loại trend
ndiffs(Nile)
```

```
## [1] 1
```

```r
dNile <- diff(Nile, 1)
plot(dNile)
```

![](61-arima_files/figure-html/unnamed-chunk-2-2.png)



```r
adf.test(dNile)
```

```
## Warning in adf.test(dNile): p-value smaller than printed p-value
```

```
## 
## 	Augmented Dickey-Fuller Test
## 
## data:  dNile
## Dickey-Fuller = -6.5924, Lag order = 4, p-value = 0.01
## alternative hypothesis: stationary
```

```r
#Xem mô hình
par(mfrow=c(1,2))
Acf(dNile)
Pacf(dNile)
```

![](61-arima_files/figure-html/unnamed-chunk-2-3.png)



```r
#ACF và PACF đưa ra gợi ý mô hình ARIMA(0,1,1): ACF giảm về 0 sau lag 1, PACF giảm dần về 0
fit <- arima(Nile, order = c(0,1,1))
fit
```

```
## 
## Call:
## arima(x = Nile, order = c(0, 1, 1))
## 
## Coefficients:
##           ma1
##       -0.7329
## s.e.   0.1143
## 
## sigma^2 estimated as 20600:  log likelihood = -632.55,  aic = 1269.09
```

```r
accuracy(fit)
```

```
##                    ME     RMSE      MAE       MPE     MAPE     MASE
## Training set -11.9358 142.8071 112.1752 -3.574702 12.93594 0.841824
##                   ACF1
## Training set 0.1153593
```

```r
#Đánh giá mô hình
par(mfrow=c(1,1))
names(fit)
```

```
##  [1] "coef"      "sigma2"    "var.coef"  "mask"      "loglik"   
##  [6] "aic"       "arma"      "residuals" "call"      "series"   
## [11] "code"      "n.cond"    "nobs"      "model"
```

```r
qqnorm(fit$residuals)
qqline(fit$residuals)
```

![](61-arima_files/figure-html/unnamed-chunk-2-4.png)



```r
Box.test(fit$residuals, type = "Ljung-Box")
```

```
## 
## 	Box-Ljung test
## 
## data:  fit$residuals
## X-squared = 1.3711, df = 1, p-value = 0.2416
```

```r
#H0: Autocorrelation của residual bằng 0
#H1: Autocorrelation của residual khác 0

#Dự báo
forecast(fit, 3)
```

```
##      Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
## 1971       798.3673 614.4307 982.3040 517.0605 1079.674
## 1972       798.3673 607.9845 988.7502 507.2019 1089.533
## 1973       798.3673 601.7495 994.9851 497.6663 1099.068
```

```r
autoplot(forecast(fit,3)) + theme_classic()
```

```
## Warning: package 'bindrcpp' was built under R version 3.4.4
```

![](61-arima_files/figure-html/unnamed-chunk-2-5.png)



```r
#Tự động chọn mô hình
auto.arima(Nile)
```

```
## Series: Nile 
## ARIMA(1,1,1)                    
## 
## Coefficients:
##          ar1      ma1
##       0.2544  -0.8741
## s.e.  0.1194   0.0605
## 
## sigma^2 estimated as 20177:  log likelihood=-630.63
## AIC=1267.25   AICc=1267.51   BIC=1275.04
```

Mô hình trên cho thấy ARIMA(0,1,1) phản ánh tốt số lượng:
  
- Residual có phân phối chuẩn
- Residual có autocorrelation bằng 0 (p value > 0.24)
