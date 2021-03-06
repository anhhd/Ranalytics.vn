# Quản lý kết quả phân tích từ nhiều mô hình




Khi phân tích nhiều mô hình cùng lúc trong R, output từ các mô hình thường được lưu ở dạng list và rất khó kết hợp với nhau. Nhiều trường hợp, ta cần xây dựng cùng lúc nhiều mô hình và tổng hợp cùng lúc kết quả từ các mô hình này. `Broom` cho phép làm sạch các output của mô hình.

Các hàm giúp làm sạch output:

- **tidy**: Data frame cho phép tổng hợp kết quả của các mô hình, bao gồm coefficient, p-value
- **augment**: Thêm cột vào tập dữ liệu được phân tích
- **glance**: Tổng quan các chỉ số của nhiều mô hình


```r
library(tidyverse)
library(broom)

lmfit <- lm(mpg ~ wt, mtcars)
lmfit %>% summary
```

```
## 
## Call:
## lm(formula = mpg ~ wt, data = mtcars)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.5432 -2.3647 -0.1252  1.4096  6.8727 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  37.2851     1.8776  19.858  < 2e-16 ***
## wt           -5.3445     0.5591  -9.559 1.29e-10 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.046 on 30 degrees of freedom
## Multiple R-squared:  0.7528,	Adjusted R-squared:  0.7446 
## F-statistic: 91.38 on 1 and 30 DF,  p-value: 1.294e-10
```

```r
#Tidy toàn bộ mô hình
lmfit %>% tidy
```

```
## # A tibble: 2 x 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)    37.3      1.88      19.9  8.24e-19
## 2 wt             -5.34     0.559     -9.56 1.29e-10
```

```r
#Nhìn tổng quan mô hình
lmfit %>% glance
```

```
## # A tibble: 1 x 11
##   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
##       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
## 1     0.753         0.745  3.05      91.4 1.29e-10     2  -80.0  166.  170.
## # ... with 2 more variables: deviance <dbl>, df.residual <int>
```

```r
#Nối kết quả với mô hình
lmfit %>% augment %>% head
```

```
## # A tibble: 6 x 10
##   .rownames   mpg    wt .fitted .se.fit .resid   .hat .sigma .cooksd
##   <chr>     <dbl> <dbl>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
## 1 Mazda RX4  21    2.62    23.3   0.634 -2.28  0.0433   3.07 1.33e-2
## 2 Mazda RX~  21    2.88    21.9   0.571 -0.920 0.0352   3.09 1.72e-3
## 3 Datsun 7~  22.8  2.32    24.9   0.736 -2.09  0.0584   3.07 1.54e-2
## 4 Hornet 4~  21.4  3.22    20.1   0.538  1.30  0.0313   3.09 3.02e-3
## 5 Hornet S~  18.7  3.44    18.9   0.553 -0.200 0.0329   3.10 7.60e-5
## 6 Valiant    18.1  3.46    18.8   0.555 -0.693 0.0332   3.10 9.21e-4
## # ... with 1 more variable: .std.resid <dbl>
```

Khi xây dựng mô hình, ta có thể kết hợp `broom` và `dplyr` như sau.


```r
mtcars %>% group_by(cyl) %>% 
  do(lm(mpg ~ disp, data=.) %>% tidy) 
```

```
## # A tibble: 6 x 6
## # Groups:   cyl [3]
##     cyl term        estimate std.error statistic    p.value
##   <dbl> <chr>          <dbl>     <dbl>     <dbl>      <dbl>
## 1     4 (Intercept) 40.9       3.59       11.4   0.00000120
## 2     4 disp        -0.135     0.0332     -4.07  0.00278   
## 3     6 (Intercept) 19.1       2.91        6.55  0.00124   
## 4     6 disp         0.00361   0.0156      0.232 0.826     
## 5     8 (Intercept) 22.0       3.35        6.59  0.0000259 
## 6     8 disp        -0.0196    0.00932    -2.11  0.0568
```

Tuy nhiên, khi dữ liệu có quá nhiều, ta có thể sử dụng `purrr` thay cho `broom` như sau



```r
library(purrr)
library(broom)
mtcars %>%
  split(.$cyl) %>%
  map(~lm(mpg ~ wt, data = .)) %>%
  map(summary)  %>% 
  map_df(tidy)
```

```
## # A tibble: 6 x 5
##   term        estimate std.error statistic    p.value
##   <chr>          <dbl>     <dbl>     <dbl>      <dbl>
## 1 (Intercept)    39.6      4.35       9.10 0.00000777
## 2 wt             -5.65     1.85      -3.05 0.0137    
## 3 (Intercept)    28.4      4.18       6.79 0.00105   
## 4 wt             -2.78     1.33      -2.08 0.0918    
## 5 (Intercept)    23.9      3.01       7.94 0.00000405
## 6 wt             -2.19     0.739     -2.97 0.0118
```

```r
#Version với broom
mtcars %>% 
  group_by(cyl) %>% 
  do(lm(mpg ~ wt, data = .) %>% tidy)  
```

```
## # A tibble: 6 x 6
## # Groups:   cyl [3]
##     cyl term        estimate std.error statistic    p.value
##   <dbl> <chr>          <dbl>     <dbl>     <dbl>      <dbl>
## 1     4 (Intercept)    39.6      4.35       9.10 0.00000777
## 2     4 wt             -5.65     1.85      -3.05 0.0137    
## 3     6 (Intercept)    28.4      4.18       6.79 0.00105   
## 4     6 wt             -2.78     1.33      -2.08 0.0918    
## 5     8 (Intercept)    23.9      3.01       7.94 0.00000405
## 6     8 wt             -2.19     0.739     -2.97 0.0118
```

Nhóm hàm của `broom` có thể sử dụng trong rất nhiều nhóm mô hình, bao gồm anova, t.test, quantile regression, hồi quy tuyến tính. Việc kết hợp và sử dụng nhóm hàm này sẽ giúp ích rất nhiều trong việc phân tích khám phá dữ liệu
