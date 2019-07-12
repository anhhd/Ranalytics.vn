# Mô hình OLS

## Giới thiệu




Với mô hình hồi quy tuyến tính, ví dụ, tìm mối quan hệ giữa giá nhà và diện tích, ý tưởng để xây dựng mô hình là tìm một tổ hợp giữa $\theta_0$ và $\theta_1$ sao cho khoảng cách giữa mô hình tìm được và các quan sát là nhỏ nhất.

![](41-mo-hinh-ols_files/figure-html/unnamed-chunk-2-1.png)

Để tìm ra được tổ hợp tham số $\theta$, mục tiêu của thuật toán là tối ưu hóa tổng khoảng cách giữa các điểm dự báo và các điểm thực tế. Nói cách khác, ta cần tối ưu hàm sau.

$$\underset{\theta_0, \theta_1}{\text{minimize}}\sum_{i=1}^m(h_\theta(x^{(i)}) - y^{(i)})^2$$

Trong đó:

- $(i)$ là thứ tự các quan sát trong tập train
- $m$ là số lượng quan sát
- $h_\theta$ là mô hình ước lượng mối quan hệ giữa x và y
- $y^{(i)}$ là quan sát thực tế của biến cần dự báo
- $x^{(i)}$ là các quan sát thực tế của biến inputs

Trong thực tế, để dễ dàng hơn trong việc tính toán, ta sẽ tìm giá trị nhỏ nhất của trung bình của bình phương độ lêch. Ta sẽ thay đổi thành hàm sau.

$$J(\theta_0. \theta_1) = \underset{\theta_0, \theta_1}{\text{minimize}}\frac{1}{2m}\sum_{i=1}^m(h_\theta(x^{(i)}) - y^{(i)})^2$$

$J(\theta_0. \theta_1)$ được gọi là `cost function`. Về mặt bản chất, `cost function` cho phép chúng ta biết được *chất lượng* của mô hình so với thực tế. Thông thường, giá trị của hàm này cang thấp, chất lượng mô hình càng tốt. 


<img src="41-mo-hinh-ols_files/figure-html/unnamed-chunk-3-1.png" width="672" />


## Xây dựng mô hình cơ bản

Để xây dựng mô hình, ta sử dụng tập dữ liệu phân tích về doanh số bán hàng với quảng cáo. 


```r
# Packages
library(tidyverse)  
library(modelr)     
library(broom)    
library(ISLR)

# Load data (remove row numbers included as X1 variable)
#advertising <- read_csv("http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv") %>%
#  select(-X1) %>% 
#  rename_all(tolower)

advertising <- read_csv("data/Advertising.csv") %>%
 select(-X1) %>%
 rename_all(tolower)

advertising %>% head
```

```
## # A tibble: 6 x 4
##      tv radio newspaper sales
##   <dbl> <dbl>     <dbl> <dbl>
## 1 230.   37.8      69.2  22.1
## 2  44.5  39.3      45.1  10.4
## 3  17.2  45.9      69.3   9.3
## 4 152.   41.3      58.5  18.5
## 5 181.   10.8      58.4  12.9
## 6   8.7  48.9      75     7.2
```

Trong tập dữ liệu trên, `sales` là doanh số bán hàng (ngàn sản phẩm), các biến khác là chi tiêu marketing theo kênh (ngàn USD). Để xây dựng mô hình, ta cũng chia dữ liệu thành hai phần: `train` và `test`.


```r
set.seed(123)
train <- sample_frac(advertising,size = 0.6)
test <- advertising %>% anti_join(train)
```

Ta cũng có thể tạo train và test theo cách khác như sau


```r
# Cách khác
train <- sample <- sample(c(TRUE, FALSE), 
                          nrow(advertising), 
                          replace = T, prob = c(0.6,0.4))
train <- advertising[sample, ]
test <- advertising[!sample, ]
```

### Mô hình hồi quy đơn biến

Mô hình hồi quy đơn biến là mô hình chỉ có 1 biến X. Trong ví dụ này, ta sẽ xây dựng mô hình mối quan hệ giữa doanh số bán hàng và ngân sách được dành cho quảng cáo.

$$ Y = \beta_0 + \beta_1X + \epsilon \tag{1}$$

Trong đó:

- $Y$ là biến *sales*
- $X$ là biến *TV advertising budget*
- $\beta_0$ là hệ số tự do (intercept)
- $\beta_1$ là hệ số tương quan hay hệ số góc (độ dốc của đường hồi quy tuyến tính) 
- $\epsilon$ là sai số của mô hình - sai số này thường được giả định là có giá trị trung bình bằng 0 và tuân theo phân phối chuẩn.

Để xây dựng mô hình, ta thực hiện như sau.


```r
model1 <- lm(sales ~ tv, data = train)
model1 %>% summary
```

```
## 
## Call:
## lm(formula = sales ~ tv, data = train)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.0398 -2.0282 -0.1357  2.2867  7.4639 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 7.066290   0.626365   11.28   <2e-16 ***
## tv          0.046164   0.003588   12.87   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.331 on 118 degrees of freedom
## Multiple R-squared:  0.5838,	Adjusted R-squared:  0.5803 
## F-statistic: 165.5 on 1 and 118 DF,  p-value: < 2.2e-16
```


```r
train %>% 
  mutate(sale_fit = predict(model1, train)) %>% 
  sample_frac(0.4) %>% 
  ggplot(aes(tv, sales)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_line(aes(x = tv, y = sale_fit), col = "darkred", size = 1) +
  geom_segment(aes(x = tv, xend = tv, y = sales, yend = sale_fit)) +
  theme_bw() +
  labs(title = "Sales vs. TV marketing budget") +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))
```

![](41-mo-hinh-ols_files/figure-html/unnamed-chunk-8-1.png)

### Diễn giải mô hình


```r
result <- model1 %>% tidy
result
```

```
## # A tibble: 2 x 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)   7.07     0.626        11.3 1.80e-20
## 2 tv            0.0462   0.00359      12.9 3.28e-24
```

Hàm `tidy` trong `broom` cho phép làm sạch kết quả mô hình hồi quy và chuyển dữ liệu thành dạng bảng `dataframe`. Mối quan hệ giữa `sale` và `marketing` được thể hiện như sau.

`sales` = 7.0662898 + 0.0461636 * `tivi marketing budget`

Như vậy, nếu marketing tăng 1 đơn vị (1000 USD), doanh số sẽ tăng 46.1636238 sản phẩm

**Đánh giá mức độ ý nghĩa của hệ số góc**

$$SE(\beta_0)^2 = \sigma^2\bigg[\frac{1}{n}+\frac{\bar{x}^2}{\sum^n_{i=1}(x_i - \bar{x})^2} \bigg], \quad SE(\beta_1)^2 = \frac{\sigma^2}{\sum^n_{i=1}(x_i - \bar{x})^2}   \tag{3} $$

trong đó $$\sigma^2 = Var(\epsilon)$$. 

Việc tính toán $SE$ cho phép ta tính được mức độ tinh cậy 95% của hệ số $\beta$ như sau.

$$ \beta \pm 2 \cdot SE(\beta)  \tag{4}$$

Trong R, để tìm ra mức độ tin cậy 95% của hệ số $\beta$, ta có thể làm như sau.


```r
confint(model1)
```

```
##                  2.5 %     97.5 %
## (Intercept) 5.82591632 8.30666330
## tv          0.03905843 0.05326882
```

Như vậy, ta thấy độ tin cậy 95% của hệ số $\beta_1$ khác 0. Ta có thể kiểm tra dựa trên `p-value` của mô hình đều nhỏ hơn `0.05`


```r
model1 %>% tidy
```

```
## # A tibble: 2 x 5
##   term        estimate std.error statistic  p.value
##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
## 1 (Intercept)   7.07     0.626        11.3 1.80e-20
## 2 tv            0.0462   0.00359      12.9 3.28e-24
```

## Đánh giá chất lượng mô hình

Để đánh giá chất lượng mô hình, ta sẽ quan tâm đến 3 chỉ số sau:

1. Sai số của mô hình
2. *R* bình phương ($R^2$)
3. F-statistic

### Sai số của mô hình

$$ RSE = \sqrt{\frac{1}{n-2}\sum^n_{i=1}(y_i - \hat{y}_i)^2} \tag{6}$$


```r
sigma(model1)
```

```
## [1] 3.331281
```

Với sai số như trên, ta có thể nói rằng, doanh số thực tế trên thị trường sẽ có sai số so với dự báo 1 khoảng 3.3. Sai số tương đối của mô hình so với thực tế sẽ là


```r
sigma(model1)/mean(train$sales)
```

```
## [1] 0.2360657
```

Như vậy, sai số của mô hình so với thực tế sẽ là 23.6%.

### R bình phương $R^2$

Chỉ số RSE cho ta biết được sai số của mô hình khi dự báo. Bên cạnh đó, 1 chỉ số thường xuyên được sử dụng là chỉ số $R^2$. Chỉ số này đo lường tỷ lệ biến động của biến Y được dự báo qua việc sử dụng mô hình (proportion of variance explained). Nếu không sử dụng mô hình, ta có thể dự báo giá trị của Y bằng giá trị trung bình. 

- Tổng bình phương các sai số giữa giá trị thực tế và giá trị trung bình được gọi là độ biến động của dữ liệu (TSS - Total Sum of Square)
- Tổng bình phương các sai số giữa giá trị dự báo và giá trị thực tế được gọi là sai số của mô hình (RSS - Residual Sum of Square)
- Tổng bình phương các sai số giữa giá trị dự báo và giá trị trung bình thực tế được gọi là tổng biến động được giải thích qua mô hình (ESS - Explained Sum of Square)

$$ R^2 = 1 - \frac{RSS}{TSS}= 1 - \frac{\sum^n_{i=1}(y_i-\hat{y}_i)^2}{\sum^n_{i=1}(y_i-\bar{y}_i)^2} \tag{7}$$



```r
rsquare(model1, data = train)
```

```
## [1] 0.58383
```

Với kết quả trên, mô hình giải thích được 58.3829997% độ biến động của dữ liệu thực tế.

![](Images/ols-r2.png)


### F statistic

Chỉ số thống kê F được tính như sau.

$$F = \frac{(TSS-RSS)/p}{RSS/(n-p-1)} \tag{8} $$

Chỉ số này được sử dụng để kiểm định các hệ số trong mô hình có thực sự khác 0.

$H_0$: Tất cả các hệ số $\beta$ trong mô hình đều bằng không
$H_1$: Có ít nhất một hệ số $\beta$ trong mô hình khác không

Trong mô hình trên, giá trị `p-value` của F-statistic rất thấp, như vậy, có ít nhất một hệ số trong mô hình khác không.

## Dự báo

Để dự báo kết quả mô hình, ta có thể dùng hàm `predict` trong R như sau.


```r
# Model result
result <- test %>% 
  select(tv, sales) %>% 
  mutate(predict = predict(model1, test)) %>% 
  mutate(error = predict - sales)

sqrt(sum(result$error^2)/nrow(result))
```

```
## [1] 3.165155
```

```r
# Cách hai
modelr::rmse(model1, test)
```

```
## [1] 3.165155
```

**Vẽ kết quả mô hình với dự báo**


```r
result %>% 
  select(-error) %>% 
  rename(predicted_sales = predict,
         real_sales = sales) %>% 
  gather(key = "type", value = "value", -tv) %>% 
  ggplot(aes(tv, value)) +
  geom_point(aes(col = type), alpha = 0.5) +
  geom_line(aes(col = type), alpha = 0.6) +
  theme_bw() +
  theme(legend.position = "top") +
  labs(title = "Sales vs. TV marketing budget",
       y = "Sales",
       x = "TV budget") +
  scale_color_manual(values = c("darkred", "darkblue")) +
  theme(panel.border = element_blank(), 
        axis.line = element_line(colour = "black"))
```

<img src="41-mo-hinh-ols_files/figure-html/unnamed-chunk-16-1.png" width="672" />


## Thuật toán tối ưu với gradient decent 

Ở phần trước, ta đã học cách áp dụng mô hình hồi quy đơn giản trong việc dự báo các biến liên tục. Ở phần này, ta sẽ tìm hiểu thêm về cách sử dụng học máy trong việc xây dựng mô hình hồi quy tuyến tính.

Việc ưu hóa hàm chi phí với các tham số khác nhau là vấn đề cốt lõi của bất kỳ thuật toán machine learning nào. Tuy nhiên làm thế nào để tối ưu được, ta cần phải áp dụng các thuật toán tối ưu. Trong đó, quan trọng và phổ biến nhất là thuật toan `gradient descent`.

Để tối ưu hóa *cost function*, ta có thể thay đổi các tổ hợp giá trị của $\theta$ cho đến khi tìm được tổ hợp mà tại đó, *cost function* đạt giá trị nhỏ nhất. Tuy nhiên, với trường hợp mô hình nhiều biến, ta sử dụng thuật toán *gradient decient* để tìm tập $\theta$ mà tại đó, cost function đạt giá trị nhỏ nhất.

Quá trình sẽ diễn ra cho đến khi kết quả của cost function hội tụ tại 1 điểm với thuật toán sau.

$$\theta_j := \theta_j - \alpha\frac{d}{d\theta_j}J(\theta_0, \theta_1)$$

Trong đó:

- $\alpha$ được gọi là `learning rate`, cho phép kiểm soát tốc độ update tham số trong mô hình.
- `:= ` là dấu gán
- $\frac{d}{d\theta_j}J(\theta_0, \theta_1)$ là đạo hàm riêng phần theo $\theta$ của hàm $cost function$

Áp dụng công thức tính đạo hàm hàm hợp, ta được

$$\cases{\theta_0 := \theta_0 - \alpha\frac{1}{m}\sum_{i = 1}^m(h_{\theta}(x^{(i) - y^{(i)}}))\\
        \theta_1 := \theta_1 - \alpha\frac{1}{m}\sum_{i = 1}^m(h_{\theta}(x^{(i) - y^{(i)}}))\times x^{(i)}}$$

Thuật toán trên còn được gọi là *batch gradient* bởi lẽ thuật toán trên tính toán đến toàn bộ quan sát trên tập dữ liệu huấn luyện (train data). Bên cạnh `batch gradient`, còn có `stochastic gradient descent` và `mini batch gradient descent`.

Công thức tổng quát để update tham số trong mô hình machine learning như sau.
 
$$\Theta^1 = \Theta^0 - \alpha \nabla J(\Theta)$$
Trong đó:

- $\Theta^1$: Giá trị tham số mới
- $\Theta^0$: Giá trị tham số hiện tại
- $\alpha$: Learning rate, tốc độc update tham số
- $\nabla J(\Theta)$: Đạo hàm riêng phần của từng tham số theo giá trị của biến đầu vào.
- $-$: Hướng update tham số. Nếu giá trị đạo hàm riêng phần là dương, ta cần giảm giá trị của tham số, nếu giá trị đạo hàm riêng phần là âm, ta cần tăng giá trị của tham số.

---

**Giải thích gradient descent bằng ngôn ngữ đơn giản**:

Thuật toán `gradient descent` trông thì phức tạp, nhưng thực tế, cách triển khai và tối ưu thuật toán khá giống với phương pháp `agile` trong phát triển phần mềm. Thuật toán có thể được mô tả đơn giản lại như sau.
 
- Xây dựng mô hình một cách nhanh chóng (build). Tại bước này, ta chọn ngẫu nhiên giá trị của tham số để có thể xây dựng mô hình một cách đơn giản nhất.
- Đo lường độ lệch của mô hình so với thực tế (measure). Tại bước này, ta tính toán sai số giữa mô hình vừa xây dựng và dữ liệu thực tế để đo lường sai số của mô hình.
- Thay đổi/update mô hình dựa trên feedback (learn/action). Bước này sẽ phức tạp hơn một chút do ta phải trả lời hai câu hỏi:
    - Tham số cần được thay đổi theo hướng nào? Tăng giá trị lên hay giảm giá trị xuống? 
    - Tốc độ thay đổi là bao nhiêu.
    
Với câu hỏi thứ nhất, ta cần tính được giá trị đạo hàm riêng phần của từng tham số tại giá trị của X. Nếu giá trị này dương, ta cần giảm giá trị của tham số và ngược lại

Với câu hỏi thứ hai, ta phải đặt trước một giá trị quy định tốc độ thay đổi của tham số. Giá trị này được gọi là `learning rate`. 

**Mô hình hồi quy**.


```r
library(dplyr)
library(ggplot2)

n     <- 200 # số lượng quan sát
bias  <- 4
slope <- 3.5
dot   <- `%*%` # Hàm tính phép nhân ma trận

x   <- rnorm(n) * 2
x_b <- cbind(x, rep(1, n)) # Thêm hệ số beta_0 cho ma trận x
y   <- bias + slope * x + rnorm(n)
df  <- data_frame(x = x,  y = y)

df %>% 
  ggplot(aes(x,y)) +
  geom_point(alpha = 0.5) +
  theme_minimal()
```

<img src="41-mo-hinh-ols_files/figure-html/unnamed-chunk-17-1.png" width="672" />

```r
learning_rate <- 0.05
n_iterations  <- 100
theta         <- matrix(c(20, 20)) # Giá trị tham số đầu tiên

b0    <- vector("numeric", length = n_iterations)
b1    <- vector("numeric", length = n_iterations)
sse_i <- vector("numeric", length = n_iterations)
```


```r
for (iteration in seq_len(n_iterations)) { 
  # Mô hình dự báo với theta
  yhat               <- dot(x_b, theta)         
  # Tính sai só của mô hình
  residuals_b        <- yhat - y                
  # Tính gradient (đạo hàm riêng phần cho giá trị theta)
  gradients          <- 2/n * dot(t(x_b), residuals_b)  # Lưu ý
  theta              <- theta - learning_rate * gradients # update theta 
  
  sse_i[[iteration]] <- sum((y - dot(x_b, theta))**2)
  b0[[iteration]]    <- theta[2]
  b1[[iteration]]    <- theta[1]
}

model_i <- data.frame(model_iter = 1:n_iterations, 
                      sse = sse_i, 
                      b0 = b0, 
                      b1 = b1)
```

**Lưu ý**: Với mô hình hồi quy, hàm chi phí (cost function) là MSE. 

$$J(\beta_0, \beta_1) = \frac{1}{n}(\beta_0 + \beta_1*X - Y)^2$$

$$\frac{dJ}{d\beta_0}=\frac{2}{n}(\beta_0 + \beta_1*X - Y) = 2 * error$$
$$\frac{dJ}{d\beta_1}=\frac{2}{n}(\beta_0 + \beta_1*X - Y)*X= 2 * error * X$$
Do đó, ta có thể khái quát hóa gradient cho hai tham số như sau.

$$\nabla J = 2*error*X'$$

Trong đó, X' là [1, X]



```r
p1 <- df %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_abline(aes(intercept = b0, 
                  slope = b1, 
                  colour = -sse),
              data = model_i, 
              alpha = .50 
              ) +
  geom_point(alpha = 0.4) + 
  geom_abline(aes(intercept = b0, 
                  slope = b1), 
              data = model_i[100, ], 
              alpha = 0.5, 
              size = 2, 
              colour = "dodger blue") +
  geom_abline(aes(intercept = b0, 
                  slope = b1),
              data = model_i[1, ],
              colour = "red", 
              alpha = 0.5,
              size = 2) + 
  scale_color_continuous(low = "red", high = "grey") +
  guides(colour = FALSE) +
  theme_minimal()

p1
```

<img src="41-mo-hinh-ols_files/figure-html/unnamed-chunk-19-1.png" width="672" />

```r
p2 <- model_i[1:30,] %>%
  ggplot(aes(model_iter, sse, colour = -sse)) + 
  geom_point(alpha = 0.4) +
  theme_minimal() +
  labs(x = "Model iteration", 
       y = "Sum of Sqaured errors") + 
  scale_color_continuous(low = "red", high = "dodger blue") +
  guides(colour = FALSE)
p2
```

<img src="41-mo-hinh-ols_files/figure-html/unnamed-chunk-19-2.png" width="672" />
