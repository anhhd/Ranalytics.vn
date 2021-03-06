# Phân rã và xoay chiều dữ liệu




Khi phân tích dữ liệu, dữ liệu sau khi được làm sạch thường cơ bản có hai dạng.

- Dạng ngang: Mỗi dòng ứng với 1 quan sát và nhiều biến
- Dạng dọc: Nhiều dòng có thể chứa cùng một quan sát nhưng với các biến khác nhau.

Xem hai ví dụ về dữ liệu dạng ngang và dọc ở dưới đây.


```
##   id Species Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1  1  setosa          5.1         3.5          1.4         0.2
## 2  2  setosa          4.9         3.0          1.4         0.2
## 3  3  setosa          4.7         3.2          1.3         0.2
## 4  4  setosa          4.6         3.1          1.5         0.2
## 5  5  setosa          5.0         3.6          1.4         0.2
## 6  6  setosa          5.4         3.9          1.7         0.4
```


```
##   id Species  Measurement Value
## 1  1  setosa Sepal.Length   5.1
## 2  1  setosa  Sepal.Width   3.5
## 3  1  setosa Petal.Length   1.4
## 4  1  setosa  Petal.Width   0.2
## 5  2  setosa Sepal.Length   4.9
## 6  2  setosa  Sepal.Width   3.0
## 7  2  setosa Petal.Length   1.4
## 8  2  setosa  Petal.Width   0.2
```

Trong thực tế, chúng ta phải sử dụng rất linh hoạt cả hai định dạng dữ liệu này (dữ liệu ngang và dữ liệu dọc). Trong chương này, chúng ta sẽ học cách sử dụng và biến đổi dữ liệu giữa hai định dạng với `tidyr`.

## Phân rã dữ liệu thành dạng dọc với `gather`


```r
library(tidyr)
data %>% 
  gather(key = name_of_key,
         value = name_of_value_variable,
         gather = c(list_of_var))
```

Xem ví dụ sau.


```r
library(dplyr)
df <- iris %>% 
  head(2) %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(6, 5, 1:4) 
df  
```

```
##   id Species Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1  1  setosa          5.1         3.5          1.4         0.2
## 2  2  setosa          4.9         3.0          1.4         0.2
```

```r
# Xoay dữ liệu sang dạng dọc
df2 <- df %>% 
  gather(key = Measurement, 
       value = Value, 
       c(3:6)) # Các biến được phân rã
df2
```

```
##   id Species  Measurement Value
## 1  1  setosa Sepal.Length   5.1
## 2  2  setosa Sepal.Length   4.9
## 3  1  setosa  Sepal.Width   3.5
## 4  2  setosa  Sepal.Width   3.0
## 5  1  setosa Petal.Length   1.4
## 6  2  setosa Petal.Length   1.4
## 7  1  setosa  Petal.Width   0.2
## 8  2  setosa  Petal.Width   0.2
```

Ở ví dụ trên, khi phân rã dữ liệu sang dạng dọc, các biến được phân rã là 4 biến ở vị trí từ `3 đến 6`. Do dữ liệu gốc `df` chỉ có 2 quan sát, nên dữ liệu mới sau khi phân rã sẽ có 8 quan sát.

## Xoay chiều dữ liệu với `spread`

Ngược lại với phân rã dữ liệu là xoay chiều dữ liệu. Trong `tidyr`, ta có thể sử dụng hàm `spread`. Công thức tổng quát để xoay chiều dữ liệu như sau.


```r
data %>% 
  #Biến được xoay thành cột
  spread(key = key_variable, 
         # Biến giá trị
         value = value_variable) 
```


Ta quay trở lại ví dụ ở phần trước vói dữ liệu `df2` đã được phân rã.


```r
df2
```

```
##   id Species  Measurement Value
## 1  1  setosa Sepal.Length   5.1
## 2  2  setosa Sepal.Length   4.9
## 3  1  setosa  Sepal.Width   3.5
## 4  2  setosa  Sepal.Width   3.0
## 5  1  setosa Petal.Length   1.4
## 6  2  setosa Petal.Length   1.4
## 7  1  setosa  Petal.Width   0.2
## 8  2  setosa  Petal.Width   0.2
```

Ta có thể xoay chiều dữ liệu lại như sau.


```r
df2 %>% 
  spread(key = Measurement, 
         value = Value)
```

```
##   id Species Petal.Length Petal.Width Sepal.Length Sepal.Width
## 1  1  setosa          1.4         0.2          5.1         3.5
## 2  2  setosa          1.4         0.2          4.9         3.0
```

## Tách một biến thành nhiều biến với `separate`

Khi phân tích dữ liệu, ta thường xuyên phải tách một biến thành nhiều biến. Khi đó, việc tách biến sẽ trở nên rất đơn giản với hàm `separate`. Công thức tổng quát của `separate` như sau:


```r
data %>% 
  separate(var_to_spread, c("new_var1", 
                            "new_var2", ...))
```


```r
df <- data.frame(date = c(NA, 
                          "2018-07-01", 
                          "2018-09-02"))
df
```

```
##         date
## 1       <NA>
## 2 2018-07-01
## 3 2018-09-02
```

```r
# Tách biến date thành 3 biến
df %>% 
  separate(date, c("year", "month", "date"))
```

```
##   year month date
## 1 <NA>  <NA> <NA>
## 2 2018    07   01
## 3 2018    09   02
```

## Gộp nhiều biến thành một biến với `unite`

Ngược lại với `spread`, ta có thể gộp nhiều biến thành một với `unite`. Công thức tổng quát của `unite` như sau.


```r
data %>% 
  unite(new_var, var_1, var2,...)
```

Trong đó, `var_1`, `var_2` là tên các biến sẽ được gộp. `new_var` là tên biến mới được tạo thành.

Quay trở lại ví dụ trên.


```r
df <- data.frame(date = c("2018-07-01", "2018-09-02")) %>%   separate(date, c("year", "month", "date"))
df
```

```
##   year month date
## 1 2018    07   01
## 2 2018    09   02
```

```r
# Gộp nhiều biến
df %>% 
  unite(full_date, 1:3, 
        sep = "/",
        remove = F)
```

```
##    full_date year month date
## 1 2018/07/01 2018    07   01
## 2 2018/09/02 2018    09   02
```



