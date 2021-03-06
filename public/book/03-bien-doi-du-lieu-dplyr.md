\mainmatter

# (PART) Kỹ thuật phân tích dữ liệu thực tế {-}

# Ngữ pháp của biến đổi dữ liệu với DPLYR



Khi bắt tay vào công việc phân tích số liệu, việc đầu tiên ta cần phải làm là thu thập dữ liệu từ nhiều nguồn khác nhau. Sau khi hoàn thành xong bước này, ta sẽ phải dành phần lớn thời gian để làm sạch, biến đổi và tổng hợp dữ liệu nhằm tìm kiếm các insights hoặc chuẩn bị dữ liệu cho các bước xây dựng mô hình, dự báo. 

R rất mạnh trong việc biến đối dữ liệu và có rất nhiều package hỗ trợ cho công việc này. Tuy nhiên, thư viện nổi tiếng nhất trong R trong việc làm sạch và biến đổi dữ liệu là `dplyr`, một thư viện nổi tiếng với những tính năng chuyên cho việc xử lý, tổng hợp dữ liệu trước khi xây dựng mô hình phân tích dữ liệu. Chương này sẽ tập trung vào giới thiệu về những hàm cơ bản nhất của `dplyr`.

Trước khi bắt đầu nội dung bài giảng, chúng ta có thể download và gọi gói dplyr.


```r
#install.packages("dplyr")
library(dplyr)
```

## Giới thiệu về pipe operator

Khi viết các câu lệnh, thông thường ta có 2 cách viết phổ biến sau.

- Cách 1: Viết với các câu lệnh lồng vào nhau (nested). Với cách viết này, các hàm sẽ được viết lồng vào nhau và kết quả của hàm sẽ được tính toán theo thứ tự từ trong ra ngoài. 

- Cách 2: Viết lưu dưới dạng các đối tượng trung gian. Với cách viết này, từng đối tượng sẽ được tính toán từng phần và kết quả sẽ được hiển thị một cách mạch lạc hơn. Tuy nhiên, nhược điểm của phương pháp này là sẽ tạo ra rất nhiều đối tượng trung gian, gây ra khó khăn trong việc theo dõi và quản lỹ. 

Giả sử ta cần tính toán độ lệch chuẩn của véc-tơ x, công thức tính độ lệch chuẩn sẽ là

$$\sigma = \frac{\sum_{i=1}^n(x_i-\overline{x})^2}{n-1}$$

Với hai cách viết code khác nhau, ta có thể tính độ lệch chuẩn theo hai cách.


```r
# Cách 1
# Tạo vector x
x <- seq(2, 100, 2)  
# Tính độ lệch chuẩn
sqrt(sum((x-mean(x))^2)/(length(x)-1))
```

```
## [1] 29.15476
```

```r
sd(x)
```

```
## [1] 29.15476
```


```r
# Cách 2
# Tạo vector x
x <- seq(2, 100, 2)
# Tính tổng bình phương 
sum_sqr <- sum((x-mean(x))^2)
len_x <- length(x)
var <- sum_sqr/(len_x - 1)
sd <- var^(1/2)
sd
```

```
## [1] 29.15476
```

Ta thấy kết quả ở hai cách tính là như nhau. Tuy nhiên, cách viết hai sẽ tạo ra nhiều đối tượng trung gian hơn cách viết 1 rất nhiều. Khi phân tích dữ liệu thực tế, ta sẽ phải áp dụng cả 2 cách viết code để có thể vận dụng linh hoạt trong từng trường hợp cụ thể.

Trong R, có cách viết code thứ ba, được gọi là cách suwr dụng `pipe operator` `(%>%)`. `Toán tử Pipe` cho phép viết code theo cách đơn giản và dễ theo dõi giúp cho người đọc và người viết code trên R có thể theo dõi được code một cách dễ dàng nhất. Câu trúc của pipe như sau


```r
f(x, y) = x %>% f(., y) 
```

Ví dụ của pipe.


```r
# Cách 1
mean(x)
# Cách 2
x %>% mean
```

Ta có thể xem xét ví dụ phức tạp hơn.


```r
# Cách 1 - dùng cách viết thường
summary(head(iris))
```

```
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width    
##  Min.   :4.600   Min.   :3.000   Min.   :1.300   Min.   :0.2000  
##  1st Qu.:4.750   1st Qu.:3.125   1st Qu.:1.400   1st Qu.:0.2000  
##  Median :4.950   Median :3.350   Median :1.400   Median :0.2000  
##  Mean   :4.950   Mean   :3.383   Mean   :1.450   Mean   :0.2333  
##  3rd Qu.:5.075   3rd Qu.:3.575   3rd Qu.:1.475   3rd Qu.:0.2000  
##  Max.   :5.400   Max.   :3.900   Max.   :1.700   Max.   :0.4000  
##        Species 
##  setosa    :6  
##  versicolor:0  
##  virginica :0  
##                
##                
## 
```

```r
# Cách 2 - dùng pipe
iris %>% head %>% summary
```

```
##   Sepal.Length    Sepal.Width     Petal.Length    Petal.Width    
##  Min.   :4.600   Min.   :3.000   Min.   :1.300   Min.   :0.2000  
##  1st Qu.:4.750   1st Qu.:3.125   1st Qu.:1.400   1st Qu.:0.2000  
##  Median :4.950   Median :3.350   Median :1.400   Median :0.2000  
##  Mean   :4.950   Mean   :3.383   Mean   :1.450   Mean   :0.2333  
##  3rd Qu.:5.075   3rd Qu.:3.575   3rd Qu.:1.475   3rd Qu.:0.2000  
##  Max.   :5.400   Max.   :3.900   Max.   :1.700   Max.   :0.4000  
##        Species 
##  setosa    :6  
##  versicolor:0  
##  virginica :0  
##                
##                
## 
```

Cả hai cách đều cho ra kết quả giống nhau. Tuy nhiên, cách hai sẽ dễ theo dõi, dễ đọc hơn cách 1 rất nhiều. Cách đọc hiểu quá trình thực hiện pipe như sau:

1. Gọi tập dữ liệu `iris` để phân tích
2. Thực hiện hàm `head` trên tập dữ liệu này, *được kết quả bao nhiêu*...
3. ... tiếp tục thực hiện hàm `summary`

Như ta thấy, cách viết theo phong cách của `pipe operator (%>%)` cho phép ta thực hiện các phép tính theo đúng mạch tư duy logic của bản thân. Điều này là một điểm rất mạnh mà hiện tại, mới chỉ ở R có toán tử `%>%` áp dụng được cho mọi hàm. 

**Một số đặc tính cơ bản của toán tử pipe**:

1. Theo mặc định, Phía tay trái (LHS) sẽ được chuyển tiếp thành yếu tố đầu tiên của hàm được sử dụng phía tay phải (RHS), ví dụ:



```r
mean(x) 
```

```
## [1] 51
```

```r
# Tương đương với:
x %>% mean
```

```
## [1] 51
```

2. Khi LHS không còn là yếu tố đầu tiên của một hàm RHS, thì dấu "." được sử dụng để định vị cho LHS, ví dụ:


```r
library(dplyr)
# Cách 1
summary(lm(mpg ~ cyl, data = mtcars))

# Cách 2
mtcars %>% 
  lm(mpg ~ cyl, data = .) %>% 
  summary
```
 
Trong tình huống trên, tham số về dữ liệu trong hàm lm không phải là ở đầu, mà sau phần công thức, nên  chúng ta sẽ dùng dấu "." như là đại diện của thực thể mtcars ở bên ngoài (LHS) của hàm lm.


## Các hàm cơ bản trong dplyr

Trong công việc biến đổi dữ liệu, bất kỳ ngôn ngữ phân tích nào cũng có 3 nhóm hàm lớn.

- Nhóm 1 - các hàm truy vấn dữ liệu: Lấy dữ liệu theo dòng, theo cột và theo điều kiện. Trong `dplyr` sẽ là các hàm `select`, `filter` và `slice`
- Nhóm 2 - Các hàm tổng hợp dữ liệu: Tính toán tổng hợp dữ liệu theo chiều. Trong `dplyr` sẽ là các hàm `group_by`, `summarise` 
- Nhóm 3 - Các hàm biến đổi dữ liệu: Tạo mới, biến đổi các dữ liệu cũ thành các dữ liệu mới. Trong `dplyr` sẽ là các hàm thuộc nhóm `mutate`, `join`,  `bind`

Trong phần này, chúng ta sẽ giới thiệu nhanh các nhóm câu lệnh cơ bản trên.

### Nhóm câu lệnh truy vấn dữ liệu

Khi truy vấn dữ liệu, ta thường phải thực hiện 3 nhóm công việc sau.

- Lấy theo cột
- Lấy theo dòng 
- Lấy theo điều kiện

Xét về mặt bản chất, lấy theo điều kiện là một trường hợp đặc biệt của việc lấy theo dòng. Đối với các ngôn ngữ như SQL, sẽ không phân biệt hai loại này. Tuy nhiên, vì R lưu thứ tự của từng quan sát trong dataframe, nên việc phân biệt được hai loại truy vấn trên là cần thiết.

#### Lấy các cột trong dataframe với `select`



```r
data %>% select(var1, var2, ...)
```

Trong đó, `var1`, `var2` là tên các cột cần truy vấn. Đặc biệt, R rất linh hoạt trong việc lọc theo cột. Ta có thể truy vấn theo tên, theo thứ tự hoặc thậm chí theo các khoảng thứ tự các biến. Xem ví dụ sau.


```r
library(dplyr)
# Xêm tên các biến trong mtcars
mtcars %>% names
```

```
##  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
## [11] "carb"
```

```r
# Chọn cột mpg và cyl
mtcars %>% select(mpg, cyl) %>% head
```

```
##                    mpg cyl
## Mazda RX4         21.0   6
## Mazda RX4 Wag     21.0   6
## Datsun 710        22.8   4
## Hornet 4 Drive    21.4   6
## Hornet Sportabout 18.7   8
## Valiant           18.1   6
```

```r
# Chọn cột thứ nhất và thứ hai
mtcars %>% select(1,2) %>% head
```

```
##                    mpg cyl
## Mazda RX4         21.0   6
## Mazda RX4 Wag     21.0   6
## Datsun 710        22.8   4
## Hornet 4 Drive    21.4   6
## Hornet Sportabout 18.7   8
## Valiant           18.1   6
```

```r
# Chọn cột thứ 3 đến cột thứ 6
mtcars %>% select(3:6) %>% head
```

```
##                   disp  hp drat    wt
## Mazda RX4          160 110 3.90 2.620
## Mazda RX4 Wag      160 110 3.90 2.875
## Datsun 710         108  93 3.85 2.320
## Hornet 4 Drive     258 110 3.08 3.215
## Hornet Sportabout  360 175 3.15 3.440
## Valiant            225 105 2.76 3.460
```

&nbsp;

Ngoài ra, khi lấy chi tiết các cột (liệt kê từng cột) khi lấy dữ liệu trên 1 bảng, bạn có thể dùng một số hàm sau để hỗ trợ việc lấy trường dữ liệu được nhanh hơn:

  - `starts_with("Ký tự là thông tin mong muốn")`: các cột dữ liệu ccó tên hứa các ký tự mong muốn đứng ở đầu của tên, ví dụ:
  

```r
iris %>%
  select(starts_with("Petal")) %>%
  head
```

```
##   Petal.Length Petal.Width
## 1          1.4         0.2
## 2          1.4         0.2
## 3          1.3         0.2
## 4          1.5         0.2
## 5          1.4         0.2
## 6          1.7         0.4
```

  - `ends_with("Ký tự là thông tin mong muốn")`: các cột dữ liệu có tên chứa các ký tự mong muốn ở cuối của tên, ví dụ:
  

```r
iris %>%
  select(ends_with("Length")) %>%
  head
```

```
##   Sepal.Length Petal.Length
## 1          5.1          1.4
## 2          4.9          1.4
## 3          4.7          1.3
## 4          4.6          1.5
## 5          5.0          1.4
## 6          5.4          1.7
```

  - `contains("Ký tự là thông tin mong muốn")`: các cột dữ liệu có tên chứa chính xác các ký tự mong muốn ở bất kỳ vị trí nào của tên, ví dụ:
  

```r
iris %>%
  select(contains("etal")) %>%
  head
```

```
##   Petal.Length Petal.Width
## 1          1.4         0.2
## 2          1.4         0.2
## 3          1.3         0.2
## 4          1.5         0.2
## 5          1.4         0.2
## 6          1.7         0.4
```

 - matches("Dạng ký tự là thông tin mong muốn"): các cột dữ liệu có tên chứa các ký tự có dạng ký tự mong muốn ở bất kỳ vị trí nào của tên, ví dụ:
 

```r
iris %>%
  select(matches(".t.")) %>% 
  head
```

```
##   Sepal.Length Sepal.Width Petal.Length Petal.Width
## 1          5.1         3.5          1.4         0.2
## 2          4.9         3.0          1.4         0.2
## 3          4.7         3.2          1.3         0.2
## 4          4.6         3.1          1.5         0.2
## 5          5.0         3.6          1.4         0.2
## 6          5.4         3.9          1.7         0.4
```

Trong ví dụ trên, R sẽ lấy tất cả các cột có tên chứa chữ t và có ký tự khác ở trước và sau (các ký tự chỉ chứa chữ t mà chữ t ở đâu hoặc cuối tên sẽ không được tính vào)

Thêm vào đó, ta có thể đổi tên biến ngay trong khi lựa chọn các biến với `select` như sau:


```r
mtcars %>%
  select(`miles per gallon` = mpg
         , cylinder = cyl
         , weight = wt) %>%
  head
```

```
##                   miles per gallon cylinder weight
## Mazda RX4                     21.0        6  2.620
## Mazda RX4 Wag                 21.0        6  2.875
## Datsun 710                    22.8        4  2.320
## Hornet 4 Drive                21.4        6  3.215
## Hornet Sportabout             18.7        8  3.440
## Valiant                       18.1        6  3.460
```

#### Lấy các dòng trong dataframe với `slice`



```r
data %>% slice(observation)
```

Tương tự như lấy theo cột, ta có thể lấy các dòng trong một dataframe. Tuy nhiên, lưu ý hàm `slice` chỉ cho phép điều kiện lấy quan sát là một véc-tơ. Xem ví dụ sau.


```r
# Lấy dòng đầu tiên
mtcars %>% slice(1)
```

```
##   mpg cyl disp  hp drat   wt  qsec vs am gear carb
## 1  21   6  160 110  3.9 2.62 16.46  0  1    4    4
```

```r
# Lấy dòng từ 1:3
mtcars %>% slice(1:3)
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
```

```r
# Lấy dòng 1:3 và 5
mtcars %>% slice(c(1:3,5))
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
## 4 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
```

#### Lọc quan sát theo điều kiện với `filter`



```r
data %>% filter(condition)
```

Hàm filter cho phép ta sử dụng các điều kiện phức tạp để truy xuất dữ liệu từ dataframe. Các điều kiện thường dùng bao gồm.

+-------------------+---------+-----------------+
|        Dấu        | Ký hiệu |      Ví dụ      |
+===================+=========+=================+
| Bằng              | `==`    | `7==8`          |
+-------------------+---------+-----------------+
| Khác              | `!=`    | `7!=8`          |
+-------------------+---------+-----------------+
| Lớn hơn           | `>`     | `a > b`         |
+-------------------+---------+-----------------+
| Lớn hơn hoặc bằng | `>=`    | `a >= b`        |
+-------------------+---------+-----------------+
| Nhỏ hơn           | `<`     | `a < b`         |
+-------------------+---------+-----------------+
| Nhỏ hơn hoặc bằng | `<=`    | `a <= b`        |
+-------------------+---------+-----------------+
| Và                | `&`     | `a > 7 & b < 9` |
+-------------------+---------+-----------------+


Xem ví dụ sau.


```r
# Lọc điều kiện mpg > 20
mtcars %>%
  filter(mpg > 20) %>% 
  dim
```

```
## [1] 14 11
```

```r
# Lọc điều kiện mpg >20 hoặc mpg <18
mtcars %>% 
  filter(mpg > 20 | mpg < 18) %>% 
  dim
```

```
## [1] 27 11
```

```r
# Lọc điều kiện mpg >=20 và cyl = 6
mtcars %>% 
  filter(mpg > 20 & cyl == 6)
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
## 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
## 3 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
```

**Lưu ý**: Khi điều kiện *hoặc* là chuỗi các giá trị rời rạc áp dụng cho cùng một trường, chúng ta có thể làm ngắn gọn hơn với cấu trúc "%in%" thay vì cấu phải liệt kê tất cả các điều kiện đơn lẻ và ngăn cách nhau bởi dấu "|":


```r
mtcars %>%
 filter(carb == 4 | carb == 3 | carb == 1)
```

```
##     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 1  21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
## 2  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
## 3  22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
## 4  21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
## 5  18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
## 6  14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
## 7  19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
## 8  17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
## 9  16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
## 10 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
## 11 15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
## 12 10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
## 13 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
## 14 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
## 15 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 16 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
## 17 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
## 18 13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
## 19 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
## 20 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
```

Câu lệnh trên tương đương với:


```r
mtcars %>%
  filter(carb %in% c(1, 3, 4))
```

#### Sắp xếp dữ liệu với `arrange`



```r
data %>% arrange(var1, var2)
```

Ngoài việc lọc dữ liệu có điều kiện, chúng ta cũng thường xuyên thực hiện việc sắp xếp dữ liệu theo một trật tự nhất định nào đó khi xem dữ liệu. Hàm arrange() hỗ trợ công việc này. Cách thức sắp xếp dữ liệu mặc định là từ nhỏ đến lớn.


```r
mtcars %>%
  arrange(mpg) %>% 
  head
```

```
##    mpg cyl disp  hp drat    wt  qsec vs am gear carb
## 1 10.4   8  472 205 2.93 5.250 17.98  0  0    3    4
## 2 10.4   8  460 215 3.00 5.424 17.82  0  0    3    4
## 3 13.3   8  350 245 3.73 3.840 15.41  0  0    3    4
## 4 14.3   8  360 245 3.21 3.570 15.84  0  0    3    4
## 5 14.7   8  440 230 3.23 5.345 17.42  0  0    3    4
## 6 15.0   8  301 335 3.54 3.570 14.60  0  1    5    8
```

Khi có nhiều biến cần được sắp xếp, hàm `arrange` sẽ ưu tiên các biến theo thứ tự từ trái sang phải.


```r
mtcars %>% arrange(mpg, cyl)
```

```
##     mpg cyl  disp  hp drat    wt  qsec vs am gear carb
## 1  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
## 2  10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
## 3  13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
## 4  14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
## 5  14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
## 6  15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
## 7  15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
## 8  15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
## 9  15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
## 10 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
## 11 16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
## 12 17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
## 13 17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
## 14 18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
## 15 18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
## 16 19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
## 17 19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
## 18 19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
## 19 21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
## 20 21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
## 21 21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2
## 22 21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
## 23 21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
## 24 22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
## 25 22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
## 26 24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
## 27 26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
## 28 27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
## 29 30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
## 30 30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
## 31 32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
## 32 33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
```

#### Đổi tên biến với `rename`


```r
data %>% rename(new_var = old_var)
```


```r
mtcars %>%
  rename(displacement = disp,
         miles_per_gallon = mpg) %>% 
  names
```

```
##  [1] "miles_per_gallon" "cyl"              "displacement"    
##  [4] "hp"               "drat"             "wt"              
##  [7] "qsec"             "vs"               "am"              
## [10] "gear"             "carb"
```

### Nhóm câu lệnh biến đổi dữ liệu

##### Tạo mới trường dữ liệu với `mutate`

Trong quá trình xử lý dữ liệu, ta thường xuyên phải tạo thêm các trường dữ liệu mới (trường dữ liệu phát sinh). Hàm mutate() được sử dụng để làm công việc này. Cấu trúc của hàm rất đơn giản như sau.



```r
data %>% mutate(new_var = statement)
```


Xem ví dụ sau:


```r
mtcars %>%
  select(mpg) %>%
  mutate(new_mpg = mpg * 2) %>%
  head
```

```
##    mpg new_mpg
## 1 21.0    42.0
## 2 21.0    42.0
## 3 22.8    45.6
## 4 21.4    42.8
## 5 18.7    37.4
## 6 18.1    36.2
```

Trong một số trường hợp, khi ta không muốn lấy các trường thông tin cũ mà chỉ muốn lấy các trường thông tin mới tạo thì có thể sử dụng hàm `transmute()` với cấu trúc giống như hàm mutate.


```r
mtcars %>%
  select(mpg) %>% 
  transmute(new_mpg = mpg * 1.61) %>%
  head
```

```
##   new_mpg
## 1  33.810
## 2  33.810
## 3  36.708
## 4  34.454
## 5  30.107
## 6  29.141
```

##### Gộp nhiều bảng với nhóm hàm `join`

- Hàm `inner_join(x, y, by = "key")`: lấy tất cả dữ liệu có trên bảng hai bảng khi trùng key, ví dụ:
  

```r
x <- data.frame(student_id = seq(1, 10, 1), 
                maths = c(10, 8, 7, 6, 7.8, 4, 
                          7.7, 9, 9.5, 6.5))
y <- data.frame(student_id = seq(2, 20, 2), 
                physics = c(8, 9.5, 7.5, 6, 5.5, 
                            6.5, 7.8, 8.2, 8, 7.5))
x
```

```
##    student_id maths
## 1           1  10.0
## 2           2   8.0
## 3           3   7.0
## 4           4   6.0
## 5           5   7.8
## 6           6   4.0
## 7           7   7.7
## 8           8   9.0
## 9           9   9.5
## 10         10   6.5
```

```r
y
```

```
##    student_id physics
## 1           2     8.0
## 2           4     9.5
## 3           6     7.5
## 4           8     6.0
## 5          10     5.5
## 6          12     6.5
## 7          14     7.8
## 8          16     8.2
## 9          18     8.0
## 10         20     7.5
```

```r
# gộp 2 bảng dữ liệu x và y theo student_id
x %>%
  inner_join(y, by = "student_id") 
```

```
##   student_id maths physics
## 1          2   8.0     8.0
## 2          4   6.0     9.5
## 3          6   4.0     7.5
## 4          8   9.0     6.0
## 5         10   6.5     5.5
```

---

- **full_join**: lấy tất cả dữ liệu có cả trên bảng x, y. 



```r
full_join(x, y, by = "key")
```


```r
x %>%
  full_join(y, by = "student_id")
```

```
##    student_id maths physics
## 1           1  10.0      NA
## 2           2   8.0     8.0
## 3           3   7.0      NA
## 4           4   6.0     9.5
## 5           5   7.8      NA
## 6           6   4.0     7.5
## 7           7   7.7      NA
## 8           8   9.0     6.0
## 9           9   9.5      NA
## 10         10   6.5     5.5
## 11         12    NA     6.5
## 12         14    NA     7.8
## 13         16    NA     8.2
## 14         18    NA     8.0
## 15         20    NA     7.5
```

Trong ví dụ trên, các giá trị về điểm toán (maths) sẽ trả về NA cho các student_id không tồn tại trên bảng y và ngược lại cho bảng x với các giá trị điểm vật lý (physics) của các student_id không tồn tại trên bảng x.

---

- Hàm **left_join**: lấy dữ liệu chỉ có trên bảng x, ví dụ:



```r
left_join(x, y, by = "var")
```


```r
x %>%
  left_join(y, by = "student_id") 
```

```
##    student_id maths physics
## 1           1  10.0      NA
## 2           2   8.0     8.0
## 3           3   7.0      NA
## 4           4   6.0     9.5
## 5           5   7.8      NA
## 6           6   4.0     7.5
## 7           7   7.7      NA
## 8           8   9.0     6.0
## 9           9   9.5      NA
## 10         10   6.5     5.5
```

Với các `student_id` không có giá trị trên bảng y, cột physics sẽ trả về giá trị NA

---

- Hàm **right_join** : lấy dữ liệu chỉ có trên bảng y, ví dụ:



```r
right_join(x, y, by = "var")
```


```r
x %>%
  right_join(y, by = "student_id") 
```

```
##    student_id maths physics
## 1           2   8.0     8.0
## 2           4   6.0     9.5
## 3           6   4.0     7.5
## 4           8   9.0     6.0
## 5          10   6.5     5.5
## 6          12    NA     6.5
## 7          14    NA     7.8
## 8          16    NA     8.2
## 9          18    NA     8.0
## 10         20    NA     7.5
```

Với các `student_id` không có giá trị trên bảng x, cột maths sẽ trả về giá trị NA

**Lưu ý**: Trong trường hợp cột dữ liệu dùng để nối các bảng có tên khác nhau, ta có thể sử dụng cấu trúc sau:



```r
left_join(x, y, by = c("key_x" = "key_y"))
```

Xem ví dụ sau:


```r
names(x)[1] <- "student_id1"
names(y)[1] <- "student_id2"

x %>%
  inner_join(y, by = c("student_id1" = "student_id2")) 
```

```
##   student_id1 maths physics
## 1           2   8.0     8.0
## 2           4   6.0     9.5
## 3           6   4.0     7.5
## 4           8   9.0     6.0
## 5          10   6.5     5.5
```


##### Ghép nhiều bảng theo dòng hoặc cột với nhóm hàm `bind`

Bên cạnh các hàm `join`, khi xử lý dữ liệu trong thực tiễn, ta có thể phải *ghép* các bảng dữ liệu theo hàng hoặc cột. Trong dplyr, có hai hàm rất hữu dụng trong hai trường hợp trên là `bind_col` và `bind_rows`



```r
bind_cols(data1, data2)
bind_rows(data1, data2)
```

Xem hai ví dụ sau.


```r
df1 <- data.frame(id = 1:3,
                  income = 8:10)
df2 <- data.frame(id = 4:9,
                  income = 8:13)
df3 <- data.frame(id = 1:3,
                  gender = c("F", "F", "M"))

# Nối theo dòng
df1 %>% bind_rows(df2)
```

```
##   id income
## 1  1      8
## 2  2      9
## 3  3     10
## 4  4      8
## 5  5      9
## 6  6     10
## 7  7     11
## 8  8     12
## 9  9     13
```

```r
# Nối theo cột
df1 %>% bind_cols(df3)
```

```
##   id income id1 gender
## 1  1      8   1      F
## 2  2      9   2      F
## 3  3     10   3      M
```

### Nhóm hàm tổng hợp dữ liệu với `summarise`

Trong quá trình xử lý dữ liệu, ta thường xuyên phải tổng hợp dữ liệu theo các cách như: tính tổng, tính số dư bình quân, phương sai, tổng số lượng quan sát... Với `dplyr`, ta có thể sử dụng hàm `summarise()` để thực hiện công việc này.



```r
data %>% 
  summarise(var_name = calculate_stats(var))
```


```r
mtcars %>% 
  summarise(mean_mpg = mean(mpg),
            sd_mpg = sd(mpg))
```

```
##   mean_mpg   sd_mpg
## 1 20.09062 6.026948
```

Đây là ví dụ đơn giản nhất với `summarise` mà ta có thể thay thế bằng `summary()` trên R base. Tuy nhiên, kết hợp giữa hàm `summarise()` và hàm group_by() trên dplyr sẽ cho chúng ta có cái nhìn về dữ liệu tổng hợp một cách đa chiều hơn. Hàm group_by() cho phép dữ liệu tổng hợp được gộp lại theo một hoặc nhiều trường thông tin khác nhau, giúp người phân tích có thể nhìn dữ liệu theo từ chiều riêng biệt hoặc gộp các chiều thông tin với nhau.


```r
mtcars %>% 
  group_by(cyl) %>% 
  summarise(mean_mpg = mean(mpg),
            mean_disp = mean(disp))
```

```
## # A tibble: 3 x 3
##     cyl mean_mpg mean_disp
##   <dbl>    <dbl>     <dbl>
## 1     4     26.7      105.
## 2     6     19.7      183.
## 3     8     15.1      353.
```

## Các hàm nâng cao trong dplyr

Bên cạnh các nhóm hàm cơ bản đã trình bày ở phần trên, `dplyr` còn có một số hàm nâng cao khác đặc biệt hữu dụng trong quá trình biến đổi, tổng hợp dữ liệu, bao gồm `case_when`, `mutate_at` & `summarise_at`

### Điều kiện phân nhóm với `case_when`

Trong quá trình phân tích và xử lý dữ liệu, chúng ta thường phải tạo thêm các trường mới hoặc tính toán dữ liệu dựa vào từng điều kiện khác nhau để đưa ra giá trị của trường hoặc cách tính cho dữ liệu. Ví dụ, khi ta muốn tính thưởng cho KH thì sẽ phải dùng nhiều công thức khác nhau như KH thuộc VIP sẽ nhân 1 tỷ lệ, KH thuộc nhóm trung bình sẽ có 1 tỷ lệ khác, hay KH thông thường thì sẽ 1 tỷ lệ khác.... 

Trong dplyr, hàm `case_when()` xử lý các trường hợp trên rất nhanh chóng.



```r
data %>% mutate(new_var = case_when(
                   condition_1 ~ "value_1",
                   condition_2 ~ "value_2",...,
                   TRUE ~ "value_n"
                 ))
```

Ta xem ví dụ sau:


```r
df <- data.frame(number = 1:10) 

df %>% mutate(nhom = case_when(
  number <= 5 ~ "nhom_1", # nhóm 1: số từ 1 đến 5
  number > 5 & number <= 8 ~ "nhom_2", # nhóm 2: số từ 6 đến 8
  TRUE ~ "nhom_3" # các số còn lại
  ))
```

```
##    number   nhom
## 1       1 nhom_1
## 2       2 nhom_1
## 3       3 nhom_1
## 4       4 nhom_1
## 5       5 nhom_1
## 6       6 nhom_2
## 7       7 nhom_2
## 8       8 nhom_2
## 9       9 nhom_3
## 10     10 nhom_3
```

### Tạo thêm biến mới theo điều kiện với `mutate_if` & `mutate_at`

Khi phân tích, ta có thể tạo thêm biến mới khi các biến trong dataframe thỏa mãn điều kiện nào đó. 

> data %>% mutate_if(condition, function)


```r
df <- data.frame(
  id = 1:5,
  gender = c("F", "M", "M", "F", "F"),
  income = c(4,5,3,6,7)
)
df %>% summary
```

```
##        id    gender     income 
##  Min.   :1   F:3    Min.   :3  
##  1st Qu.:2   M:2    1st Qu.:4  
##  Median :3          Median :5  
##  Mean   :3          Mean   :5  
##  3rd Qu.:4          3rd Qu.:6  
##  Max.   :5          Max.   :7
```

```r
# Biến đổi các biến factor thành character
df %>% mutate_if(is.factor, 
                 as.character) %>% 
  summary
```

```
##        id       gender              income 
##  Min.   :1   Length:5           Min.   :3  
##  1st Qu.:2   Class :character   1st Qu.:4  
##  Median :3   Mode  :character   Median :5  
##  Mean   :3                      Mean   :5  
##  3rd Qu.:4                      3rd Qu.:6  
##  Max.   :5                      Max.   :7
```

Ngoài ra, ta có thể tự tạo các hàm mới và áp dụng với `mutate_if`. Xem ví dụ sau.


```r
my_func <- function(x){x*100}
# Nhân các biến numeric lên 100 lần
df %>% 
  mutate_if(is.numeric, my_func)
```

```
##    id gender income
## 1 100      F    400
## 2 200      M    500
## 3 300      M    300
## 4 400      F    600
## 5 500      F    700
```

Đối với `mutate_at`, ta cũng có thể thực hiện tương tự. Cấu trúc tổng quát của `mutate_at` như sau.



```r
data %>% 
  mutate_at(vars(var1, var2, ...), function())
```


```r
df 
```

```
##   id gender income
## 1  1      F      4
## 2  2      M      5
## 3  3      M      3
## 4  4      F      6
## 5  5      F      7
```

```r
# Nhân biến income lên 100 lần
df %>% mutate_at(vars(income), my_func)
```

```
##   id gender income
## 1  1      F    400
## 2  2      M    500
## 3  3      M    300
## 4  4      F    600
## 5  5      F    700
```

### Tổng hợp dữ liệu theo điều kiện với `summarise_at` và `summarise_if`

Tương tự như `mutate_at` và `mutate_if`, ta có thể tổng hợp nhanh dữ liệu theo điều kiện.

Cấu trúc tổng quát của `summarise_at`



```r
data %>% 
     group_by(var) (không bắt buộc)
     summarise_at(vars(variables), funs(functions))
```

Xem ví dụ sau


```r
mtcars %>% 
  group_by(am) %>% 
  summarise_at(vars(mpg, disp),
               funs(mean, max, median))
```

```
## # A tibble: 2 x 7
##      am mpg_mean disp_mean mpg_max disp_max mpg_median disp_median
##   <dbl>    <dbl>     <dbl>   <dbl>    <dbl>      <dbl>       <dbl>
## 1     0     17.1      290.    24.4      472       17.3        276.
## 2     1     24.4      144.    33.9      351       22.8        120.
```

Tương tự, ta có cấu trúc tổng quát của `summarise_if`



```r
data %>% 
   group_by(var) (không bắt buộc)
   summarise_if(condition, funs(functions))
```
   

```r
iris %>% 
  select(Species, Sepal.Length, Sepal.Width) %>% 
  group_by(Species) %>% 
  summarise_if(is.numeric,
               funs(mean, median))
```

```
## # A tibble: 3 x 5
##   Species Sepal.Length_me~ Sepal.Width_mean Sepal.Length_me~
##   <fct>              <dbl>            <dbl>            <dbl>
## 1 setosa              5.01             3.43              5  
## 2 versic~             5.94             2.77              5.9
## 3 virgin~             6.59             2.97              6.5
## # ... with 1 more variable: Sepal.Width_median <dbl>
```

