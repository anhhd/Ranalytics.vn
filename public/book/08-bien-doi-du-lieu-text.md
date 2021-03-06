# Biến đổi dữ liệu text 





```r
library(dplyr)
library(stringr)
```

## Giới thiệu

Bên cạnh dữ liệu định dạng số, dữ liệu chứa rất nhiều định dạng character. Do đó, việc nắm vững các nguyên lý và biến đổi dữ liệu định dạng character trong R sẽ giúp rất nhiều trong việc xử lý dữ liệu. 

Để tạo chuỗi string, trong R có 3 cách cơ bản:

- Dùng dấu ngoặc kép
- Dùng dấu ngoặc đơn
- Dùng hàm **character**


```r
x <- "Chú mèo nằm trên lan can"
y <- 'Chú mèo nằm trên lan can'
```


**Lưu ý**: Ta có thể để dấu ngoặc đơn hoặc ngoặc kép trong character như sau:

- Ngoặc đơn trong ngoặc kép *hoặc*
- Ngoặc kép trong ngoặc đơn


```r
(x <- "This is 'MY' apple")
```

```
## [1] "This is 'MY' apple"
```

```r
(y <-  'This is "HIS" orange')
```

```
## [1] "This is \"HIS\" orange"
```

**Lưu ý**: **Empty string** là chuỗi không có ký tự, được viết **""**


```r
a <- ""
a
```

```
## [1] ""
```

```r
a %>% class
```

```
## [1] "character"
```

- `paste` và `paste0`: Đây là hai hàm cơ bản cho phép ghép các chuỗi lại với nhau


```r
#Hàm paste mặc định có khoảng cách giữa các object
paste("I love", pi)
```

```
## [1] "I love 3.14159265358979"
```

```r
#Hàm paste0 không có khoảng cách
paste0("I love", pi)
```

```
## [1] "I love3.14159265358979"
```

```r
#Paste có sep
paste("I love", pi, sep = "___")
```

```
## [1] "I love___3.14159265358979"
```

```r
#Paste khi có length khác nhau
paste(1:3, "a", sep = "")
```

```
## [1] "1a" "2a" "3a"
```

```r
#Paste không có collapse
paste(1:3, "a", sep = "", collapse = "")
```

```
## [1] "1a2a3a"
```

```r
#Khi paste giá trị NA, R biến NA thành string
paste("abc ", NA)
```

```
## [1] "abc  NA"
```

- `toString` cho phép biến đổi thành một vector thành chuối


```r
a <- c(pi, 19)
a
```

```
## [1]  3.141593 19.000000
```

```r
a %>% toString
```

```
## [1] "3.14159265358979, 19"
```

- Các hàm  `toupper`, `tolower`, `casefold` cho phép biến đổi chuỗi thành các dạng in hoa và in thường


```r
#Chuyển sang lower
tolower(c("aLL ChaRacterS in LoweR caSe", "ABCDE"))
```

```
## [1] "all characters in lower case" "abcde"
```

```r
#Chuyển sang upper
toupper(c("aLL ChaRacterS in LoweR caSe", "ABCDE"))
```

```
## [1] "ALL CHARACTERS IN LOWER CASE" "ABCDE"
```

```r
#Sử dụng với case fold
casefold(c("aLL ChaRacterS in LoweR caSe", "ABCDE"))
```

```
## [1] "all characters in lower case" "abcde"
```

```r
casefold(c("aLL ChaRacterS in LoweR caSe", "ABCDE"), upper = T)
```

```
## [1] "ALL CHARACTERS IN LOWER CASE" "ABCDE"
```

- **str_c**: Tương tự như paste, sử dụng `""` là ký tự phân tách chuỗi mặc định
- **str_length**: Kiểm tra độ dài của ký tự


```r
library(stringr)
str_c("Ola", "ede")
```

```
## [1] "Olaede"
```

```r
str_c("Ola", "ede", sep = "_")
```

```
## [1] "Ola_ede"
```

```r
c("olala") %>% str_length()
```

```
## [1] 5
```

- **str_sub**: Lấy chuỗi con trong 1 chuỗi


```r
x <- c("English", "Polish", "Other language")
#Lấy 2 ký tự đầu tiên
str_sub(x, start = 1, end = 2)
```

```
## [1] "En" "Po" "Ot"
```

```r
#Lấy 2 ký tự cuối cùng
str_sub(x, start = -2, end = -1)
```

```
## [1] "sh" "sh" "ge"
```

```r
#Sử dụng str_sub để thay thế
str_sub(x, start = -2, end = -1) <- str_sub(x, start = -2, end = -1) %>% toupper()
x
```

```
## [1] "EngliSH"        "PoliSH"         "Other languaGE"
```

- **str_wrap**: Wrap ký tự - hàm này rất hữu dụng khi sử dụng tên trong biểu đồ


```r
some_quote = c(
  "I may not have gone",
  "where I intended to go,",
  "but I think I have ended up",
  "where I needed to be"
  )

some_quote <- str_c(some_quote)
some_quote  
```

```
## [1] "I may not have gone"         "where I intended to go,"    
## [3] "but I think I have ended up" "where I needed to be"
```

```r
str_wrap(some_quote, width = 30, indent = 2) %>% cat
```

```
##   I may not have gone   where I intended to go,   but I think I have ended up   where I needed to be
```

**Lưu ý**: Khi làm việc với chuỗi, hàm `cat` cho phép thể hiện được các ký tự đặc biệt trên R console


```r
# Không dùng cat
x <- "Dòng 1 \nDòng 2"
x
```

```
## [1] "Dòng 1 \nDòng 2"
```

```r
# Sử dụng cat
x %>% cat
```

```
## Dòng 1 
## Dòng 2
```


- **str_trim**: Sử dụng để cắt các đoạn text có ký tự trắng


```r
bad_text = c("This", " example ", "has several ", " whitespaces ")
bad_text %>% str_trim(side = "left")
```

```
## [1] "This"         "example "     "has several " "whitespaces "
```

```r
bad_text %>% str_trim(side = "both")
```

```
## [1] "This"        "example"     "has several" "whitespaces"
```

- **str_extract**: Chiết xuất giá trị khỏi chuỗi
- **str_extract_all**: Chiết xuất giá trị khỏi chuỗi, trả ra list


```r
strings <- c(
  "apple", 
  "219 733 8965", 
  "329-293-8753", 
  "Work: 579-499-7527; Home: 543.355.3679"
)
# Chiết xuất giá trị 5 ra khỏi chuỗi
strings %>% str_extract("5")
```

```
## [1] NA  "5" "5" "5"
```

```r
strings %>% str_extract_all("5")
```

```
## [[1]]
## character(0)
## 
## [[2]]
## [1] "5"
## 
## [[3]]
## [1] "5"
## 
## [[4]]
## [1] "5" "5" "5" "5" "5"
```

- **str_count**: Kiểm tra số lần xuất hiện của chuỗi con trong chuỗi


```r
states <- rownames(USArrests)
states
```

```
##  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
##  [5] "California"     "Colorado"       "Connecticut"    "Delaware"      
##  [9] "Florida"        "Georgia"        "Hawaii"         "Idaho"         
## [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"        
## [17] "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
## [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
## [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"        
## [29] "New Hampshire"  "New Jersey"     "New Mexico"     "New York"      
## [33] "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
## [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
## [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"          
## [45] "Vermont"        "Virginia"       "Washington"     "West Virginia" 
## [49] "Wisconsin"      "Wyoming"
```

```r
#Kiểm tra độ dài ký tự trong vector
states %>% str_length()
```

```
##  [1]  7  6  7  8 10  8 11  8  7  7  6  5  8  7  4  6  8  9  5  8 13  8  9
## [24] 11  8  7  8  6 13 10 10  8 14 12  4  8  6 12 12 14 12  9  5  4  7  8
## [47] 10 13  9  7
```

```r
#Đếm số lần xuất hiện ký tự trong vector
str_count(states %>% toupper(), "A")
```

```
##  [1] 4 3 2 3 2 1 0 2 1 1 2 1 0 2 1 2 0 2 1 2 2 1 1 0 0 2 2 2 1 0 0 0 2 2 0
## [36] 2 0 2 1 2 2 0 1 1 0 1 1 1 0 0
```

- `str_detect`: Kiểm tra tồn tại của ký tự trong chuỗi


```r
string <- c("Hello", "Vietnam", "Apple")
str_detect(string, "l")
```

```
## [1]  TRUE FALSE  TRUE
```

- `str_replace`: Thay thế ký tự đầu tiên trong chuỗi, `str_replace_all` - thay thế tất cả ký tự trong chuỗi


```r
string <- c("Hello", "Vietnam", "Apple", "", NA)
# Thay thế ký tự đầu tiên
string %>% str_replace("l", "99")
```

```
## [1] "He99lo"  "Vietnam" "App99e"  ""        NA
```

```r
# Thay thế tất cả ký tự
string %>% str_replace_all("l", "99")
```

```
## [1] "He9999o" "Vietnam" "App99e"  ""        NA
```

```r
# Thay thế NA
string %>% str_replace_na("9999")
```

```
## [1] "Hello"   "Vietnam" "Apple"   ""        "9999"
```

**Lưu ý**: `""` được coi là một ký tự vẫn có độ dài bằng 0


```r
"" %>% str_length()
```

```
## [1] 0
```

- `str_locate`: Kiểm tra vị trí đầu tiên xuất hiện ký tự, `str_locate_all` - kiểm tra tất cả các vị trí xuất hiện ký tự


```r
"abbbbcab" %>% str_locate("a")
```

```
##      start end
## [1,]     1   1
```

```r
"abbbbcaab" %>% str_locate_all("a")
```

```
## [[1]]
##      start end
## [1,]     1   1
## [2,]     7   7
## [3,]     8   8
```

## Regular expression

**Regular expression** (regex) là việc chỉ dẫn một hàm cách thức tìm, thay thế và xử lý dữ liệu dạng string. Cơ bản có 4 dạng:

- **Concetenation (chuỗi ký tự)**: Tìm chuỗi ký tự: "abcd"
- **Logical (phép logic)**: Tìm chuỗi chứa ab hoặc cd: "ab|cd"
- **Repetition (lặp lại ký tự)**: Tìm chuỗi lặp lại một hoặc nhiều lần ký tự a: "a+"
- **Grouping (Nhóm ký tự)**: Nhóm các ký tự muốn tìm, sử dụng dấu ngoặc đơn ()

Khi phân tích **regex**, có hai khía cạnh cần quan tâm:

- Hàm sử dụng trên R
- Cách thức tương tác

Đối với cách thức tương tác trong R, có các nhóm vấn đề sau:

- Metadata
- Sequennces
- Character class

### Metadata

Metadata là các ký hiệu đặc biệt được thể hiện trong R nhằm biểu đạt toán tử nhất định. Để làm việc với các ký tự có định dạng giống meta data, cần thêm dâu `\\`. Xem ví dụ dưới đây


```r
x <- "Vietnam|HaNoi"
# Không đúng định dang
x %>% str_replace_all("|", "_")
```

```
## [1] "_V_i_e_t_n_a_m_|_H_a_N_o_i_"
```

```r
# Sử dụng đúng metadata
x %>% str_replace_all("\\|", "_")
```

```
## [1] "Vietnam_HaNoi"
```

Trong trường hợp trên, gsub xử lý ký tự **$** như *regular expression* cho các ký tự đặc biệt. 


```r
gsub(pattern = "\\$", replacement = ".", "Vietnam$Ha$Noi")
```

```
## [1] "Vietnam.Ha.Noi"
```

Metadata trong regular expressions bao gồm các ký tự sau: 

> $ * + . ? [ ] ^ { } | ( ) \



+--------+-------------------------------------------------------------+
| Ký tự  |                           Ý nghĩa                           |
+========+=============================================================+
| `.`    | matches everything except for the empty sting ``.           |
+--------+-------------------------------------------------------------+
| `+`    | the preceding item will be matched one or more times.       |
+--------+-------------------------------------------------------------+
| `*`    |  the preceding item will be matched zero or more times.     |
+--------+-------------------------------------------------------------+
| `^`    |  matches the empty string at the at the beginning of a line |
+--------+-------------------------------------------------------------+
| `$`    |  matches empty string at the end of a line.                 |
+--------+-------------------------------------------------------------+
| `|`    |  infix operator: OR                                         |
+--------+-------------------------------------------------------------+
| `()`   | brackets for grouping.                                      |
+--------+-------------------------------------------------------------+
| `[]`   | character class brackets                                    |
+--------+-------------------------------------------------------------+



**Lưu ý**: Khi sử dụng dấu gạch ngoặc kép (\\\\) là ký tự bình thưởng, sử dụng 4 gạch chéo. Xem các ví dụ dưới đây


```r
meta_char <- c("$","*","+",".","?","[","^","{","|","(","\\")
meta_char
```

```
##  [1] "$"  "*"  "+"  "."  "?"  "["  "^"  "{"  "|"  "("  "\\"
```

```r
#Không sử dụng chính xác
str_locate(meta_char, "^")
```

```
##       start end
##  [1,]     1   0
##  [2,]     1   0
##  [3,]     1   0
##  [4,]     1   0
##  [5,]     1   0
##  [6,]     1   0
##  [7,]     1   0
##  [8,]     1   0
##  [9,]     1   0
## [10,]     1   0
## [11,]     1   0
```

```r
#Sử dụng chính xác
meta_char %>% str_locate("\\^")
```

```
##       start end
##  [1,]    NA  NA
##  [2,]    NA  NA
##  [3,]    NA  NA
##  [4,]    NA  NA
##  [5,]    NA  NA
##  [6,]    NA  NA
##  [7,]     1   1
##  [8,]    NA  NA
##  [9,]    NA  NA
## [10,]    NA  NA
## [11,]    NA  NA
```

```r
#Hai dâu gạch chéo
meta_char %>% str_locate("\\\\")
```

```
##       start end
##  [1,]    NA  NA
##  [2,]    NA  NA
##  [3,]    NA  NA
##  [4,]    NA  NA
##  [5,]    NA  NA
##  [6,]    NA  NA
##  [7,]    NA  NA
##  [8,]    NA  NA
##  [9,]    NA  NA
## [10,]    NA  NA
## [11,]     1   1
```

### Sequences

Sequence trong R có các nhóm sau:

![](Images/08-plot3.png)


```r
#sub thay thế giá trị phù hợp đầu tiên
sub("\\d","*", "Y2K was in 2000")
```

```
## [1] "Y*K was in 2000"
```

```r
#gsub thay thế tất cả các giá trị phù hợp
gsub("\\d","*", "Y2K was in 2000")
```

```
## [1] "Y*K was in ****"
```

```r
#Thay thế những ký tự không phải là số
gsub("\\D","*", "Y2K was in 2000")
```

```
## [1] "*2*********2000"
```

```r
#Thay thế space
gsub("\\s","*", "Y2K was in 2000")
```

```
## [1] "Y2K*was*in*2000"
```

```r
#Non space
gsub("\\S","*", "Y2K was in 2000")
```

```
## [1] "*** *** ** ****"
```

### Character class

Character class sẽ tìm **MỘT** ký tự phù hợp trong bracket []


```r
#Tìm ký tự từ a-z
gsub("[a-z]", "*", "Vietnam in 2016")
```

```
## [1] "V****** ** 2016"
```

```r
#Tìm số từ 0-9
gsub("[0-9]", "*", "Vietnam in 2016")
```

```
## [1] "Vietnam in ****"
```

```r
#Tìm các ký tự trừ số
grep("[^0-9]", c("Vietnam in", " $%^$", "2016"), value = T)
```

```
## [1] "Vietnam in" " $%^$"
```

### POSIX class

Posix class được thể hiện trong 2 dấu ngoặc kép định dạng `[[:class:]]` như sau.

![](Images/08-plot4.png)


```r
example <- c("Alo ala #^ 12,6.7")
#Loại blank
gsub("[[:blank:]]", "", example)
```

```
## [1] "Aloala#^12,6.7"
```

```r
#Loại dấu
gsub("[[:punct:]]", "", example)
```

```
## [1] "Alo ala  1267"
```

### Quantifiers

Quantifier là nhóm điều kiện đếm số lần lặp lại của 1 nhóm ký tự

![](Images/08-plot5.png)


```r
# people names
people = c("rori", "emilia", "matteo", "mehmet", "filipe", "anna", "tyler",
"rasmus", "jacob", "youna", "flora", "adi")
# Tìm tên có xuất hiện m nhiều nhất 1 lần
grep(pattern = "m?", people, value = TRUE)
```

```
##  [1] "rori"   "emilia" "matteo" "mehmet" "filipe" "anna"   "tyler" 
##  [8] "rasmus" "jacob"  "youna"  "flora"  "adi"
```

```r
# Tên xuất hiện m chính xác 1 lần
grep(pattern = "m{1}", people, value = TRUE, perl = FALSE)
```

```
## [1] "emilia" "matteo" "mehmet" "rasmus"
```

```r
# Tên có xuất hiện hoặc không có xuất hiện m rồi xuất hiện t
grep("m*t", people, value = T)
```

```
## [1] "matteo" "mehmet" "tyler"
```

```r
#Tên có m một hoặc nhiều lần
grep("m+t", people, value = T)
```

```
## character(0)
```

```r
#Tên có m một hoặc nhiều lần rồi đến t
grep("m+.t", people, value = T)
```

```
## [1] "matteo" "mehmet"
```


```r
paris_tweets = c(
"#Paris is chock-full of cultural and culinary attractions",
"Some time in #Paris along Canal St.-Martin famous by #Amelie",
"While you're in #Paris, stop at cafe: http://goo.gl/yaCbW",
"Paris, the city of light")
# match (all) hashtags in 'paris_tweets'
str_match_all(paris_tweets, "#[a-zA-Z]{1,}")
```

```
## [[1]]
##      [,1]    
## [1,] "#Paris"
## 
## [[2]]
##      [,1]     
## [1,] "#Paris" 
## [2,] "#Amelie"
## 
## [[3]]
##      [,1]    
## [1,] "#Paris"
## 
## [[4]]
##      [,1]
```

## Các ví dụ tổng hợp 

### Ví dụ 1

- **Vấn đề**: Thay thế các ký tự đặc biệt lặp đi lặp lại
- **Giải pháp**: Sử dụng dấu **+**


```r
d1 = data.frame(
  id...of....patient = c(1, 2), 
  patient....age = c(1, + 2))
names(d1) <- gsub(pattern = "\\.+", 
                  replacement = "_", x = names(d1))
names(d1)
```

```
## [1] "id_of_patient" "patient_age"
```

### Ví dụ 2

- **Vấn đề**: Lọc các ký tự trong 1 nhóm
- **Giải pháp**: 
    - Sử dụng ký tự []
    - Có thể sử dụng dấu - để tìm trong khoảng


```r
#Tìm vị trí các ký tự
grep("[abcAB]", x = c("Con ca", "Boong", "Xoong"))
```

```
## [1] 1 2
```

```r
grep("[0-3]", 
     c("nose", "letter38", "window9", "apple0"), value = TRUE)
```

```
## [1] "letter38" "apple0"
```

**Chi tiết xem trong bảng dưới đây**

![](Images/08-plot2.png)

### Ví dụ 3


```r
d <- data.frame(
  id = c(11, 22, 33, 44, 55, 66, 77, 88), 
  drug = c("vitamin E", "vitamin ESTER-C", 
           " vitamin Eabc ", "vitamin E(ointment)", "", 
           "vitamin E ", "provitamin E\n", "vit E"), 
  text = c(""," ", " 3 times a day after meal", 
           "once a day", " ",
           " one per day ", "\t", "\n "), 
  stringsAsFactors = FALSE)
(s <-  d$text)
```

```
## [1] ""                          " "                        
## [3] " 3 times a day after meal" "once a day"               
## [5] " "                         " one per day "            
## [7] "\t"                         "\n "
```

```r
#Các giá trị riêng của s
unique(s)
```

```
## [1] ""                          " "                        
## [3] " 3 times a day after meal" "once a day"               
## [5] " one per day "             "\t"                        
## [7] "\n "
```

```r
#Xử lý ký tự
#Các ký tự trong ngoặc vuông chuyển thành NA
gsub("[\t\n\r\f\v]+", NA, s)
```

```
## [1] ""                          " "                        
## [3] " 3 times a day after meal" "once a day"               
## [5] " "                         " one per day "            
## [7] NA                          NA
```

```r
#Chuyển tất cả các ký tự
gsub("^$|^( +)$|[\t\n\r\f\v]+", NA, s)
```

```
## [1] NA                          NA                         
## [3] " 3 times a day after meal" "once a day"               
## [5] NA                          " one per day "            
## [7] NA                          NA
```

```r
#Chuyển tất cả các khoảng trắng sang NULL
gsub("^([ \t\n\r\f\v]+)|([ \t\n\r\f\v]+)$", "", s)
```

```
## [1] ""                         ""                        
## [3] "3 times a day after meal" "once a day"              
## [5] ""                         "one per day"             
## [7] ""                         ""
```

```r
######
#Tìm vitamin e
t <- d$drug
grep("vitamin e", t, ignore.case = T, value = T)
```

```
## [1] "vitamin E"           "vitamin ESTER-C"     " vitamin Eabc "     
## [4] "vitamin E(ointment)" "vitamin E "          "provitamin E\n"
```

```r
#Tìm vitamin e mà đi sau đi kèm ký tự alphabet
grep("vitamin e([a-zA-Z])", t, ignore.case = T, value = T)
```

```
## [1] "vitamin ESTER-C" " vitamin Eabc "
```

```r
#Tìm vitamin e mà đi sau không đi kèm gì
grep("vitamin e($)", t, ignore.case = T, value = T)
```

```
## [1] "vitamin E"
```

```r
#Tìm vitamin e mà đi sau không phải là ký tự alphabet
grep("vitamin e($|[^a-zA-Z])", s, ignore.case = TRUE, value = TRUE)
```

```
## character(0)
```

```r
#Tìm thêm vit e
grep("vitamin e($|[^a-zA-Z])|vit e($|[^a-zA-Z])", t, 
     ignore.case = TRUE, value = TRUE)
```

```
## [1] "vitamin E"           "vitamin E(ointment)" "vitamin E "         
## [4] "provitamin E\n"      "vit E"
```

```r
#Tìm ký tự bắt đầu với _vit
grep(" vit", t, ignore.case = T, value = T)
```

```
## [1] " vitamin Eabc "
```

```r
#Loại prodvitamin
grep("([^a-z]+|^)vitamin e($|[^a-zA-Z])|([^a-z]+|^)vit e($|[^a-zA-Z])",
     t, ignore.case = TRUE, value = TRUE)
```

```
## [1] "vitamin E"           "vitamin E(ointment)" "vitamin E "         
## [4] "vit E"
```

### Ví dụ 4

- Vấn đề: Thay thế ký tự rỗng 
- Giải pháp: Sử dụng `"^$"`


```r
df <- data.frame(x = c("1", "viet", ""))
#Không chạy
df$x %>% as.character %>% str_replace_na("Missing")
```

```
## [1] "1"    "viet" ""
```

```r
#Chạy
df$x %>% as.character %>% str_replace_all("^$", "Missing")
```

```
## [1] "1"       "viet"    "Missing"
```

## Tài liệu tham khảo

- [gsub](http://www.endmemo.com/program/R/gsub.php)






