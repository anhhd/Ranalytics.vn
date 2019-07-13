# Các mẹo trong R




## Hiển thị số bình thường

__Vấn đề__: Khi sử dụng R, dữ liệu thường xuyên hiển thị dưới dạng khoa học (scientific). 

__Giải pháp__: Sử dụng options `options(scipen = 999)` để bỏ hiển thị dạng khoa học trong R

## Export dữ liệu ra excel

__Vấn đề__: Export dữ liệu từ dataframe ra excel
__Giải pháp__: Sử dụng `openxlsx` để export dữ liệu từ dataframe ra excel


```r
library(openxlsx)
library(tidyverse)

# Tạo dữ liệu ----

df <- data.frame("Date" = Sys.Date()-0:4,
                 "Logical" = c(TRUE, FALSE, TRUE, TRUE, FALSE),
                 "Currency" = paste("$",-2:2),
                 "Accounting" = -2:2,
                 "hLink" = "https://CRAN.R-project.org/",
                 "Percentage" = seq(-1, 1, length.out=5),
                 "TinyNumber" = runif(5) / 1E9, stringsAsFactors = FALSE)
df$Date <- df$Date %>% as.character()
class(df$Currency) <- "currency"
class(df$Accounting) <- "accounting"
class(df$hLink) <- "hyperlink"
class(df$Percentage) <- "percentage"
class(df$TinyNumber) <- "scientific"

## Format ----
options("openxlsx.borderStyle" = "thin")
options("openxlsx.borderColour" = "#4F81BD")

## Heading format
hs1 <- createStyle(fgFill = "darkgreen", 
                   halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")

## Insert data (simple) ----
wb <- createWorkbook()
addWorksheet(wb, "writeData auto-formatting")

writeData(wb, 1, df, startRow = 1, startCol = 1, headerStyle = hs1,
          borders = "rows", borderStyle = "thin")

## Thêm dữ liệu (datatable)

addWorksheet(wb, "Sheet2")
setColWidths(wb, 2, 1:100, widths = "auto")

writeDataTable(wb, 2, df, startRow = 1, startCol = 1, 
               tableStyle = "TableStyleLight1",
               headerStyle = hs1,
               withFilter = F)
openXL(wb) 

## Xóa gridLine cho tât cả các sheet ----

1:length(wb$sheet_names) %>% 
  map(as_mapper(function(x){
    showGridLines(wb, x, showGridLines = F)
  }))

# Lưu dữ liệu ----
saveWorkbook(wb, file = "my_file.xlsx", overwrite = TRUE)
```

## Làm việc khi proxy bị chặn

- Sửa option chọn mặc định proxy: `Tools-Global` >> `Options-Packages` >> Uncheck `Use Internet Explorer library/proxy for HTTP`
- Restart lại R
- Gõ câu lệnh `file.edit('~/.Renviron')`
- Thêm nội dung sau vào trong R


```r
options(internet.info = 0)
http_proxy="http://user_id:password@your_proxy:your_port"
# Ví dụ
http_proxy="http://anhhd3:password*@10.128.10.88:8080"
```

## Tự động render ra kết quả phân tích

**Lưu ý**:

- Cần có hai dấu cách (space) trước `\n`
- Đặt chế độ `result = 'asis'` để biến text thành kết quả

````markdown
```{r result = 'asis'}
for(i in unique(Month)) {
  cat("  \n###",  month.name[i], "Air Quaility  \n")
  #print(plot(airquality[airquality$Month == i,]))
  plot(airquality[airquality$Month == i,])
  cat("  \n")
}
```
`````




