\mainmatter

# Feature Engineering




Khi xây dựng mô hình dự báo, ta phải cân bằng giữa độ chính xác và khả năng giải thích của mô hình. Trong nhiều trường hợp, khả năng giải thích được ưu tiên hơn, thể hiện rất rõ trong các mô hình score card của rủi ro trong hệ thống ngân hàng. Tuy nhiên, ta không thể hy sinh độ chính xác để lấy khả năng giải thích nếu độ chính xác của mô hình không đạt đến một ngưỡng nhất định. Khi đó, có hai cách tiếp cận:

- Cho thêm biến vào mô hình.
- Thay đổi các biến có sẵn bằng các nhóm biến phái sinh để mô hình tốt hơn - cách tiếp cận này gọi là `feature engineering`.

`Feature Engineering` là quá trình thể hiện các biến đầu vào (variables) với những cách thức khác nhau để giúp cho tăng độ chính xác của mô hình dự báo. Ví dụ:

- Địa điểm của khách hàng có thể được thể hiện bằng ZIP code hoặc cùng lúc 2 biến, kinh độ và vĩ độ. 
- Biến dự báo `age` có thể cho ra kết quả tốt hơn trong mô hình khi ta dùng biến $\frac{1}{age}$

Với mỗi mô hình, thuật toán khác nhau sẽ yêu cầu những cách thức triển khai và thay đổi biến đầu vào khác nhau. Do đó, khi lựa chọn cách thức biến đổi biến đầu vào, ta phải nắm rất rõ thuật toán và cách thức biến đổi dữ liệu theo từng trường hợp khác nhau. Đối với `feature engineering`, ta có thể chia làm 2 nhóm chính.

- Biến đổi các biến định dạng nhóm (categorical)
- Biến đổi các biến định dạng số (numeric)

## Feature engineering cho các biến nhóm

### Tạo dữ liệu giả (dummy data) cho biến không phân biệt thứ tự

Trong phương pháp này, toàn bộ các dữ liệu gốc được chuyển sang dạng 0-1. Tuy nhiên, 
dữ liệu mới được tạo ra sẽ ít hơn dữ liệu gốc 1 trường hợp. Bởi lẽ khi biết giá trị của 6 biến, ta có thể biết được giá trị của biến cuối cùng.


+----------+-----+------------+-----+----------+--------+--------+
| Biến gốc | Mon |    Tues    | Wed |    Thurs |    Fri |    Sat |
+==========+=====+============+=====+==========+========+========+
| Sun      |  0  | 0          | 0   | 0        | 0      | 0      |
+----------+-----+------------+-----+----------+--------+--------+
| Mon      |  1  | 0          | 0   | 0        |  0     | 0      |
+----------+-----+------------+-----+----------+--------+--------+
| Tues     |  0  | 1          |  0  | 0        | 0      | 0      |
+----------+-----+------------+-----+----------+--------+--------+
| Wed      |  0  | 0          |  1  | 0        | 0      | 0      |
+----------+-----+------------+-----+----------+--------+--------+
| Thurs    |  0  | 0          |  0  | 1        | 0      | 0      |
+----------+-----+------------+-----+----------+--------+--------+
| Fri      |  0  | 0          |  0  | 0        | 1      | 0      |
+----------+-----+------------+-----+----------+--------+--------+
| Sat      |  0  | 0          |  0  | 0        | 0      | 1      |
+----------+-----+------------+-----+----------+--------+--------+

**zero-variance predictor**: là biến chỉ có một giá trị. Khi xây dựng mô hình, ta cần loại biến này.

### Dữ liệu có rất nhiều nhóm

Đối với các biến có rất nhiều nhóm (ví dụ: 200 chi nhánh trong ngân hàng), ta có 2 cách tiếp cận.

- Cách một, dựa vào kiến thức nghiệp vụ tự nhóm. Ví dụ, các chi nhánh ở Hà Nội sẽ đánh dấu là HN, ở Hồ Chí Minh là HCM, các chi nhánh còn lại là `Others`.
- Cách hai, sử dụng `hash function`. Trong trường hợp này, các biến category sẽ được tạo thành một biến hoàn toàn mới có giá trị số. Xem ví dụ dưới đây.

+-------------------+-------------+
|      Giá trị      |    Hash     |
+===================+=============+
| belvedere tiburon | 58275378    |
+-------------------+-------------+
| berkeley          | 1166288024  |
+-------------------+-------------+



**Lưu ý**: Nhiều thí nghiệm đã được sử dụng để so sánh sự khác biệt giữa việc dùng factor và encoding 0-1 trong dữ liệu. Kết quả cho thấy không có nhiều sự khác biệt giữa hai cách.

## Các biến liên tục

Đối với các biến liên tục, khi xây dựng mô hình, ta sẽ gặp phải các vấn đề sau.

- Các biến có các đơn vị khác nhau. Ví dụ, tuổi có giá trị từ 15-75, thu nhập có giá trị từ 2 triệu VND đến 200 triệu VND
- Các biến bị lệch sang phải (skewness)
- Các biến có xuất hiện giá trị ngoại lai (outliers)
- Các biến có thể bị chặn trai hoặc chặn phải. Ví dụ, độ tuổi có giá trị không quá 80

Đối với các biến số, có ba nhóm kỹ thuật lớn biến đổi dữ liệu.

- Biến đổi 1:1 - một biến được biến đổi thành một biến khác
- Biến đổi 1:n - một biến được biến đổi thành nhiều biến khác nhau
- Biến đổi n:n - n biến gốc được biến đổi cùng lúc thành n biến khác

### Biến đổi 1:1

Trong biến đổi 1:1, có rất nhiều cách khác nhau.

- Biến đổi theo scale của dữ liệu: log, Box-Cox


$$x^{*} = \left\{ \begin{array}{l l} \frac{x^{\lambda}-1}{\lambda\: \tilde{x}^{\lambda-1}}, & \lambda \neq 0 \\ \tilde{x} \: \log x, & \lambda = 0 \\ \end{array} \right.$$
