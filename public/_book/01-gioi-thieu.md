# Lời mở đầu {-}

Trong quá trình triển khai công việc thực tế tại nhiều tổ chức khác nhau, tôi nhận thấy một hiện thực là mặt bằng kiến thức thực tế về phân tích dữ liệu trên thị trường Việt Nam còn rất yếu. Phần lớn, các bạn làm trong ngành phân tích dữ liệu rơi vào một trong hai nhóm sau:

- Nhóm một, **chưa có nền tảng về phân tích thống kê**. Ở nhóm này, các bạn thường không được đào tạo bài bản hoặc không có đủ điều kiện (phần lớn là về thời gian) để học các kiến thức về phân tích thống kê. Do nhu cầu của công việc, các bạn có thành thạo các kỹ năng về xây dựng báo cáo và phân tích khám phá dữ liệu đơn giản với SQL và Excel. Các bạn này thường thuộc các nhóm phân tích báo cáo (Business Intelligence) tại các tổ chức lớn hoặc làm trong startup. Điểm mạnh của nhóm này là làm việc sát với các bộ phận kinh doanh, hiểu rõ nghiệp vụ và nhu cầu nghiệp vụ. Tuy nhiên, điểm yếu của các bạn lại là không thể ứng dụng hoặc tự học và không biết cách triển khai các ứng dụng của khoa học dữ liệu ở mức độ cao vào công việc thực tế. 

- Nhóm hai, **có nền tảng vững vàng về kiến thức thống kê, dự báo nhưng lại quá chú trọng vào các yếu tố kỹ thuật**. Ở nhóm này, các bạn đều có nền tảng kiến thức về toán, thống kê rất tốt. Một số bạn được học và đào tạo cơ bản về khoa học dữ liệu, học máy và các ứng dụng của khoa học dữ liệu. Các bạn này có thiên hướng thích xây dựng mô hình dự báo, thích làm các bài toán lớn trong kinh doanh. Điểm mạnh của nhóm này là rất thông minh, chịu khó học hỏi và có thể áp dụng những kỹ thuật phân tích mới vào thực tế một cách nhanh chóng. Nhưng ngược lại, nhóm này lại có nhược điểm chết người là có thói quen chỉ tập trung vào việc phân tích dữ liệu mà thiếu đi cái nhìn tổng quát trong việc giải quyết bài toán thực tế. Không chỉ thế, nhóm này không có thế mạnh trong việc trình bày và giao tiếp, dẫn đến các kết quả thực tế không được các đơn vị kinh doanh nghiệp vụ đón nhận và sử dụng.

Đối với một tổ chức muốn phát triển dựa vào dữ liệu và muốn biến các quyết định của tổ chức dựa vào phân tích dữ liệu, cả hai nhóm trên đều là các trạng thái nên tránh và phải cân bằng được cả hai. Cuốn sách này sẽ phân tích và giúp các bạn làm trong lĩnh vực phân tích dữ liệu hiểu rõ hơn các ưu nhược điểm của chính mình.

# Khoa học dữ liệu và nghề phân tích dữ liệu {#intro}

## Khoa học dữ liệu là gì

Khoa học dữ liệu là nhóm ngành ứng dụng việc tổ chức và khai thác dữ liệu để hỗ trợ hoạt động ra quyết định. Phạm vi của khoa học dữ liệu rất rộng, từ việc tổ chức, quản lý, quản trị dữ liệu cho đến khai thác dữ liệu dưới dạng các báo cáo đơn giản cho đến các ứng dụng của Machine Learning, AI.

## Tại sao phân tích dữ liệu là nghề khó?

Phân tích dữ liệu đòi hỏi cùng lúc thực hiện ba nhóm công việc sau:

- Hiểu biết về vấn đề kinh doanh. Nghe có vẻ đơn giản nhưng qua quá trình làm việc thực tế, kinh nghiệm của tác giả cho thấy đây có lẽ là phần dễ bị bỏ qua nhất bỏi lẽ mấy nguyên nhân sau.
    - Sự khác biệt của vận hành kinh doanh so với hoạt động phân tích dữ liệu.
    - Tư duy và thái độ của nhóm phân tích dữ liệu. Các bạn phân tích kinh doanh (hoặc đôi khi được gọi là phân tích kinh doanh) thường tự coi mình là đơn vị hỗ trợ và *cung cấp* dữ liệu theo yêu cầu. Với lối tư duy thụ động này, các bạn sẽ không có nhu cầu tìm hiểu cặn kẽ các hoạt động kinh doanh, dẫn đến không hiểu hoạt động kinh doanh đủ sâu để có thể tư vấn và thuyết phục các bên kinh doanh trong việc triển khai các dự án phân tích dữ liệu mới. 
    - Nghiệp vụ kinh doanh rất phức tạp và thiếu hệ thống tài liệu ghi chép dưới góc độ khái quát cho hoạt động phân tích dữ liệu. Đây là vấn đề phần lớn các nhóm phân tích dữ liệu gặp phải. Để giải quyết vấn đề này, trong phần sau tác giả sẽ đưa ra phương pháp tìm hiểu hoạt động kinh doanh theo 6 nhóm vấn đề.
- Khai thác và phân tích dữ liệu
- Trình bày, thuyết phục và tư vấn cho các bên kinh doanh về kết quả phân tích dữ liệu.

**Ba cấp độ của viết code**: Khi sử dụng các công cụ phân tích dữ liệu (viết code), ta sẽ trải qua 3 cấp độ như sau:

1. Viết thứ đơn giản. Ở cấp độ này, các bạn thường mới nhập môn phân tích dữ liệu, đầy lo lắng và thiếu tự tin ở bản thân và bắt đầu với những bài phân tích đơn giản để áp dụng các kiến thức mới học.
2. Viết càng nguy hiểm càng tốt. Ở giai đoạn này, các bạn đã có một lượng kiến thức nền tương đối vững và bắt đầu đi vào các phương pháp nâng cao. Do đó, các bạn thường có xu hướng khiến mọi thứ trở nên *nguy hiểm*, tô vẽ và đưa ra nhiều yếu tố không thực sự cần thiết. Với các bạn có xu hướng trực quan hóa, sẽ là đưa ra các biểu đồ đầy màu sắc và cực kỳ nguy hiểm. Với các bạn có xu hướng xây dựng mô hình dự báo, sẽ là dùng mô hình dự báo, học máy hoặc deep-learning mọi lúc, mọi nơi. Kết quả sẽ khiến người đọc ấn tượng nhưng có thể chưa thực sự có tính ứng dụng cao.
3. Chỉ viết những thứ có khả năng tái sử dụng, giải quyết vấn đề bằng phương pháp đơn giản nhất có thể có. Ở giai đoạn này, các bạn đã dung hòa được rất nhiều kiến thức của nghành phân tích dữ liệu với nhau và tiếp cận các vấn đề một cách mạch lạc, logic và rất chặt chẽ. Các bạn nắm rất vững khi nào nên dùng các phương pháp phân tích khám phá dữ liệu đơn giản thay cho các thuật toán phức tạp trong việc giải quyết các vấn đề kinh doanh. 

**Công thức tổng quát khi viết code**: 

Khi phân tích dữ liệu (hay bất cứ hoạt động nào cần phải viết code), công thức tổng quát cần phải ghi nhớ là:

> $$e = mc^2 \Leftrightarrow error = ({more\;code})^2$$

Do đó, càng viết đơn giản, khả năng chúng ta giảm thiểu được các sai sót trong quá trình phân tích dữ liệu càng nhiều.

## Phân loại hoạt động phân tích dữ liệu

### Dựa theo cách thức tác động đến khách hàng

Khi làm việc thực tế, có nhiều nhánh trong hoạt động phân tích dữ liệu dẫn tới việc gây hiểu nhầm giữa các khía cạnh khác nhau. Khái quát hóa có thể chia làm hai nhánh lớn sau.

- Phân tích dữ liệu để tác động lên khách hàng một cách trực tiếp. Với cách tiếp cận này, để có thể tác động trực tiếp lên khách hàng, kết quả của hoạt động phân tích dữ liệu phải được đưa vào thực tế dưới dạng API và tác động trực tiếp lên UI mà người dùng sử dụng. Các hệ thống `recommendation engine` trên các trang thương mại điện tử thuộc loại này

- Phân tích dữ liệu để tác động lên khách hàng thông qua những người ra quyết định. Đây là nhóm được sử dụng đặc biệt nhiều trong thực tế nhưng lại bị đánh giá thấp hơn so với nhóm đầu tiên. Nhánh ứng dụng phân tích dữ liệu này được dùng đặc biệt nhiều trong các tổ chức lớn như ngân hàng, bảo hiểm... Trong nhóm này lại tách thành hai nhóm nhỏ:
    - Kết quả phân tích được thể hiện dưới dạng trình chiếu - công cụ thường hay được sử dụng nhất là powerpoint
    - Kết quả được sử dụng dưới dạng phần mềm (data tools). Với dạng này, người ra quyết định sẽ sử dụng các kết quả từ các công cụ này để đánh giá và ra quyết định thay đổi chính sách, sản phẩm cho phù hợp với khách hàng. Các công cụ như Google Analytics, Tableau... thuộc dạng này. 

**Lưu ý**: 

- Đối với mô hình nâng cao hơn, kết quả phân loại, dự báo có thể được hiển thị trên các công cụ này và khiến người sử dụng dễ dàng hơn trong việc ra quyết định.
- Nguồn dữ liệu sử dụng để phân tích có thể tách ra thành hai nhóm lớn:
    - Dữ liệu trong nội bộ doanh nghiệp. Nguồn dữ liệu này thể hiện phần lớn hành vi, thói quen của khách hàng đối với sản phẩm, dịch vụ do doanh nghiệp nắm giữ. Tổ chức càng lớn, nguồn dữ liệu này càng khó được khai thác hết nếu không có hệ thống Data Warehouse tốt do gặp phải vấn đề phân tán và chất lượng dữ liệu. Các hệ thống như T24, W4, EBank, LOS của ngân hàng, Google Analytics trên website/apps... thuộc vào nhóm này
    - Dữ liệu bên ngoài doanh nghiệp. Nguồn dữ liệu không do doanh nghiệp quản lý, phản ánh hành vi của khách hàng nói chung. Ví dụ - social network, blog, giao dịch trên các trang thương mại điện tử, etc.

Mục tiêu toàn bộ của hoạt động phân tích dữ liệu là sử dụng dữ liệu, thông qua phân tích, dự báo để tác động lên hành vi khách hàng nhằm gia tăng giá trị của sản phẩm, dịch vụ của doanh nghiệp đối với khách hàng. 

Khái quát hóa hoạt động phân tích dữ liệu có thể thể hiện qua sơ đồ sau.

<center>![](Images/01-analysis-overview.png)</center>

### Dựa theo đặc tính của nghề phân tích dữ liệu

<center>![](Images/01-data-science-job.png)</center>

Nếu bám sát vào hoạt động của sơ đồ phía trên, ta có thể chia các hoạt động có liên quan đến khoa học dữ liệu thành 4 nhóm lớn.

- **Data Governance** - Quản trị dữ liệu: Quản trị các hoạt động ghi nhận dữ liệu vào hệ thống của tổ chức, đưa ra các quy định, quy trình để đảm bảo dữ liệu được đưa vào hệ thống một cách chính xác và đảm bảo yêu cầu của hoạt động kinh doanh. Ví dụ, dữ liệu địa chỉ của khách hàng phải để dạng dropdown theo các đơn vị hành chính để tránh sai sót (quận/huyện, phường/xã, tỉnh, thành phố)
- **Data Management** - Quản lý dữ liệu: Quản lý dữ liệu sau khi dữ liệu được đưa vào hệ thống của tổ chức, bao gổm làm sạch, tái cấu trúc, sắp xếp dữ liệu theo hệ thống để đảm bảo sẵn sàng sử dụng dữ liệu
- **Business Intelligence**: Khai thác dữ liệu để hỗ trợ hoạt động kinh doanh và quá trình ra quyết định. Đối với hoạt động BI, có thể chia thành ba cấp độ.
    - **Data provider** - cung cấp dữ liệu: Đơn vị kinh doanh/ nghiệp vụ yêu cầu như thế nào, cung cấp chính xác như vậy
    - **Information & basic insight provider** - cung cấp thông tin & các hiểu biết đơn giản về khách hàng và hoạt động kinh doanh: Đưa ra các nhận định, thông tin ở mức độ đơn giản nhưng rất hữu dụng cho hoạt động kinh doanh. Công cụ sử dụng cho cấp độ này phần lớn là Excel với pivot table đi kèm với các chỉ số thống kê cơ bản. *Ví dụ, top 10% khách hàng chiếm đến 90% doanh thu của cả công ty.*
    - **Advanced insights provider** - cung cấp các hiểu biết sâu sắc về khách hàng và hoạt động kinh doanh. Để đưa ra các hiểu biết này, phải sử dụng nhiều đến các công cụ và mô hình thuộc nhóm `inference` nhằm tác động đến hoạt động kinh doanh. *Ví dụ, khách hàng nếu có ít nhất 3 loại giao dịch trong 2 tháng đầu tiên có khả năng ở lại với ngân hàng cao hơn 20% so với các khách hàng khác.*
- **Predictive Modelling/ Optimization** - Phân tích dự báo, tối ưu hóa. Ở mức độ này, công việc phân tích dữ liệu sẽ tập trung nhiều vào việc dự báo và tối ưu hóa. *Ví dụ: Khách hàng nào là khách hàng có nhiều khả năng bán chéo được sản phẩm thấu chi?* Các công cụ sử dụng trong cấp độ này là các phần mềm chuyên biệt về phân tích thống kê, dự báo như R/Python/SAS.

### Dựa theo chu kỳ của khách hàng đối với sản phẩm

<center>![](Images/1_customer_lifecycle.png){width=70%}</center>

Xét về khía cạnh vòng đời sản phẩm, chu kỳ khách hàng, bất cứ hoạt động kinh doanh nào cũng có thể chia thành chu kỳ như sau.

- **Tim kiếm khách hàng** - acquisition: Giai đoạn thu hút khách hàng mới, mục tiêu của giai đoạn này là đưa khách hàng mới về doanh nghiệp. Giai đoạn này, khách hàng chưa đem lại doanh số mà phần lớn chỉ là ở mức đăng ký dịch vụ, sản phẩm và trải nghiệm.
- **Kích hoạt khách hàng** - activation: Sau khi một người đã trở thành khách hàng của doanh nghiệp, không có nghĩa người đó sẽ sử dụng sản phẩm dịch vụ mà thường phải có quá trình `on-boarding`. Mục tiêu của giai đoạn này không chỉ là để khách hàng có những trải nghiệm đầu tiên với sản phẩm dịch vụ mà còn khiến các sản phẩm đó trở nên quen thuộc và khách hàng bắt đầu *thực sự* sử dụng sản phẩm dịch vụ. Đây là giai đoạn rất nhiều doanh nghiệp lớn *quên* không sử dụng và đưa vào vận hành, khiến khách hàng vừa ở giai đoạn acquisition đã chuyển sang giai đoạn `churn`
- **Khai thác khách hàng** - deep farming: Chỉ sau khi khách hàng thực sự sử dụng sản phẩm, dịch vụ, lúc này doanh nghiệp mới có thể tiếp tục khai thác khách hàng. Thông thường có hai nhóm là bán chéo (cross-selling) và bán thêm (up-selling).
- **Giữ chân khách hàng** - retaining customers: Sau một thời gian sử dụng sản phẩm, dịch vụ, khách hàng lúc này sẽ có xu hướng rời bỏ doanh nghiệp để sử dụng các sản phẩm khác cạnh tranh hơn. Do đó, mục tiêu phân tích trong giai đoạn này là dự báo, tìm kiếm và hỗ trợ các hoạt động kinh doanh giúp giảm `churn`
- **Thu hồi nợ** - collection/bad bank: Giai đoạn này thường chỉ sử dụng đối với các doanh nghiệp có hoạt động cho vay như ngân hàng. Mục tiêu phân tích là giảm tỷ lệ nợ, tỷ lệ nợ xấu

Ngoài các giai đoạn lớn trên, hoạt động phân tích dữ liệu còn tập trung vào 2 mảng lớn:

- **Giữ chân khách hàng chủ động** - proactively retaining customers: Phân tích để giữ chân khách hàng chủ động, ngay từ khi khách hàng chuyển qua giai đoạn có thể khai thác được. Nhiều doanh nghiệp lớn chưa chú trọng đến vấn đề này mà thường để đến khi khách hàng sắp rời bỏ doanh nghiệp mới tìm cách giữ chân. Khi đó, phần lớn mọi việc đã trở nên quá muộn.
- **Phân nhóm khách hàng** - customers segmentation: Phân nhóm khách hàng theo các đặc trưng về hành vi, tính chất để có thể đưa ra các sản phẩm phù hợp

## Cách xây dựng nhóm phân tích dữ liệu

Để xây dựng nhóm phân tích dữ liệu, cần đảm bảo 3 nguyên tắc lớn sau:

- Nắm vững kỹ năng phân tích khám phá dữ liệu
- Bám sát hoạt động kinh doanh
- Đẩy mạnh tương tác nhóm & phân nhóm kỹ năng dựa trên sở thích và tính cách cá nhân.

**Thứ nhất**, nền tảng của toàn bộ hoạt động phân tích dữ liệu (Analytics) phải giải quyết được bài toán kinh doanh dưới góc độ phân tích dữ liệu. Do đó, không nhất thiết phải xây dựng các mô hình phức tạp để giải quyết bài toán. Ngược lại, trong nhiều tình huống, ta nên sử dụng các phương pháp đơn giản bởi hai lý do: thứ nhất - có thể giải thích cho kinh doanh và thứ hai - rất nhiều vấn đề không cần xây mô hình cũng có thể giải quyết ngay được bởi các nguyên nhân từ vận hành, con người và sản phẩm. Chỉ khi nắm vững được kỹ năng phân tích khám phá dữ liệu ta mới có thể đưa ra các nhận định và kết quả một cách nhanh chóng và chính xác hỗ trợ hoạt động kinh doanh.

**Thứ hai**, hoạt động phân tích được sinh ra nhằm phục vụ kinh doanh. Do đó, nếu không nắm vững đến một mức độ nhất định hoạt động kinh doanh, ta không thể đưa ra các kết luận thực tế và có thể triển khai được. Rất nhiều trường hợp, nhóm phân tích dữ liệu quá chú trọng đến mặt kỹ thuật và thuật toán, đưa ra những kết luận phi thực tế. Điều này cần tránh dối với các tổ chức hướng đến việc ứng dụng phân tích dữ liệu thưc tiễn hỗ trợ kinh doanh.

**Thứ ba**, các nhà phân tích dữ liệu thường khá thông minh và thích làm việc tương đối độc lập. Không chỉ thế, có rất nhiều các khía cạnh khác nhau trong hoạt động phân tích dữ liệu. Có người ưa thích thuật toán và kiên trì và có thể tự học tốt nhưng lại yếu trong khâu giao tiếp và tương tác. Nhóm này nên khuyến khích phát triển các bài toán và vấn đề dự báo. Ngược lại, có nhóm rất thông minh, nhanh nhạy, thích thể hiện trước đông người nhưng lại thường xuyên cả thèm chóng chán. Nhóm này nên phát triển các kỹ năng khám phá dữ liệu, tìm kiếm insights và nên giao cho nhiệm vụ tương tác chính với kinh doanh. Ngoài ra, còn có nhóm trunh bình, việc gì cũng có thể làm ở mức độ khá nhưng không có việc gì thực sự nổi trội. Với nhóm này nên cố gắng bồi dưỡng để họ phát huy kiến thức tổng thể và hướng họ có thể back-up hai nhóm trên.

Thêm vào đó, việc phân rã kỹ năng còn giúp cho kiến thức không bị quá tập trung vào 1-2 thành viên, điều này có thể khiến cho hoạt động phân tích dữ liệu bị chậm lại do gặp phải vấn đề bottle-neck của những thành viên đó.

## Lưu ý khi đọc tài liệu

Việc viết cuốn sách này xuất phát thuần túy từ nhu cầu cá nhân của tác giả. Trong quá trình làm việc thực tế, bản thân tác giả luôn có mong muốn tổng hợp và đúc rút các kiến thức phân tích thực tế. Tuy nhiên, khi bắt đầu bắt tay vào xây dựng cuốn sách, tác giả cũng có mong muốn có thể đúc kết và giúp cho các tổ chức, bộ phận muốn tập trung vào và khai thác sức mạnh của phân tích dữ liệu trong thực tế có thể có thêm các nguồn tài liệu và kinh nghiệm, vốn rất ít được chia sẻ thực tế, để có thể thành công hơn trong công việc triển khai. 

Thêm vào đó, trong quá trình làm việc và giảng dạy cho các học viên, tác giả luôn nhận được câu hỏi: *Thưa thày, em không được đào tạo bài bản về phân tích thống kê cũng như khoa học dữ liệu, liệu em có thể trở thành chuyên gia phân tích dữ liệu được không?*

Tác giả luôn trăn trở với câu hỏi trên cũng như với kinh nghiệm đào tạo thực tế và xây dựng năng lực phân tích dữ liệu ở VPBank, quyển sách này được viết ra theo cách tiếp cận *ứng dụng* của khoa học dữ liệu trong hoạt động kinh doanh. Do đó, ngôn ngữ cũng như cách tiếp cận trong cuốn sách này được cố gắng viết một cách đơn giản, dễ hiểu để trình bày các kiến thức, thuật ngữ khó hiểu của khoa học dữ liệu thành các ngôn ngữ bình dân phù hợp với nhiều đối tượng. 

Cuốn sách này sẽ không đi sâu vào lý thuyết của các thuật toán, mô hình thống kê mà sẽ cố gắng trả lời các khía cạnh sau.

- Thuật toán, mô hình đó là gì?
- Mô hình đó được sử dụng như thế nào?
- Khi giải thích cho các đơn vị kinh doanh, ta cần phải giải thích điều gì?
- Ứng dụng của mô hình trong thực tế