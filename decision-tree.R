install.packages('caret')
library(caret)    

install.packages('rpart') #thư viện dùng xây dựng cây quyết định (decision tree)

library(rpart)
install.packages("rpart.plot")

library('rpart.plot')

install.packages("rpart.plot") #thư viện dùng trực quan hoá cây quyết định

options(repr.plot.width = 6, repr.plot.height = 5) #thiết lập kích thước của đồ thị được hiển thị, rong 6, cao 5


dec_tree <- rpart(Cluster ~ year_birth,              # Dự đoán mua dựa trên cluster
                     data = data_with_cluster) 


prp(dec_tree,        #plot the decision tree
    space = 4,       # khoảng cách 
    split.cex = 1.5, # kích thước cỡ chữ
    nn.border.col = 0) #điều chỉnh màu sắc và đường viền 

class_tree = rpart(Cluster ~ Year_Birth + Marital_Status + education,
                   data = data_with_cluster)

prp(class_tree,
    space = 4,
    split.cex = 1.2,
    nn.border.col = 0)

complex_tree = rpart(Cluster ~ Year_Birth + Marital_Status + education
                      + data_with_cluster$MntWines +
                        data_with_cluster$MntFruits +
                        data_with_cluster$MntMeatProducts +
                        data_with_cluster$MntFishProducts +
                        data_with_cluster$MntSweetProducts +
                        data_with_cluster$MntGoldProds +
                        data_with_cluster$NumDealsPurchases +
                        data_with_cluster$NumWebPurchases +
                        data_with_cluster$NumCatalogPurchases +
                        data_with_cluster$NumStorePurchases +
                        data_with_cluster$NumWebVisitsMonth,
                      cp = 0.001,                 # Cài thông số complexity
                      data = data_with_cluster)       

options(repr.plot.width = 8, repr.plot.height = 8) #cài độ rộng, cao 

prp(complex_tree, 
    type = 1,
    nn.border.col=0, 
    border.col=1, 
    cex=0.4)


limited_complex_tree = rpart(Cluster ~ Year_Birth + Marital_Status + education
                      + data_with_cluster$MntWines +
                        data_with_cluster$MntFruits +
                        data_with_cluster$MntMeatProducts +
                        data_with_cluster$MntFishProducts +
                        data_with_cluster$MntSweetProducts +
                        data_with_cluster$MntGoldProds +
                        data_with_cluster$NumDealsPurchases +
                        data_with_cluster$NumWebPurchases +
                        data_with_cluster$NumCatalogPurchases +
                        data_with_cluster$NumStorePurchases +
                        data_with_cluster$NumWebVisitsMonth,
                      cp = 0.001,   # Set complexity parameter
                      maxdepth = 5,
                      minbucket = 5,
                      method = "class",
                      data = data_with_cluster)       # Use the titanic training data

options(repr.plot.width = 8, repr.plot.height = 8)

prp(complex_tree, 
    type = 1,
    nn.border.col=0, 
    border.col=1, 
    cex=0.4)

train_preds = predict(limited_complex_tree, #hàm predict tạo ra dự đoán từ mô hình cây quyết định
                       newdata = data_with_cluster, #dự đoán được thực hiện trên tập data_with_cluster
                       type = "class")               # Trả về dự đoán lớp

confusionMatrix(factor(train_preds), factor(data_with_cluster$Cluster))
#confusionMatrix đưa ra tính toán và hiển thị ma trận nhầm lẫn giữa dự đoán và giá trị thực tế
#factor(train_preds): chuyển dự đoán thành đối tượng factor


