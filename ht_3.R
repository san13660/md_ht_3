install.packages("cluster")
install.packages("fpc")
install.packages("factoextra")
install.packages("ggplot2")

library(cluster) #Para calcular la silueta
library(fpc) #para hacer el plotcluster
library(factoextra) #Para hacer gr?ficos bonitos de clustering
library(ggplot2)
library(e1071)#para cmeans


install.packages("rpart")
install.packages("tree")
install.packages("rpart.plot")
install.packages("randomForest")

library(rpart)
library(tree)
library(rpart.plot)
library(randomForest)

install.packages("plyr")
library(plyr)


data_test <- read.csv("test.csv", stringsAsFactors = FALSE)
data_training <- read.csv("train.csv", stringsAsFactors = FALSE)
data_sample <- read.csv("sample_submission.csv", stringsAsFactors = FALSE)

pairs(~MSSubClass+LotFrontage+LotArea+OverallQual+OverallCond+YearBuilt+SalePrice,data=data_training,
      main="Matriz de dispersion 1")

pairs(~YearRemodAdd+MasVnrArea+BsmtFinSF1+BsmtFinSF2+BsmtUnfSF+TotalBsmtSF+SalePrice,data=data_training,
      main="Matriz de dispersion 2")

pairs(~X1stFlrSF+X2ndFlrSF+LowQualFinSF+GrLivArea+BsmtFullBath+BsmtHalfBath+FullBath+SalePrice,data=data_training,
      main="Matriz de dispersion 3")

data_training_filtered <- data_training[, c("SalePrice", "OverallQual", "YearBuilt", "TotalBsmtSF", "X1stFlrSF", "GrLivArea", "FullBath")]

wss <- (nrow(data_training_filtered)-1)*sum(apply(data_training_filtered,2,var))

for (i in 2:10) 
  wss[i] <- sum(kmeans(data_training_filtered, centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")

hist(data_training$SalePrice)

# a. Método de las siluetas para k-means
km<-kmeans(data_training_filtered,3)
silkm<-silhouette(km$cluster,dist(data_training_filtered))
mean(silkm[,3])
plotcluster(data_training_filtered, km$cluster)
data_training_filtered$Class<-km$cluster
# Resultado de 0.83

cluster1 <- data_training_filtered[data_training_filtered$Class=="Cara",]
cluster2 <- data_training_filtered[data_training_filtered$Class=="Intermedia",]
cluster3 <- data_training_filtered[data_training_filtered$Class=="Economica",]


summary(cluster1)
summary(cluster2)
summary(cluster3)


data_training_filtered$Class <- mapvalues(data_training_filtered$Class,
                                          from = c(2,3,1),
                                          to = c("Economica", "Intermedia", "Cara"))

data_training_filtered_no_price <- data_training_filtered[,2:8]
dt_model<-rpart(Class~.,data_training_filtered_no_price,method = "class")
plot(dt_model);text(dt_model)
prp(dt_model)
rpart.plot(dt_model, box.palette = "GnYlRd")

data_training_filtered_no_class <- data_training_filtered[,1:7]
dt_model_regression<-rpart(SalePrice~.,data_training_filtered_no_class,method = "anova")
plot(dt_model_regression);text(dt_model_regression)
prp(dt_model_regression)
rpart.plot(dt_model_regression, box.palette = "GnYlRd")
