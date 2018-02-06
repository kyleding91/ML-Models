install.packages("evtree")
library(evtree)
library(pROC)
library(help = "datasets")
library(caTools)

#Data Preprocess
data(iris)

iris_data <- iris

iris_data$Species <- as.factor(iris_data$Species)


train_index <- sample.split(iris_data$Species,1/3)

iris_data_train <- iris_data[train_index,]
iris_data_test <- iris_data[-train_index,]


#OCT
ev <- evtree(Species~.,iris_data_train)

plot(ev)

pred_ev<-predict(ev,iris_data_test)

table(pred_ev, iris_data_test$Species)

#CART
library(rpart)

ct <- rpart(Species~.,iris_data_train,method = "class")

plot(ct)
text(ct)

printcp(ct)

pred_ct<-predict(ct,iris_data_test,type="class")

pred_ct

table(pred_ct, iris_data_test$Species)

#change from mck




