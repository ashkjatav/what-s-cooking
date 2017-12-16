library(jsonlite)
library(tree)
library(Matrix)
library(tm)
library(SnowballC)
library(topicmodels)

data1 <- fromJSON("train.json")

## pre-preprocessing data
library(tm)

corpus_data1= Corpus(VectorSource(data1$ingredients))

## convert text to lowercase
corpus_data1= tm_map(corpus_data1, content_transformer(tolower))

## remove punctuation
corpus_data1= tm_map(corpus_data1, content_transformer(removePunctuation))

## remove whitespaces
corpus_data1= tm_map(corpus_data1, content_transformer(stripWhitespace))

## remove stopwords
corpus_data1= tm_map(corpus_data1, removeWords, stopwords("english"))

## stemming
corpus_data1= tm_map(corpus_data1, content_transformer(stemDocument))

## document term matrix and remove sparse terms

maxdocfreq_data1= length(corpus_data1)
mindocfreq_data1= length(corpus_data1)*0.0001 ## change this if tail(freq) values are not around 5
dtm_data1= DocumentTermMatrix(corpus_data1, control = list(bounds = list(global = c(mindocfreq_data1, maxdocfreq_data1), weighting= weightTfIdf)))
freq= sort(colSums(as.matrix(dtm_data1)), decreasing= TRUE)
tail(freq) ## check if values for this are around 5, otherwise change 0.0001 two commands above to something else

## data visualization
library(ggplot2)
wf <- data.frame(word = names(freq), freq = freq)
head(wf)
chart <- ggplot(subset(wf, freq >8000), aes(x = word, y = freq))
chart <- chart + geom_bar(stat = 'identity', color = 'black', fill = 'white')
chart <- chart + theme(axis.text.x=element_text(angle=45, hjust=1))
chart
library(wordcloud)
wordcloud(names(freq), freq, min.freq = 1000, scale = c(3, 0.00005), colors=brewer.pal(1, "Dark2"))


## converting into data frame
dtm2_data1= as.data.frame(as.matrix(dtm_data1))
dtm2_data1$cuisine= as.factor(data1$cuisine)


# splitting data : 5 different sample sizes ~[4k,8k,12K,18K,25k] split into test and train data
set.seed(1)
train_index1=sample(1:nrow(dtm2_data1), 4000)
train1= dtm2_data1[train_index1,]
test1 = dtm2_data1[-train_index1,]

train_index2=sample(1:nrow(dtm2_data1), 8000)
train2= dtm2_data1[train_index2,]
test2 = dtm2_data1[-train_index2,]

train_index3=sample(1:nrow(dtm2_data1),12000)
train3= dtm2_data1[train_index3,]
test3 = dtm2_data1[-train_index3,]

train_index4=sample(1:nrow(dtm2_data1), 18000)
train4= dtm2_data1[train_index4,]
test4 = dtm2_data1[-train_index4,]

train_index5=sample(1:nrow(dtm2_data1),25000)
train5= dtm2_data1[train_index5,]
test5 = dtm2_data1[-train_index5,]
train_vec = c(train1,train2,train3,train4,train5)
test_vec = c(test1,test2,test3,test4,test5)

## CART model
#Assigning test and train data


library(rpart)
library(rpart.plot)

## model 1
train<-train1
test<-test1

treefit= rpart(cuisine~., data= train, method='class')
prp(treefit)
prob.tree= predict(treefit, newdata = test, method= 'class')
pred.tree1= colnames(prob.tree)[max.col(prob.tree)]
mean(pred.tree1==test$cuisine)

## model 2
train<-train2
test<-test2

treefit= rpart(cuisine~., data= train, method='class')
prp(treefit)
prob.tree= predict(treefit, newdata = test, method= 'class')
pred.tree1= colnames(prob.tree)[max.col(prob.tree)]
mean(pred.tree1==test$cuisine)

## model 3
train<-train3
test<-test3

treefit= rpart(cuisine~., data= train, method='class')
prp(treefit)
prob.tree= predict(treefit, newdata = test, method= 'class')
pred.tree1= colnames(prob.tree)[max.col(prob.tree)]
mean(pred.tree1==test$cuisine)

## model 4
train<-train4
test<-test4

treefit= rpart(cuisine~., data= train, method='class')
prp(treefit)
prob.tree= predict(treefit, newdata = test, method= 'class')
pred.tree1= colnames(prob.tree)[max.col(prob.tree)]
mean(pred.tree1==test$cuisine)

## model 5
train<-train5
test<-test5

treefit= rpart(cuisine~., data= train, method='class')
prp(treefit)
prob.tree= predict(treefit, newdata = test, method= 'class')
pred.tree1= colnames(prob.tree)[max.col(prob.tree)]
mean(pred.tree1==test$cuisine)

## Support Vector Machines
library(e1071)

## model 1
train<-train1
test<-test1

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=200)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)

## model 2
train<-train2
test<-test2

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=200)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)

## model 3
train<-train3
test<-test3

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=200)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)

## model 4
train<-train4
test<-test4

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=200)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)

## model 5
train<-train5
test<-test5

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=200)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)
summary(pred.svm)

## model 6

svmfit= svm(cuisine~., data= train, kernel= 'linear', cost=200)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)

## model 7

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=100)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)

## model 8

svmfit= svm(cuisine~., data= train, kernel= 'radial', cost=300)
pred.svm= predict(svmfit, test)
mean(pred.svm==test$cuisine)


## KNN
# splitting data : 5 different sample sizes ~[4k,8k,12K] split into test and train data
set.seed(1)

train_index1=sample(1:nrow(dtm2_data1), 5000)
train<-dtm2_data1[train_index1,]
train1= train[1:4000,]
test1 = train[4001:5000,]

train_index2=sample(1:nrow(dtm2_data1), 10000)
train<-dtm2_data1[train_index2,]
train2= train[1:8000,]
test2 = train[8001:10000,]

train_index3=sample(1:nrow(dtm2_data1),15000)
train<-dtm2_data1[train_index3,]
train3= train[1:12000,]
test3 = train[12001:15000,]

## model 1
train<-train1
test<-test1

library(class)
train.x= train[,!colnames(train) %in% c('cuisine')]
test.x= test[,!colnames(test) %in% c('cuisine')]
knn.pred= knn(train.x, test.x, train$cuisine, k=100)
mean(knn.pred==test$cuisine)

## model 2

train<-train2
test<-test2

train.x= train[,!colnames(train) %in% c('cuisine')]
test.x= test[,!colnames(test) %in% c('cuisine')]
knn.pred= knn(train.x, test.x, train$cuisine, k=100)
mean(knn.pred==test$cuisine)


## model 2

train<-train3
test<-test3

train.x= train[,!colnames(train) %in% c('cuisine')]
test.x= test[,!colnames(test) %in% c('cuisine')]
knn.pred= knn(train.x, test.x, train$cuisine, k=100)
mean(knn.pred==test$cuisine)


## Neural Networks

library(nnet)

## model 1

train<-train1
test<-test1

nnetfit= nnet(cuisine~., data=train, size=3, MaxNWts=10000)
prob.nnet= predict(nnetfit, test)
pred.nnet= colnames(prob.nnet)[max.col(prob.nnet)]
mean(pred.nnet==test$cuisine)

## model 2

train<-train2
test<-test2

nnetfit= nnet(cuisine~., data=train, size=3, MaxNWts=10000)
prob.nnet= predict(nnetfit, test)
pred.nnet= colnames(prob.nnet)[max.col(prob.nnet)]
mean(pred.nnet==test$cuisine)

## model 3

train<-train3
test<-test3

nnetfit= nnet(cuisine~., data=train, size=3, MaxNWts=10000)
prob.nnet= predict(nnetfit, test)
pred.nnet= colnames(prob.nnet)[max.col(prob.nnet)]
mean(pred.nnet==test$cuisine)

## model 4
train<-train4
test<-test4

nnetfit= nnet(cuisine~., data=train, size=3, MaxNWts=10000)
prob.nnet= predict(nnetfit, test)
pred.nnet= colnames(prob.nnet)[max.col(prob.nnet)]
mean(pred.nnet==test$cuisine)

## model 5
train<-train5
test<-test5

nnetfit= nnet(cuisine~., data=train, size=3, MaxNWts=10000)
prob.nnet= predict(nnetfit, test)
pred.nnet= colnames(prob.nnet)[max.col(prob.nnet)]
mean(pred.nnet==test$cuisine)


## XGBoost
library(xgboost)

## model 1

xgtrain_4000 <- xgb.DMatrix((data.matrix(train1[,!colnames(train1) %in% c('cuisine')])), label = as.numeric(train1$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_4000, max.depth = 25, eta = 0.1, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test1[, !colnames(test1) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train1$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test1$cuisine)

## model 2

xgtrain_8000 <- xgb.DMatrix((data.matrix(train2[,!colnames(train2) %in% c('cuisine')])), label = as.numeric(train2$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_8000, max.depth = 25, eta = 0.1, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test2[, !colnames(test2) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train2$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test2$cuisine)

## model 3

xgtrain_12000 <- xgb.DMatrix((data.matrix(train3[,!colnames(train3) %in% c('cuisine')])), label = as.numeric(train3$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_12000, max.depth = 25, eta = 0.1, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test3[, !colnames(test3) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train3$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test3$cuisine)

## model 4

xgtrain_18000 <- xgb.DMatrix((data.matrix(train4[,!colnames(train4) %in% c('cuisine')])), label = as.numeric(train4$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_18000, max.depth = 25, eta = 0.1, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test4[, !colnames(test4) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train4$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test4$cuisine)

## model 5	

xgtrain_25000 <- xgb.DMatrix((data.matrix(train5[,!colnames(train5) %in% c('cuisine')])), label = as.numeric(train5$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_25000, max.depth = 25, eta = 0.1, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test5[, !colnames(test5) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train5$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test5$cuisine)

## model 6

xgtrain_25000 <- xgb.DMatrix((data.matrix(train5[,!colnames(train5) %in% c('cuisine')])), label = as.numeric(train5$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_25000, max.depth = 25, eta = 0.01, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test5[, !colnames(test5) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train5$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test5$cuisine)

## model 7

xgtrain_25000 <- xgb.DMatrix((data.matrix(train5[,!colnames(train5) %in% c('cuisine')])), label = as.numeric(train5$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_25000, max.depth = 25, eta = 0.3, nround = 100, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test5[, !colnames(test5) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train5$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test5$cuisine)

## model 8

xgtrain_25000 <- xgb.DMatrix((data.matrix(train5[,!colnames(train5) %in% c('cuisine')])), label = as.numeric(train5$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_25000, max.depth = 7, gamma = 2, min_child_weight = 2, eta = 0.1, nround = 75, objective = "multi:softmax", num_class = 20, verbose = 2)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test5[, !colnames(test5) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train5$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test5$cuisine)

## model 9

xgtrain_25000 <- xgb.DMatrix((data.matrix(train5[,!colnames(train5) %in% c('cuisine')])), label = as.numeric(train5$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_25000, max.depth = 25, eta = 0.5, nround = 100, objective = "multi:softmax", num_class = 20)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test5[, !colnames(test5) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train5$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test5$cuisine)

## model 10 (best model)

xgtrain_25000 <- xgb.DMatrix((data.matrix(train5[,!colnames(train5) %in% c('cuisine')])), label = as.numeric(train5$cuisine)-1)
xgbmodel <- xgboost(data = xgtrain_25000, max.depth = 25, eta = 0.3, nround = 150, objective = "multi:softmax", num_class = 20, verbose = 1)
xgbmodel.predict <- predict(xgbmodel, newdata = data.matrix(test5[, !colnames(test5) %in% c('cuisine')]))
xgbmodel.predict.text <- levels(train5$cuisine)[xgbmodel.predict + 1]
mean(xgbmodel.predict.text==test5$cuisine)
## importance features plot

names <- colnames(train5[, !colnames(train5) %in% c("cuisine")])
importance_matrix= xgb.importance(names, model=xgbmodel)

