customer <- data.frame(read.csv("customer.csv"))
str(customer)

for(i in 2:21){
  # tenure, MonthlyCharges, TotalCharges
  if (!(i %in% c(6, 19, 20))){
    customer[,i] = factor(customer[,i])
  }
}
levels(customer$SeniorCitizen) <- c("No", "Yes") # no=1, yes=2
str(customer)

summary(customer)  # there are 11 NA in TotalCharges
customer <- na.omit(customer)
sum(is.na(customer))
str(customer)

customerNum = customer
# convert categorical variable as numeric 
for(i in 2:21){
  # tenure, MonthlyCharges, TotalCharges
  if (!(i %in% c(6, 19, 20, 21))){
    customerNum[,i] = as.numeric(customerNum[,i])
  }
}
customerNum$customerID = NULL
str(customerNum)

customer_final=customerNum
customer_final$Churn <- factor(customer_final$Churn)
str(customer_final)

fullScale = customer_final
fullScale[c(5, 18, 19)]<- scale(fullScale[c(5, 18, 19)], center = TRUE, scale = TRUE)
str(fullScale)
set.seed(1)
customer_sampe <- sample(2, nrow(fullScale), replace=TRUE, prob=c(0.75, 0.25))
cus_train_full <- fullScale[customer_sampe==1, 1:19]
cus_test_full <- fullScale[customer_sampe==2, 1:19]
# y
cus_train_full_y <- fullScale[customer_sampe==1, 20]
cus_test_full_y <- fullScale[customer_sampe==2, 20]

str(cus_train_full)
str(cus_test_full)

loadPkg("class")
chooseK = function(k, train_set, val_set, train_class, val_class){
  
  # Build knn with k neighbors considered.
  set.seed(1)
  class_knn = knn(train = train_set,    #<- training set cases
                  test = val_set,       #<- test set cases
                  cl = train_class,     #<- category for classification
                  k = k) #,                #<- number of neighbors considered
  # use.all = TRUE)       #<- control ties between class assignments. If true, all distances equal to the k-th largest are included
  
  tab = table(class_knn, val_class)
  #cm = confusionMatrix(class_knn, reference = cus_test_y ) # from caret library
  # print.confusionMatrix(cm)
  # 
  #cmaccu = cm$overall['Accuracy']
  
  # Calculate the accuracy.
  accu = sum(tab[row(tab) == col(tab)]) / sum(tab)                         
  cbind(k = k, accuracy = accu)
}


knn_full_different_k = sapply(seq(1, 21, by = 2),  #<- set k to be odd number from 1 to 21
                              function(x) chooseK(x, 
                                                  train_set = cus_train_full,
                                                  val_set = cus_test_full,
                                                  train_class = cus_train_full_y,
                                                  val_class = cus_test_full_y))

str(knn_full_different_k)
knn_full_different_k = data.frame(k = knn_full_different_k[1,],
                                  accuracy = knn_full_different_k[2,])

library("ggplot2")

ggplot(knn_full_different_k,
       aes(x = k, y = accuracy)) +
  geom_line(color = "orange", size = 1.5) +
  geom_point(size = 3) + 
  labs(title = "accuracy vs k")
xkabledply((knn_full_different_k))

loadPkg("rpart")
loadPkg("caret")



# create an empty dataframe to store the results from confusion matrices
confusionMatrixResultDf = data.frame( Depth=numeric(0), Accuracy= numeric(0), Sensitivity=numeric(0), Specificity=numeric(0), Pos.Pred.Value=numeric(0), Neg.Pred.Value=numeric(0), Precision=numeric(0), Recall=numeric(0), F1=numeric(0), Prevalence=numeric(0), Detection.Rate=numeric(0), Detection.Prevalence=numeric(0), Balanced.Accuracy=numeric(0), row.names = NULL )

for (deep in 2:8) {
  kfit <- rpart(Churn ~ TotalCharges + MonthlyCharges + tenure + Contract +  OnlineSecurity + PaymentMethod, data=customerNum, method="class", control = list(maxdepth = deep) )
  # 
  cm = confusionMatrix( predict(kfit, type = "class"), reference = customerNum[, "Churn"] ) # from caret library
  # 
  cmaccu = cm$overall['Accuracy']
  # print( paste("Total Accuracy = ", cmaccu ) )
  # 
  cmt = data.frame(Depth=deep, Accuracy = cmaccu, row.names = NULL ) # initialize a row of the metrics 
  cmt = cbind( cmt, data.frame( t(cm$byClass) ) ) # the dataframe of the transpose, with k valued added in front
  confusionMatrixResultDf = rbind(confusionMatrixResultDf, cmt)
  # print("Other metrics : ")
}

unloadPkg("caret")
xkabledply(confusionMatrixResultDf, title="Churn Classification Trees summary with varying MaxDepth")
