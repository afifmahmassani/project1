library(ezids)
knitr::opts_chunk$set(warning = F, results = "hide", message = F)
options(scientific=T, digits = 3) 
#load data
cardio_vascular_disease <- read.csv2("cardio_train.csv", header = TRUE)
str(cardio_vascular_disease)
#change variable types
df <- cardio_vascular_disease
df$id <- NULL
df$gender <- factor(cardio_vascular_disease$gender)
df$smoke <- factor(cardio_vascular_disease$smoke)
df$alco <- factor(cardio_vascular_disease$alco)
df$active <- factor(cardio_vascular_disease$active)
df$cardio <- factor(cardio_vascular_disease$cardio)
df$cholesterol <- factor(cardio_vascular_disease$cholesterol)
df$gluc <- factor(cardio_vascular_disease$gluc)
df$weight <- as.integer(cardio_vascular_disease$weight)

levels(df$cardio) <- c("No", "Yes")
levels(df$gender) <- c("Female", "Male")
levels(df$cholesterol) <- c("Normal", "Above Normal", "Well Above Normal")
levels(df$gluc) <- c("Normal", "Above Normal", "Well Above Normal")
levels(df$smoke) <- c("No", "Yes")
levels(df$alco) <- c("No", "Yes")
levels(df$active) <- c("No", "Yes")
df <- na.omit(df)
str(df)

#correlation plot
df_num <- df
for(i in 1:12){
  if (!(i %in% c(1,3:6))){
    df_num[,i] = as.numeric(df_num[,i])
  }
}
str(df_num)
cor <- cor(df_num, method = "spearman")
loadPkg("corrplot")
corrplot(cor, type="lower", addCoef.col="black", number.cex=0.5, tl.cex=0.7,title="Cardio Vascular Disease Correlation", mar=c(0,0,1,0))

#logistic model alpha = 0.01
logistic_all <- glm(cardio ~ age + gender + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active, data = df, family = "binomial")
summary(logistic_all)
#delete gender
logistic_1 <- glm(cardio ~ age + height + weight + ap_hi + ap_lo + cholesterol + gluc + smoke + alco + active, data = df, family = "binomial")
summary(logistic_1)
#delete gluc
logistic_2 <- glm(cardio ~ age + height + weight + ap_hi + ap_lo + cholesterol + smoke + alco + active, data = df, family = "binomial")
summary(logistic_2)

#model evulation
#confusion matrix
loadPkg("regclass")
confusion_matrix(logistic_2)
xkabledply( confusion_matrix(logistic_2), title = "Confusion matrix from Logit Model" )
cfmatrix1 = confusion_matrix(logistic_2)
accuracy1 <- (cfmatrix1[1,1]+cfmatrix1[2,2])/cfmatrix1[3,3]
precision1 <- cfmatrix1[2,2]/(cfmatrix1[2,2]+cfmatrix1[1,2])
recall1 <- cfmatrix1[2,2]/(cfmatrix1[2,2]+cfmatrix1[2,1])
specificity1 <- cfmatrix1[1,1]/(cfmatrix1[1,1]+cfmatrix1[1,2])
F1_score1 <- 2*(precision1)*(recall1)/(precision1 + recall1)
accuracy1
precision1
recall1
specificity1
F1_score1
#AUC and ROC
loadPkg("pROC")
prob <- predict(logistic_2, type = "response")
df$prob <- prob
h <- roc(cardio ~ prob, data = df)
auc(h)
plot(h)
