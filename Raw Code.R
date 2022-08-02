library(ezids)
knitr::opts_chunk$set(warning = F, results = "hide", message = F)
options(scientific=T, digits = 3) 
#load data
cardio_vascular_disease <- read.csv2("cardio_train.csv", header = TRUE)
str(cardio_vascular_disease)
#change variable types
df <- cardio_vascular_disease
df$gender <- factor(cardio_vascular_disease$gender)
df$smoke <- factor(cardio_vascular_disease$smoke)
df$alco <- factor(cardio_vascular_disease$alco)
df$active <- factor(cardio_vascular_disease$active)
df$cardio <- factor(cardio_vascular_disease$cardio)
df$cholesterol <- factor(cardio_vascular_disease$cholesterol)
df$weight <- as.integer(cardio_vascular_disease$weight)
df <- na.omit(df)
df$id <- NULL
str(df)
#correlation plot
df_num <- df
for(i in 2:13){
  if (!(i %in% c(2, 4:7, 9))){
    df_num[,i] = as.numeric(df_num[,i])
  }
}
cor <- cor(subset(df_num, select = -c(id)), method = "spearman")
loadPkg("corrplot")
corrplot(cor, type="lower", addCoef.col="black", number.cex=0.5, tl.cex=0.7,title="Cardio Vascular Disease Correlation", mar=c(0,0,1,0))
#logistic model
library(bestglm)
bestglm <- bestglm(df, IC = "AIC")
