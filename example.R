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

logistic_all = glm(Churn ~ gender + SeniorCitizen + Partner + Dependents + tenure
                   + PhoneService + MultipleLines + InternetService + OnlineSecurity
                   + OnlineBackup + DeviceProtection + TechSupport + StreamingTV
                   + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                   + MonthlyCharges + TotalCharges, data = customer, family = 'binomial')
summary(logistic_all)

logistic_1 = glm(Churn ~ SeniorCitizen + Partner + Dependents + tenure
                 + PhoneService + MultipleLines + InternetService + OnlineSecurity
                 + OnlineBackup + DeviceProtection + TechSupport + StreamingTV
                 + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                 + MonthlyCharges + TotalCharges, data = customer, family = 'binomial')
summary(logistic_1)

logistic_2 = glm(Churn ~ SeniorCitizen + Dependents + tenure
                 + PhoneService + MultipleLines + InternetService + OnlineSecurity
                 + OnlineBackup + DeviceProtection + TechSupport + StreamingTV
                 + StreamingMovies + Contract + PaperlessBilling + PaymentMethod
                 + MonthlyCharges + TotalCharges, data = customer, family = 'binomial')
summary(logistic_2)
