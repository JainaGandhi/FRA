#Financial Risk Analytics Project

library(readxl)
setwd("E:/GitHub_Repositories/FRA")
mydata = read_xlsx("raw-data.xlsx")

names(mydata)
dim(mydata)

#Renaming the column names
names(mydata) <- make.names(names(mydata))

names(mydata)
summary(mydata) #summary of dataset


#creating new variables

#Liquidity

mydata$Liquidity.CashTo.TotalAsset.Ratio = mydata$Current.assets / mydata$Total.assets
mydata$Liquidity.CashTo.TotalAsset.Ratio

#Profitability

mydata$Profitability.Net.Profit.Margin = mydata$PBT/mydata$Sales
mydata$Profitability.Net.Profit.Margin


mydata$Profitability.Return.on.Investment = mydata$Profit.after.tax/(mydata$Shareholders.funds+mydata$Cumulative.retained.profits+mydata$Borrowings)
mydata$Profitability.Return.on.Investment

mydata$Profitability.ExpensesTo.Sales.ratio = mydata$Total.expenses / mydata$Sales
mydata$Profitability.ExpensesTo.Sales.ratio

mydata$ReturnOn.Assest.Ratio = mydata$Profit.after.tax / mydata$Total.assets
mydata$ReturnOn.Assest.Ratio


#leverage
mydata$Leverage.Debt.ratio = mydata$Borrowings/(mydata$Total.assets-mydata$Current.liabilities...provisions)
mydata$Leverage.Debt.ratio

mydata$Leverage.OwnerEquity.TotalEquityRatio = (mydata$Shareholders.funds+mydata$Cumulative.retained.profits)/ (mydata$Shareholders.funds+mydata$Cumulative.retained.profits+mydata$Borrowings+mydata$Current.liabilities...provisions)
mydata$Leverage.OwnerEquity.TotalEquityRatio

mydata$Leverage.EquityRatio = mydata$Shareholders.funds/ (mydata$Total.assets-mydata$Current.liabilities...provisions)
mydata$Leverage.EquityRatio

mydata$Leverage.ProprietaryRatio = (mydata$Shareholders.funds+mydata$Cumulative.retained.profits) / mydata$Total.assets
mydata$Leverage.ProprietaryRatio

mydata$Leverage.FixedAssets.LongTermFundRatio = mydata$Net.fixed.assets / (mydata$Borrowings+mydata$Shareholders.funds+mydata$Cumulative.retained.profits)
mydata$Leverage.FixedAssets.LongTermFundRatio

#size
mydata$Size.FixedAsset.Turnover.ratio = mydata$Sales / mydata$Net.fixed.assets
mydata$Size.FixedAsset.Turnover.ratio

mydata$Size.Capital.Turnover.ratio = mydata$Sales / (mydata$Borrowings+mydata$Shareholders.funds+mydata$Cumulative.retained.profits)
mydata$Size.Capital.Turnover.ratio



#Converting from character variables to numeric variables
mydata$Creditors.turnover = as.numeric(mydata$Creditors.turnover)
mydata$Debtors.turnover = as.numeric(mydata$Debtors.turnover)
mydata$Finished.goods.turnover = as.numeric(mydata$Finished.goods.turnover)
mydata$WIP.turnover = as.numeric(mydata$WIP.turnover)
mydata$Raw.material.turnover = as.numeric(mydata$Raw.material.turnover)
mydata$Shares.outstanding = as.numeric(mydata$Shares.outstanding)
mydata$Equity.face.value = as.numeric(mydata$Equity.face.value)
mydata$PE.on.BSE = as.numeric(mydata$PE.on.BSE)

#creating default variable
mydata$Default = ifelse(mydata$Networth.Next.Year>0,0,1)

str(mydata) #checking structure of dataset
View(mydata)
mydata1 = mydata[c(3:21,23:50,52:65)] #Removing (1)num,(2)networth.next.year,(22)deposits & (51)total liabilities

str(mydata1)
#View(mydata1)
attach(mydata1)

#Default = ifelse(Networth.Next.Year>0,0,1)
summary(mydata1[Default=0])
summary(mydata1[Default=1])


#Outliers Treatment
library(corrplot)

options(scipen = 999)
boxplot(mydata1)

#__________________________________________________________________________________


#outliers have been treated by replacing with extreme values

#The IQR(inter-quartile-range) method extracts the top and bottom 25% values from the data. Any value beyond 1.5 times the inter quartile range is considered as an outlier and that value is replaced with either 5% or 95%th observation value.


#creating function for outliers treatment where
# x = a numeric, na.rm will remove na's
outlierTreament=function(x){
  qnt = quantile(x, probs=c(.25, .75), na.rm = TRUE)
  caps = quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H = 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H)] = caps[1]
  x[x > (qnt[2] + H)] = caps[2]
  return(x)
}

mydata_IQR = as.data.frame(sapply(mydata1,outlierTreament))
boxplot(mydata_IQR)

mydata1 = mydata_IQR
boxplot(mydata1)
str(mydata1)
summary(mydata1)


#%in% is a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not for its left operand.
#_______________________________________________________________________________


#Treating Missing Values

library(mice)

sum(is.na(mydata1))
md.pattern(mydata1)
init.impute = mice(mydata1, m = 5,method = "rf",seed = 123)
mydata.wo.nas = complete(init.impute, 2)
md.pattern(mydata.wo.nas)
sum(is.na(mydata.wo.nas))

mydata1 = mydata.wo.nas
summary(mydata1)

#_____________________________________________________________________________

#checking multicollinearity
View(mydata1)
CorMatrix = cor(mydata1[c(1:60)],use ="complete.obs")

#this will give pearson's correplation
#Ideally we have to exclude categorical variable while checking for multicollinearity so i've excluded Default variable. 
CorMatrix
corrplot(CorMatrix,type = "upper",method = "number",tl.cex = 0.18)


#interpretation - multicollinearity also exits
#there is a relation between the variables

#____________________________________________________________________________________
#EDA

library(rpivotTable) 
rpivotTable(mydata) 


#____________________________________________________________________________________
#Validation data set

mydatatest = read_xlsx("validation_data.xlsx")

names(mydatatest)
dim(mydatatest)


names(mydatatest) <- make.names(names(mydatatest))

names(mydatatest)
summary(mydatatest)

#creating new variables

#Liquidity

mydatatest$Liquidity.CashTo.TotalAsset.Ratio = mydatatest$Current.assets / mydatatest$Total.assets
mydatatest$Liquidity.CashTo.TotalAsset.Ratio

#Profitability

mydatatest$Profitability.Net.Profit.Margin = mydatatest$PBT/mydatatest$Sales
mydatatest$Profitability.Net.Profit.Margin


mydatatest$Profitability.Return.on.Investment = mydatatest$Profit.after.tax/(mydatatest$Shareholders.funds+mydatatest$Cumulative.retained.profits+mydatatest$Borrowings)
mydatatest$Profitability.Return.on.Investment

mydatatest$Profitability.ExpensesTo.Sales.ratio = mydatatest$Total.expenses / mydatatest$Sales
mydatatest$Profitability.ExpensesTo.Sales.ratio

mydatatest$ReturnOn.Assest.Ratio = mydatatest$Profit.after.tax / mydatatest$Total.assets
mydatatest$ReturnOn.Assest.Ratio


#leverage
mydatatest$Leverage.Debt.ratio = mydatatest$Borrowings/(mydatatest$Total.assets-mydatatest$Current.liabilities...provisions)
mydatatest$Leverage.Debt.ratio

mydatatest$Leverage.OwnerEquity.TotalEquityRatio = (mydatatest$Shareholders.funds+mydatatest$Cumulative.retained.profits)/ (mydatatest$Shareholders.funds+mydatatest$Cumulative.retained.profits+mydatatest$Borrowings+mydatatest$Current.liabilities...provisions)
mydatatest$Leverage.OwnerEquity.TotalEquityRatio

mydatatest$Leverage.EquityRatio = mydatatest$Shareholders.funds/ (mydatatest$Total.assets-mydatatest$Current.liabilities...provisions)
mydatatest$Leverage.EquityRatio

mydatatest$Leverage.ProprietaryRatio = (mydatatest$Shareholders.funds+mydatatest$Cumulative.retained.profits) / mydatatest$Total.assets
mydatatest$Leverage.ProprietaryRatio

mydatatest$Leverage.FixedAssets.LongTermFundRatio = mydatatest$Net.fixed.assets / (mydatatest$Borrowings+mydatatest$Shareholders.funds+mydatatest$Cumulative.retained.profits)
mydatatest$Leverage.FixedAssets.LongTermFundRatio

#size
mydatatest$Size.FixedAsset.Turnover.ratio = mydatatest$Sales / mydatatest$Net.fixed.assets
mydatatest$Size.FixedAsset.Turnover.ratio


mydatatest$Size.Capital.Turnover.ratio = mydatatest$Sales / (mydatatest$Borrowings+mydatatest$Shareholders.funds+mydatatest$Cumulative.retained.profits)
mydatatest$Size.Capital.Turnover.ratio



str(mydatatest)

#Converting character variables to numeric
mydatatest$Creditors.turnover = as.numeric(mydatatest$Creditors.turnover)
mydatatest$Debtors.turnover = as.numeric(mydatatest$Debtors.turnover)
mydatatest$Finished.goods.turnover = as.numeric(mydatatest$Finished.goods.turnover)
mydatatest$WIP.turnover = as.numeric(mydatatest$WIP.turnover)
mydatatest$Raw.material.turnover = as.numeric(mydatatest$Raw.material.turnover)
mydatatest$Shares.outstanding = as.numeric(mydatatest$Shares.outstanding)
mydatatest$Equity.face.value = as.numeric(mydatatest$Equity.face.value)
mydatatest$PE.on.BSE = as.numeric(mydatatest$PE.on.BSE)


str(mydatatest)
View(mydatatest)
mydatatest1 = mydatatest[c(2:21,23:50,52:64)]
str(mydatatest1)
View(mydatatest1)
attach(mydatatest1)

#Outliers Treatment
library(corrplot)

options(scipen = 999)
boxplot(mydatatest)

#__________________________________________________________________________________


#outliers have been treated by replacing with extreme values

#The IQR(inter-quartile-range) method extracts the top and bottom 25% values from the data. Any value beyond 1.5 times the inter quartile range is considered as an outlier and that value is replaced with either 5% or 95%th observation value.


#creating function for outliers treatment where
# x = a numeric, na.rm will remove na's
outlierTreamenttest=function(x){
  qnt = quantile(x, probs=c(.25, .75), na.rm = TRUE)
  caps = quantile(x, probs=c(.05, .95), na.rm = TRUE)
  H = 1.5 * IQR(x, na.rm = TRUE)
  x[x < (qnt[1] - H)] = caps[1]
  x[x > (qnt[2] + H)] = caps[2]
  return(x)
}

mydatatest_IQR = as.data.frame(sapply(mydatatest1,outlierTreamenttest))
boxplot(mydatatest_IQR)

mydatatest1 = mydatatest_IQR
boxplot(mydatatest1)
str(mydatatest1)
summary(mydatatest1)


#%in% is a more intuitive interface as a binary operator, which returns a logical vector indicating if there is a match or not for its left operand.
#_______________________________________________________________________________

#Treating Missing Values

library(mice)
sum(is.na(mydatatest1))
md.pattern(mydatatest1)
init.impute = mice(mydatatest1, m = 5,method = "rf",seed = 123)
mydata.wo.nas.test = complete(init.impute, 2)
md.pattern(mydata.wo.nas.test)
sum(is.na(mydata.wo.nas.test))

mydatatest1 = mydata.wo.nas.test
summary(mydatatest1)

#_________________________________________________________________________________
#Building model


model1 = glm(Default~.,data = mydata1,family = binomial)
summary(model1)

variables = attributes(alias(model1)$Complete)$dimnames[[1]]
variables

#Checking the VIF score for all the variables as per model2
library(car)
vif(model1)


#Removing variables with high vif (vif greater than 10)
model2 = glm(Default~ Net.worth+
                      Change.in.stock+
                      PBDITA+PBT+Cash.profit+
                      PBDITA.as...of.total.income+
                      Cash.profit.as...of.total.income+
                      PAT.as...of.net.worth+
                      Income.from.financial.services+
                      Total.capital+
                      Other.income+
                      Reserves.and.funds+
                      Borrowings+
                      Current.liabilities...provisions+
                      Deferred.tax.liability+
                      Shareholders.funds+
                      Cumulative.retained.profits+
                      Capital.employed+
                      TOL.TNW+
                      Total.term.liabilities...tangible.net.worth+
                      Contingent.liabilities...Net.worth....+
                      Contingent.liabilities+
                      Net.fixed.assets+
                      Debt.to.equity.ratio..times.+
                      Cash.to.current.liabilities..times.+
                      Cash.to.average.cost.of.sales.per.day+
                      Creditors.turnover+Debtors.turnover+
                      Finished.goods.turnover+
                      WIP.turnover+
                      Raw.material.turnover+
                      Shares.outstanding+
                      Equity.face.value+
                      EPS+
                      Adjusted.EPS+
                      PE.on.BSE+
                      Liquidity.CashTo.TotalAsset.Ratio+
                      Profitability.Return.on.Investment+
                      Profitability.ExpensesTo.Sales.ratio+
                      ReturnOn.Assest.Ratio+
                      Leverage.Debt.ratio+
                      Leverage.OwnerEquity.TotalEquityRatio+
                      Leverage.EquityRatio+
                      Leverage.ProprietaryRatio+
                      Leverage.FixedAssets.LongTermFundRatio+
                      Size.FixedAsset.Turnover.ratio+
                      Size.Capital.Turnover.ratio, 
                      data=mydata1,family = binomial)

summary(model2)
vif(model2)

#Now that we have removed high vif value, now we will consider only significant variables only

model3 = glm(Default~ PAT.as...of.net.worth + 
                      Income.from.financial.services+
                      Other.income+
                      TOL.TNW+
                      Total.term.liabilities...tangible.net.worth+
                      Contingent.liabilities...Net.worth....+
                      Debt.to.equity.ratio..times.+
                      Debtors.turnover+
                      Profitability.ExpensesTo.Sales.ratio+
                      Liquidity.CashTo.TotalAsset.Ratio+
                      Leverage.ProprietaryRatio+
                      Leverage.FixedAssets.LongTermFundRatio+
                      Size.FixedAsset.Turnover.ratio,
                      data = mydata1,family = binomial)

summary(model3)
vif(model3)

#Again removing insignificant values from model3

model4 = glm(Default~ PAT.as...of.net.worth + 
               TOL.TNW+
               Total.term.liabilities...tangible.net.worth+
               Debt.to.equity.ratio..times.+
               Debtors.turnover+
               Profitability.ExpensesTo.Sales.ratio+
               Liquidity.CashTo.TotalAsset.Ratio+
               Leverage.ProprietaryRatio+
               Leverage.FixedAssets.LongTermFundRatio+
               Size.FixedAsset.Turnover.ratio,
               data = mydata1,family = binomial)

summary(model4)
vif(model4)

#Finalising the model with most significant values

#Log Likelihood ratio test
library(lmtest)
lrtest(model4)

# Pseudo R-square
library(pscl)
pR2(model4)

# Odds Ratio
exp(coef(model4))

# Probability
exp(coef(model4))/(1+exp(coef(model4)))



#Prediction using Training data
mydata1$predicted=predict(model4,newdata = mydata1,type = "response")
head(mydata1)


tab.LR=table(mydata1$Default,mydata1$predicted>0.5)
tab.LR
sum(diag(tab.LR))/sum(tab.LR)


#Prediction using test data
mydatatest1$predicted=predict(model4,newdata = mydatatest1,type = "response")
head(mydatatest1)


tab.LR.test=table(mydatatest1$Default,mydatatest1$predicted>0.5)
tab.LR.test
sum(diag(tab.LR.test))/sum(tab.LR.test)

#sorting Probability of default  into descending order
Model4.fittedValues = model4$fitted.values
#model4$fitted.values
order(-model4$fitted.values) #now data is sorted to descending order 

library(rattle)
library(data.table)
library(scales)

## deciling code
decile = function(x){
  deciles = vector(length=10)
  for (i in seq(0.1,1,.1)){
    deciles[i*10] = Model4.fittedValues
  }
  return (
    ifelse(x<deciles[1], 1,
           ifelse(x<deciles[2], 2,
                  ifelse(x<deciles[3], 3,
                         ifelse(x<deciles[4], 4,
                                ifelse(x<deciles[5], 5,
                                       ifelse(x<deciles[6], 6,
                                              ifelse(x<deciles[7], 7,
                                                     ifelse(x<deciles[8], 8,
                                                            ifelse(x<deciles[9], 9, 10
                                                            ))))))))))
}

View(decile(mydata1))

## Ranking code

tmp_DT = data.table(mydata1)
rank = tmp_DT[, list(cnt = length(Default), 
                cnt_resp = sum(Default), 
                cnt_non_resp = sum(Default == 0)) , by=deciles][order(-deciles)]


rank$rrate = round(rank$cnt_resp / rank$cnt,4);
rank$cum_resp = cumsum(rank$cnt_resp)
rank$cum_non_resp = cumsum(rank$cnt_non_resp)
rank$cum_rel_resp = round(rank$cum_resp / sum(rank$cnt_resp),4);
rank$cum_rel_non_resp = round(rank$cum_non_resp / sum(rank$cnt_non_resp),4);
rank$ks = abs(rank$cum_rel_resp - rank$cum_rel_non_resp) * 100;
rank$rrate = percent(rank$rrate)
rank$cum_rel_resp = percent(rank$cum_rel_resp)
rank$cum_rel_non_resp = percent(rank$cum_rel_non_resp)

View(rank)


