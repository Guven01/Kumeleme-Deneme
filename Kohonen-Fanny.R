library(tidyverse)
library(cluster)
library(FactoMineR)
library(factoextra)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lattice)
library(clValid)
library(caret)
library(parallel)
library(NbClust)
library(DescTools)
library(dataMaid)
library(DataExplorer)
library(dplyr)
library(snow)
library(doSNOW)

#Bu projenin amacý, iþletmenin müþteri segmentlerini daha iyi anlayabilmesi ve her birine farklý 
#pazarlama stratejileri adapte edebilmesi, böylece gelirini ve pazar payýný artýrabilmesi için kredi kartý sahiplerinin kümelerini belirlemek, incelemek ve analiz etmektir. 
#Bunun için, 8950 aktif kredi kartý sahibini içeren Kaggle'da bulunan Kümeleme için Kredi Kartý Veri Kümesini kullanacaðýz.
#Her müþteri aþaðýdaki özelliklere sahiptir:

#CUSTID :Müþterinin kimliði

#BALANCE : Bu adam ne kadar zengin?

#BALANCEFREQUENCY :Bu adam, hesabýna ne kadar sýklýkla $$$ ekler?

#PURCHASES : Bu kiþi þu ana kadar ne kadar para harcadý?

#ONEOFFPURCHASES : Þimdiye kadarki en pahalý fatura..

#INSTALLMENTSPURCHASES : Bu kiþinin tek seferde yapmakta tereddüt ettiði faturalar.

#CASHADVANCE :Bu kiþi tarafýndan peþin verilen nakit

#PURCHASESFREQUENCY : Satýn alma sýklýðý Bu adam nasýl?

#ONEOFFPURCHASESFREQUENCY : Þimdiye kadarki en pahalý fatura frekansý (En pahalý faturada)

#PURCHASESINSTALLMENTSFREQUENCY : tek seferde ödemeye tereddüt edilen fatura frekansý

#CASHADVANCEFREQUENCY : Nakit peþin ne sýklýkla ödeniyor

#CASHADVANCETRX : "Nakit Avans" ile Yapýlan Ýþlem Sayýsý

#PURCHASESTRX : Bu adam bir þeyler satýn almakla ne kadar meþgul?

#CREDITLIMIT : ne kadar limiti olduðu

#PAYMENTS : Kullanýcý tarafýndan yapýlan Ödeme Miktarý

#MINIMUM_PAYMENTS : Kullanýcý tarafýndan yapýlan minimum ödeme miktarý

#PRCFULLPAYMENT :Kullanýcý tarafýndan ödenen tam ödeme miktarý yüzdesi

#TENURE : Kullanýcý için kredi kartý hizmetinin kullaným süresi




df=read.csv("/Users/HP/Documents/Python/VeriSetleri/general.csv",header = TRUE)
glimpse(df)
dim(df)
names(df)
summary(df)
sum(is.na(df))

sum(is.na(df$CREDIT_LIMIT))

df$CREDIT_LIMIT[which(is.na(df$CREDIT_LIMIT))]=median(df$CREDIT_LIMIT,na.rm=TRUE)

sum(is.na(df$MINIMUM_PAYMENTS))

df$MINIMUM_PAYMENTS[which(is.na(df$MINIMUM_PAYMENTS))]=0 

sum(is.na(df))

newdf=df[-1]          

str(newdf)

dim(newdf)  

library(DMwR2)

aykýrý=lofactor(newdf,k=17)

aykýrýlar=order(aykýrý,decreasing=T)[1:17]

print(newdf[aykýrýlar,])

newnewdf <- newdf[-c(3245, 4377, 6591,7402,6014,1040,8313,4906,5001,8624,4484,5418,7133,2160,3749,6973,8554),]

newdf=newnewdf[1:17]

df <- df[-c(3245, 4377, 6591,7402,6014,1040,8313,4906,5001,8624,4484,5418,7133,2160,3749,6973,8554),]

cols = c("BALANCE","PURCHASES" ,"ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES", "CASH_ADVANCE" ,"PURCHASES_INSTALLMENTS_FREQUENCY", "CREDIT_LIMIT", "PAYMENTS", "MINIMUM_PAYMENTS", "PRC_FULL_PAYMENT")

for(i in cols){
 newdf[i] = log(1 + newdf[i])
}

newdf

library(kohonen)

library(fpc)

sonuclog6=clValid(newdf,6, clMethods = c("sota","fanny","som"), validation ="stability", maxitems = 8950, metric = "euclidean", method = "ward")

sonuclog2=clValid(newdf,2, clMethods = c("sota","fanny","som"), validation ="stability", maxitems = 8950, metric = "euclidean", method = "ward")

sonuclog3=clValid(newdf,3, clMethods = c("sota","fanny","som"), validation ="stability", maxitems = 8950, metric = "euclidean", method = "ward")

sonuclog4=clValid(newdf,4, clMethods = c("sota","fanny","som"), validation ="stability", maxitems = 8950, metric = "euclidean", method = "ward")

sonuclog5=clValid(newdf,5, clMethods = c("sota","fanny","som"), validation ="stability", maxitems = 8950, metric = "euclidean", method = "ward")

sonuclogson=clValid(newdf,nClust=2:6, clMethods = c("sota","fanny","som"), validation ="stability", maxitems = 8950, metric = "euclidean", method = "ward")



optimalScores(sonuclogson)

optimalScores(sonuclog3)

optimalScores(sonuclog4)

optimalScores(sonuclog5)

optimalScores(sonuclog6)


newsota=sonuclogson@clusterObjs[["sota"]][["2"]]

names(newsota)

plot(newsota,cl=1)
plot(newsota,cl=2)
print(newsota)



newdf["kumelersota"]=sonuclogson@clusterObjs[["sota"]][["2"]][["clust"]]

group1=subset(newdf, newdf$kumelersota == 1)

group2=subset(newdf,newdf$kumelersota ==2)

summary(group1)

summary(group2)


a=sonuclogson@clusterObjs[["sota"]][["2"]][["tree"]]

Centroids=data.frame(a)

Centroids[4:17]

cesýtlýlýk=a[1:3]

cesýtlýlýk

library(heatmaply)

heatmaply(Centroids[4:17])






