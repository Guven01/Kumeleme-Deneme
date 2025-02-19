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

#Bu projenin amac�, i�letmenin m��teri segmentlerini daha iyi anlayabilmesi ve her birine farkl� 
#pazarlama stratejileri adapte edebilmesi, b�ylece gelirini ve pazar pay�n� art�rabilmesi i�in kredi kart� sahiplerinin k�melerini belirlemek, incelemek ve analiz etmektir. 
#Bunun i�in, 8950 aktif kredi kart� sahibini i�eren Kaggle'da bulunan K�meleme i�in Kredi Kart� Veri K�mesini kullanaca��z.
#Her m��teri a�a��daki �zelliklere sahiptir:

#CUSTID :M��terinin kimli�i

#BALANCE : Bu adam ne kadar zengin?

#BALANCEFREQUENCY :Bu adam, hesab�na ne kadar s�kl�kla $$$ ekler?

#PURCHASES : Bu ki�i �u ana kadar ne kadar para harcad�?

#ONEOFFPURCHASES : �imdiye kadarki en pahal� fatura..

#INSTALLMENTSPURCHASES : Bu ki�inin tek seferde yapmakta teredd�t etti�i faturalar.

#CASHADVANCE :Bu ki�i taraf�ndan pe�in verilen nakit

#PURCHASESFREQUENCY : Sat�n alma s�kl��� Bu adam nas�l?

#ONEOFFPURCHASESFREQUENCY : �imdiye kadarki en pahal� fatura frekans� (En pahal� faturada)

#PURCHASESINSTALLMENTSFREQUENCY : tek seferde �demeye teredd�t edilen fatura frekans�

#CASHADVANCEFREQUENCY : Nakit pe�in ne s�kl�kla �deniyor

#CASHADVANCETRX : "Nakit Avans" ile Yap�lan ��lem Say�s�

#PURCHASESTRX : Bu adam bir �eyler sat�n almakla ne kadar me�gul?

#CREDITLIMIT : ne kadar limiti oldu�u

#PAYMENTS : Kullan�c� taraf�ndan yap�lan �deme Miktar�

#MINIMUM_PAYMENTS : Kullan�c� taraf�ndan yap�lan minimum �deme miktar�

#PRCFULLPAYMENT :Kullan�c� taraf�ndan �denen tam �deme miktar� y�zdesi

#TENURE : Kullan�c� i�in kredi kart� hizmetinin kullan�m s�resi




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

ayk�r�=lofactor(newdf,k=17)

ayk�r�lar=order(ayk�r�,decreasing=T)[1:17]

print(newdf[ayk�r�lar,])

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

ces�tl�l�k=a[1:3]

ces�tl�l�k

library(heatmaply)

heatmaply(Centroids[4:17])






