
setwd("D:/ESILV/5A/Apprentissage")

require(jsonlite) 
require(data.table)
library(caTools)

#lecture des donneees
train<-read.csv("train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 


# cr???ation d'une colonne indicatrice train test avant assemblage des deux tables
train$datasplit<-"train" ; test$datasplit<-"test"


# suppression d'une colonne visiblement inutile -> n'existait pas dans nos datasets
train$campaignCode<-NULL ; test$campaignCode<-NULL

# identification des 4 colonnes au format json
json<-c("trafficSource","totals","geoNetwork","device")
tables<-c("train","test")
glob<-data.table() #table vide qui va recuperer les tableas transformees
# lecture et transformation successive train et test (suppression au passage de colonnes inutiles) 
for (t in tables) {
  partiel<-get(t)[,setdiff(names(get(t)),json)] # colonnes non json
  for (j in json) partiel<-cbind(partiel,fromJSON(paste("[", paste(get(t)[[j]], collapse = ","), "]")))
  temp<-partiel$adwordsClickInfo
  partiel$adwordsClickInfo<-NULL
  temp$targetingCriteria<-NULL
  result<-as.data.table(cbind(partiel,temp))
  if(t=="train") result$campaignCode<-NULL else result$transactionRevenue<-NA
  glob<-rbind(glob,result)
}
#Liberer l'espace allou??? a partiel, train et test
rm(partiel, train, test) ; gc()


##### pour moi , ??? retirer
glob2 = glob

summary(glob2)



#Si le test et le train sont dans le meme table c'est pour qu'on split nous meme
summary(glob)
# En analysant le summary on observe plusieurs besoin ???vident pour le pr???processing


##Pre-processing

#Valeurs manquates

# Cette colonne ne contient qu'une seul valeur (observ??? lorqu'on voulait la passer en bool)
print((nrow(glob[socialEngagementType == "Not Socially Engaged"])/1708337)*100)
# On l'a laisse pour l'instant, elle ne semble quand m???me pas tr???s interresante

#Sum des na pour chaque colonne
for (col in 0:55) {
  if (sum(is.na(glob[,..col])) != 0)
  {
    print(colnames(glob[,..col]))
    print((sum(is.na(glob[,..col]))/1708337)*100)
  }
}
# On remarque que 6 colonnes sont au dessus de 95 % de na (adContent, page, slot, gclId, adNetworktype, isVideoAd)
#Nous n'avons que trop peu d'informations sur ces colonnes et de donn???e sur transactionRevenue (pour pr???dire/???valuer ces colonnes)
#Nous allons les supprimer 

glob[,c("isVideoAd","adNetworkType", "gclId", "page", "slot", "adContent"):=NULL]
names(glob)

#Transaction revenue est remplit a 99 % de na , nous allons les remplacer par des z???ros puisqu'ils correspondent ??? des non revenue. 
glob$transactionRevenue[is.na(glob$transactionRevenue)] = 0

#Sum des not available in demo dataset
for (col in 10:55) {
  print(colnames(glob[,..col]))
  print((sum((glob[,..col]) == "not available in demo dataset")/1708337)*100)
}
#On observe des 100 % de valeurs "not available in demo dataset"
#Nous allons les supprimer (17 colonnes)
glob[,c("cityId","operatingSystemVersion", "browserVersion", "browserSize", "networkLocation", "criteriaParameters","screenResolution", "screenColors", "latitude", "longitude", "language", "flashVersion", "mobileDeviceMarketingName", "mobileDeviceInfo", "mobileDeviceModel", "mobileInputSelector", "mobileDeviceBranding"):=NULL]
names(glob)



#Changer date en date
glob <- transform(glob, date = as.Date(as.character(date), "%Y%m%d"))


# transactionRevenue en float
glob <- transform(glob, transactionRevenue = as.numeric(transactionRevenue))

#isMobile en bool 
glob <- transform(glob, isMobile = as.logical(isMobile))

#changer colonnes num???riques en float: hits, pageviews, visits, bounces et newVisits
glob <- transform(glob, hits = as.numeric(hits))
glob <- transform(glob, pageviews = as.numeric(pageviews))
glob <- transform(glob, visits = as.numeric(visits))
glob <- transform(glob, bounces = as.numeric(bounces))
glob <- transform(glob, newVisits = as.numeric(newVisits))

# On pourrais labelliser/factoriser d'autres var


## Split
library(lubridate)
library(ggplot2)

year =  year(glob$date)
month = month(glob$date)
test = glob(glob$transactionRevenue != 0)

ggplot(data = glob, aes(x = month, y = visits)) +
  geom_point()


# Comme ???nonc??? dans la pr???sentation des donn???es kaggle: le test set est apr???s le 2018-12-1 jusqu'au 2019-01-31
date_split<- ymd("20170601")


set.seed(123)
train_index <- sample(seq_len(nrow(glob)))

train = glob[train_index,]
test = glob[-train_index,]
val = train[train$date >= date_split,]
train = train[train$date < date_split,]


# Utilisons GBM :

require(dismo)
require(gbm)


set.seed(92)

summary(train)

numVars <- c("visits", "hits", "bounces", "pageviews", "newVisits")
train[, numVars] <- lapply(train[, c("visits", "hits", "bounces", "pageviews", "newVisits")], as.numeric)
train$transactionRevenue <- as.numeric(train$transactionRevenue)
val[, numVars] <- lapply(val[, c("visits", "hits", "bounces", "pageviews", "newVisits")], as.numeric)
train$fullVisitorIdNum <- as.numeric(train$fullVisitorId)
val$fullVisitorIdNum <- as.numeric(val$fullVisitorId)

str(train)
train_num <- train[,c(6,7,8,16,17,18,19,20,21,33)]
str(train_num)


gbm.fit <- gbm(
  formula = train$transactionRevenue ~ .,
  distribution = "gaussian",
  data = train_num,
  n.trees = 1000,
  shrinkage = 0.001)

best.iter <- gbm.perf(gbm.fit)
print(best.iter)

summary(gbm.fit)


pred <- predict(gbm.fit, n.trees = best.iter, val)

caret::RMSE(pred, val$transactionRevenue)

