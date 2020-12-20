setwd("C:/Users/jdelebec/Documents/esilv/5_annee/apprentissage/projet")
require(jsonlite) 
require(data.table)
#lecture des donneees
train<-read.csv("train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 

# création d'une colonne indicatrice train test avant assemblage des deux tables
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
#Liberer l'espace alloué a partiel, train et test
rm(partiel, train, test) ; gc()


##### pour moi , à retirer
glob2 = glob

summary(glob2)



#Si le test et le train sont dans le meme table c'est pour qu'on split nous meme
summary(glob)
# En analysant le summary on observe plusieurs besoin évident pour le préprocessing



##### pour moi , à retirer
glob2 = glob

summary(glob2)


##Pre-processing

#Valeurs manquates

# Cette colonne ne contient qu'une seul valeur (observé lorqu'on voulait la passer en bool)
print((nrow(glob[socialEngagementType == "Not Socially Engaged"])/1708337)*100)
# On l'a laisse pour l'instant, elle ne semble quand même pas très interresante
#On l'a en fait retirer car il genait la regression

glob[,c("socialEngagementType"):=NULL]

#Sum des na pour chaque colonne
for (col in 0:ncol(glob)) {
  if (sum(is.na(glob[,..col])) != 0)
  {
    print(colnames(glob[,..col]))
    print((sum(is.na(glob[,..col]))/nrow(glob))*100)
  }
}
# On remarque que 6 colonnes sont au dessus de 95 % de na (adContent, page, slot, gclId, adNetworktype, isVideoAd)
#Nous n'avons que trop peu d'informations sur ces colonnes et de donnée sur transactionRevenue (pour prédire/évaluer ces colonnes)
#Nous allons les supprimer 

glob[,c("isVideoAd","adNetworkType", "gclId", "page", "slot", "adContent"):=NULL]
names(glob)

#Transaction revenue est remplit a 99 % de na , nous allons les remplacer par des zéros puisqu'ils correspondent à des non revenue. 
glob$transactionRevenue[is.na(glob$transactionRevenue)] = 0

#Sum des not available in demo dataset
for (col in 1:ncol(glob)) {
  print(colnames(glob[,..col]))
  print((sum((glob[,..col]) == "not available in demo dataset")/nrow(glob))*100)
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

#changer colonnes numériques en float: hits, pageviews, visits, bounces et newVisits
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


ggplot(data = glob, aes(x = year, y = transactionRevenue)) +
  geom_point()


# Comme énoncé dans la présentation des données kaggle: le test set est après le 2018-12-1 jusqu'au 2019-01-31
date_split<- ymd("20170601")


set.seed(123)
train_index <- sample(seq_len(nrow(glob)))

train = glob[train_index,]
test = glob[-train_index,]
val = train[train$date >= date_split,]
train = train[train$date < date_split,]


#Regression linéaire
#mod.lm=lm(transactionRevenue~.,data = train)



for (col in 0:ncol(train)) {
  if (sum(is.na(train[,..col])) != 0)
  {
    print(colnames(train[,..col]))
    print((sum(is.na(train[,..col]))/nrow(train))*100)
  }
}

#Ces dernières colonnes remplit de na gène la regression : keyword, isTrueDirect, referralPath, bounces
#Les temps et la mémoire demandé nous oblige a reduire les sets
library(dplyr)
train = sample_n(train,50000)
val = sample_n(val,50000)


# Observation des correlations pour les colonnes numériques: "visitId", "visitNumber", visitStartTime" "visits" "hits" "pageviews" "bounces" "newVisits" 
i1 = sapply(train, is.numeric)
y1 = "transactionRevenue" #change it to actual column name
x1 = setdiff(names(train)[i1], y1)
cor(train[, x1,with = FALSE], train[[y1]])
# On observe une grosse correlation avec hits , 4 colonnes ne sont pas utilisables

#regardons pour les colonnes non-numéric (date, chanelGrouping, campaign, source, medium, continent,operatingSystem, isMobile, deviceCategory" ) : 
m_v1 = model.matrix(~ transactionRevenue - 1, train)
m_v2 = model.matrix(~ date - 1, train)
m_v3 = model.matrix(~ chanelGrouping - 1, train)
m_v4 = model.matrix(~ campaign + date - 1, train)
m_v5 = model.matrix(~ source - 1, train)
m_v6 = model.matrix(~ medium - 1, train)
m_v7 = model.matrix(~ continent - 1, train)
m_v8 = model.matrix(~ operatingSystem - 1, train)
m_v9 = model.matrix(~ isMobile - 1, train)
m_v10 = model.matrix(~ deviceCategory - 1, train)

library(caret)


cor(m_v1, m_v2)
cor(m_v1, m_v3)
cor(m_v1, m_v4)
cor(m_v1, m_v5)
cor(m_v1, m_v6)
cor(m_v1, m_v7)
cor(m_v1, m_v8)
cor(m_v1, m_v9)
cor(m_v1, m_v10)

#Faible correlation sur date, nous avons garder les colonnes n'ayant pas de na ou de correlation faible
mod.lm=lm(transactionRevenue~ hits + visitNumber + channelGrouping + campaign + source + medium + continent + isMobile + operatingSystem + deviceCategory ,data = train)

modelSummary <- summary(mod.lm)  
modelCoeffs <- modelSummary$coefficients  

summary(mod.lm) 
y = val$transactionRevenue
y_hat = predict(mod.lm)
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE
#Nous avons une faible erreur  (2.26428e+14)



#Voyons voir si la regression generalise marche mieux

model2 = glm(transactionRevenue~ hits + visitNumber + channelGrouping + campaign + source + medium + continent + isMobile + operatingSystem + deviceCategory ,data = train)
summary(model2)
prediction = predict(model2)
error2 = y-prediction
error_squared2 = error2^2
MSE2 = mean(error_squared2)
MSE2

# L'erreur baise un peu 2.231412e+14 mais ce n'est pas flagrant

#Pour aller plus loin: encoder la plupart des colonnes 

channel_variety = unique(train$channelGrouping)
factors <- factor(channel_variety)
train2 = train
train2$channelGrouping = factor(train2$channelGrouping)

model3 = glm(transactionRevenue~ hits + visitNumber + channelGrouping + campaign + source + medium + continent + isMobile + operatingSystem + deviceCategory ,data = train2)
summary(model3)
prediction2 = predict(model3)
error3 = y-prediction2
error_squared3 = error3^2
MSE3 = mean(error_squared3)
MSE3

# l'erreur a légérement augmenté: 2.26428e+14 cela ne change quasiment rien
# Axe d'amélioration : en rajoutant des features (mois et jour)


#Pour le gbm 
set.seed(123)
train_index <- sample(seq_len(nrow(glob)))

train = glob[train_index,]
test = glob[-train_index,]
val = train[train$date >= date_split,]
train = train[train$date < date_split,]
