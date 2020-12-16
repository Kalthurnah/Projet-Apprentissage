setwd(" ....")
require(jsonlite) 
#lecture des données
train<-read.csv("train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
# création d'une colonne indicatrice train test avant assemblage des deux tables
train$datasplit<-"train" ; test$datasplit<-"test"
# suppression d'une colonne visiblement inutile
train$campaignCode<-NULL ; test$campaignCode<-NULL
# identification des 4 colonnes au format json
json<-c("trafficSource","totals","geoNetwork","device")
tables<-c("train","test")
glob<-data.table() #table vide qui va récupérer les tableas transformées
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
rm(partiel, train, test) ; gc()