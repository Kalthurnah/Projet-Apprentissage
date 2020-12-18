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



#Si le test et le train sont dans le meme table c'est pour qu'on split nous meme
head(glob)



#Changer date en date
glob <- transform(glob, date = as.Date(as.character(date), "%Y%m%d"))

# transactionRevenue en int
glob <- transform(glob, transactionRevenue = as.integer(transactionRevenue))

# 
glob <- transform(glob, socialEngagementType = as.integer(as.logical(socialEngagementType)))

