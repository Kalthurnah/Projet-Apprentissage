
setwd("")

require(jsonlite) 
require(data.table)
library(caTools)
#lecture des donneees
train<-read.csv("train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 

#on réduit le dataset pour diminuer les temps d'importation
nrow(train)
nrow(test)
smp_size1 <- floor(0.1 * nrow(train))
smp_size2 <- floor(0.1 * nrow(test))


set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size1)
test_ind <- sample(seq_len(nrow(test)), size = smp_size2)

train2 <- train[train_ind, ]
test2 <- test[test_ind, ]
nrow(train2)
nrow(test2)

# cr?ation d'une colonne indicatrice train test avant assemblage des deux tables
train2$datasplit<-"train" ; test2$datasplit<-"test"

# suppression d'une colonne visiblement inutile -> n'existait pas dans nos datasets
train2$campaignCode<-NULL ; test2$campaignCode<-NULL

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
#Liberer l'espace allou? a partiel, train et test
rm(partiel, train, test, train2, test2) ; gc()



#Si le test et le train sont dans le meme table c'est pour qu'on split nous meme
head(glob)

summary(glob) 

#Changer date en date
glob <- transform(glob, date = as.Date(as.character(date), "%Y%m%d"))

# transactionRevenue en int
glob <- transform(glob, transactionRevenue = as.integer(transactionRevenue))

# 
glob <- transform(glob, socialEngagementType = as.integer(as.logical(socialEngagementType)))

# On commence par regarde les donnees � notre disposition :
summary(glob)
str(glob)

# On commence par supprimer les donnes inexistantes de notre dataset pour all�ger le poids :
glob[,c("cityId", "metro", "operatingSystemVersion", "browserVersion", "browserSize", "networkLocation", "criteriaParameters","screenResolution", "screenColors", "latitude", "longitude", "language", "flashVersion", "mobileDeviceMarketingName", "mobileDeviceInfo", "mobileDeviceModel", "mobileInputSelector", "obileDeviceBranding"):=NULL]
names(glob)
str(glob)

head(glob)

# On commence par supprimer les donnes inexistantes de notre dataset pour alléger le poids :
glob[,c("cityId", "metro", "operatingSystemVersion", "browserVersion", "browserSize", "networkLocation", "criteriaParameters","screenResolution", "screenColors", "latitude", "longitude", "language", "flashVersion", "mobileDeviceMarketingName", "mobileDeviceInfo", "mobileDeviceModel", "mobileInputSelector", "obileDeviceBranding"):=NULL]
names(glob)
str(glob)

