setwd("D:/ESILV/5A/Apprentissage")

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

summary(glob) 

#Changer date en date
glob <- transform(glob, date = as.Date(as.character(date), "%Y%m%d"))

# transactionRevenue en int
glob <- transform(glob, transactionRevenue = as.integer(transactionRevenue))

# 
glob <- transform(glob, socialEngagementType = as.integer(as.logical(socialEngagementType)))

# On commence par regarde les donnees à notre disposition :
summary(glob)
str(glob)

# On commence par supprimer les donnes inexistantes de notre dataset pour alléger le poids :
glob[,c("cityId", "metro", "operatingSystemVersion", "browserVersion", "browserSize", "networkLocation", "criteriaParameters","screenResolution", "screenColors", "latitude", "longitude", "language", "flashVersion", "mobileDeviceMarketingName", "mobileDeviceInfo", "mobileDeviceModel", "mobileInputSelector", "obileDeviceBranding"):=NULL]
names(glob)
str(glob)


# Utilisons GBM :

library(doParallel)
#Find out how many cores are available (if you don't already know)
cores<-detectCores()
#Create cluster with desired number of cores, leave one open for the machine         
#core processes
cl <- makeCluster(cores[1]-1)
#Register cluster
registerDoParallel(cl)

require(dismo)
require(gbm)

set.seed(92)

gbm.step(glob, gbm.x = which(names(glob) != "transactionRevenue"), gbm.y = which(names(glob) =="transactionRevenue"),
         offset = NULL, fold.vector = NULL, tree.complexity = 5,
         learning.rate = 0.01, bag.fraction = 0.75,
         var.monotone = rep(0, length(gbm.x)), n.folds = 10, prev.stratify = TRUE, 
         family = "gaussian", n.trees = 50, step.size = 50, max.trees = 10000,
         tolerance.method = "auto", tolerance = 0.001, plot.main = TRUE, plot.folds = FALSE,
         verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, 
         keep.fold.fit = FALSE)
