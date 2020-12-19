
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


set.seed(92)
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
tables<-c("train2","test2")
train3 <- data.table()
test3 <- data.table()
# lecture et transformation successive train et test (suppression au passage de colonnes inutiles) 
for (t in tables) {
  partiel<-get(t)[,setdiff(names(get(t)),json)] # colonnes non json
  for (j in json) partiel<-cbind(partiel,fromJSON(paste("[", paste(get(t)[[j]], collapse = ","), "]")))
  temp<-partiel$adwordsClickInfo
  partiel$adwordsClickInfo<-NULL
  temp$targetingCriteria<-NULL
  result<-as.data.table(cbind(partiel,temp))
  if(t=="train2"){
    result$campaignCode<-NULL
    train3<- rbind(train3, result)
    }
  else {
    result$transactionRevenue<-NA
    test3 <- rbind(test3, result)
    }
}
#Liberer l'espace allou? a partiel, train et test
rm(partiel, train, test, train2, test2) ; gc()



#Si le test et le train sont dans le meme table c'est pour qu'on split nous meme
head(train3)
head(test3)

summary(train3) 
summary(test3) 

#Changer date en date
train3 <- transform(train3, date = as.Date(as.character(date), "%Y%m%d"))
test3 <- transform(test3, date = as.Date(as.character(date), "%Y%m%d"))

# transactionRevenue en int
train3 <- transform(train3, transactionRevenue = as.integer(transactionRevenue))
test3 <- transform(test3, transactionRevenue = as.integer(transactionRevenue))

# 
train3 <- transform(train3, socialEngagementType = as.integer(as.logical(socialEngagementType)))
test3 <- transform(test3, socialEngagementType = as.integer(as.logical(socialEngagementType)))

# On commence par regarde les donnees � notre disposition :
summary(train3)
str(train3)
summary(test3)
str(test3)

# On commence par supprimer les donnes inexistantes de notre dataset pour all�ger le poids :
train3[,c("cityId", "metro", "operatingSystemVersion", "browserVersion", "browserSize", "networkLocation", "criteriaParameters","screenResolution", "screenColors", "latitude", "longitude", "language", "flashVersion", "mobileDeviceMarketingName", "mobileDeviceInfo", "mobileDeviceModel", "mobileInputSelector", "obileDeviceBranding"):=NULL]
test3[,c("cityId", "metro", "operatingSystemVersion", "browserVersion", "browserSize", "networkLocation", "criteriaParameters","screenResolution", "screenColors", "latitude", "longitude", "language", "flashVersion", "mobileDeviceMarketingName", "mobileDeviceInfo", "mobileDeviceModel", "mobileInputSelector", "obileDeviceBranding"):=NULL]
names(train3)
names(test3)
str(train3)
str(test3)



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

gbm.step(train3, gbm.x = which(names(train3) != "transactionRevenue"), gbm.y = which(names(train3) == "transactionRevenue"),
         offset = NULL, fold.vector = NULL, tree.complexity = 5,
         learning.rate = 0.01, bag.fraction = 0.75,
         var.monotone = rep(0, length(gbm.x)), n.folds = 10, prev.stratify = TRUE, 
         family = "gaussian", n.trees = 50, step.size = 50, max.trees = 10000,
         tolerance.method = "auto", tolerance = 0.001, plot.main = TRUE, plot.folds = FALSE,
         verbose = TRUE, silent = FALSE, keep.fold.models = FALSE, keep.fold.vector = FALSE, 
         keep.fold.fit = FALSE)


