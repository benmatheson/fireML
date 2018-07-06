library(tidyverse)
library(caret)
library(skimr)
library(lubridate)

setwd("projects/fireML")
getwd()


# source data: https://afsmaps.blm.gov/imf/imf.jsp?site=firehistory
# http://afsmaps.blm.gov/output/Extract_afsmaps4044355215.zip
# need to extract attribute table from a shapefile


#raw data (from shapefile export)
fireHistoryraw <- read_csv("fireHistory.csv")

fireHx <- fireHistoryraw
fireHx$jd <- yday(fireHx$DiscDate) #creates julien day for discovey day
fireHx <- fireHx %>%filter(Prescribed != "Y") #removes prescribed fires
fireHx$duration <- fireHx$OutDate - fireHx$DiscDate #creates fire duration variable
fireHx <- filter(fireHx, duration>-1 & duration<365 ) #removes problematic durations
fireHx <- filter(fireHx, GenCause %in% c("Human", "Lightning")) #selects only human and lightning fires


#creates two sets of variables for . numFields primarly includes numerical variables
basicFields <-  c("MgmOption","Latitude","Longitude" ,"OriginOwn","OriginUnit","DiscDate",    "DiscSize","IADate","IASize","InitBehave" , "CntrlDate","OutDate","EstAcres",   "EstCost","GenCause" ,"PrimFuel","SUPPRESSIO","FIRECAUSE", "duration")
numFields <-  c("Latitude","Longitude" ,"GenCause", "DiscSize","IASize","EstAcres",   "EstCost", "duration", "jd")



# reduced fire data based on numerical and basic fields

fireHxBasic <- fireHx %>% select(basicFields ) #not used in analysis
fireHxNum <- fireHx %>% select(numFields ) #full dataset to split into training and test


#####Creates training (80%) and test (20%) data from fireHxNum ######

fireSample <- createDataPartition(fireHxNum$EstAcres, p=.8, list = F)
fireTrain <- fireHxNum[fireSample, ]
fireTest <- fireHxNum[-fireSample, ]


#not used logicBagCause <- train(GenCause~., data=fireTrain, method = "logicBag")


## train random forest model to predict general cause using predictor variables:
## "Latitude","Longitude", "Discovery Size","Inital Attack Size","Estimated Acres",   "Estimated Cost", "duration", "julien day"

rfCause <- train(GenCause~., data=fireTrain, method = "rf")


##examine model and results

rfCause$finalModel
rfCause$results
rfCause$modelInfo
summary(rfCause)


##run random forest model on test data

predictRfCause <- predict(rfCause, fireTest)

##create confusion matrix 

confusionMatrix(predictRfCause, fireTest$GenCause)



###########Predict fire size########## (performs poorly)

lm <- train(EstAcres~., data=fireTrain, method = "lm")  #linear regression
rf <- train(EstAcres~., data=fireTrain, method = "rf") #random forest

class(rfCause)
attributes(rfCause)

lm$finalModel
View(fireTrain)



###########Test scatterplots###########

ggplot(fireHx)+
    geom_point(aes(x=jd, y=EstAcres), alpha=.05)+
    theme_minimal()

ggplot(fireHx)+
    geom_point(aes(x=Latitude, y=EstAcres), alpha=.05)+
    theme_minimal()
ggplot(fireHx)+
    geom_point(aes(x=Longitude, y=EstAcres), alpha=.05)+
    theme_minimal()

ggplot(fireHx)+
  geom_point(aes(x=EstCost, y=EstAcres), alpha=.05)+
  theme_minimal()



