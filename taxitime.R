# Loading the data

train <- read.csv("train.csv", header=TRUE)
test_csv <- read.csv("test.csv", header=TRUE)

# Train has 11 columns while test has 9.
col_train <- colnames(train)
col_test <- colnames(test)
col_train[!col_train %in% col_test] #viewing the columns that are not in col_test but present in col_train
# It is seen that dropoff_datetime and trip_duration is not present in the test data, but is present in the training data
# so dropping the column dropff_datetime.The trip_duration is the target variable

# dropping the dropoff_datetime from training data as it isnt present in testdata
train$dropoff_datetime <- NULL


# Adding the trip_duration (target variable) to the test data
test.trip_duration <- data.frame(test_csv[],trip_duration = rep("None",nrow(test)))

# Now as both the dataframes have the same number of variables, combine both.
data.combined <- rbind(train,test.trip_duration)
str(data.combined)
data.combined$trip_duration <- as.integer(data.combined$trip_duration)


# Now exploring the variables
str(data.combined)

# The pickup_datatime is a factor with a lot of different levels. Focusing on that,
head(data.combined$pickup_datetime)

#The datetime are together concatenated and seperated with space.
#So Splitting them into date and time respectively (as 2 columns)
data.combined$pickup_datetime <- as.character(data.combined$pickup_datetime)
library(stringr) 
split_DT <- as.data.frame(str_split_fixed(data.combined$pickup_datetime," ",2))
data.combined1<-cbind(data.combined,split_DT)
str(data.combined1)
library(data.table)
# Renaming the pickup date and pickup time columns
setnames(data.combined1,"V1","pickup_date")
setnames(data.combined1,"V2","pickup_time")
data.combined1$pickup_date<-as.character(data.combined1$pickup_date)
data.combined1$pickup_time<-as.character(data.combined1$pickup_time)
str(data.combined1)
data.combined1$pickup_date<-as.Date(data.combined1$pickup_date)

# Time is in hr:min:sec format. Considering only the hour
time_split <- as.data.frame(str_split_fixed(data.combined1$pickup_time,":",3))
time_hr <-time_split[,c("V1")]
time_hr <- as.data.frame(time_hr)
data.combined2<-cbind(data.combined1,time_hr)
data.combined2$time_hr<-as.factor(data.combined2$time_hr)
# checking and visualizing the range of values and distribution of time_hr
library(ggplot2)
a<-table(data.combined2$time_hr)
a<-as.data.frame(a)
ggplot(a,aes(x=as.factor(Var1),fill=Freq))+geom_bar()+xlab("Hour")+ylab("Number of Passengers")+labs(fill="Travelled")



library(data.table)



# Checking for NA values
table(is.na(data.combined2$passenger_count))
table(is.na(data.combined2$pickup_longitude))
table(is.na(data.combined2$pickup_latitude))
table(is.na(data.combined2$dropoff_longitude))
table(is.na(data.combined2$dropoff_latitude))
# All values are filled.

# Checking summary of the above examined quantites
summary(data.combined2$passenger_count)
summary(data.combined2$pickup_longitude)
summary(data.combined2$pickup_latitude)
summary(data.combined2$dropoff_longitude)
summary(data.combined2$dropoff_latitude)

#========================================================================================================================

# SOME EXPLORATORY MODELING

library(h2o)

# in h20 the labels are training data are combined and then the respective columns are mentioned
localH2O<-h2o.init(nthreads = 2)

# Centering and scaling the numerical data in data.combined2

data.combined2$passenger_count<-as.numscale(data.combined2$passenger_count)
data.combined2$pickup_longitude<-scale(data.combined2$pickup_longitude)
data.combined2$pickup_latitude<-scale(data.combined2$pickup_latitude)
data.combined2$dropoff_longitude<-scale(data.combined2$dropoff_longitude)
data.combined2$dropoff_latitude<-scale(data.combined2$dropoff_latitude)


# Training a random forest with passenger_count,pickup and drop long and lat,time_hr
rf.train.1<-data.combined2[1:1458644,c]
rf.label.1<-data.combined2[1:1458644,c("passenger_count","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","trip_duration")]
rf.train.1<-as.data.table(rf.train.1)

#====================================================================================================================
# first attempt with linear regression without centering, scaling and using "passenger_count","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude"
train.h2o<-rf.train.1

train.h2o <- as.h2o(rf.train.1)

# check column index number
colnames(train.h2o)
# applying multiple regression
regression.model <- h2o.glm(y=6, training_frame = train.h2o, family = "gaussian")

test<-data.combined2[-(1:nrow(rf.train.1)),c("passenger_count","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","trip_duration")]

# test it on the dataset
h2o.test <- as.h2o(test)
predict.mulreg <- as.data.frame(h2o.predict(regression.model, h2o.test))
submission <- data.frame(id = test_csv$id, trip_duration = round(predict.mulreg,0))
setnames(submission, "predict","trip_duration")
mean <- mean(data.combined2$trip_duration[1:1458644])
mean<- round(mean,0)
submission$trip_duration[which(submission$trip_duration<0)] <- mean
write.csv(submission, file = "sub_reg_Jul26_1953.csv", row.names =  F)

#=========================================================================================================================================================

library(h2o)
# TRY2 : Centering and scaling and adding "time_hr"
# in h20 the labels are training data are combined and then the respective columns are mentioned
localH2O<-h2o.init(nthreads = 2)
# Centering and scaling
data.combined2.scaled<-as.data.frame(scale(data.combined2[,c("passenger_count","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude")]))
data.combined2.scaled<-cbind(data.combined2.scaled,data.combined2which(submission$trip_duration<0)[,c("time_hr","trip_duration")])
train.h2o<-as.h2o(data.combined2.scaled)
colnames(train.h2o)
regression.model <- h2o.glm(y=7, training_frame = train.h2o, family = "gaussian")
h2o.performance(regression.model)

test<-data.combined2.scaled[-(1:nrow(rf.train.1)),c("passenger_count","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","trip_duration","time_hr")]
h2o.test<-as.h2o(test)

predict.mulreg <- as.data.frame(h2o.predict(regression.model, h2o.test))
submission <- data.frame(id = test_csv$id, trip_duration = round(predict.mulreg,0))
setnames(submission, "predict","trip_duration")
mean <- mean(data.combined2$trip_duration[1:1458644])
mean<- round(mean,0)
submission$trip_duration[which(submission$trip_duration<0)] <- mean
write.csv(submission, file = "sub_reg_Jul26_2253.csv", row.names =  F)


# =======================================================================================================================================
# Trying a random forest on the same
system.time(
  rforest.model <- h2o.randomForest(y=7, training_frame = train.h2o, ntrees = 1000, mtries = 3,
                                    max_depth = 4, seed = 1122)
)

system.time(predict.rforest <- as.data.frame(h2o.predict(rforest.model, h2o.test)))

submission <- data.frame(id = test_csv$id, trip_duration = round(predict.rforest,0))
setnames(submission, "predict","trip_duration")
mean <- mean(data.combined2$trip_duration[1:1458644])
mean<- round(mean,0)
submission$trip_duration[which(submission$trip_duration<0)] <- mean
write.csv(submission, file = "sub_RF_Jul26_2308.csv", row.names =  F)


# ===============================================================================================================================================
# trying a deep learning model.
localH2O<-h2o.init(nthreads = 6)
system.time(
  dlearning.model <- h2o.deeplearning(y = 7,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(128,128),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)

predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, h2o.test))

submission <- data.frame(id = test_csv$id, trip_duration = round(predict.dl2,0))
setnames(submission, "predict","trip_duration")
mean <- mean(data.combined2$trip_duration[1:1458644])
mean<- round(mean,0)
submission$trip_duration[which(submission$trip_duration<0)] <- mean
write.csv(submission, file = "sub_DL_Jul27_0035.csv", row.names =  F)

#===============================================================================================================================
# Categorizing time into Early Morning, morning, afternoon, evening, night, late night
# 03-07 :  Early morning, 08-11 : Morning, 12-15 : Afternoon, 16-19 : Evening, 20-22 : Night, 
# 23-02: Late Night

library(data.table)
library(h2o)



data.combined2.scaled2<-data.combined2.scaled
data.combined2.scaled$time_hr <- as.numeric(data.combined2.scaled$time_hr)
time_day <- as.data.frame(as.numeric(data.combined2.scaled$time_hr))
setnames(time_day,"as.numeric(data.combined2.scaled$time_hr)","time_hr")
time_mod2 <- data.frame(time_day= rep("None",nrow(data.combined2.scaled2)))
time_mod2$time_day <- as.character(time_mod2$time_day)

time_mod2$time_day[which(time_day$time_hr>=8 & time_day$time_hr <=11)] <- as.character("M")
time_mod2$time_day[which(time_day$time_hr>=12 & time_day$time_hr <=15)] <- as.character("A")
time_mod2$time_day[which(time_day$time_hr>=16 & time_day$time_hr <=19)] <- as.character("E")
time_mod2$time_day[which(time_day$time_hr>=20 & time_day$time_hr <=22)] <- as.character("N")
time_mod2$time_day[which(time_day$time_hr>=23)] <- as.character("LN")
time_mod2$time_day[which(time_day$time_hr <=2)] <- as.character("LN")
time_mod2$time_day[which(time_day$time_hr>=3 & time_day$time_hr <=7)] <- as.character("EM")



data.combined2.scaled2<-cbind(data.combined2.scaled2,time_mod2)
data.combined2.scaled2$time_hr<-NULL

data.scaled3<-cbind(data.combined2.scaled2,data.combined$vendor_id)
setnames(data.scaled3,"data.combined$vendor_id","vendor_id")

data.scaled3$vendor_id <- as.factor(data.scaled3$vendor_id) 
data.scaled3$time_day <- as.factor(data.scaled3$time_day) 

train<-data.scaled3[1:nrow(train),]
test<-data.scaled3[-(1:nrow(train)),]



localH2O<-h2o.init(nthreads = 6)

train.h2o<-as.h2o(train)
test.h2o<-as.h2o(test)



system.time(
  dlearning.model <- h2o.deeplearning(y = 6,
                                      training_frame = train.h2o,
                                      epoch = 60,
                                      hidden = c(128,128),
                                      activation = "RectifierWithDropout",
                                      seed = 1122
  )
)

predict.dl2 <- as.data.frame(h2o.predict(dlearning.model, test.h2o))

submission <- data.frame(id = test_csv$id, trip_duration = round(predict.dl2,0))
setnames(submission, "predict","trip_duration")
mean <- mean(as.numeric(data.combined$trip_duration[1:1458644]))
mean<- round(mean,0)
submission$trip_duration[which(submission$trip_duration<0)] <- mean
write.csv(submission, file = "sub_DL_Jul28_2236.csv", row.names =  F)

#==============================================================================================================================================================
# Checking the importance of each variable
library(h2o)
localH2O<-h2o.init(nthreads = 6)


h2o.varimp_plot(dlearning.model)
h2o.performance(dlearning.model)
# From the varimp plot longitudes are the most important.

# Adding a column for the day extracted data to DATA.SCALED3
data.scaled3$day<- weekdays(data.combined2$pickup_date) #Adds a new column with the day extracted from date, using 'weekdays' function

# Combining afternoon and evening to reduce overfitting.
data.scaled3$time_day <- as.character(data.scaled3$time_day)
str(data.scaled3)
data.scaled3$time_day[which(data.scaled3$time_day == "E" | data.scaled3$time_day == "A")] <- "mid-day" 
data.scaled3$time_day <- as.factor(data.scaled3$time_day)

table(data.combined2$time_hr)
table(data.scaled3$time_day)

library(data.table)

# Visualizing the trip duration and time of the day
library(ggplot2)
data.scaled3<-as.data.table(data.scaled3)
train<-fread("train.csv", header=TRUE)
train$time_day <- as.data.table(data.scaled3$time_day[1:nrow(train)])

ggplot(train,aes(trip_duration,fill=time_day))+geom_histogram(bins=10)

# Data cleansing is left. 
summary(train$trip_duration) # The max value corresponds to 979 hours and the minimum is one second. Both arent possible

# Checking out the entries where the trip duration is more than 3 hours (10800 secs)
train[which(train$trip_duration>10800)]


#store_and_fwd_flag - This flag indicates whether the trip record was held in vehicle memory 
#before sending to the vendor because the vehicle did not have a connection to the server - Y=store and forward; 
#N=not a store and forward trip

table(train$store_and_fwd_flag[which(train$trip_duration>10800)])
table(train$store_and_fwd_flag[which(train$trip_duration<10800)])
table(train$vendor_id[which(train$trip_duration<10800)])
table(train$vendor_id[which(train$trip_duration>10800)])

table(train$vendor_id[which(train$store_and_fwd_flag=="N")])
table(train$vendor_id[which(train$store_and_fwd_flag=="")])

summary(train$trip_duration[which(train$store_and_fwd_flag == "Y")])

#=====================================================================================================================================================
# DATA REQUIRES CLEANSING AS SOME DURATIONS ARE LESS THAN 1 MIN AND MAX IS 979 HOURS WHICH ARE ERRORS.
# Visualizing using plotly

library('plotly')

attach(train)
hist<-plot_ly(x=store_and_fwd_flag, type='bar')
layout(hist, title = "trip duration distribution")

attach(data.scaled3[1:nrow(train)])
hist1<-plot_ly(x=day,type='histogram')
layout(hist1, title="Count as per Day")


hist1<-plot_ly(x=trip_duration, type='histogram',color=day)
layout(hist1, "trip duration")


# Adding a distance column by computing distance(in KM) between pickup and drop off
library(geosphere)# in data.scaled3 the co-ords are scaled. so using data.combined2.

data.combined2$distance <- distHaversine(data.combined2[,5:6], data.combined2[,7:8])/1000

summary(data.combined2$distance[which(data.combined2$trip_duration<5)])

# trips less than a minute are quite odd. they can be errors. Analysing them,

library(plotly)

data_sm <- data.combined2[which(data.combined2$trip_duration < 50),]
summary(data_sm$trip_duration)
summary(data_sm$distance)

attach(data_sm)
scatter1 <- plot_ly(x=trip_duration, y=distance)

layout(scatter1, "distance and duration")

# Computing average velocity
velocity <- (data.combined2[1:1458644,]$distance/data.combined2[1:1458644,]$trip_duration)
velocity <- velocity*3600
train$velocity <- velocity
data.scaled4 <- data.scaled3[which(train$velocity < 90)]
data.scaled4$velocity <- data.scaled4$distance/data.scaled4$trip_duration
data.scaled4$velocity <- data.scaled4$velocity*3600
summary(data.scaled4$velocity)
# Hence removed all the columns with avg velocity > 90, which is practically impossible as the speed limit in 
# NYC is 25mph or 41kmph

# Now checking the ones where the distance is more than 50 kms,
data.dist_err <- data.scaled4[which(data.scaled4$distance > 50)]  
data.vel_err <- data.scaled4[which(data.scaled4$velocity < 5)]

summary(data.vel_err)



plot_ly(x=~data.vel_err$trip_duration, y= ~data.vel_err$distance)

# The trip duration for certain quantities is very high reaching more than 15,000 seconds and ranging further into millions
# but displacement is far lesser. And its pretty obvious that the passenger is not going to sit in the car for 23 hours.
# maybe not even in 3 hours
# There are records where the distance travelled is more than 60km and the time taken is 85000 seconds.

# Assuming that a cab ride does not go beyond 3 hours or 10800 seconds
data.scaled5 <- data.scaled4[which(data.scaled4$trip_duration < 10800)]
data.dist_err <- data.scaled5[which(data.scaled4$distance > 50)]  
plot_ly(x=~data.dist_err$trip_duration, y= ~data.dist_err$distance)

# Removing a

data.vel_err <- data.scaled5[which(data.scaled4$velocity < 5)]
data.dist_err <- data.scaled5[which(data.scaled5$distance == 0)]  


# Assuming that a man cant be inside the cab for more than an hour and cover zero displacement,
# removing all quantities having zero displacement and trip duration more than 3600
data.scaled5 <- data.scaled5[which(!(data.scaled5$trip_duration >3600 & data.scaled5$distance == 0))]
data.dist_err <- data.scaled5[which(data.scaled5$distance == 0)]  
plot_ly(x=~data.scaled5$trip_duration,type = "histogram")

library(geosphere)
# Adding distance for test
colnames(test_csv)
test_csv$distance <- data.scaled3$distance[-(1:nrow(train))]
length(test_csv$distance[which(test_csv$distance==0)])
 
summary(data.scaled5)


colnames(data.scaled5)
colnames(data.scaled3)

# Now training a deep learning model on this data. (first removing velocity)
data.scaled5$velocity<-NULL
# VELOCITY LIST IS SAVED, so use that for ACCESSING VELOCITY AGAIN


#===================================================================================================================

library(h2o)
localH2O<-h2o.init(nthreads = 6)
trip_duration.back <-data.scaled5$trip_duration

data.scaled5$day <- as.factor(data.scaled5$day)
data.scaled3$day <- as.factor(data.scaled3$day)

data.scaled6<-rbind(data.scaled5,data.scaled3[-(1:nrow(train))])

data.scaled7<-as.data.frame(scale(data.scaled6[,c("passenger_count","pickup_longitude","pickup_latitude","dropoff_longitude","dropoff_latitude","distance")]))
data.scaled7$trip_duration <- data.scaled6$trip_duration
data.scaled7$time_day <- data.scaled6$time_day
data.scaled7$vendor_id <- data.scaled6$vendor_id
data.scaled7$day <- data.scaled6$day

colnames(data.scaled7)



h2o.train<-as.h2o(data.scaled7[1:nrow(data.scaled5),])
h2o.test<-as.h2o(data.scaled7[-(1:nrow(data.scaled5)),])

colnames(h2o.train)
system.time(
  dlearning.model <- h2o.deeplearning(y = 7,
                                      training_frame = h2o.train,
                                      epoch = 100,
                                      hidden = c(64,64),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)

system.time(
  dlearning.model2 <- h2o.deeplearning(y = 7,
                                      training_frame = h2o.train,
                                      epoch = 60,
                                      hidden = c(64,64),
                                      activation = "Rectifier",
                                      seed = 1122
  )
)
system.time(
  dlearning.model3 <- h2o.deeplearning(y = 7,
                                       training_frame = h2o.train,
                                       epoch = 60,
                                       hidden = c(64,64,64),
                                       activation = "Rectifier",
                                       seed = 1122
  )
)

system.time(
  dlearning.model4 <- h2o.deeplearning(y = 7,
                                       training_frame = h2o.train,
                                       epoch = 200,
                                       hidden = c(64,64,64),
                                       activation = "Rectifier",
                                       seed = 1122
  )
)

system.time(
  dlearning.model4 <- h2o.deeplearning(y = 7,
                                       training_frame = h2o.train,
                                       epoch = 100,
                                       hidden = c(32,16,8),
                                       activation = "Rectifier",
                                       seed = 1122
  )
)






predict.dl3 <- as.data.frame(h2o.predict(dlearning.model2, h2o.test))
predict.dl4 <- as.data.frame(h2o.predict(dlearning.model4, h2o.test))

submission <- data.frame(id = test_csv$id, trip_duration = round((predict.dl3$predict+predict.dl4$predict)/2,0))

library(data.table)

setnames(submission, "predict","trip_duration")
mean <- mean(as.numeric(data.scaled7$trip_duration[1:nrow(data.scaled5)]))
mean<- round(mean,0)
submission$trip_duration[which(submission$trip_duration>10800)] <- mean
write.csv(submission, file = "sub_DL_Aug1_2142.csv", row.names =  F)


#==============================================================================================================================================================
# Classifying the days as weekdays and weekends.
data.scaled7$week<-ifelse(as.character(data.scaled7$day) %in% c("Saturday","Sunday"),"Weekend","Weekday")


h2o.train<-as.h2o(data.scaled7[1:nrow(data.scaled5),])
h2o.test<-as.h2o(data.scaled7[-(1:nrow(data.scaled5)),])


system.time(
  dlearning.model3 <- h2o.deeplearning(x= c(2,3,4,5,6,7,8,9,11),y = 7,
                                       training_frame = h2o.train,
                                       epoch = 60,
                                       hidden = c(64,64,64),
                                       activation = "Rectifier",
                                       seed = 1122
  )
)
























