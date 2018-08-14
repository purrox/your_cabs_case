# This code is created by Carlos Salas and Frarlon Rodriguez 
# for the the Avantica Machine Learning course 2018

# Load Libreries
library(DataExplorer)
library(readxl)
library(dplyr)
library(chron)
library(lubridate)
library(plyr)
library(geosphere)
library(ggplot2)
library(GGally)
library(polycor)
library(gridExtra)
library(MASS)
library(caret)
library(kernlab)
library(XML)
library(bitops)
library(RCurl)
library(ggthemes)
library(rpart)
library(rpart.plot)

# Import Data
data <- read_excel("Downloads/Taxi-cancellation-case.xlsx")
View(data)

# Summary Data
summary(data)

# Histograms
plot_bar(data)

# Columns with date values in unix format
date_colums <- c("from_date", "to_date", "booking_created")

# Formating values if each date columns to a readble one
for (col in date_colums) {
  data[[col]] = as.POSIXct(as.Date(as.numeric(data[[col]]), origin="1899-12-30"))
}

# Change the values of Cancellation to Yes or No.
data <- data %>% mutate(Car_Cancellation = 
                          case_when (Car_Cancellation==1 ~ "Yes",  Car_Cancellation==0 ~ "No"))


# Create new column to group type of booking 
# and eliminate booking online and mobile
# mobile, online or other.
data['booking_type'] <- NA

data <- data %>% mutate(booking_type = 
                          case_when (online_booking == 1 & mobile_site_booking == 0 ~ "Online",
                                     online_booking == 0 & mobile_site_booking == 1 ~ "Mobile",
                                     online_booking == 0 & mobile_site_booking == 0 ~ "Other"))

# Change some character class type to factor types

factor_colums <- c("Car_Cancellation", "booking_type", "user_id", "vehicle_model_id",
                   "travel_type_id", "package_id", "from_area_id", "to_area_id",
                   "from_city_id","to_city_id", "period", "booking_day","booking_month")

# Changing type if each columns 
for (col in factor_colums) {
  data[[col]] = as.factor(data[[col]])
}

# Summary Data
summary(data)

# Create Dummy columns to day
data$period <- as.POSIXct(paste(data$booking_created, data$booking_created), format="%Y-%m-%d %H:%M:%S")

data$period <- mapvalues(hour(data$period),from=c(0:23),
          to=c(rep("Night",times=5), rep("Morning",times=6), 
               rep("Afternoon",times=5),rep("Night", times=8)))

data$booking_day <- weekdays(as.Date(data$booking_created)) # Create Days columns
data$booking_month <- months(as.Date(data$booking_created)) # Create month columns


# Create a new column with the distance of the travels

data$to_long = as.numeric(data$to_long) # Convert to long to a numeric type
data$to_lat = as.numeric(data$to_lat)   # Convert to lat to a numeric type
data$from_area_id = as.numeric(data$from_area_id)   # Convert to lat to a numeric type
data$to_area_id = as.numeric(data$to_area_id)   # Convert to lat to a numeric type

plot_missing(data) # A lot of missing values  

geo_colums <- c("from_lat", "from_long", "to_lat", "to_long", "from_area_id", "to_area_id")

for (col in geo_colums) {
  data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm=TRUE), 
                        data[[col]])                                                      
}



f <- function(x){
  a <- as.numeric(x["from_long"])
  b <- as.numeric(x["from_lat"])
  c <- as.numeric(x["to_long"])
  d <- as.numeric(x["to_lat"])
  distm(c(a, b), c(c, d), fun = distHaversine)
}

data$distance <- apply( data, 1,  f  )
  
# Missing Data plot
plot_missing(data)  
  
#Calculating waiting time
data$waitingTime = difftime(data$from_date,data$booking_created, units = "mins")  

# Graphing some interesting statistics

# Relation of each cancellation with vehicles
vehicle_table <- table(data$Car_Cancellation , data$vehicle_model_id)  

barplot(vehicle_table, main="Cancellation Distribution for Vehicle",
        xlab="Vehicle", col=c("darkblue","red"),
        legend = rownames(vehicle_table))

grid.table(vehicle_table)

# Days of the week and cancellations

data.cancellation <- data[data$Car_Cancellation == 'Yes', ]

cancellation <- table(data.cancellation$Car_Cancellation, data.cancellation$booking_day)  

print(cancellation) 

barplot(cancellation, main="Cancellation Distribution for Vehicle",
        xlab="Vehicle", col=c("darkblue","red"),
        legend = rownames(cancellation),beside=TRUE )

plot(data$booking_day, data$Car_Cancellation)


#Cancellation for Booking type

booking_type <- table(data$Car_Cancellation , data$booking_type)

barplot(booking_type, main="Cancellation Distribution for Booking Type",
        xlab="Booking Type", col=c("darkblue","red"),
        legend = rownames(booking_type),beside=TRUE )


#User information
data$user_id = as.character(data$user_id)
#Customer statistics - takes a lot to complete
data$user_first_time = NA
data$user_past_rides = NA
data$user_past_cancellations = NA
data$user_past_distance = NA
for(i in 1:nrow(data)) {
  hist_user_count = which(data$user_id == data$user_id[i] & data$from_date < data$from_date[i])

  data$user_past_rides[i] = length(hist_user_count)
  data$user_past_cancellations[i] =  length(which(data$Car_Cancellation[hist_user_count] == 1))
  data$user_past_distance[i] =  mean(data$distance[hist_user_count], na.rm = T)
  ifelse(length(hist_user_count) > 0, data$user_first_time[i] <- 0, data$user_first_time[i] <- 1)
}


#Adding more columns to try to improve models predictions
data$travel_duration <- NA

# Converting duration to minutes
data$travel_duration <- data$travel_duration / 60
data$travel_duration <- round(data$travel_duration, digits = 0)

#Models 
# Create random data with similar cancellations features
data.cancellation <- data[data$Car_Cancellation == 'Yes', ]
data.nocancellation <- data[data$Car_Cancellation == 'No', ]

data.nocancellationRandom = sample_n(data.nocancellation, dim(data.cancellation)[1])

data.random <- rbind(data.nocancellationRandom, data.cancellation)

# Fill data random with values for distance and duration
for(i in 1:nrow(data.random)) {
  origin = gsub(" ", "", toString(c(data.random$from_lat[i],data.random$from_long[i])))
  destination = gsub(" ", "", toString(c(data.random$to_lat[i],data.random$to_long[i])))
  xml.url <- paste0('https://maps.googleapis.com/maps/api/distancematrix/xml?key=AIzaSyAvvElHcqHolDQM9zDweLyXtrdeIZEhRnE&origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  print(xmlfile)
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  dura <- xmlValue(xmlChildren(xpathApply(xmlfile,"//duration")[[1]])$value)
  print(dura)
  print(i)
  distance <- as.numeric(sub(" km","",dist))
  duration <- as.numeric(sub(" mins","", dura))
  data.random$distance[i] <- distance
  data.random$travel_duration[i] <- duration
}

# Converting duration to minutes
data.random$travel_duration <- data.random$travel_duration / 60
data.random$travel_duration <- round(data.random$travel_duration, digits = 0)

smp_size <- floor(0.75 * nrow(data.random ))
set.seed(123)
train_ind <- sample(seq_len(nrow(data.random )), size = smp_size)

train <- data.random [train_ind, ]
test <- data.random [-train_ind, ]

data.cancellation <- train[train$Car_Cancellation == 'Yes', ]

# CARET SVM LINEAR
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(123)
svm_Linear <- caret::train(Car_Cancellation ~ waitingTime + distance 
                    + booking_day 
                    + travel_type_id
                    + booking_month
                    + period
                    + booking_type
                    + from_lat 
                    + from_long 
                    + to_lat 
                    + to_long
                    + first_time_customer
                    + user_past_rides
                    + from_area_id
                    + to_area_id
                    + travel_duration,
                    data = train,
                    method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

test_pred <- predict(svm_Linear, newdata = test, na.action = na.pass)

confusionMatrix(table(test_pred, test$Car_Cancellation))


# Random Forest example
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

set.seed(754)

factor_colums <- c("Car_Cancellation", "booking_type", "user_id", "vehicle_model_id",
                   "travel_type_id", "package_id",
                   "from_city_id","to_city_id", "period", "booking_day","booking_month")

# Changing type if each columns 
for (col in factor_colums) {
  test[[col]] = as.factor(test[[col]])
  train[[col]] = as.factor(train[[col]])
}

new_test <- rbind(test, train)

smp_size <- floor(0.75 * nrow(new_test))
set.seed(123)
train_ind <- sample(seq_len(nrow(new_test )), size = smp_size)
train <- new_test [train_ind, ]
test <- new_test [-train_ind, ]

rf_model <- randomForest(Car_Cancellation ~ waitingTime + distance
                         + vehicle_model_id
                         + booking_day
                         + travel_type_id
                         + booking_month
                         + period
                         + booking_type
                         + from_lat 
                         + from_long 
                         + to_lat 
                         + to_long
                         + first_time_customer
                         + user_past_rides
                         + from_area_id
                         + to_area_id
                         + travel_duration,
                         data = train)


# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

prediction <- predict(rf_model, test)

confusionMatrix(table(prediction, test$Car_Cancellation))


# RPART Method
rpar_model <- rpart(Car_Cancellation ~ waitingTime + distance
             + vehicle_model_id
             + booking_day
             + travel_type_id
             + booking_month
             + period
             + booking_type
             + from_lat 
             + from_long 
             + to_lat 
             + to_long
             + first_time_customer
             + user_past_rides
             + from_area_id
             + to_area_id
             + travel_duration, 
             data = train, parms = list(split = 'gini'), method="class", control = rpart.control(minbucket = 3))

print(rpar_model)
rpart.plot(rpar_model)

prediction_rpar <- predict(rpar_model, test, type="class")
confusionMatrix(table(prediction_rpar, test$Car_Cancellation))

importance_vars <- rpar_model$variable.importance
par(mar=c(3, 8, 5, 1))
barplot(importance_vars, horiz=TRUE, col = rainbow(20),ylim=c(0,10),
        xlim=c(0,100),las=2, beside=FALSE )

write.csv(data, file = "~/Desktop/MyData.csv")

