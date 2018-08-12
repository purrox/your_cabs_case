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
                   "from_city_id","to_city_id", "period")

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

plot_missing(data) # A lot of missing values  

geo_colums <- c("from_lat", "from_long", "to_lat", "to_long")

for (col in geo_colums) {
  data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm=TRUE), 
                        data[[col]])                                                      
}

f <- function(x){
  a <- x["from_long"]
  b <- x["from_lat"]
  c <- x["to_long"]
  d <- x["to_lat"]
  print(c(a,b))
  print(c(c,d))
  distm( c(a,b), c(c,d), fun = distHaversine)
}
data$distance <- apply( data, 1,  f  )
  
# Missing Data plot
plot_missing(data)  
  
  
  
  