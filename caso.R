# This code is created by Carlos Salas and Frarlon Rodriguez 
# for the the Avantica Machine Learning course 2018

# Load Libreries
library(DataExplorer)
library(readxl)
library(dplyr)

# Import Data
data <- read_excel("Downloads/Taxi-cancellation-case.xlsx")
View(data)

# Summary Data
summary(data)

# Histograms
plot_bar(data)

# Columns with date values in unix format
date_colums <- c("from_date", "to_date", "booking_created")

# Formating values if each columns 
for (col in date_colums) {
  data[[col]] = as.POSIXct(as.Date(as.numeric(data[[col]]), origin="1899-12-30"))
}

# Missing Data plot
plot_missing(data)

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
                   "from_city_id","to_city_id")

# Changing type if each columns 
for (col in factor_colums) {
  data[[col]] = as.factor(data[[col]])
}

# Summary Data
summary(data)





