#load libraries
library(readr)
library(tidyverse)
library(dplyr)
library(mice)
library(forcats) 

#import data
training <- read_csv("training.csv")
labels <- read_csv("labels.csv")
main_test <- read_csv("test.csv")

#merge data with training labels
main_data <- merge(training,labels, by = "id")
clean_data <- main_data
colnames(clean_data)

#check for missing data
md.pattern(clean_data)
colSums(is.na(clean_data))
sum(with(clean_data, construction_year == 0))
sum(with(clean_data, longitude == 0))
sum(with(clean_data, latitude == 0))

#drop variables which are similar or not important
clean_data$wpt_name <- NULL
clean_data$num_private <- NULL
clean_data$region_code <- NULL
clean_data$recorded_by <- NULL
clean_data$subvillage <- NULL
clean_data$lga <- NULL
clean_data$scheme_name <- NULL
clean_data$extraction_type_group <- NULL
clean_data$extraction_type_class <- NULL
clean_data$payment <- NULL
clean_data$quality_group <- NULL
clean_data$quantity_group <- NULL
clean_data$source_type<- NULL
clean_data$source_class <- NULL
clean_data$waterpoint_type_group <- NULL
clean_data$gps_height <- NULL
clean_data$management_group <- NULL
clean_data$district_code <- NULL
clean_data$funder <- NULL

glimpse(clean_data)

#impute for missing data and assign it to permit and public meeting
imp <- mice(clean_data, method = "cart", m=1)
imputed <- mice::complete(imp)
clean_data$permit <- imputed$permit
clean_data$public_meeting <- imputed$public_meeting

#assign missing values as others

clean_data$installer[is.na(clean_data$installer)] <- "others"
clean_data$scheme_management[is.na(clean_data$scheme_management)] <- "other"

#replace values which are 0 with the mean
clean_data$longitude[clean_data$longitude == 0] <- NA
clean_data$longitude[is.na(clean_data$longitude)] <- round(mean(clean_data$longitude, na.rm = TRUE))

clean_data$latitude[clean_data$latitude == -0.00000002] <- NA
clean_data$latitude[is.na(clean_data$latitude)] <- round(mean(clean_data$latitude, na.rm = TRUE))

#convert all character variables to factors and assign them a numeric value
clean_data$basin <- as.factor(clean_data$basin)
clean_data$basin <- unclass(clean_data$basin)
print(clean_data$basin)


clean_data$region <- as.factor(clean_data$region)
clean_data$region <- unclass(clean_data$region)

clean_data$installer <- as.factor(clean_data$installer)
clean_data$installer <- unclass(clean_data$installer)

clean_data$scheme_management <- as.factor(clean_data$scheme_management)
clean_data$scheme_management <- unclass(clean_data$scheme_management)

clean_data$permit <- as.factor(clean_data$permit)
clean_data$permit <- unclass(clean_data$permit)

clean_data$extraction_type <- as.factor(clean_data$extraction_type)
clean_data$extraction_type <- unclass(clean_data$extraction_type)

clean_data$management <- as.factor(clean_data$management)
clean_data$management <- unclass(clean_data$management)

clean_data$payment_type <- as.factor(clean_data$payment_type)
clean_data$payment_type <- unclass(clean_data$payment_type)

clean_data$water_quality <- as.factor(clean_data$water_quality)
clean_data$water_quality <- unclass(clean_data$water_quality)

clean_data$quantity <- as.factor(clean_data$quantity)
clean_data$quantity <- unclass(clean_data$quantity)

clean_data$source <- as.factor(clean_data$source)
clean_data$source <- unclass(clean_data$source)

clean_data$waterpoint_type <- as.factor(clean_data$waterpoint_type)
clean_data$waterpoint_type <- unclass(clean_data$waterpoint_type)

clean_data$status_group <- as.factor(clean_data$status_group)
clean_data$status_group <- unclass(clean_data$status_group)

clean_data$ward <- as.factor(clean_data$ward)
clean_data$ward <- unclass(clean_data$ward)

clean_data$public_meeting <- as.factor(clean_data$public_meeting)
clean_data$public_meeting <- unclass(clean_data$public_meeting)

#Extract year from year recorded
clean_data$year_recorded <- format(clean_data$date_recorded, "%Y")
as.double(clean_data$year_recorded)

#remove date recorded
clean_data$date_recorded <- NULL

#Create new dataset with construction year not zero and status group only opearational and functional needs repair
main_clean <- clean_data %>% filter(construction_year != 0) %>% filter(status_group == 3)

glimpse(main_clean)
main_clean$X3 <- NULL
main_clean$X4 <- NULL
main_clean$X5 <- NULL
main_clean$X6 <- NULL
main_clean$X7 <- NULL
main_clean$X8 <- NULL

write.csv(main_clean, file = "cleaned_train.csv")



