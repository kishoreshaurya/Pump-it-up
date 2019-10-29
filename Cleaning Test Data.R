#Drop variavles from test file
main_test$wpt_name <- NULL
main_test$num_private <- NULL
main_test$region_code <- NULL
main_test$recorded_by <- NULL
main_test$subvillage <- NULL
main_test$date_recorded <- NULL
main_test$lga <- NULL
main_test$scheme_name <- NULL
main_test$extraction_type_group <- NULL
main_test$extraction_type_class <- NULL
main_test$payment <- NULL
main_test$quality_group <- NULL
main_test$quantity_group <- NULL
main_test$source_type<- NULL
main_test$source_class <- NULL
main_test$waterpoint_type_group <- NULL
main_test$gps_height <- NULL
main_test$management_group <- NULL
main_test$district_code <- NULL
main_test$funder <- NULL
main_test$construction_year <- NULL


main_test$basin <- as.factor(main_test$basin)
main_test$basin <- unclass(main_test$basin)
print(main_test$basin)

main_test$region <- as.factor(main_test$region)
main_test$region <- unclass(main_test$region)

main_test$installer <- as.factor(main_test$installer)
main_test$installer <- unclass(main_test$installer)

main_test$scheme_management <- as.factor(main_test$scheme_management)
main_test$scheme_management <- unclass(main_test$scheme_management)

main_test$permit <- as.factor(main_test$permit)
main_test$permit <- unclass(main_test$permit)

main_test$extraction_type <- as.factor(main_test$extraction_type)
main_test$extraction_type <- unclass(main_test$extraction_type)

main_test$management <- as.factor(main_test$management)
main_test$management <- unclass(main_test$management)

main_test$payment_type <- as.factor(main_test$payment_type)
main_test$payment_type <- unclass(main_test$payment_type)

main_test$water_quality <- as.factor(main_test$water_quality)
main_test$water_quality <- unclass(main_test$water_quality)

main_test$quantity <- as.factor(main_test$quantity)
main_test$quantity <- unclass(main_test$quantity)

main_test$source <- as.factor(main_test$source)
main_test$source <- unclass(main_test$source)

main_test$waterpoint_type <- as.factor(main_test$waterpoint_type)
main_test$waterpoint_type <- unclass(main_test$waterpoint_type)

main_test$ward <- as.factor(main_test$ward)
main_test$ward <- unclass(main_test$ward)

main_test$public_meeting <- as.factor(main_test$public_meeting)
main_test$public_meeting <- unclass(main_test$public_meeting)

#Check for missing values
md.pattern(main_test)
main_test$installer[is.na(main_test$installer)] <- "others"
main_test$scheme_management[is.na(main_test$scheme_management)] <- "other"
main_test$installer <- as.factor(main_test$installer)
main_test$installer <- unclass(main_test$installer)

main_test$scheme_management <- as.factor(main_test$scheme_management)
main_test$scheme_management <- unclass(main_test$scheme_management)

#Impute missing variables
imp_1 <- mice(main_test, method = "cart", m=1)
imputed <- mice::complete(imp_1)
main_test$permit <- imputed$permit
main_test$public_meeting <- imputed$public_meeting

#New column to predict time to failure for pump
main_test$time_failure <- ""
glimpse(main_test)

#Merge status group in test file
pump1 <- read_csv("pump1.csv", col_types = cols(status_group = col_character()))
main_test <- merge(main_test,pump1, by = "id")

#Unclass status group
main_test$status_group <- as.factor(main_test$status_group)
main_test$status_group <- unclass(main_test$status_group)

#filter for non operational
main_test <- main_test %>% filter(status_group == 3)
glimpse(main_test)

write.csv(main_test, file = "cleaned_test.csv")
