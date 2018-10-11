######################
#
#  Visualization Project
#  Fall 2
#  Orange Team 1
#
######################

library(plyr)
library(zoo)

# All well date/times corrected to UTC in Excel

# Read in all well file corrected datasets
setwd("/Users/matttrombley/Documents/GitHub/Visualization-/")
well_F_45 <- read.csv("F-45.csv")
well_F_179 <- read.csv("F-179.csv")
well_F_319 <- read.csv("F-319.csv")
well_G_561_T <- read.csv("G-561_T.csv")
well_G_580A <- read.csv("G-580A.csv")
well_G_852 <- read.csv("G-852.csv") # Has 31 row of random data - remove?
well_G_852 <- well_G_852[32:nrow(well_G_852),]
well_G_860 <- read.csv("G-860.csv")
well_G_1220_T <- read.csv("G-1220_T.csv")
well_G_1260_T <- read.csv("G-1260_T.csv")
well_G_2147_T <- read.csv("G-2147_T.csv")
well_G_2866_T <- read.csv("G-2866_T.csv")
well_G_3549 <- read.csv("G-3549.csv")
well_PB_1680_T <- read.csv("PB-1680_T.csv")

# Create list of well names to loop over
well_names <- list('well_F_45','well_F_179','well_F_319','well_G_561_T','well_G_580A','well_G_852','well_G_860','well_G_1220_T','well_G_1260_T','well_G_2147_T','well_G_2866_T','well_G_3549','well_PB_1680_T')

# Create loop to clean well data for all 13 wells
for (i in 1:13) {

   # Assign current well data to variables
   well_data <- eval(parse(text=well_names[[i]]))
   out_name <- paste(well_names[[i]],'_imputed.csv',sep='')

   # Remove values having code "P"
   well_data <- subset(well_data, Code == "A")

   # Create a time element that is hourly to aggregate on
   well_data$UTC.Hour <- as.character(well_data$UTC.Hour)
   well_data$date_hour <- paste(paste(well_data$UTC.Date,substr(well_data$UTC.Hour,1,nchar(well_data$UTC.Hour)-3),sep=" "),":00",sep="")

   # Aggregate the corrected well height data to hourly
   well_agg <- aggregate(well_data$Corrected, list(well_data$date_hour), mean)

   # Clean up formatting of dates, times, and row/column names
   well_agg$Group.1 <- as.POSIXct(strptime(well_agg$Group.1, "%m/%d/%y %H:%M"), tz="UTC")
   well_agg <- well_agg[order(well_agg$Group.1),]
   rownames(well_agg) <- 1:nrow(well_agg)
   colnames(well_agg) <- c("Date_Time", "Avg_Corrected_Well_Height")

   # Check to see if there are missing values
   x1 <- as.POSIXct(well_agg[1,1],tz='UTC')
   x2 <- as.POSIXct(well_agg[nrow(well_agg),1],tz='UTC')
   full_date_time <- as.data.frame(seq(from=x1,to=x2,by="hour"))
   nrow(full_date_time) - nrow(well_agg)
   colnames(full_date_time) <- c("Date_Time")

   # Create full data set including missing times
   well_imputed <- join(full_date_time,well_agg, by = "Date_Time", type = "left")
   sum(is.na(well_imputed$Avg_Corrected_Well_Height))

   # Impute missing values with desired measure
   well_imputed$Avg_Corrected_Well_Height <- na.approx(well_imputed$Avg_Corrected_Well_Height)

   # Check to make sure there are no more missing values
   sum(is.na(well_imputed$Avg_Corrected_Well_Height))

   # Write the set out to a CSV for use in other visualization programs
   write.csv(well_imputed, file = paste(out_name), row.names = TRUE)

   # Print out progress indicator
   cat("Step ", i, " completed. Cleaned ", well_names[[i]], " data written as csv.\n", sep="")
}
