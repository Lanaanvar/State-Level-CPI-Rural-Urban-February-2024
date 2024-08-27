install.packages("tidyverse")
install.packages("dplyr")
library(tidyverse)
view(Statewise_General_Index_Upto_Feb24)

summary(Statewise_General_Index_Upto_Feb24)
class(Statewise_General_Index_Upto_Feb24$Himachal.Pradesh)

names(Statewise_General_Index_Upto_Feb24)

library(dplyr)
Statewise_General_Index_Upto_Feb24 <- Statewise_General_Index_Upto_Feb24 %>% select(-Delhi, -Bihar, -Rajasthan, -Nagaland, -Sikkim, -Telangana, -Uttarakhand, -Arunachal.Pradesh)

# Ensure row 371 exists
# Check if the maximum row number in the vector exists in the data frame
if (max(c(364, 365, 366, 367, 368, 369, 370, 371, 372, 373)) <= nrow(Statewise_General_Index_Upto_Feb24)) {
  # Remove specified rows
  Statewise_General_Index_Upto_Feb24 <- Statewise_General_Index_Upto_Feb24[-c(364, 365, 366, 367, 368, 369, 370, 371, 372), ]
} else {
  cat("Some of the specified rows do not exist in the dataset.\n")
}


# View the updated dataset
View(Statewise_General_Index_Upto_Feb24)
summary(Statewise_General_Index_Upto_Feb24)
glimpse(Statewise_General_Index_Upto_Feb24)


# For Interpolating NA values 
install.packages("zoo")
library(zoo)
Statewise_General_Index_Upto_Feb24$West.Bengal <- na.approx(Statewise_General_Index_Upto_Feb24$West.Bengal)
Statewise_General_Index_Upto_Feb24$Andhra.Pradesh <- na.approx(Statewise_General_Index_Upto_Feb24$Andhra.Pradesh)
Statewise_General_Index_Upto_Feb24$Tripura <- na.approx(Statewise_General_Index_Upto_Feb24$Tripura)
Statewise_General_Index_Upto_Feb24$Himachal.Pradesh <- na.approx(Statewise_General_Index_Upto_Feb24$Himachal.Pradesh)

# Check for NA values
any(is.na(Statewise_General_Index_Upto_Feb24))
sum(is.na(Statewise_General_Index_Upto_Feb24))
colSums(is.na(Statewise_General_Index_Upto_Feb24))

#rm(Statewise_General_Index_Upto_Feb24)
