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

#rm(Statewise_General_Index_Upto_Feb24)
