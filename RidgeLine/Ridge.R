# Install and load necessary packages
install.packages("ggridges")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")

library(ggridges)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the dataset (replace with your actual file path)
data <- read.csv("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/State-Level-CPI-Rural-Urban-February-2024/Cleaned_data.csv")

# Step 1: Subset the data for the Rural+Urban sector
rural_urban_data <- subset(data, Sector == "Rural+Urban")

# Step 2: Identify the columns that contain CPI data (adjust the column range based on your dataset)
cpi_columns <- colnames(rural_urban_data)[4:ncol(rural_urban_data)]

# Step 3: Reshape data to a long format using pivot_longer for CPI values
melted_data <- rural_urban_data %>%
  pivot_longer(cols = all_of(cpi_columns), names_to = "State", values_to = "CPI")

# Step 4: Convert month names to numbers and create a Date column for easier handling
melted_data$Month <- match(melted_data$Name, month.name)
melted_data$Date <- as.Date(paste(melted_data$Year, melted_data$Month, "01", sep = "-"))

# Step 5: Create the ridgeline plot
ggplot(melted_data, aes(x = CPI, y = State, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "Ridgeline Plot of State-wise CPI Distributions (Rural+Urban Sector)",
       x = "CPI", y = "State") +
  theme_minimal()
