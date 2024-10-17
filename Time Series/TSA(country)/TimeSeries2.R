# Install necessary packages if not already installed
install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")

# Load required libraries
library(ggplot2)
library(forecast)
library(tseries)
library(dplyr)

# Load the dataset
data <- read.csv("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/Cleaned_data.csv")

# Step 1: Subset the data for the Rural+Urban sector
rural_urban_data <- subset(data, Sector == "Rural+Urban")

# Step 2: Calculate the mean CPI for all states for each row (each month and year)
# Select only columns that correspond to CPI values of states (assuming they start from column 4)
state_columns <- colnames(rural_urban_data)[4:ncol(rural_urban_data)]

# Calculate the row-wise mean of all states to get the mean CPI for the whole country
rural_urban_data$Country_Mean_CPI <- rowMeans(rural_urban_data[, state_columns], na.rm = TRUE)

# Convert the month names into numeric format for easier handling
rural_urban_data$Month <- match(rural_urban_data$Name, month.name)

# Create a Date column to represent year and month
rural_urban_data$Date <- as.Date(paste(rural_urban_data$Year, rural_urban_data$Month, "01", sep = "-"))

# Step 3: Create a time series object for the whole country CPI data
country_ts <- ts(rural_urban_data$Country_Mean_CPI, start = c(2011, 1), frequency = 12)

# Step 4: Plot the Time Series for the entire country
plot.ts(country_ts, main = "Mean CPI Time Series for India (Rural+Urban)", ylab = "CPI", xlab = "Year")

# Step 5: Decompose the Time Series (into trend, seasonality, and noise)
country_decomp <- decompose(country_ts)
plot(country_decomp)

# Step 6: Check for stationarity using Augmented Dickey-Fuller test
adf_test <- adf.test(country_ts)
print(adf_test)

# Step 7: Fit ARIMA model (if necessary) based on ACF and PACF plots
auto_fit_country <- auto.arima(country_ts)
summary(auto_fit_country)

# Step 8: Forecast future values (for the next 12 months)
forecasted_values_country <- forecast(auto_fit_country, h = 12)
plot(forecasted_values_country)

# Step 9: Plotting the CPI for the whole country over time
ggplot(rural_urban_data, aes(x = Date, y = Country_Mean_CPI)) +
  geom_line(color = "blue") +
  labs(title = "Mean CPI for India (Rural+Urban) Over Time", x = "Year", y = "Mean CPI") +
  theme_minimal()
