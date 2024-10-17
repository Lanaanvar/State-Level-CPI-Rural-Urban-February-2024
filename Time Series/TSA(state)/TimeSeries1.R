# Install necessary packages if not already installed
install.packages("ggplot2")
install.packages("forecast")
install.packages("tseries")

# Load required libraries
library(ggplot2)
library(forecast)
library(tseries)

# Load the dataset
data <- read.csv("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/Cleaned_data.csv")

# Step 1: Convert to Time Series for Andhra Pradesh (for example)
# Subset Andhra Pradesh data for Rural+Urban sector
andhra_pradesh_data <- subset(data, Sector == "Rural+Urban")[, c("Year", "Name", "Andhra.Pradesh")]

# Convert the month names into numeric format for easier handling
andhra_pradesh_data$Month <- match(andhra_pradesh_data$Name, month.name)

# Create a Date column to represent year and month
andhra_pradesh_data$Date <- as.Date(paste(andhra_pradesh_data$Year, andhra_pradesh_data$Month, "01", sep = "-"))

# Step 2: Create a time series object for Andhra Pradesh CPI data
andhra_ts <- ts(andhra_pradesh_data$Andhra.Pradesh, start = c(2011, 1), frequency = 12)

# Step 3: Plot the Time Series
plot.ts(andhra_ts, main = "CPI Time Series for Andhra Pradesh", ylab = "CPI", xlab = "Year")

# Step 4: Decompose the Time Series (into trend, seasonality, and noise)
andhra_decomp <- decompose(andhra_ts)
plot(andhra_decomp)

# Step 5: Check for stationarity using Augmented Dickey-Fuller test
adf_test <- adf.test(andhra_ts)
print(adf_test)

# Step 6: Fit ARIMA model (if necessary) based on ACF and PACF plots
auto_fit <- auto.arima(andhra_ts)
summary(auto_fit)

# Step 7: Forecast future values (for the next 12 months)
forecasted_values <- forecast(auto_fit, h = 12)
plot(forecasted_values)

# Step 8: Plotting the CPI for all years and months for Andhra Pradesh
ggplot(andhra_pradesh_data, aes(x = Date, y = Andhra.Pradesh)) +
  geom_line(color = "blue") +
  labs(title = "CPI for Andhra Pradesh Over Time", x = "Year", y = "CPI") +
  theme_minimal()
