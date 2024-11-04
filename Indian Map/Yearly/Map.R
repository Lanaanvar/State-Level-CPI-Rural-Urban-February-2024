# Install the necessary packages
install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("viridis")
install.packages("tidyr")

# Load the libraries
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

# Read the local shapefile
india_shp <- st_read("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/INDIA.shp")

# Transform CRS if needed
india_shp <- st_transform(india_shp, 4326)

# Plot to ensure it's working
plot(st_geometry(india_shp))

# Load your CPI dataset
cpi_data <- read.csv("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/Cleaned_data.csv")

# Pivot the data for easier handling (if needed)
cpi_long <- cpi_data %>%
  pivot_longer(cols = starts_with("Andhra.Pradesh"):ends_with("Puducherry"), names_to = "State", values_to = "CPI")

# Group by Year, Sector, and State to calculate the yearly average CPI
cpi_yearly_avg <- cpi_long %>%
  group_by(Year, Sector, State) %>%
  summarize(average_CPI = mean(CPI, na.rm = TRUE)) %>%
  ungroup()

# Check the state names in the shapefile
unique(india_shp$ST_NAME)

# Standardize the state names in the CPI dataset to match the shapefile
cpi_yearly_avg$State <- recode(cpi_yearly_avg$State,
                               "Andaman.and.Nicobar" = "ANDAMAN AND NICOBAR ISLANDS",
                               "Arunachal.Pradesh" = "Arunachal Pradesh",
                               "Andhra.Pradesh" = "Andhra Pradesh",
                               "Chattisgarh" = "Chhattisgarh",
                               "Delhi" = "NCT of Delhi",
                               "Dadra and Nagar Haveli" = "DADRA AND NAGAR HAVELI",
                               "Goa" = "Goa",
                               "Gujarat" = "Gujarat",
                               "Haryana" = "Haryana",
                               "Himachal.Pradesh" = "Himachal Pradesh",
                               "Jammu.and.Kashmir" = "Jammu And Kashmir",
                               "Jharkhand" = "Jharkhand",
                               "Karnataka" = "Karnataka",
                               "Kerala" = "Kerala",
                               "Madhya.Pradesh" = "Madhya Pradesh",
                               "Maharashtra" = "Maharashtra",
                               "Manipur" = "Manipur",
                               "Meghalaya" = "Meghalaya",
                               "Mizoram" = "Mizoram",
                               "Nagaland" = "Nagaland",
                               "Orissa" = "Orissa",
                               "Puducherry" = "Puducherry",
                               "Punjab" = "Punjab",
                               "Rajasthan" = "Rajasthan",
                               "Sikkim" = "Sikkim",
                               "Tamil.Nadu" = "Tamil Nadu",
                               "Telangana" = "Telangana",
                               "Tripura" = "Tripura",
                               "Uttar.Pradesh" = "Uttar Pradesh",
                               "Uttarakhand" = "Uttarakhand",
                               "West.Bengal" = "West Bengal")

# Merge CPI data with the shapefile
india_map <- india_shp %>%
  left_join(cpi_yearly_avg, by = c("ST_NAME" = "State"))

# Function to plot CPI map by year and sector
plot_cpi_map <- function(year, sector) {
  # Filter CPI data for the selected year and sector
  cpi_year_sector <- cpi_yearly_avg %>%
    filter(Year == year & Sector == sector)
  
  # Merge CPI data for that year and sector with the shapefile
  india_map <- india_shp %>%
    left_join(cpi_year_sector, by = c("ST_NAME" = "State"))
  
  # Plot the map
  ggplot(data = india_map) +
    geom_sf(aes(fill = average_CPI)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
    labs(title = paste(sector, "CPI Across Indian States in", year),
         fill = "Average CPI") +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
}

# Example: Plot the CPI map for Rural sector in 2020
plot_cpi_map(2020, "Rural")

# Define the unique years and sectors
years <- unique(cpi_yearly_avg$Year)
sectors <- unique(cpi_yearly_avg$Sector)

# Generate and save maps for each year and sector
for (year in years) {
  for (sector in sectors) {
    plot <- plot_cpi_map(year, sector)
    ggsave(filename = paste0("CPI_Map_", sector, "_", year, ".png"), plot = plot, width = 8, height = 6)
  }
}

india_shp$ST_NAME
