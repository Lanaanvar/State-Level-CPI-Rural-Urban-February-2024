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
india_shp <- st_read("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/State-Level-CPI-Rural-Urban-February-2024/Indian Map/INDIA.shp")

# Transform CRS if needed
india_shp <- st_transform(india_shp, 4326)

# Plot to ensure it's working
plot(st_geometry(india_shp))

# Load your CPI dataset
cpi_data <- read.csv("C:/Users/ashfa/VScode/SEM5/R Programming/R_PROJECT/State-Level-CPI-Rural-Urban-February-2024/Cleaned_data.csv")

# Pivot the data for easier handling (if needed)
cpi_long <- cpi_data %>%
  pivot_longer(cols = starts_with("Andhra.Pradesh"):ends_with("Puducherry"), names_to = "State", values_to = "CPI")

# Group by Sector and State to calculate the overall average CPI across all years
cpi_overall_avg <- cpi_long %>%
  group_by(Sector, State) %>%
  summarize(average_CPI = mean(CPI, na.rm = TRUE)) %>%
  ungroup()

# Standardize the state names in the CPI dataset to match the shapefile
cpi_overall_avg$State <- recode(cpi_overall_avg$State,
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

# Merge overall CPI data with the shapefile
india_map <- india_shp %>%
  left_join(cpi_overall_avg, by = c("ST_NAME" = "State"))

# Function to plot CPI map by sector (Rural, Urban, or Rural+Urban)
plot_overall_cpi_map <- function(sector) {
  # Filter CPI data for the selected sector
  cpi_sector <- cpi_overall_avg %>%
    filter(Sector == sector)
  
  # Merge CPI data for that sector with the shapefile
  india_map <- india_shp %>%
    left_join(cpi_sector, by = c("ST_NAME" = "State"))
  
  # Plot the map
  ggplot(data = india_map) +
    geom_sf(aes(fill = average_CPI)) +
    scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey50") +
    labs(title = paste(sector, "CPI Across Indian States (Overall)"),
         fill = "Average CPI") +
    theme_minimal() +
    theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())
}

# Plot the CPI map for each sector
plot_overall_cpi_map("Rural")
plot_overall_cpi_map("Urban")
plot_overall_cpi_map("Rural+Urban")

# Save each map
sectors <- unique(cpi_overall_avg$Sector)
for (sector in sectors) {
  plot <- plot_overall_cpi_map(sector)
  ggsave(filename = paste0("CPI_Map_", sector, "_Overall.png"), plot = plot, width = 8, height = 6)
}
