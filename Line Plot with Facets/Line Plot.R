library(ggplot2)
library(dplyr)
library(tidyr)

df <- Cleaned_data

df$Year <- as.numeric(df$Year)

create_plot <- function(sector_name) {
  sector_data <- df %>% filter(Sector == sector_name)
  
  sector_data_means <- sector_data %>%
    group_by(Year) %>%
    summarize(across(Andhra.Pradesh:Puducherry, ~mean(.x, na.rm = TRUE)))
  
  sector_long <- sector_data_means %>%
    pivot_longer(cols = Andhra.Pradesh:Puducherry, names_to = "state", values_to = "mean_value")
  
  ggplot(sector_long, aes(x = Year, y = mean_value, color = state)) +
    geom_line() +
    facet_wrap(~ state, scales = "free_y") +
    labs(title = paste("Trends over Time for", sector_name, "Sector"),
         x = "Year", y = "Mean Value") +
    theme_minimal()
  
}


plot_rural <- create_plot("Rural")
plot_urban <- create_plot("Urban")
plot_rural_urban <- create_plot("Rural+Urban")

plot_rural
plot_urban
plot_rural_urban