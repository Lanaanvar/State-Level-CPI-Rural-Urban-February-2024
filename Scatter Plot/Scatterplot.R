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
  
  ggplot(sector_long, aes(x = state, y = Year, color = mean_value)) + 
    geom_point() +
    scale_color_gradient(low = "purple", high = "yellow") +
    scale_y_continuous(breaks = seq(min(sector_long$Year), max(sector_long$Year), by = 1))+
    labs(title = paste("Scatterplot for", sector_name, "Sector"),
         x = "State", y = "Year", color = "Mean Value") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


plot_rural <- create_plot("Rural")
plot_urban <- create_plot("Urban")
plot_rural_urban <- create_plot("Rural+Urban")

plot_rural
plot_urban
plot_rural_urban