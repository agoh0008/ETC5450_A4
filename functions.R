library(readr)
library(ggplot2)
library(dplyr) 
library(mgcv)

# Read data from a CSV file

get_data <- function(file) {
  read_csv(file)
}

# Check for missing values:
check_missing_values <- function(data) {
  missing <- sapply(data, function(x) sum(is.na(x)))
  if (any(missing)) {
    print("Missing values found:")
    print(missing[missing > 0])
  } else {
    print("No missing values found.")
  }
}

# Check data type:
check_data_types <- function(data) {
  types <- sapply(data, class)
  print("Data types:")
  print(types)
}

# Plot average life expectancy: 

plot_life_expectancy <- function(data) {
  # Aggregate data by year and calculate average life expectancy
  agg_data <- data %>%
    group_by(Year) %>%
    summarize(avg_ex = mean(ex))
  
  # Plot aggregated data
  plot <- ggplot(data = agg_data, aes(x = Year, y = avg_ex)) +
    geom_point() +   
    geom_smooth(method = "loess", se = FALSE) +  
    labs(x = "Year", y = "Average Life Expectancy (years)") +   
    ggtitle("Average Life Expectancy in Australia (1921-2020)") +  
    theme_minimal()
  
  return(plot)
}


# Group age into broader categories
group_age_into_bands <- function(age) {
  if (age >= 0 && age <= 1) {
    return("Infants")
  } else if (age >= 2 && age <= 12) {
    return("Children")
  } else if (age >= 13 && age <= 19) {
    return("Teenagers")
  } else if (age >= 20 && age <= 59) {
    return("Adults")
  } else {
    return("Seniors")
  }
}

# Plot average mortality rate:
plot_mortality_rate <- function(data) {
  # Aggregate data into averages
  raw_data_avg <- data %>%
    mutate(Age_Band = sapply(Age, group_age_into_bands)) %>%
    group_by(Year, Age_Band) %>%
    summarise(avg_mx = mean(mx))
  
  # Convert Age_Band to factor with the desired order
  raw_data_avg$Age_Band <- factor(raw_data_avg$Age_Band, 
                                  levels = c("Infants", "Children", "Teenagers", "Adults", "Seniors"))
  
  color_palette <- c("Infants" = "blue", "Children" = "orange", "Teenagers" = "#029E73",
                     "Adults" = "red", "Seniors" = "purple")
  
  age_group_ranges <- c("Infants (0-1)", "Children (2-12)", "Teenagers (13-19)", "Adults (20-59)", "Seniors (60+)")
  
  # Plot mortality rate against year for each age band separately 
  plot <- ggplot(data = raw_data_avg, aes(x = Year, y = avg_mx, color = Age_Band)) +
    geom_line() +
    facet_wrap(~Age_Band, scales = "free_y") +
    labs(title = "Average Mortality Rates in Australia by Age Band (1921-2020)",
         x = "Decade",
         y = "Average Mortality Rate") +
    theme_minimal() +
    scale_color_manual(name = "Age Band",  
                       values = color_palette, 
                       labels = age_group_ranges)
  
  return(plot)
}

# Fit GAM model 

fit_gam_summary <- function(data) {
  gam_model <- gam(mx ~ s(Age), data = data, family = poisson)
  summary_gam <- summary(gam_model)
  return(summary_gam)
}

save_gam_plot <- function(data, plot_path) {
  gam_model <- gam(mx ~ s(Age), data = data, family = poisson)
  png(plot_path)
  plot(gam_model)
  dev.off()
  return(plot_path)
}
