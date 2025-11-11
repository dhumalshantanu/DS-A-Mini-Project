# 1.1 Load the Data
library(readxl)
sbi_data <- read_excel("SBI Dataset.xlsx")
print(sbi_data)

#Getting the data types of all the columns present in the dataset : 
str(sbi_data)

#Converting the datatype of the date column : 
sbi_data$Date <- ymd(sbi_data$Date)
str(sbi_data)

mean()

#Calculating the missing Values present in the datset : 
missing_count_summary <- sbi_data %>%
  summarise(
    # The 'across(everything(), ...)' applies the function to all columns.
    # 'sum(is.na(.))' counts the TRUE values (missing values) in each column.
    across(everything(), ~sum(is.na(.)), .names = "Missing_{.col}")
  )

# Print the resulting one-row data frame
print(missing_count_summary)

# --- IMPUTATION STEP (Handling NAs) ---

# Apply Last Observation Carried Forward (LOCF) to all financial columns.
# The '.direction = "down"' tells R to carry the last observed value downward.
sbi_data_clean <- sbi_data %>%
  fill(Open, High, Low, Close, `Adj Close`, Volume, .direction = "down")

# 3. Verify that missing values are gone
# This code confirms the count of NAs after the imputation
sbi_data_clean %>%
  summarise(across(everything(), ~sum(is.na(.))))

# 1. Ensure you are using the clean data frame
# (sbi_data_clean from the previous step)
# You will need the tidyverse package loaded.

# 2. Calculate the Log Returns (ln(Price_t / Price_{t-1}))
sbi_data_analysis <- sbi_data_clean %>%
  arrange(Date) %>% # Ensure data is ordered by date
  mutate(
    # Use the lag() function to get the previous day's adjusted close price
    Adj_Close_Lag = lag(`Adj Close`),
    # Calculate Log Returns
    Log_Returns = log(`Adj Close` / Adj_Close_Lag)
  ) %>%
  # Remove the first row, as it will have an NA for Log_Returns (no prior price)
  filter(!is.na(Log_Returns))

# Check the first few rows of the new column
head(sbi_data_analysis$Log_Returns)

library(ggplot2)

# Plot the Adjusted Close Price over time
price_plot <- ggplot(sbi_data_analysis, aes(x = Date, y = `Adj Close`)) +
  geom_line(color = "blue") +
  labs(
    title = "SBI Stock Adjusted Close Price Over Time",
    x = "Year",
    y = "Adjusted Close Price (INR)"
  ) +
  theme_minimal()

print(price_plot)

# Plot the Volume over time
volume_plot <- ggplot(sbi_data_analysis, aes(x = Date, y = Volume)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "SBI Stock Trading Volume Over Time",
    x = "Year",
    y = "Volume Traded"
  ) +
  theme_minimal()

print(volume_plot)

# Plot the distribution of Log Returns
returns_distribution <- ggplot(sbi_data_analysis, aes(x = Log_Returns)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "orange", color = "black") +
  geom_density(color = "red", linewidth = 1) +
  labs(
    title = "Distribution of Daily Log Returns",
    x = "Daily Log Returns",
    y = "Density"
  ) +
  theme_minimal()

print(returns_distribution)

# Calculate key statistics for Log Returns (skewness and kurtosis)
# You might need the 'moments' package for this
# install.packages("moments")
library(moments)

summary_stats <- list(
  Mean = mean(sbi_data_analysis$Log_Returns),
  Std_Dev = sd(sbi_data_analysis$Log_Returns),
  Skewness = skewness(sbi_data_analysis$Log_Returns),
  Kurtosis = kurtosis(sbi_data_analysis$Log_Returns)
)

print(summary_stats)
