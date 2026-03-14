###########################################################
# FIN 435/635 – Fundamentals of Fintech
# Final Project 
# Name: Alec Raboin
###########################################################

# Load required libraries for data manipulation and plotting
library(tidyverse)
library(lubridate)

# Set working directory so R knows where the data files are located
setwd("C:/R Assignments/FinTech/R Assignments")

# Confirm working directory
getwd()

# Load datasets (Bitcoin prices, S&P 500 prices, and Fed announcement dates)
btc <- read.csv("BTCUSD_daily.csv")
sp <- read.csv("S&P500_daily.csv")
fomc <- read.csv("FOMC_Announcements_2018_2024.csv")

# Convert date columns from character to Date format
btc$Date <- as.Date(btc$Date)
sp$Date <- as.Date(sp$Date)
fomc$Date <- as.Date(fomc$Date)

# Robust date parsing in case date formats differ between files
# This ensures the dates are properly recognized by R
btc$Date <- suppressWarnings(ymd(btc$Date))
if (all(is.na(btc$Date))) btc$Date <- mdy(read.csv("BTCUSD_daily.csv")$Date)

sp$Date  <- suppressWarnings(ymd(sp$Date))
if (all(is.na(sp$Date))) sp$Date <- mdy(read.csv("SP500_daily.csv")$Date)

# Define the time window for the analysis (2018–2024)
start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2024-12-31")

# Filter both datasets to only include observations within this window
btc <- btc %>% filter(Date >= start_date & Date <= end_date) %>% arrange(Date)
sp  <- sp  %>% filter(Date >= start_date & Date <= end_date) %>% arrange(Date)

# Quick checks to ensure the filtering worked correctly
range(btc$Date)
range(sp$Date)

# Preview first rows of data
head(btc)
head(sp)

###########################################################
# Plot Bitcoin price over time
###########################################################

ggplot(btc, aes(x = Date, y = Close)) +
  geom_line() +
  labs(
    title = "Bitcoin Price (2018–2024)",
    x = "Date",
    y = "BTC Close Price (USD)"
  ) +
  theme_minimal()

###########################################################
# Plot S&P 500 price over time
###########################################################

ggplot(sp, aes(x = Date, y = Close)) +
  geom_line() +
  labs(
    title = "S&P 500 Price (2018–2024)",
    x = "Date",
    y = "S&P 500 Close"
  ) +
  theme_minimal()

###########################################################
# Prepare Federal Reserve announcement data
###########################################################

start_date <- as.Date("2018-01-01")
end_date   <- as.Date("2024-12-31")

# Reload FOMC data (ensures clean parsing)
fomc <- read.csv("FOMC_Announcements_2018_2024.csv")

# Convert date column and filter dataset
fomc <- fomc %>%
  mutate(Date = ymd(Date)) %>%                     # convert to proper date format
  filter(Date >= start_date & Date <= end_date) %>%# keep only relevant time period
  filter(Decision %in% c("Hike", "Cut")) %>%       # remove "Hold" announcements
  filter(!weekdays(Date) %in% c("Saturday", "Sunday")) %>% # remove weekends
  mutate(line_color = ifelse(Decision == "Hike", "Hike", "Cut"))

# Quick sanity checks
head(fomc)
table(fomc$Decision)

###########################################################
# Bitcoin chart with Fed policy announcement markers
###########################################################

# Reload BTC data and ensure dates are parsed correctly
btc <- read.csv("BTCUSD_daily.csv")
btc$Date <- ymd(btc$Date)

# Filter date range and remove weekends for consistency with stock market data
btc <- btc %>%
  filter(Date >= start_date & Date <= end_date) %>%
  filter(!weekdays(Date) %in% c("Saturday", "Sunday")) %>%
  arrange(Date)

# Plot BTC with vertical lines representing Fed policy changes
ggplot(btc, aes(x = Date, y = Close)) +
  geom_line() +
  geom_vline(
    data = fomc,
    aes(xintercept = Date, color = line_color),
    linewidth = 0.4,
    alpha = 0.5
  ) +
  scale_color_manual(values = c(
  "Hike" = "red",
  "Cut" = "green"
)) +
  labs(
    title = "Bitcoin Price and Federal Reserve Policy Changes (2018–2024)",
    x = "Date",
    y = "BTC Close Price (USD)",
    color = "Fed Action"
  ) +
  theme_minimal()

###########################################################
# S&P 500 chart with Fed announcement markers
###########################################################

# Load S&P 500 data
sp$Date <- ymd(sp$Date)

# Filter to the analysis time window
sp <- sp %>%
  filter(Date >= start_date & Date <= end_date) %>%
  arrange(Date)

# Reload and clean FOMC data again
fomc <- read.csv("FOMC_Announcements_2018_2024.csv")

fomc <- fomc %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date >= start_date & Date <= end_date) %>%
  filter(Decision %in% c("Hike", "Cut")) %>%
  filter(!weekdays(Date) %in% c("Saturday", "Sunday")) %>%
  mutate(line_color = Decision)

# Plot S&P with Fed announcement lines
ggplot(sp, aes(x = Date, y = Close)) +
  geom_line() +
  geom_vline(
    data = fomc,
    aes(xintercept = Date, color = line_color),
    linewidth = 0.4,
    alpha = 0.5
  ) +
  scale_color_manual(values = c("Hike" = "red", "Cut" = "green")) +
  labs(
    title = "S&P 500 Price and Federal Reserve Policy Changes (2018–2024)",
    x = "Date",
    y = "S&P 500 Close",
    color = "Fed Action"
  ) +
  theme_minimal()

###########################################################
# Combine BTC and S&P into a single dataset
###########################################################

market <- btc %>%
  select(Date, BTC = Close) %>%                    # rename BTC close column
  left_join(sp %>% select(Date, SP = Close), by = "Date") # merge with S&P data

# Normalize both assets to start at 100 for easier comparison
market <- market %>%
  mutate(
    BTC_index = BTC / first(BTC) * 100,
    SP_index  = SP / first(SP) * 100
  )

# Plot normalized BTC vs S&P with Fed announcements
ggplot(market, aes(x = Date)) +

  geom_line(aes(y = BTC_index, color = "Bitcoin"), linewidth = 1) +

  geom_line(aes(y = SP_index, color = "S&P 500"), linewidth = 1) +

  geom_vline(
    data = fomc,
    aes(xintercept = Date, color = line_color),
    linewidth = 0.4,
    alpha = 0.5
  ) +

  scale_color_manual(values = c(
    "Bitcoin" = "orange",
    "S&P 500" = "blue",
    "Hike" = "red",
    "Cut" = "green"
  )) +

  scale_y_log10() +

  labs(
    title = "Bitcoin vs S&P 500 Around Federal Reserve Rate Changes (2018–2024)",
    x = "Date",
    y = "Indexed Price (Log Scale)",
    color = "Legend"
  ) +

  theme_minimal()

###########################################################
# Calculate daily log returns for both assets
###########################################################

market <- market %>%
  mutate(
    BTC_return = log(BTC / lag(BTC)),
    SP_return  = log(SP / lag(SP))
  )

###########################################################
# Plot Bitcoin daily returns
###########################################################

ggplot(market, aes(x = Date, y = BTC_return)) +
  geom_line(color = "orange") +
  labs(
    title = "Bitcoin Daily Returns (2018–2024)",
    x = "Date",
    y = "Daily Log Return"
  ) +
  theme_minimal()

###########################################################
# Plot S&P 500 daily returns
###########################################################

ggplot(market, aes(x = Date, y = SP_return)) +
  geom_line(color = "blue") +
  labs(
    title = "S&P 500 Daily Returns (2018–2024)",
    x = "Date",
    y = "Daily Log Return"
  ) +
  theme_minimal()

###########################################################
# Plot both assets' returns together
###########################################################

ggplot(market, aes(x = Date)) +

  geom_line(aes(y = BTC_return, color = "Bitcoin"), alpha = 0.8) +

  geom_line(aes(y = SP_return, color = "S&P 500"), alpha = 0.8) +

  scale_color_manual(values = c(
    "Bitcoin" = "orange",
    "S&P 500" = "blue"
  )) +

  labs(
    title = "Daily Returns: Bitcoin vs S&P 500 (2018–2024)",
    x = "Date",
    y = "Daily Log Return",
    color = "Asset"
  ) +

  theme_minimal()

###########################################################
# Add Fed announcement markers to the return chart
###########################################################

ggplot(market, aes(x = Date)) +

  geom_line(aes(y = BTC_return, color = "Bitcoin"), alpha = 0.8) +

  geom_line(aes(y = SP_return, color = "S&P 500"), alpha = 0.8) +

  geom_vline(
    data = fomc,
    aes(xintercept = Date, color = line_color),
    linewidth = 0.4,
    alpha = 0.5
  ) +

  scale_color_manual(values = c(
    "Bitcoin" = "orange",
    "S&P 500" = "blue",
    "Hike" = "red",
    "Cut" = "green"
  )) +

  labs(
    title = "Daily Returns: Bitcoin vs S&P 500 Around Federal Reserve Rate Changes (2018–2024)",
    x = "Date",
    y = "Daily Log Return",
    color = "Legend"
  ) +

  theme_minimal()

###########################################################
# Event Study: create -7 to +7 windows around each Fed announcement
###########################################################

event_data <- data.frame()

for(i in 1:nrow(fomc)) {
  
  event_date <- fomc$Date[i]
  decision <- fomc$Decision[i]
  
  window <- market %>%
    filter(Date >= event_date - 7 & Date <= event_date + 7) %>%
    mutate(
      Event_Date = event_date,
      Decision = decision,
      Day = as.numeric(Date - event_date)
    )
  
  event_data <- rbind(event_data, window)
}

# Preview the event window dataset
head(event_data)

###########################################################
# Calculate cumulative returns for each event window
###########################################################

event_summary <- event_data %>%
  group_by(Event_Date, Decision) %>%
  summarise(
    BTC_Cumulative = sum(BTC_return, na.rm = TRUE),
    SP_Cumulative = sum(SP_return, na.rm = TRUE)
  )

event_summary

# Confirm number of rows
nrow(event_data)

###########################################################
# Calculate correlation between BTC and S&P reactions
###########################################################

cor(event_summary$BTC_Cumulative, event_summary$SP_Cumulative)

###########################################################
# Summary statistics of cumulative returns
###########################################################

summary(event_summary$BTC_Cumulative)
summary(event_summary$SP_Cumulative)

###########################################################
# Compare average reaction for hikes vs cuts
###########################################################

event_summary %>%
group_by(Decision) %>%
summarise(
BTC_avg = mean(BTC_Cumulative),
SP_avg = mean(SP_Cumulative),
Events = n()
)

###########################################################
# Scatter plot showing relationship between BTC and S&P reactions
###########################################################

ggplot(event_summary, aes(x = SP_Cumulative, y = BTC_Cumulative)) +
geom_point(size = 3) +
geom_smooth(method = "lm", se = FALSE) +
labs(
title = "Bitcoin vs S&P 500 Reaction to Fed Announcements",
x = "S&P 500 Cumulative Return (-7,+7)",
y = "Bitcoin Cumulative Return (-7,+7)"
) +
theme_minimal()

###########################################################
# Statistical test for correlation significance
###########################################################

cor.test(event_summary$BTC_Cumulative, event_summary$SP_Cumulative)