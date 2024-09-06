# Front Matter----------------------------------------------



# Note: Google Trends API became non-functional
# after the completion of section 1, so some of
# this code may not work

.libPaths(paste0(getwd(), "/packages"))
library(gtrendsR)
library(ggfortify)
library(ggplot2)
library(lubridate)
library(tidyquant)
library(forecast)
library(gridExtra)

options(scipen = 999)



# Decompositions----------------------------------------------------------------



gtrend <- gtrends(
  keyword = "Uhaul",
  geo = "US",
  time = "today+5-y",
  gprop = c("web"),
  category = 0,
  hl = "en-US",
  compared_breakdown = FALSE,
  low_search_volume = FALSE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0,
  onlyInterest = FALSE
)

start <- c(year(gtrend$interest_over_time[1, "date"]),
           week(gtrend$interest_over_time[1, "date"]))
ts <- ts(gtrend$interest_over_time$hits,
         start = start,
         frequency = 52)
autoplot(decompose(ts))


gtrend.christmas <- gtrends(
  keyword = "Christmas Day",
  geo = "US",
  time = "2004-01-01 2020-01-01",
  gprop = c("web"),
  category = 0,
  hl = "en-US",
  compared_breakdown = FALSE,
  low_search_volume = FALSE,
  cookie_url = "http://trends.google.com/Cookies/NID",
  tz = 0,
  onlyInterest = FALSE
)
gtrend.christmas$interest_over_time$hits <- 
  gsub("<", "", gtrend.christmas$interest_over_time$hits)
gtrend.christmas$interest_over_time$hits <- 
  as.numeric(gtrend.christmas$interest_over_time$hits)

start.christmas <- c(year(gtrend.christmas$interest_over_time[1, "date"]),
           month(gtrend.christmas$interest_over_time[1, "date"]))
ts.christmas <- ts(gtrend.christmas$interest_over_time$hits,
         start = start.christmas,
         frequency = 12)
autoplot(decompose(ts.christmas,
                   type = "multiplicative"))



# Seasonal Plots----------------------------------------------------------------



wayfair <- read.csv("data/wayfair.csv",
                    skip = 1)
wayfair$Wayfair...United.States. <- 
  gsub("<", "", wayfair$Wayfair...United.States.)
wayfair$Wayfair...United.States. <- 
  as.numeric(wayfair$Wayfair...United.States.)
start.wayfair <- 
  c(year(wayfair[1, "Week"]),
    week(wayfair[1, "Week"]))
ts.wayfair <- ts(wayfair$Wayfair...United.States.,,
                   start = start.wayfair,
                   frequency = 52)
autoplot(ts.wayfair) + 
  labs(title = "Search Interest for Term 'Wayfair'",
       caption = "Source: Google Trends") +
  ylab("Interest")

wayfair.monthly <- to.monthly(xts(wayfair$Wayfair...United.States.,
                                    order.by = as.Date(wayfair$Week)))
names(wayfair.monthly) <- c("Open", "High", "Low", "Close")
start.wayfair.monthly <- 
  c(year(index(wayfair.monthly)[1]),
    month(index(wayfair.monthly)[1]))
ts.wayfair.monthly <- ts(wayfair.monthly$Close,
                 start = start.wayfair.monthly,
                 frequency = 12)
ggsubseriesplot(ts.wayfair.monthly, 
                year.labels = FALSE
                ) + 
  labs(title = "Monthly Search Interest for Term 'Wayfair'",
       subtitle = "July 2019 to Present",
       caption = "Source: Google Trends") +
  ylab("Interest")



# Lag Plots----------------------------------------------------------------



getSymbols("W")
getSymbols("ABNB")

chart1 <- autoplot(W$W.Adjusted) +
  labs(title = "Wayfair Stock Price at Close",
       caption = "Source: Yahoo Finance")
chart2 <- autoplot(ABNB$ABNB.Adjusted) +
  labs(title = "AirBNB Stock Price at Close",
       caption = "Source: Yahoo Finance")
grid.arrange(chart1, chart2, ncol = 2)

lagChart1 <- gglagplot(W$W.Adjusted, 
                       lags = 1,
                       do.lines = F) +
  labs(title = "Wayfair Stock Price",
       caption = "Source: Yahoo Finance") +
  theme(plot.title = element_text(hjust = 0.5))
lagChart2 <- gglagplot(ABNB$ABNB.Adjusted, 
                       lags = 1,
                       do.lines = F) +
  labs(title = "AirBNB Stock Price",
       caption = "Source: Yahoo Finance") +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(lagChart1, lagChart2, ncol = 2)

lagChart3 <- gglagplot(ts, 
                       lags = 52,
                       do.lines = F) +
  labs(title = "Uhaul Search Interest") +
  theme(plot.title = element_text(hjust = 0.5))

