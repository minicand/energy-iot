## Loading libraries
library("pacman")
pacman::p_load(lattice, ggplot2, caret, corrplot, dplyr, tidyr,
               plotly, party, rsconnect, ggridges, RMySQL, imputeTS, ggfortify, 
               lubridate, padr, RColorBrewer, fpp2, forecast, shinydashboard)

## Heatmap
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
calendarHeat(newDF$Date, newDF0610$global_active_power)

#### TASK 1 ####
## Create a database connection 
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!',
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

## Use asterisk to specify all attributes for download
# irisALL <- dbGetQuery(con, "SELECT * FROM iris")

## Learn the attributes
dbListFields(con,'yr_2006')

## Download the tables
yr2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power, global_reactive_power FROM yr_2006")
yr2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                     Sub_metering_3, global_active_power, global_reactive_power FROM yr_2007")
yr2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                     Sub_metering_3, global_active_power, global_reactive_power FROM yr_2008")
yr2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, 
                     Sub_metering_3, global_active_power, global_reactive_power FROM yr_2009")
yr2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2,
                     Sub_metering_3, global_active_power, global_reactive_power FROM yr_2010")

## Create multi-year df
newDF <- bind_rows(yr2007, yr2008, yr2009)
newDF0610 <- bind_rows(yr2006, yr2007, yr2008, yr2009, yr2010)

## Combine Date and Time attribute values in a new attribute column
newDF <- cbind(newDF, paste(newDF$Date, newDF$Time), stringsAsFactors = FALSE)
newDF0610 <- cbind(newDF0610, paste(newDF0610$Date, newDF0610$Time), stringsAsFactors = FALSE)
## Give the new attribute in the 6th column a header name "DateTime"
colnames(newDF)[ncol(newDF)] <- "DateTime"
colnames(newDF0610)[ncol(newDF0610)] <- "DateTime"
## Move the DateTime attribute within the dataset
newDF <- newDF[,c(ncol(newDF), 1:(ncol(newDF)-1))]
newDF0610 <- newDF0610[,c(ncol(newDF0610), 1:(ncol(newDF0610)-1))]

## Convert DateTime from POSIXlt to POSIXct
newDF$DateTime <- as.POSIXct(newDF$DateTime, "%Y/%m/%d %H:%M:%S")
newDF0610$DateTime <- as.POSIXct(newDF0610$DateTime, "%Y/%m/%d %H:%M:%S")

# newDF0610$global_reactive_power <- newDF0610$global_reactive_power * 1000 / 60
# newDF0610[2:3] <- round(newDF0610[2:3], 2)

# New column for non-subbed
newDF0610$non_subbed <- newDF0610$global_active_power - newDF0610$Sub_metering_1 - 
  newDF0610$Sub_metering_2 - newDF0610$Sub_metering_3

# Timezone
attr(newDF$DateTime, "tzone") <- "GMT"  # not to mess it up no Europe/Paris 
attr(newDF0610$DateTime, "tzone") <- "GMT"  # not to mess it up no Europe/Paris 
tz(newDF$DateTime)
str(newDF$DateTime)

## Inspect the data types
str(newDF)

## Create year, day, etc attribute with lubridate
newDF$year <- year(newDF$DateTime)
newDF$day <- day(newDF$DateTime)
newDF$month <- month(newDF$DateTime)
newDF$week <- week(newDF$DateTime)

## Check rows with missing data
summary(is.na(newDF))

## Explore data
summary(newDF)
max(newDF$Sub_metering_1)
mean(newDF$Sub_metering_1[newDF$year == 2007])
mean(newDF$Sub_metering_1)


# Replace missing data with NAs
newDF.na <- pad(newDF0610, break_above = 3000000)
summary(is.na(newDF.na))

# Impute missing data
newDF.nona <- na.kalman(newDF.na)

# Clean date and time columns 
newDF.nona <- newDF.nona[-c(7:10)]
newDF.nona$year <- year(newDF.nona$DateTime)
newDF.nona$day <- day(newDF.nona$DateTime)
newDF.nona$month <- month(newDF.nona$DateTime)
newDF.nona$week <- week(newDF.nona$DateTime)


#### TASK 1- 2 ####
## Voltage volatility
voltage2010 <- dbGetQuery(con, "SELECT Date, Time, voltage FROM yr_2010")
min(voltage2010$voltage)
mean(voltage2010$voltage)
max(voltage2010$voltage)  # volatility less than +/-6% so OK

## Subset a day - All observations
houseDay <- filter(newDF0610, year == 2008 & month == 1 & day == 9)  # weekday
houseDayWe <- filter(newDF0610, year == 2008 & month == 1 & day == 5)  # weekend
# houseRd <- sample_n(newDF, 1)  # Random data

## Plot sub-meter 1, 2 and 3 with title, legend and labels - All observations 
plotly.wd <- plot_ly(houseDay, x = ~houseDay$DateTime, 
                     y = ~houseDay$Sub_metering_1, 
                     name = 'Kitchen', 
                     type = 'scatter', 
                     mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  layout(text = "Power Consumption January 9th, 2008",
         xaxis = list(title = "WEEKDAY: January 9th, 2008"),
         yaxis = list (title = "Power (watt-hours)"))

plotly.we <- plot_ly(houseDayWe, x = ~houseDayWe$DateTime, 
                     y = ~houseDayWe$Sub_metering_1, 
                     name = 'Kitchen', 
                     type = 'scatter', 
                     mode = 'lines') %>%
  add_trace(y = ~houseDayWe$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDayWe$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  layout(text = "Daily Power Consumption",
         xaxis = list(title = "WEEKEND: January 5th, 2008"),
         yaxis = list (title = "Power (watt-hours)"))

plotly.wd
plotly.we
# Kind of like facet_grid of ggplot
p <- subplot(plotly.wd, plotly.we, titleX = TRUE) 
p

# Choose a random day
house.day.by15 <- by15[961:1057,]  # use xts package
house.day.by15$Time <-strftime(house.day.by15$DateTime, format="%H:%M:%S")  # Extracting time from DateTime
# Cleaner daily graph
plot_ly(house.day.by15, 
        x = ~house.day.by15$Time, 
        y = ~house.day.by15$Sub_metering_1, 
        name = 'Kitchen', 
        type = 'scatter', 
        mode = 'lines') %>%
  add_trace(y = ~house.day.by15$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~house.day.by15$Sub_metering_3, name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 11th, 2007",
         xaxis = list(title = "Time", tickangle = 45),
         yaxis = list(title = "Power (watt-hours)"))

## Group data in periods

bylist <- c()
timelist <- list("15 mins", "hour", "day", "week", "month", "year")
for (i in timelist){
bylist[[i]]  <- newDF.nona %>% group_by(cut(DateTime, i)) %>%
    dplyr::summarize_at(vars("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                             "global_active_power", "global_reactive_power", "non_subbed"), funs(sum))  
colnames(bylist[[i]])[1] <- "DateTime"
}
bylist[[6]]$DateTime <- c('2006','2007','2008','2009','2010')
bylist[[6]]


pals <- c("#56b75b", "#efe013", "#ff0000", "#ffa800", "#65cecb", "#7953ba")
plotly.yearly <- by.year %>% plot_ly(x = ~by.year$DateTime, type= 'bar', colors= pals) %>%  # Ask trace 0
  add_trace( y = ~by.year$global_active_power, 
        name = 'Overall', 
        mode = 'lines')%>%
  add_trace(y = ~by.year$Sub_metering_1, name = 'Kitchen', mode = 'lines') %>%
  add_trace(y = ~by.year$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by.year$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  add_trace(y = ~by.year$non_subbed, name = 'Not-submetered', mode = 'lines') %>%
  layout(text = "Daily Power Consumption",
         xaxis = list(title = "Years"),
         yaxis = list (title = "Power (watt-hours)"))
plotly.yearly
# Gather yearly totals for pie chart
by.year.gathered <- by.year %>% gather(key = "Sub_meter",
                                       value = "Reading",
                                       -DateTime)
by.year.pie <- by.year.gathered %>% group_by(Sub_meter) %>% summarize(sum(Reading))
by.year.pie <- by.year.pie[-1,]
colnames(by.year.pie)[2] <- "Reading"


colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
plotly.pie <- by.year.pie %>% plot_ly(labels= ~Sub_meter, values= ~Reading, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        hoverinfo = 'text',
                        text = ~paste(round(Reading/1000, 2), 'kWh'),
                        marker = list(colors = colors,
                                      line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title= " Consumption per Submeter <br> (Dec 2006 - Nov 2010)",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
plotly.pie
## A weekly plotly
# Subset the second week of 2008 - All Observations
houseWeek <- filter(newDF.nona, year == 2008 & week == 2)
houseWeek$week.day <- weekdays(houseWeek$DateTime)

# Plot subset houseWeek
plot(houseWeek$Sub_metering_1)  # Reduce granulity 
#Plotly
plotly.week <- plot_ly(houseWeek, x = ~houseWeek$DateTime, 
                     y = ~houseWeek$Sub_metering_1, 
                     name = 'Kitchen', 
                     type = 'scatter', 
                     mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  layout(text = "Weekly Power Consumption",
         xaxis = list(title = "January 8th-15th, 2008"),
         yaxis = list (title = "Power (watt-hours)"))
plotly.week

## Graph for average consumption for weekdays
newDF.nona$weekday <- weekdays(newDF.nona$DateTime)
by.weekday <- newDF.nona %>% group_by(weekday) %>%
  dplyr::summarize_at(vars("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                           "global_active_power", "global_reactive_power", "non_subbed"), funs(mean))

colnames(by.weekday)[1] <- "Weekdays"
# Reorder by normal weekdays
target <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
by.weekday <- by.weekday[match(target, by.weekday$weekday),]

# Correct the order!!!!
plotly.weekdays <- plot_ly(by.weekday, x = ~by.weekday$weekday, 
                       y = ~by.weekday$Sub_metering_1, 
                       name = 'Kitchen', 
                       type = 'bar', 
                       mode = 'lines') %>%
  add_trace(y = ~by.weekday$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by.weekday$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  add_trace(y = ~by.weekday$non_subbed, name = 'Others', mode = 'lines') %>%
  add_trace(y = ~by.weekday$global_active_power, name = 'Total', mode = 'lines') %>%
  layout(text = "Weekly Power Consumption",
         xaxis = list(title = "Weekday"),
         yaxis = list (title = "Power (watt-hours)"))
plotly.weekdays # do something with the colors



#### TASK 2 ####

#Timeseries sub-meter3 by day
ts.by.day3 <- ts(by.day$Sub_metering_3, frequency=365, start=c(2007,1))
autoplot(ts.by.day3, ts.colour= 'purple', xlab= 'Time', ylab= 'Power(Whour)')

# ggplot facet_grid by meter
by.day.melted <- gather(by.day, "Sub_metering_1","Sub_metering_2","Sub_metering_3",
       "global_active_power", "non_subbed", key= "Meter", value= "Power")


ggplot(by.day.melted, aes(DateTime, Power, fill= Meter))+
  geom_col()+
  facet_grid(Meter ~.)+
  scale_fill_manual(labels = c("Total", "Kitchen", "Laundry", "Boiler&AC", "Not sub-metered"), 
                    values = c("Sub_metering_1"="blue","Sub_metering_2"="red",
                               "Sub_metering_3"="brown","global_active_power"="orange",
                               "non_subbed" = "grey"))  # change label text
str(by.day) #wtf

## Time Series

# graph of the thickened df
# plot_ly(by.day, x = ~DateTime,
#         y = ~by.day$Sub_metering_1,
#         name = 'Kitchen',
#         type = 'scatter',
#         mode = 'lines') 
# %>%
#   add_trace(x = DateTime_year, name = 'Laundry Room', mode = 'lines') %>%
#   add_trace(x = DateTime_year == "2009-01-01", name = 'Boiler & AC', mode = 'lines') %>%
#   layout(text = "Weekly Power Consumption",
#          xaxis = list(title = "January 8th-15th, 2008"),
#          yaxis = list (title = "Power (watt-hours)"))

# Days of a random month
house.month <- filter(newDF.nona, year == 2009 & month == 10)
house.month.by.day <- house.month %>% dplyr::group_by(house.month$day) %>%
  dplyr::summarize_at(vars("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                       "global_active_power", "global_reactive_power", "non_subbed"), funs(sum))
str(house.month.by.day)
colnames(house.month.by.day)[1] <- "days"
ts.mbyd <- ts(house.month.by.day$global_active_power, frequency= 30, start= c(1, 1))

plotly.mbyd <- plot_ly(house.month.by.day, x = ~house.month.by.day$days, 
                           y = ~house.month.by.day$Sub_metering_1, 
                           name = 'Kitchen', 
                           mode = 'lines') %>%
  add_trace(y = ~house.month.by.day$Sub_metering_1, name = 'Kitchen', mode = 'lines') %>%
  add_trace(y = ~house.month.by.day$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~house.month.by.day$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  add_trace(y = ~house.month.by.day$non_subbed, name = 'Not sub-metered', mode = 'lines') %>%
  add_trace(y = ~house.month.by.day$global_active_power, name = 'Total', mode = 'lines') %>%
  layout(text = "Daily Power Consumption",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (watt-hours)"))
plotly.mbyd

## Polar visualization by month
# Create monthly totals by year
house.monthly.totals <- newDF.nona %>% dplyr::group_by(cut(DateTime, "month")) %>%
  dplyr::summarize_at(vars("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                           "global_active_power", "global_reactive_power", "non_subbed"), funs(sum))
colnames(house.monthly.totals)[1] <- "Months"
# Time series for monthly total power usage by year
ts.month <- ts(house.monthly.totals$global_active_power, frequency= 12, start= c(2007, 1))
# Polar gg
ggseasonplot(ts.month, polar=TRUE) +
  ylab("Months") +
  ggtitle("Polar seasonal plot: energy usage")

#### TASK 2 - FORECASTING ####

### LINEAR MODEL
## 1- Linear model of total consumption by day
fitSM3 <- tslm(ts.by.day3 ~ trend + season)
summary(fitSM3)
# Create the forecast for sub-meter 3. Forecast ahead 30 time periods 
forecastfitSM3 <- forecast(fitSM3, h=365, level=c(80,90))
# Plot the forecast for sub-meter 3. 
plot(forecastfitSM3)
# subset the time series
ts.by.day3.2 <- window(ts.by.day3, start=c(2008, 1), end=c(2010, 1))
# Decompose sub-meter 3 into trend, seasonal and remainder
components.by.day3.2 <- decompose(ts.by.day3.2)
# Plot decomposed sub-meter 3 
plot(components.by.day3.2)
# Check summary statistics for decomposed sub-meter 3 
summary(components.by.day3.2)
# Loess decomposition
plot(stl(ts.by.day3, s.window="periodic"))

## 2- Forecast sub-meter 2 by week
# thicken per year
by.week <- by.week %>% thicken(interval = "year")

# Create weekly time series for sub-meter 2
ts.by.week2 <- ts(by.week$Sub_metering_2, frequency=52, start=c(2007,1))
# Plot the time series
autoplot(ts.by.week2, ts.colour= 'purple', xlab= 'Time', ylab= 'Power(Whour)')
# Linear model of sub-meter 2 by week
fitSM2 <- tslm(ts.by.week2 ~ trend + season)
summary(fitSM2)
# Create the forecast for sub-meter 2. Forecast ahead 12 time periods 
forecastfitSM2 <- forecast(fitSM2, h=12, level=c(80,90))
# Plot the forecast for sub-meter 2. 
plot(forecastfitSM2)
# subset the time series
ts.by.week2.2 <- window(ts.by.week2, start=c(2008, 1), end=c(2010, 1))
# Decompose sub-meter 2 into trend, seasonal and remainder
components.by.week2 <-stl(ts.by.week2)
# Plot decomposed sub-meter 2 
autoplot(components.by.week2)

# Check summary statistics for decomposed sub-meter 2 
summary(components.by.week2)
# Loess decomposition
plot(stl(ts.by.week2, s.window="periodic"))


## Comparison chart showing the summary statistics from each decomposed object

# Melted tables


### HOLT WINTERS
#1
ts.by.day1.adjusted <- ts.by.day1 - components.by.day1$seasonal
autoplot(ts.by.day1.adjusted)
plot(decompose(ts.by.day1.adjusted))
# Holt winters exponential smoothing
ts.by.day1.HW <- HoltWinters(ts.by.day1.adjusted, gamma=FALSE)

plot(ts.by.day1.HW, ylim = c(-5000, 10000))
# HoltWinters forecast & plot
ts.by.day1.HWfor <- forecast(ts.by.day1.HW, h=30)
plot(ts.by.day1.HWfor, ylim = c(0, 5000), ylab= "Watt-Hours", xlab="Time - Sub-meter 1")
# Add back the seasonality
fcseas <- forecast(components.by.day1$seasonal, h=30)
ts.by.day1.HWfor$mean <- ts.by.day1.HWfor$mean + fcseas$mean
ts.by.day1.HWfor$lower <- ts.by.day1.HWfor$lower + fcseas$lower
ts.by.day1.HWfor$upper <- ts.by.day1.HWfor$upper + fcseas$upper
summary(ts.by.day1.HWfor)
summary(ts.by.day2.HWfor)

1
# Training set 1.022559 1787.194 1113.918 NaN  Inf 0.6946472 0.0193628
# Training set -5.386334e-14 1338.212 976.6579 NaN  Inf 0.609051 0.0387033
# #2
# Training set 13.25254 3273.275 2206.735 -9.101541 31.32726 0.6396956 -0.02885782
# Training set -6.973092e-14 2498.139 1979.067 Inf  Inf 0.5736986 0.2351275

### AUTO ARIMA
auto.arima()  


### ARIMA


### AUTOLAYER to combine all model forecasts in one graph
autolayer