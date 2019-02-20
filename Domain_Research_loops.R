## Loading libraries
library("pacman")
pacman::p_load(lattice, ggplot2, caret, corrplot, dplyr, tidyr, devtools, DALEX, 
               plotly, party, rsconnect, ggridges, RMySQL, imputeTS,
               lubridate, padr, RColorBrewer, fpp2, forecast, shinydashboard, esquisse,
               seasonal, keras, forecast, xts)

## Heatmap
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")
calendarHeat(newDF$Date, newDF0610$global_active_power)

## Esquisse for basic visualizing
esquisse::esquisser()

#### TASK 1 - DATA CLEANING ####
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

newDF0610$global_active_power <- newDF0610$global_active_power * 1000 / 60
newDF0610$global_reactive_power <- newDF0610$global_reactive_power * 1000 / 60
newDF0610[2:3] <- round(newDF0610[2:3], 2)

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
newDF.nona$year <- year(newDF.nona$DateTime)
newDF.nona$day <- day(newDF.nona$DateTime)
newDF.nona$month <- month(newDF.nona$DateTime)
newDF.nona$week <- week(newDF.nona$DateTime)

# Put prices in
newDF.nona[, 14:19] <- newDF.nona %>% transmute_at(vars(global_active_power, global_reactive_power, 
                              Sub_metering_1, Sub_metering_2, Sub_metering_3, non_subbed),
                         funs(./1000*0.1483)
                         )

# Peak hours tariff
newDF.nona[, 20:25] <- newDF.nona %>% transmute_at(vars(global_active_power, global_reactive_power,
                                 Sub_metering_1, Sub_metering_2, Sub_metering_3, non_subbed
                                 ),
                            funs(ifelse(hour(newDF.nona$DateTime) > 6 | hour(newDF.nona$DateTime) < 22, ./1000*0.1593, ./1000*0.1244
                                        )
                                 )
                            )


costlist <- c()
timelist3 <- list("day", "week", "month", "year")
for (i in timelist3){
  costlist[[i]]  <- newDF.nona %>% group_by(cut(DateTime, i)) %>%
    dplyr::summarize_at(vars(global_active_power.1, global_reactive_power.1, 
                             Sub_metering_1.1, Sub_metering_2.1, Sub_metering_3.1, non_subbed.1,
                             global_active_power.2, global_reactive_power.2, 
                             Sub_metering_1.2, Sub_metering_2.2, Sub_metering_3.2, non_subbed.2), 
                        funs(sum)) 
  colnames(costlist[[i]])[1] <- "DateTime"
}
# For shiny
costlist[[4]]$DateTime <- c('2006','2007','2008','2009','2010')
costlist[[3]]$DateTime <- c('2006-12',
                            '2007-01','2007-02','2007-03','2007-04','2007-05','2007-06',
                            '2007-07','2007-08','2007-09','2007-10','2007-11','2007-12',
                            '2008-01','2008-02','2008-03','2008-04','2008-05','2008-06',
                            '2008-07','2008-08','2008-09','2008-10','2008-11','2008-12',
                            '2009-01','2009-02','2009-03','2009-04','2009-05','2009-06',
                            '2009-07','2009-08','2009-09','2009-10','2009-11','2009-12',
                            '2010-01','2010-02','2010-03','2010-04','2010-05','2010-06',
                            '2010-07','2010-08','2010-09','2010-10','2010-11')
head(costlist[[4]])
tail(costlist[[3]])

# Save it to a file
saveRDS(costlist, "costlist.rds")


#### TASK 1- 2 ####
## Voltage volatility
voltage2010 <- dbGetQuery(con, "SELECT Date, Time, voltage FROM yr_2010")
min(voltage2010$voltage)
mean(voltage2010$voltage)
max(voltage2010$voltage)  # volatility less than +/-6% so OK

## Subset a day - All observations
houseDay <- filter(newDF.nona, year == 2008 & month == 1 & day == 9)  # weekday
houseDayWe <- filter(newDF.nona, year == 2008 & month == 1 & day == 5)  # weekend
# houseRd <- sample_n(newDF, 1)  # Random data

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
# Day by hour for dashboard
tail(bylist[[2]])
lastdaybh <- bylist[[2]][(nrow(bylist[[2]])-21):nrow(bylist[[2]]),]
lastdaybh$DateTime <- hour(lastdaybh$DateTime)
plotly.dbyh <- plot_ly(lastdaybh, x = ~lastdaybh$DateTime, 
                     y = ~lastdaybh$Sub_metering_1, 
                     name = 'Kitchen', 
                     type = 'scatter', 
                     mode = 'lines') %>%
  add_trace(y = ~lastdaybh$Sub_metering_2, name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~lastdaybh$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  add_trace(y = ~lastdaybh$non_subbed, name = 'Not Sub-metered', mode = 'lines') %>%
  layout(title = "Daily Power Consumption <br> (Current Day)",
         xaxis = list(title = "Hours"),
         yaxis = list (title = "Power (wh)"))
plotly.dbyh
# Save it to a file
saveRDS(plotly.dbyh, "pdbyh.rds")

#Yearly plot by 
colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')  
plotly.yearly <- bylist[[6]] %>% plot_ly(x = ~bylist[[6]]$DateTime,
                                         y = ~bylist[[6]]$global_active_power, 
                                         name= "Overall", type= 'bar', colors = colors) %>% # how to change the colors??! 
  add_trace(y = ~bylist[[6]]$non_subbed, name = 'Not-submetered', mode = 'lines') %>%
  add_trace(y = ~bylist[[6]]$Sub_metering_3, name = 'Boiler & AC', mode = 'lines') %>%
  add_trace(y = ~bylist[[6]]$Sub_metering_2, name = 'Kitchen', mode = 'lines') %>%
  add_trace(y = ~bylist[[6]]$Sub_metering_1, name = 'Laundry Room', mode = 'lines') %>%
  layout(text = "Daily Power Consumption",
         xaxis = list(title = "Years"),
         yaxis = list (title = "Power (watt-hours)"))
plotly.yearly

# Save it to a file
saveRDS(plotly.yearly, "pyearly.rds")

# Gather yearly totals for pie-chart
by.year.gathered <- bylist[[6]] %>% gather(key = "Sub_meter",
                                       value = "Reading",
                                       -DateTime)
by.year.pie <- by.year.gathered %>% group_by(Sub_meter) %>% summarize(sum(Reading))
by.year.pie <- by.year.pie[-1,]
colnames(by.year.pie)[2] <- "Reading"
# Plot the pie-chart
plotly.pie <- by.year.pie %>% plot_ly(labels= factor(c("Energy Lost", 
                                                       "Not Sub-metered", "Kitchen", "Laundry Room", "HVAC"), 
                                                     ordered =  TRUE), values= ~Reading, type = 'pie',
                        textposition = 'inside',
                        textinfo = 'label+percent',
                        insidetextfont = list(color = '#FFFFFF'),
                        hoverinfo = 'text',
                        text = ~paste(round(Reading/1000, 2), 'kWh'),
                      #  marker = list(colors = colors,
                      #               line = list(color = '#FFFFFF', width = 1)),
                        showlegend = FALSE) %>%
  layout(title= " Consumption per Submeter <br> (Dec 2006 - Nov 2010)",
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
plotly.pie
# Save it to a file
saveRDS(plotly.pie, "ppie.rds")

## Graph for average consumption for weekdays
newDF.nona$weekday <- weekdays(newDF.nona$DateTime)
by.weekday <- newDF.nona %>% group_by(weekday, month == c(1:3)) %>%  # a grouping by seasons
  dplyr::summarize_at(vars("Sub_metering_1","Sub_metering_2","Sub_metering_3",
                           "global_active_power", "global_reactive_power", "non_subbed"), funs(mean))

# Reorder by normal weekdays
target <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
by.weekday <- by.weekday[match(target, by.weekday$weekday),]


# Plotly for weekdays
plotly.weekdays <- plot_ly(by.weekday, x = ~factor(by.weekday$weekday,
                                                   labels = c("Monday", "Tuesday", "Wednesday",
                                                              "Thursday", "Friday", "Saturday", "Sunday")), 
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

# Save it to a file
saveRDS(plotly.weekdays, "pwd.rds")

# ggplot facet_grid by meter
by.day.melted <- gather(bylist[[3]], "Sub_metering_1","Sub_metering_2","Sub_metering_3",
                        "global_active_power", "non_subbed", key= "Meter", value= "Power")
by.day.melted$Power <- round(by.day.melted$Power, 2)

ggplot(by.day.melted, aes(DateTime, Power, fill= Meter))+
  geom_col()+
  facet_grid(Meter ~.)+
  scale_fill_manual(labels = c("Total", "Kitchen", "Laundry", "Boiler&AC", "Not sub-metered"), 
                    values = c("Sub_metering_1"="blue","Sub_metering_2"="red",
                               "Sub_metering_3"="brown","global_active_power"="orange",
                               "non_subbed" = "grey"))  # change label text

str(bylist[[3]]) #wtf
# Thickened by year
bylist[[3]]$DateTime <- as.POSIXct(bylist[[3]]$DateTime, "%Y/%m/%d")
bylist[[3]] <- bylist[[3]] %>% thicken(interval = "year")


## Days of the last month
bylist[[3]]$year <- year(bylist[[3]]$DateTime)
bylist[[3]]$month <- month(bylist[[3]]$DateTime)
house.month <- filter(bylist[[3]], year == 2010 & month == 11)

plotly.mbyd <- plot_ly(house.month, x = ~house.month$DateTime,
                           y = ~house.month$Sub_metering_1/ 1000,
                           name = 'Kitchen',
                           mode = 'bar') %>%
  add_trace(y = ~house.month$Sub_metering_1/ 1000, name = 'Kitchen', mode = 'bar') %>%
  add_trace(y = ~house.month$Sub_metering_2/ 1000, name = 'Laundry Room', mode = 'bar') %>%
  add_trace(y = ~house.month$Sub_metering_3/ 1000, name = 'Boiler & AC', mode = 'bar') %>%
  add_trace(y = ~house.month$non_subbed/ 1000, name = 'Not sub-metered', mode = 'bar') %>%
  add_trace(y = ~house.month$global_active_power/ 1000, name = 'Total', mode = 'bar') %>%
  layout(title = "Daily Power Consumption <br> (Current Month)",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (Kwh)"))
plotly.mbyd
# Save it to a file
saveRDS(plotly.mbyd, "pmbyd.rds")

plotly.wbyd <- plot_ly(house.month[20:26,], x = ~house.month[20:26,]$DateTime,
                       y = ~house.month$Sub_metering_1/ 1000,
                       name = 'Kitchen',
                       mode = 'bar') %>%
  add_trace(y = ~house.month[20:26,]$Sub_metering_1/ 1000, name = 'Kitchen', mode = 'bar') %>%
  add_trace(y = ~house.month[20:26,]$Sub_metering_2/ 1000, name = 'Laundry Room', mode = 'bar') %>%
  add_trace(y = ~house.month[20:26,]$Sub_metering_3/ 1000, name = 'Boiler & AC', mode = 'bar') %>%
  add_trace(y = ~house.month[20:26,]$non_subbed/ 1000, name = 'Not sub-metered', mode = 'bar') %>%
  add_trace(y = ~house.month[20:26,]$global_active_power/ 1000, name = 'Total', mode = 'bar') %>%
  layout(title = "Daily Power Consumption <br> (Current Week)",
         xaxis = list(title = "Days"),
         yaxis = list (title = "Power (Kwh)"))
plotly.wbyd
saveRDS(plotly.wbyd, "pwbyd.rds")

# ## Polar visualization by month
# # Time series for monthly total power usage by year
# ts.month <- ts(bylist[[5]]$global_active_power, frequency= 12, start= c(2007, 1))
# ggseasonplot(ts.month, polar=TRUE) +
#   ylab("Months") +
#   ggtitle("Polar seasonal plot: energy usage")

ggplot(by.day.melted, aes(x=factor(DateTime_year), y=global_active_power)) + geom_boxplot(outlier.colour = "red")

# Save multiple objects
save(plotly.wbyd, plotly.mbyd, plotly.dbyh, house.month, bylist, costlist, lastdaybh, plotly.yearly, plotly.pie, file = "shinydata.RData")

#### TASK 2 - TIME SERIES ####

head(bylist[[2]])
head(ts.day2)
tail(ts.day2)
### Creating Time Series
ts.hour <- ts(bylist[[2]]$global_active_power, frequency = 8760, start = c(2006, 8417))
ts.day <- ts(bylist[[3]]$global_active_power, frequency = 365, start = c(2006, 350))
ts.week <- ts(bylist[[4]]$global_active_power, frequency = 52, start = c(2006, 51))
ts.month <- ts(bylist[[5]]$global_active_power, frequency = 12, start = c(2006,11))
msts.day <- msts(bylist[[3]]$global_active_power, seasonal.periods = c(7,365.25), start = c(2006, 350))

tail(ts.week)
autoplot(ts.hour, ts.colour= 'pink', xlab= 'Time', ylab= 'Power(Whour)')
autoplot(ts.day, ts.colour= 'purple', xlab= 'Time', ylab= 'Power(Whour)')
autoplot(ts.week, ts.colour= 'magenta', xlab= 'Time', ylab= 'Power(Whour)')
autoplot(ts.month, ts.colour= 'blue', xlab= 'Time', ylab= 'Power(Whour)')
autoplot(msts.day, xlab= 'Time', ylab= 'Power(Whour)')

### Train and Test Sets
ts.hour.tr <- window(ts.hour, start=c(2007,1), end=2010)
ts.day.tr <- window(ts.day, start=c(2007,1), end=c(2009,365))
ts.week.tr <- window(ts.week, start=c(2007,1), end=c(2009,52))
ts.month.tr <- window(ts.month, start=c(2007,1), end=c(2009,12))
msts.day.tr <- window(msts.day, start=c(2007,1), end=c(2009,365))
ts.hour.te <- window(ts.hour, start=c(2010,1))
ts.day.te <- window(ts.day, start=c(2010,1))
ts.week.te <- window(ts.week, start=c(2010,1))
ts.month.te <- window(ts.month, start=c(2010,1))
msts.day.te <- window(msts.day, start=c(2010,1))

### DECOMPOSING
## with STL - Loess decomposition
stllist <- c()
remlist <- c()
timelist2 <- list(ts.hour, ts.day, ts.week, ts.month)
counter <- 1
for (i in timelist2){
  print(i)
  stllist[[counter]] <- stl(i, s.window = "periodic")
  remlist[[counter]] <- abs(remainder(stllist[[counter]]))
  counter <- counter+1
}
# Compare the relative randomness
mean(rem.hour)/mean(ts.hour)
mean(rem.day)/mean(ts.day)
mean(rem.week)/mean(ts.week)
mean(rem.month)/mean(ts.month)
mean(abs(remainder(mstl(msts.day, s.window = "periodic"))))/mean(msts.day)

### MODELING
## Linear model
fitLM <- tslm(ts.month.tr ~ trend + season)
summary(fitLM)

## Holt Winters
fitHW <- HoltWinters(ts.month.tr)
summary(fitHW)

## Auto ARIMA
auto.arima(ts.month.tr)
auto.arima(ts.month)
fitAA <- arima(ts.month.tr, order= c(0,0,1), seasonal = c(1,1,0)) 
summary(fitAA)
getwd()

### FORECASTING model comparison
## Linear Model 
forecastLM <- forecast(fitLM, h=10, level=c(80,90))
## Holt Winters
forecastHW <- forecast(fitHW, h=10, level=c(80,90))
## Auto ARIMA
forecastAA <- forecast(fitAA, h=10, level=c(80,90))


## Compare accuracies
accuracy(forecastLM, ts.month.te)
accuracy(forecastHW, ts.month.te)
accuracy(forecastAA, ts.month.te)

### AUTOLAYER to combine all model forecasts in one graph
autoplot(ts.month, series = "Real")+ 
  autolayer(forecastLM, series = "LM", PI= FALSE)+
  autolayer(forecastHW, series = "HW")+
  autolayer(forecastAA, series = "ARIMA")+
  guides(color= guide_legend(title = "dk"))

autoplot(ts.month.te, series= "Real")
autoplot(forecastLM, series= "LM")

### Forecasting with chosen model (HW)
fitHW2 <- HoltWinters(ts.month)
forecastHW2 <- forecast(fitHW2, h=3, level=c(80,90))
autoplot(ts.month, series= "Real")+
  autolayer(forecastHW2, series = "HW")