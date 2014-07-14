library(ggplot2)
install.packages("ggmap")
library(ggmap)
desired.format = matrix(c(44.96,44.96,44.96,44.96,44.96,
                         44.97,44.97,44.97,44.97,44.97,44.98,44.98,44.98,
                         44.98,44.98,44.99,44.99,44.99,44.99,44.99,45,45,
                         45,45,45,-93.31,-93.3,-93.29,-93.28,-93.27,-93.31,
                         -93.3,-93.29,-93.28,-93.27,-93.31,-93.3,-93.29,
                         -93.28,-93.27,-93.31,-93.3,-93.29,-93.28,-93.27,
                         -93.31,-93.3,-93.29,-93.28,-93.27,69,0,0,0,0,0,0,
                         0,0,0,0,306,0,0,173,0,0,0,198,0,0,0,68,0,0),
                       nrow=25, ncol=3)

colnames(desired.format) <- c("Lat", "Lon", "Count")
desired.format <- as.data.frame(desired.format)

minneapolis = get_map(location = "minneapolis, mn", zoom = 12)
ggmap(minneapolis) + geom_tile(data = desired.format, aes(x = Lon, y = Lat, alpha = Count), fill="red")
?get_map
italy <- get_map(location="italy", maptype="roadmap")
ggmap(italy)
italy <- get_map(location="italy", maptype="roadmap")
ggmap(italy)

# -----------------------------------------------------------------------------
getwd()
#old setwd("C:\\Users\\mmorelli\\Documents\\work\\Kaggle\\valued_shoppers")
setwd("C:\\Users\\mmorelli\\Documents\\work\\DP_Forecasting")

# perl -ne 'print if (rand() < .01)' biglist.txt > subset.txt
h <- read.csv("header.csv")
names(h)

tran <- read.csv("sample_big.csv", header=F)
dim(tran)
names(tran) <- names(h)
tran$dateTran <- as.Date(as.character(tran$date),format="%Y-%m-%d")
str(tran)
head(tran)
length(unique(tran$id)) # 36K uniq cust on 43K purchase     # --> 174K on 352 K
length(unique(tran$categ)) # 743 uniq categ on 43K purchase # --> 804  on 352 K
length(unique(tran$chain)) # 126 uniq chain on 43K purchase # --> 133  on 352 K
length(unique(tran$dept)) # 82 uniq categ on 43K purchase   # they stay 82

library(plyr)
library(ggplot2)
acqXdate <- ddply(tran, "dateTran" ,function(df){sum(df$purchaseamount)})
names(acqXdate)[2] <- "ValXDate"
ggplot(acqXdate, aes(x=dateTran, y=ValXDate))+geom_line()+geom_smooth()
?geom_smooth
# the particular form (reversed hockey stick) still is there with ten times the data...

# ------------------ calendar heatmpap with ggplot2
# see receipt in https://gist.githubusercontent.com/theHausdorffMetric/2387786/raw/margintale_blog_2.R
# acqXdate$year    <- as.numeric(as.POSIXlt(acqXdate$dateTran)$year+1900)
# acqXdate$month   <- as.numeric(as.POSIXlt(acqXdate$dateTran)$mon+1)
# acqXdate$monthf  <- factor(acqXdate$month, levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
# acqXdate$weekday <- as.POSIXlt(acqXdate$dateTran)$wday
# acqXdate$weekdayf<-factor(acqXdate$weekday,levels=rev(0:6),labels=rev(c("Sun" ,"Mon","Tue","Wed","Thu","Fri","Sat")),ordered=TRUE)
# 
# require(quantmod)
# acqXdate$yearmonth<-as.yearmon(acqXdate$dateTran)
# acqXdate$yearmonthf<-factor(acqXdate$yearmonth)
# # then find the "week of year" for each day
# acqXdate$week <- as.numeric(format(acqXdate$date,"%W"))
# # and now for each monthblock we normalize the week to start at 1 
# acqXdate<-ddply(acqXdate,.(yearmonthf),transform,monthweek=1+week-min(week))
# 
# P<- ggplot(acqXdate, aes(monthweek, weekdayf, fill = ValXDate)) + 
#   geom_tile(colour = "white") + facet_grid(year~monthf) + scale_fill_gradient(low="red", high="yellow") +
#   xlab("Week of Month") + ylab("")
# P
# ok, it works but it is ugly
# ------------------ calendar heatmpap with ggplot2

# ------------------ calendar heatmpap with ggplot2
# work in console but crash R studio with shiny
#library(googleVis)
#plot( 
# g <-    gvisCalendar(data=acqXdate, datevar="dateTran", numvar="ValXDate",
#                options=list(
#                  title="Calendar heat map of data",
#                  calendar="{cellSize:10,
# yearLabel:{fontSize:20, color:'#444444'},
# focusedCellColor:{stroke:'red'}}",
#                  width=590, height=320),
#                chartid="Calendar")
#       #)
# a <- print(g)
# ------------------ calendar heatmpap with ggplot2

# calendar heatmap with makeR
require(devtools)
install_github('makeR', 'jbryer')
library(makeR)
calendarHeat(acqXdate$dateTran, acqXdate$ValXDate)
# it works

# history not needed yet
hist <- read.csv("trainHistory.csv")
str(hist)
head(hist)
#---------------------------------------------------------------------


# --- working with time series date.
# ts not adapt with date. Better to use xts
# myts <- ts(data=acqXdate$ValXDate)
# plot.ts(myts)
# mytsComp <- decompose(myts)
# ?decompose
# plot(ValXDate ~ dateTran, acqXdate,  type = "l")
# ggplot( data = acqXdate, aes( dateTran, ValXDate )) + geom_line() 


install.packages("zoo")
install.packages("xts")

library(xts)
?xts
?zoo
?stl
myxts <- xts(x=acqXdate$ValXDate, order.by=acqXdate$dateTran)
  xts::periodicity(myxts)
  endpoints(myxts,on='weeks')
  plot(to.period(myxts, 'months'))
  plot(to.period(myxts, 'weeks'))

  str(myxts)
#plot(myxts, ticks.on='weeks')

?stl
library(forecast)
# Automated forecasting using an exponential model
 
fit <- auto.arima(myxts) 
# head(fit)
?auto.arima
# accuracy(fit)
# summary(fit)
xfor <- forecast(fit,30)

plot(xfor, axes=F) # traditional plot
axis(2)
l <- length(acqXdate$dateTran)
axis(1,at = pretty(1:l,n = 6),
     labels = (acqXdate$dateTran[1]-1) + pretty(1:l,n = 6),
     cex.axis = 0.65)
# axis problem


myxts
?forecast
----------------------------------------------
    # wyhdham
# ggplot(acqXdate, aes(x=dateTran, y=ValXDate))+geom_line()
# 
# require(zoo)
# require(forecast) # Needed for the ses function
# 
# acqXdate$ValXDate <- zoo(acqXdate$ValXDate, acqXdate$dateTran) # Allows for irregular dates
# plot(acqXdate$ValXDate, xlab="Date", ylab="Val") # Produce time plot
# acqXdate
# 
# ewma <- as.vector(fitted(ses(ts(acqXdate$ValXDate)))) # Compute ewma with parameter selected using MLE
# lines(zoo(ewma,acqXdate$date),col="red") # Add ewma line to plot    
    
    
    

# dat <- cumsum(rnorm(12*4))
# x <- ts(dat)
# stl(x, "periodic")
# xx <- ts(dat, frequency = 12)
# stl(xx, "periodic")
?reactive
?xts::periodicity
#---------------
library(shiny)

setwd("C:\\Users\\mmorelli\\Documents\\work\\DP_Forecasting")
runApp()

runApp("test")

# runGitHub('leaflet-shiny', "jcheng5")
# runGitHub("shiny_example", "rstudio")

#require(devtools)
#install_github('leaflet-shiny', 'jcheng5')

# it works on cloned directory
#setwd("C:\\Users\\mmorelli\\Documents\\work\\leaflet-shiny")
#runApp("inst\\examples\\choropleth")

?output
#
# TBD: find a list of: category, customers, product, ecc... geo
sort(unique(tran$dept))



#-----------------------------------------------------------------
# playground freitag
#-----------------------------------------------------------------
setwd("C:\\Users\\mmorelli\\Documents\\work\\DP_Forecasting")


# see also https://blenditbayes.shinyapps.io/heatmapStock/

setwd("C:\\Users\\mmorelli\\Documents\\work\\DP_Forecasting")
tran <- read.csv("sample_big.csv", header=F)
h <- read.csv("header.csv")
names(tran) <- names(h)
dn <- read.csv("random_dept.csv", sep=";")
# intersect(names(dn), names(tran))
tran <- merge(dn, tran)
#head(tran)
#unique(tran$deptName)
str(tran)
unique(tran$company)
length(unique(tran$chain))

# use chain as 

?column
?fluidRow
?uiOutput

# reproduce all
tran$dateTran <- as.Date(as.character(tran$date),format="%Y-%m-%d")
acqXdate <- ddply(tran, "dateTran" ,function(df){sum(df$purchaseamount)})
names(acqXdate)[2] <- "DailyVal"   
ggplot(acqXdate, aes(x=dateTran, y=DailyVal))+geom_line()+geom_smooth(method="loess")


myxts <- xts(x=acqXdate$DailyVal, order.by=acqXdate$dateTran)
# arima model
fit <- auto.arima(myxts) 

# forecast with the model 
xfor <- forecast(fit,30)
plot(xfor)


?forecast
summary(xfor)

# tried HW post 2012: http://davenportspatialanalytics.squarespace.com/blog/2012/3/21/plotting-forecast-objects-in-ggplot-part-2-visualize-observa.html
# doesn't work anymore

# cannibalize https://gist.github.com/fernandotenorio/3889834
serie.orig <- xfor$x
serie.fit  <- xfor$fitted
pi.strings <- paste(xfor$level, '%', sep = '')
dates      <- acqXdate$dateTran
serie.df   <- data.frame(date = dates, serie.orig = serie.orig, serie.fit = serie.fit)
forec.M    <- cbind(xfor$mean, xfor$lower[, 1:2], xfor$upper[, 1:2])
forec.df   <- as.data.frame(forec.M)
names(forec.df) <- c('forec.val', 'l0', 'l1', 'u0', 'u1')
forec.df$date <- max(dates) + 1:dim(forec.df)[1]
method <- xfor$method

# keep only the last 90 days
last <- dim(serie.df)[1]
serie.df <- serie.df[(last-90):last,]


p <-  ggplot() 
p <- p + geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) 
p <- p + geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df)
p <- p + scale_y_continuous() 
p <- p + geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) 
p <- p + geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) 
p <- p + geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df)
p <- p + ggtitle(paste("Forecasted with ", method))
p

?geom_ribbon
?as_data_frame
serie.df[(512-90):512,]
dim(serie.df)[1]
sort(unique(as.character(tran$deptName)))
tran$deptName
names(tran)
dx <- ddply(tran, "deptName" ,function(df){sum(df$purchaseamount)})
dx[order(-dx[,2]),]$deptName

# simulate Bundesland with variable chain
tran$code <- (tran$chain %% 16) +1
unique(tran$code)
tran <- merge(tran, pop)


filtland <- ddply(tran, "land" ,function(df){sum(df$purchaseamount)})
names(filtland)[2] <- "landVal"   

cut(merge(data.frame(land=gadm$NAME_1), filtland)$landVal, breaks=7)
?cut

col_no <- cut(merge(data.frame(land=gadm$NAME_1), filtland)$landVal)
gadm$col_no <- col_no





pop
pop <- read.csv("germanpop.csv", sep=";")

# ------------------------

