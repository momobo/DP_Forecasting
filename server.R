library(shiny)
library(ggplot2)
library(plyr)
library(makeR)
library(xts)
library(forecast)

# read header
h <- read.csv("header.csv")

# read data
tran <- read.csv("sample_big.csv", header=F)
names(tran) <- names(h)
dn <- read.csv("random_dept.csv", sep=";")
tran <- merge(dn, tran)

# interpret date
tran$dateTran <- as.Date(as.character(tran$date),format="%Y-%m-%d")

# prepare date for graph (temporary, should made dynamic and cached)
filt <- function(df, deptName){
    if(deptName != "[[ ALL-dept ]]"){
        dff <- df[df$deptName == deptName, ]
    }else{
        dff <- df
    }
    filtered <- ddply(dff, "dateTran" ,function(df){sum(df$purchaseamount)})
    names(filtered)[2] <- "DailyVal"   
    return(filtered)
}

depts     <- c("[[ This doesn't work ]]" , sort(unique(tran$dept)))
# order from the biggest down
dx <- ddply(tran, "deptName" ,function(df){sum(df$purchaseamount)})
deptNames <- c("[[ ALL-dept ]]" , as.character(dx[order(-dx[,2]),]$deptName))


shinyServer(function(input, output) {
    
    # Drop-down selection box for which data set
    output$choose_dept_name <- renderUI({
        selectInput("deptName", "Department Names", as.list(deptNames))
    })
    
    # BASE PLOT
    output$distPlot <- renderPlot({
    
        if(is.null(input$deptName)){PLOT <- FALSE}else{PLOT <- TRUE}
        if(PLOT){
            
            # result not cached, slow:
            acqXdate <- filt(tran, input$deptName)
            # condition to plotting
            
    
            # plotting it (line)
            g <- ggplot(acqXdate, aes(x=dateTran, y=DailyVal))+geom_line() + geom_smooth(method="loess")
    
            # print is necessary for ggplot2
            print(g)
        } 
    })
    
    output$heatmapchart<- renderPlot({
     # result not cached, slow:
        acqXdate <- filt(tran, input$deptName)
     # calendar heatmap (from makeR)
        calendarHeat(acqXdate$dateTran, acqXdate$DailyVal)
   
    })
 
    output$forecastplot <- renderPlot({
        # result not cached, slow:
        acqXdate <- filt(tran, input$deptName)
        # calendar heatmap (from makeR)
        myxts <- xts(x=acqXdate$DailyVal, order.by=acqXdate$dateTran)
        # arima model
        fit <- auto.arima(myxts) 
     
        # forecast with the model, with the chosen horizont
        xfor <- forecast(fit,input$checkGroup)
     
#         # plot (axis shenanigan to have date in x)
#         plot(xfor, axes=F) # traditional plot
#         axis(2)
#         l <- length(acqXdate$dateTran)
#         axis(1,at = pretty(1:l,n = 6), labels = (acqXdate$dateTran[1]-1) + pretty(1:l,n = 6), cex.axis = 0.65)

# I have cannibalized https://gist.github.com/fernandotenorio/3889834
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
# for better show keep only the last 90 days
        last <- dim(serie.df)[1]
        serie.df <- serie.df[(last-90):last,]

        p <-  ggplot() 
        p <- p + geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) 
        p <- p + geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df)
        p <- p + scale_y_continuous() 
        p <- p + geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) 
        p <- p + geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) 
        p <- p + geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df)
        p <- p + ggtitle(paste(input$checkGroup, " days forecast with ", method, "\n(last 90 days)")) 
        print(p)

 })
 
})
