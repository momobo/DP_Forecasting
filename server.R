library(shiny)
library(ggplot2)
library(plyr)
library(makeR)
library(xts)
library(forecast)
library(sp)
library(RColorBrewer)

# read header
h <- read.csv("header.csv")

# read data
tran <- read.csv("sample_big.csv", header=F)
names(tran) <- names(h)
dn <- read.csv("random_dept.csv", sep=";")
tran <- merge(dn, tran)

# interpret date
tran$dateTran <- as.Date(as.character(tran$date),format="%Y-%m-%d")

mindate <- min(tran$dateTran)
maxdate <- max(tran$dateTran)

# read geo data
pop <- read.csv("germanpop.csv", sep=";")
# get spatial data for Germany on 1 level (länder)
con <- url("http://gadm.org/data/rda/DEU_adm1.RData")
print(load(con))
close(con)

# simulate Bundesland with variable chain
tran$code <- (tran$chain %% 16) +1
tran <- merge(tran, pop)



# prepare date for graph (temporary, should made dynamic and cached)
filt <- function(df, deptName, mind, maxd){
    if(deptName != "[[ ALL-dept ]]"){
        dff <- df[df$deptName == deptName & df$dateTran >= mind & df$dateTran < maxd,]
    }else{
        dff <- df[df$dateTran >= mind & df$dateTran < maxd,]
    }
    filtered <- ddply(dff, "dateTran" ,function(df){sum(df$purchaseamount)})
    names(filtered)[2] <- "DailyVal"   
    return(filtered)
}

filtland <- function(df, deptName, mind, maxd){
    if(deptName != "[[ ALL-dept ]]"){
        dff <- df[df$deptName == deptName & df$dateTran >= mind & df$dateTran < maxd,]
    }else{
        dff <- df[df$dateTran >= mind & df$dateTran < maxd,]
    }
    filtland <- ddply(dff, "land" ,function(df){sum(df$purchaseamount)})
    names(filtland)[2] <- "landVal"   
    return(filtland)
}

depts     <- c("[[ This doesn't work ]]" , sort(unique(tran$dept)))
# order from the biggest down
dx <- ddply(tran, "deptName" ,function(df){sum(df$purchaseamount)})
deptNames <- c("[[ ALL-dept ]]" , as.character(dx[order(-dx[,2]),]$deptName))


shinyServer(function(input, output) {
    
#     output$text1 <- renderText({ 
#         paste("You have selected", paste(input$deptName, ";", input$dates[1], ";", input$dates[2]))
#     })

    # Drop-down selection box for which data set
    output$choose_dept_name <- renderUI({
        selectInput("deptName", "Department Names", as.list(deptNames))
    })
    
    # date Range
    output$choose_time <- renderUI({
        dateRangeInput("dates", label = "Date range", 
                       start = mindate, end = maxdate, min = mindate, max = maxdate)
    })
    
    # BASE PLOT
    output$distPlot <- renderPlot({
    
        if(is.null(input$deptName)){PLOT <- FALSE}else{PLOT <- TRUE}
        if(PLOT){
            
            # result not cached, slow:
            acqXdate <- filt(tran, input$deptName, format(input$dates[1]), format(input$dates[2]) )
            # condition to plotting
            
            # plotting it (line)
            g <- ggplot(acqXdate, aes(x=dateTran, y=DailyVal))+geom_line() + geom_smooth(method="loess")
    
            # print is necessary for ggplot2
            print(g)
        } 
    })
    
    output$heatmapchart<- renderPlot({
     # result not cached, slow:
        acqXdate <- filt(tran, input$deptName, format(input$dates[1]), format(input$dates[2]))
     # calendar heatmap (from makeR)
        calendarHeat(acqXdate$dateTran, acqXdate$DailyVal)
   
    })
 
    output$forecastplot <- renderPlot({
        # result not cached, slow:
        acqXdate <- filt(tran, input$deptName, format(input$dates[1]), format(input$dates[2]))
        # calendar heatmap (from makeR)
        myxts <- xts(x=acqXdate$DailyVal, order.by=acqXdate$dateTran)
        # arima model
        fit <- auto.arima(myxts) 
     
        # forecast with the model, with the chosen horizont
        xfor <- forecast(fit,input$checkGroup)
     

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

output$geoplot <- renderPlot({
    
    if(is.null(input$deptName)){PLOT <- FALSE}else{PLOT <- TRUE}
    if(PLOT){
        acqXland <- filtland(tran, input$deptName, format(input$dates[1]), format(input$dates[2]))
      
        # prepare 
        myPalette<-brewer.pal(7,"Purples")
        # this works
#         col_no <- cut(merge(data.frame(land=gadm$NAME_1), pop)$population, 
#                       c(500000, 1000000, 2000000, 4000000, 8000000, 16000000, 32000000))
#         levels(col_no) <- c(">0.5M", "0,5-1M", "1-2M", "2-4M", "4-8M", "8-16M", "<16M")

        col_no <- cut(merge(data.frame(land=gadm$NAME_1), acqXland)$landVal, breaks=7)
        gadm$col_no <- col_no
        
        # plotting it (geo)

        spplot(gadm, zcol="col_no", col=grey(.9), col.regions=myPalette, main="Value per Land")

    } 
})

 
})
