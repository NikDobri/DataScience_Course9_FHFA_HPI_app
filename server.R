library(shiny)
shinyServer(
        function(input, output) {
shortdata <- read.csv("./data/metro_short.csv")

output$plot1 <- renderPlot({
        # Date
        dataX <- shortdata$YYYY.Q
        
        # Metro areas t display
        Denver <- if(input$Denver){shortdata$Denver}
        NewYork <- if(input$NewYork){shortdata$NewYork}
        Miami <- if(input$Miami){shortdata$Miami}
        SanFrancisco <- if(input$SanFrancisco){shortdata$SanFrancisco}
        
        dataY <- cbind(Denver,NewYork,Miami,SanFrancisco)
        
        # plot
        matplot(dataX, dataY, pch=19, xlab="Time, quarterly", ylab="FHFA HPI", 
        main="Figure: FHFA House Price Index, Metro-area", xlim=c(1991.1,2016.4),
        ylim=c(88,500), type = "l")#, col=c(1,2,3,4))

         if (length(colnames(dataY))==1) {legend("topleft", inset=.05, legend=colnames(dataY)[1], pch=1,  horiz=TRUE, col=1)}
         else if (length(colnames(dataY))==2) {legend("topleft", inset=.05, legend=c(colnames(dataY)[1],colnames(dataY)[2]), pch=1,  horiz=TRUE, col=c(1,2))}
         else if (length(colnames(dataY))==3) {legend("topleft", inset=.05, legend=c(colnames(dataY)[1],colnames(dataY)[2],colnames(dataY)[3]), pch=1,  horiz=TRUE, col=c(1,2,3))}
         else legend("topleft", inset=.05, legend=c(colnames(dataY)[1],colnames(dataY)[2],colnames(dataY)[3],colnames(dataY)[4]), pch=1,  horiz=TRUE, col=c(1,2,3,4))

})                


output$plot2 <- renderPlot({
#shortdata <- read.csv("./data/metro_short.csv")

        # subset data
        shortdata1 <- shortdata[as.numeric(shortdata$YYYY.Q)>=2001.3 & as.numeric(shortdata$YYYY.Q)<=2016.4 & !is.na(shortdata$YYYY.Q),]

        Y11 <- if(input$metro == "Denver"){shortdata1$Denver}
        Y12 <- if(input$metro == "NewYork"){shortdata1$NewYork}
        Y13 <- if(input$metro == "Miami"){shortdata1$Miami}
        Y14 <- if(input$metro == "SanFrancisco"){shortdata1$SanFrancisco}

        Y21 <- if(input$econ == "RealGDP" & input$lag == "0"){shortdata1$RealGDP}
        Y22 <- if(input$econ == "RealGDP" & input$lag == "2"){shortdata1$RealGDP.L2}
        Y23 <- if(input$econ == "RealGDP" & input$lag == "3"){shortdata1$RealGDP.L3}
        Y24 <- if(input$econ == "RealGDP" & input$lag == "4"){shortdata1$RealGDP.L4}
        Y25 <- if(input$econ == "RealGDP" & input$lag == "6"){shortdata1$RealGDP.L6}

        Y26 <- if(input$econ == "CaseShiller_HPI" & input$lag == "0"){shortdata1$CaseShiller_HPI}
        Y27 <- if(input$econ == "CaseShiller_HPI" & input$lag == "2"){shortdata1$CaseShiller_HPI.L2}
        Y28 <- if(input$econ == "CaseShiller_HPI" & input$lag == "3"){shortdata1$CaseShiller_HPI.L3}
        Y29 <- if(input$econ == "CaseShiller_HPI" & input$lag == "4"){shortdata1$CaseShiller_HPI.L4}
        Y210 <- if(input$econ == "CaseShiller_HPI" & input$lag == "6"){shortdata1$CaseShiller_HPI.L6}

        Y211 <- if(input$econ == "UnemplRate" & input$lag == "0"){shortdata1$UnemplRate}
        Y212 <- if(input$econ == "UnemplRate" & input$lag == "2"){shortdata1$UnemplRate.L2}
        Y213 <- if(input$econ == "UnemplRate" & input$lag == "3"){shortdata1$UnemplRate.L3}
        Y214 <- if(input$econ == "UnemplRate" & input$lag == "4"){shortdata1$UnemplRate.L4}
        Y215 <- if(input$econ == "UnemplRate" & input$lag == "6"){shortdata1$UnemplRate.L6}

        dataX1 <- shortdata1$YYYY.Q

        # library
        library(latticeExtra)

        # create data
        data <- data.frame(cbind(dataX1,Y11,Y12,Y13,Y14,Y21,Y22,Y23,Y24,Y25,Y26,Y27,Y28,Y29,Y210,Y211,Y212,Y213,Y214,Y215))
        names(data) <- c("Date", "MetroArea", "MacroEcon")

        # --> construct separate plots for each series
        obj1 <- xyplot(MetroArea ~ Date, data, type = "l" , lwd=2)
        obj2 <- xyplot(MacroEcon ~ Date, data, type = "l", lwd=2)

        ## 3=== Two y-axis graph with a key legend
        doubleYScale(obj1, obj2, text = c("Metro-area", "Macro-econ") , add.ylab2 = TRUE)
})


output$text <- renderText({
        # subset data
        shortdata1 <- shortdata[as.numeric(shortdata$YYYY.Q)>=2001.3 & as.numeric(shortdata$YYYY.Q)<=2016.4 & !is.na(shortdata$YYYY.Q),]
        
        Y11 <- if(input$metro == "Denver"){shortdata1$Denver}
        Y12 <- if(input$metro == "NewYork"){shortdata1$NewYork}
        Y13 <- if(input$metro == "Miami"){shortdata1$Miami}
        Y14 <- if(input$metro == "SanFrancisco"){shortdata1$SanFrancisco}
        
        Y21 <- if(input$econ == "RealGDP" & input$lag == "0"){shortdata1$RealGDP}
        Y22 <- if(input$econ == "RealGDP" & input$lag == "2"){shortdata1$RealGDP.L2}
        Y23 <- if(input$econ == "RealGDP" & input$lag == "3"){shortdata1$RealGDP.L3}
        Y24 <- if(input$econ == "RealGDP" & input$lag == "4"){shortdata1$RealGDP.L4}
        Y25 <- if(input$econ == "RealGDP" & input$lag == "6"){shortdata1$RealGDP.L6}
        
        Y26 <- if(input$econ == "CaseShiller_HPI" & input$lag == "0"){shortdata1$CaseShiller_HPI}
        Y27 <- if(input$econ == "CaseShiller_HPI" & input$lag == "2"){shortdata1$CaseShiller_HPI.L2}
        Y28 <- if(input$econ == "CaseShiller_HPI" & input$lag == "3"){shortdata1$CaseShiller_HPI.L3}
        Y29 <- if(input$econ == "CaseShiller_HPI" & input$lag == "4"){shortdata1$CaseShiller_HPI.L4}
        Y210 <- if(input$econ == "CaseShiller_HPI" & input$lag == "6"){shortdata1$CaseShiller_HPI.L6}
        
        Y211 <- if(input$econ == "UnemplRate" & input$lag == "0"){shortdata1$UnemplRate}
        Y212 <- if(input$econ == "UnemplRate" & input$lag == "2"){shortdata1$UnemplRate.L2}
        Y213 <- if(input$econ == "UnemplRate" & input$lag == "3"){shortdata1$UnemplRate.L3}
        Y214 <- if(input$econ == "UnemplRate" & input$lag == "4"){shortdata1$UnemplRate.L4}
        Y215 <- if(input$econ == "UnemplRate" & input$lag == "6"){shortdata1$UnemplRate.L6}
        
        dataX1 <- shortdata1$YYYY.Q

        # create data
        data <- data.frame(cbind(dataX1,Y11,Y12,Y13,Y14,Y21,Y22,Y23,Y24,Y25,Y26,Y27,Y28,Y29,Y210,Y211,Y212,Y213,Y214,Y215))
        names(data) <- c("Date", "MetroArea", "MacroEcon")
        
        # calculate the text
#        paste("dimensions", dim(data))})
        paste("The correlation is", round(cor(data$MetroArea,data$MacroEcon),2))})
#         paste("dimensions", dim(shortdata1),length(dataX1),length(Y11),length(Y21))})
})
