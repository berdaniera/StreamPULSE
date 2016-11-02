library(shiny)
library(readr)
library(dplyr)
dat = read_csv("graphdata.csv")
dat$LocalTime = dat$DateTime_UTC
attributes(dat$LocalTime)$tzone <- "EST"

shinyServer(function(input, output, session) {
  s = reactive( parseQueryString(session$clientData$url_search)$s )
  d = reactiveValues() # master data
  dd = reactiveValues() # plotting data
  observe(if(!is.null(s())){
    output$sitename = renderUI( h3( paste(unlist(strsplit(s(),"_")),collapse=" ") ) )
    d$d = dat %>% filter(Site==s()) %>% arrange(LocalTime)
    dmin = as.Date(min(d$d$LocalTime))
    dmax = as.Date(max(d$d$LocalTime))
    dfrom = max(d$d$LocalTime) - 60*60*24*14
    dto = max(d$d$LocalTime)
    updateDateRangeInput(session, "daterange", start=as.Date(dfrom), end=as.Date(dto), min=dmin, max=dmax)

    dd$d = d$d %>% filter(LocalTime>dfrom & LocalTime<dto)

    observeEvent(input$update,{
      dfrom = as.POSIXct(paste(input$daterange[1],"00:00:01"),format="%Y-%m-%d %T",tz="EST")
      dto = as.POSIXct(paste(input$daterange[2],"23:59:00"),format="%Y-%m-%d %T",tz="EST")
      dd$d = d$d %>% filter(LocalTime>dfrom & LocalTime<dto)
    })

    output$pT = renderPlot({
      par(mar=c(4,4,0.5,0.5), las=1, cex=1.1)
      plot(dd$d$WaterTemp_C~dd$d$LocalTime,
        type="l", bty="l",
        xlab="", ylab="Temperature, C", lwd=2)
    })
    output$pD = renderPlot({
      par(mar=c(4,4,0.5,0.5), las=1, cex=1.1)
      plot(dd$d$fDOsat~dd$d$LocalTime,
        type="l", bty="l",
        xlab="", ylab="Fraction DO saturation", lwd=2)
    })
    output$pL = renderPlot({
      par(mar=c(4,4,0.5,0.5), las=1, cex=1.1)
      plot(dd$d$Depth_m~dd$d$LocalTime,
        type="l", bty="l",
        xlab="Date", ylab="Water depth, m", lwd=2)
    })

  })

})
