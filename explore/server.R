library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)

load("data.Rda")
load("powdata.Rda")
colnames(data) = c("Date","site","DOconcentration-mgL","Temperature-C","AbsolutePressure-kPa")

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output, session) {

  # Function for getting rows that are in brush
  brushedLogic = function(df,pb) c(df[pb$mapping$panelvar1]==pb$panelvar1 & df[pb$mapping$x]>pb$xmin & df[pb$mapping$x]<pb$xmax)
  ranges = reactiveValues(x=NULL)
  observeEvent(input$plot_dblclick,{
    brush = input$plot_brush
    if(!is.null(brush)){
      ranges$x = as.POSIXct(c(brush$xmin, brush$xmax),origin='1970-01-01')
    }else{
      ranges$x = NULL
    }
  })
  #observeEvent(input$flag_reset,{ ranges$x = NULL })
  observe({ # draw plot
    dat = data %>% filter(site %in% input$site) %>% gather(variable, value, -Date, -site)
    output$flagplot = renderPlot({
      ggplot(dat, aes(Date, value, colour=site, group=site)) +
        geom_line(lwd=1) +
        facet_wrap(~variable, ncol=1, scales='free_y') +
        theme(legend.position='top',
          axis.text=element_text(size=12),
          axis.title=element_text(size=14),
          legend.text=element_text(size=14),
          strip.text.x=element_text(size=14,hjust=0)) + # , legend.text=c()
        # scale_colour_manual(values=cbPalette, limits=levels(flags$d$f)) +
        coord_cartesian(xlim=ranges$x)
    }, height=600)
  })

  observe({
    output$powplot = renderPlot({
      insites = input$powsite
      # for each selected site, add lines
      par(mar=c(8,8,0,0))
      plot(0,0,type="n",xlim=c(0,15),cex.axis=1,cex.lab=1.5,ylim=c(-15,0),bty="n",xlab=expression('GPP gO'[2]*' m'^{-2}*' d'^{-1}),ylab=expression('ER gO'[2]*' m'^{-2}*' d'^{-1}),las=1)
      for(s in insites){
        X = powdata %>% filter(site==s & gpp>0 & er<0)
        z = kde2d(X$gpp, X$er, lims=c(0,quantile(X$gpp,0.95),quantile(X$er,0.05),0), n=100)
        contour(z, drawlabels=FALSE, levels=quantile(as.numeric(z$z),0.9), col="#000000", add=TRUE,lwd=2)
      }
      abline(0,-1,lty=2)
      abline(v=0,col="grey")
      abline(h=0,col="grey")
    })
  })

  observeEvent(input$contact,{
    cat(input$email,sep="\n",file=paste0("names",Sys.Date(),".txt"),append=TRUE)
    updateTextInput(session,"email","","")
  })

})
