library(shiny)
library(e1071)
library(dplyr)
library(tidyr)
library(ggplot2)
load("powstreamseg.Rda")
cbPalette <- c("#333333", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

shinyServer(function(input, output) {

  moddat = reactive({
    sit = input$site
    dat = dd %>% filter(site==sit) %>% select(DateTime,gpp,er,K600)
    dat
  })

  model = reactive({
    aaa = input$all
    fff = input$filt
    err = ifelse(input$err == 0, 0.001, input$err)
    if(aaa){ddm = dd}else{ddm = moddat()}
    if(fff){ddm = ddm %>% filter(gpp > -1.5 & er < 3)}
    mod = svm(ddm %>% select(gpp,er,K600), # can use eval(parse(text=input$cond))
                type='one-classification', kernel='radial',
                scale=TRUE, nu=err)
    mod
  })

  data = reactive({
    dat = moddat()
    mod = model()
    flags = !predict(mod, dat %>% select(gpp,er,K600))
    dat$f = flags
    dat
  })

  output$numobs = renderText( nrow(data()) )
  output$numerr = renderText( length(which(data()$f)) )

  output$plot = renderPlot({
    din = data() %>% gather(variable, value, -DateTime, -f)
    ggplot(din, aes(DateTime, value, col=f)) +
      geom_point(shape=20,size=din$f*6+1) +
      facet_grid(variable~.,scales='free_y') +
      theme(legend.position='none') +
      scale_colour_manual(values=cbPalette)
  })

  output$pairplot = renderPlot({
    cx = data()$f*2+1
    cx[is.na(cx)] = 0
    pairs(data() %>% select(gpp,er,K600), panel=function(x,y,...) points(x,y,cex=cx,pch=20,col=cbPalette[data()$f+1]))
  })

})
