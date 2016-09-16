library(shiny)
library(e1071)
library(dplyr)
library(tidyr)
library(ggplot2)
load("powstreamseg.Rda")
cbPalette <- c("#333333", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

dis = dd %>% group_by(site) %>% summarise(charge=mean(dischdaily))
dis$group = as.numeric(cut(log(dis$charge), breaks=c(-Inf,quantile(log(dis$charge),seq(0.2,1,by=0.2)))))


shinyServer(function(input, output) {

  moddat = reactive({
    sit = input$site
    dat = dd %>% filter(site==sit) %>% select(DateTime,gpp,er,K600)
    dat
  })

  model = reactive({
    err = ifelse(input$err == 0, 0.001, input$err)
    similarsites = dis$site[which(dis$group==dis$group[dis$site==input$site])] # the groups
    ddm = dd %>% filter(site%in%similarsites)
    # aaa = input$all
    # if(aaa){ddm = dd}else{ddm = moddat()}
    if(input$filt_gpp){ddm = ddm %>% filter(gpp > -1.5)}
    if(input$filt_er){ddm = ddm %>% filter(er < 1.5)}
    traindat = ddm %>% select(gpp,er,K600) %>% mutate_each(funs( delta=c(0,diff(.)) ))
    mod = svm(traindat, type='one-classification', kernel='radial',
                scale=TRUE, nu=err)
    mod
  })

  data = reactive({
    dat = moddat()
    mod = model()
    preddat = dat %>% select(gpp,er,K600) %>% mutate_each(funs( delta=c(0,diff(.)) ))
    flags = !predict(mod, preddat)
    dat$f = flags
    dat
  })

  output$numobs = renderText( nrow(data()) )
  output$numerr = renderText( length(which(data()$f)) )

  output$plot = renderPlot({
    din = data() %>% gather(variable, value, -DateTime, -f)
    ggplot(din, aes(DateTime, value, col=f)) +
      geom_point(shape=20,size=din$f*3+1) +
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
