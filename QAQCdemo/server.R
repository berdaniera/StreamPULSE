library(tidyr)
library(ggplot2)
library(shiny)
library(e1071)
library('aws.s3')
cbPalette <- c("#333333", "deeppink", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

server = function(input, output,session){
  flags = reactiveValues(d=NULL)
  train = reactiveValues(d=NULL)

  #s3load('training.Rdata','streampulse') # load user dataset

  observeEvent(input$cleanButton, { # load the cleaner data into "flags"
    f = paste0("data/",input$cleanIn,".csv")
    d = read.csv(f)
    d$TIMESTAMP = as.POSIXct(as.character(d$TIMESTAMP))
    if(is.null(train$d)){ #exists("training")
      training = d
    }else{
      training = train$d
    }
    #     if(file.exists("data/traindat.Rdata")){ 
    #       load("data/traindat.Rdata")
    #       train$d = training # add training data to reactive value
    #     }else{
    #       training = d
    #     }
    # tune model
#     tuned = tune.svm(x=training[,c("HDO_mgL","Depth","TempC","Nitrate_mgL")], 
#                      y=rep(TRUE,nrow(training)), type='one-classification', kernel='radial',
#                      nu =  0.001, gamma = c(0.1,0.25,0.5,1), scale=TRUE)
#     print(tuned$best.parameters$gamma)
    # fit SVM
    svmod = svm(training[,c("HDO_mgL","Depth","TempC","Nitrate_mgL")],
                type='one-classification',kernel='radial',scale=TRUE,
                nu = 0.001, gamma = 0.25)
#                nu = tuned$best.parameters$nu, gamma = tuned$best.parameters$gamma)
    d$f = !predict(svmod,d[,c("HDO_mgL","Depth","TempC","Nitrate_mgL")])
    
    d2 = d %>% gather(variable, value, -TIMESTAMP, -RECORD, -f)
    d3 = d2[d2$variable%in%c("HDO_mgL","Depth","TempC","Nitrate_mgL"),]
    d3$variable = factor(d3$variable, levels = unique(d3$variable))
    flags$d = d3
  })
  
  # # ADD A NEW FLAG
  observeEvent(input$flag_new,{
    newflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
    print(table(newflags))
    if(any(newflags)) flags$d$f[newflags] = TRUE
    # need to enter text too
  })
  # # ERASE A FLAG
  observeEvent(input$flag_erase,{
    eraseflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
    if(any(eraseflags)) flags$d$f[eraseflags] = FALSE
  })  
  # CLEAR ALL FLAGS
  observeEvent(input$flag_clear,{ 
    flags$d$f = rep(FALSE,nrow(flags$d))
  })
  # SUBMIT FLAGS
  observeEvent(input$flag_send,{
    #remove rows with flags as true
    flagged = spread(flags$d[!flags$d$f,],variable,value)
    #spread data to create training data, append to existing training data
    training = rbind(train$d,flagged)
    # some day, might need to subsample data...
    train$d = training
    #save file as traindat
    #save(training,file="data/traindat.Rdata")
    output$loadtext = renderText(paste0("Added ",length(which(flags$d$f))," flags, choose another file..."))
    updateTextInput(session,'flag_comments',value="")
  })
  observe({
    if(!is.null(flags$d)){
      # PLOT THE DATA
      output$flag_plot <- renderPlot({
        ggplot(flags$d, aes(TIMESTAMP,value,col=f)) + 
          geom_point(shape=20,size=flags$d$f*4+1) + 
          facet_grid(variable~.,scales='free_y') + 
          theme(legend.position='none') + 
          scale_colour_manual(values=cbPalette)
        #    if(any(flags$f)) points(d$speed[flags$f],d$dist[flags$f],col="red",pch=19)
      }, height=150*length(unique(flags$d$variable)))
      output$loadtext = renderText("")
    }else{
      output$loadtext = renderText("Choose a file...")
    }
  })
}