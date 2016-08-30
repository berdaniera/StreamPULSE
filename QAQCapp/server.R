library(shiny)
library(e1071)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
cbPalette = c("#333333", "#E69F00", "#337ab7", "#56B4E9", "#739E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

shinyServer(function(input, output, session) {

######## DATA CONTROLS
  flags = reactiveValues(d=NULL,f=NULL) # placeholder for model flagged data
  # d is the data with a flag column
  # f is the flag list

  # Load new data
  dataup = reactive({
    inFile = input$file1
    print(input$file1)
    if (is.null(inFile))
      return(NULL)
    read_csv(inFile$datapath)
  })
  # datain = read_csv("/home/aaron/Documents/StreamPULSE/Joanna/W26_DO_lvl.csv")
  # If data is loaded, update variable list
  observe({
    if(!is.null(dataup())){
      dat = dataup()
      cvars = colnames(dat)[!colnames(dat)%in%c("DateTime","flags")]
      updateSelectizeInput(session, "vari", choices=cvars, server=TRUE)
      output$fitbutton = renderUI(actionButton("fitButton", "Fit model"))
    }
  })

######## MODEL CONTROLS
  # If the model directory exists, update model list
  if(dir.exists("previously_flagged_data/")){
    fdfiles = grep(".Rda", list.files("previously_flagged_data/"), value=TRUE)
    if(length(fdfiles) > 0){
      emods = strsplit(fdfiles, ".Rda")
      updateSelectizeInput(session, "predat", choices=c("Choose a dataset...",emods), server=FALSE)
    }
  }
  # Fit the model
  observeEvent(input$fitButton,{
    if( (input$traindat=="old"&input$predat%in%c("No data found...","Choose a dataset...")) |
      (input$traindat=="new"&is.null(input$vari)) ){
      # If nothing is selected in the open tab, throw an error
      output$fiterr = renderUI(HTML("<font color='red'><i>Please choose training variables or a previously-trained dataset.</i></font>"))
    }else{
      dat = dataup()
      output$fiterr = renderUI(HTML("<font color='green'><i>Model fitting...</i></font>"))
      if(input$traindat=="old"){ # If a pre-existing model is selected, load it
        load(paste0("previously_flagged_data/",input$predat,".Rda")) # load existing model objects
        # needs to have "mod" and "mcols"
        traindat = flagged
        mcols = colnames(traindat)
        updateSelectizeInput(session, "flag_name", choices=flagids, server=FALSE)
      }else{ # Else, fit a new model
        mcols = input$vari
        if(input$filt==""){
          traindat = dat %>% select_(.dots=mcols)
        }else{
          traindat = dat %>% filter_(.dots=input$filt) %>% select_(.dots=mcols)
        }
      }
      # check if all data columns in model are in data
      if(all(mcols%in%colnames(dat))){
        mod = svm(traindat, nu=input$nu, scale=TRUE, type='one-classification', kernel='radial')
        # predict flags for uploaded data
        dat$f = 0
        dat$f[!predict( mod, dat %>% select_(.dots=mcols) )] = 1 # flagged
        d = dat %>% select_(.dots=c("DateTime",mcols,"f")) %>% gather(variable, value, -DateTime, -f)
        d$variable = factor(d$variable, levels=unique(d$variable))
        flags$d = d
        output$fiterr = renderUI(HTML(""))
      }else{
        output$fiterr = renderUI(HTML(paste("<font color='red'>Model columns <i>",mcols[which(!mcols%in%colnames(dat))],"</i> not in loaded data.<br>Please check your data and reupload.</font>")))
      }
    }
  })

######## PLOT CONTROLS
  ranges = reactiveValues(x=NULL)
  observeEvent(input$plot_dblclick,{
    brush = input$plot_brush
    if(!is.null(brush)){
      ranges$x = as.POSIXct(c(brush$xmin, brush$xmax),origin='1970-01-01')
    }else{
      ranges$x = NULL
    }
  })
  observeEvent(input$flag_reset,{ ranges$x = NULL })
  observe({ # draw plot
    if(!is.null(flags$d)){
      output$flagplot = renderPlot({
        ptsz = rep(0.5,nrow(flags$d))
        ptsz[flags$d$f > 0] = 4
        ggplot(flags$d, aes(DateTime, value, col=factor(f))) +
          geom_point(shape=20,size=ptsz) +
          facet_grid(variable~.,scales='free_y') +
          theme(legend.position='none') +
          scale_colour_manual(values=cbPalette) +
          coord_cartesian(xlim=ranges$x)
      }, height=200*length(unique(flags$d$variable)))
    }
  })

######## FLAG CONTROLS
  # # ADD A NEW FLAG
  observeEvent(input$flag_new,{
    newflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
    if(any(newflags)) flags$d$f[newflags] = 1
  })
  # # ERASE A FLAG
  observeEvent(input$flag_erase,{
    eraseflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
    if(any(eraseflags)) flags$d$f[eraseflags] = 0
  })
  # CLEAR ALL UNSTORED FLAGS
  observeEvent(input$flag_clear,{
    flags$d$f[flags$d$f==1] = 0
    ranges$x = NULL
  })
  # STORE FLAGS
  observeEvent(input$flag_store,{
    storeflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
    if(any(storeflags) & !is.null(input$flag_name)){
      flags$d$f[which(storeflags&flags$d$f==1)] = 2 # stored!
      flaglist = list(ID=input$flag_name, comment=input$flag_comment, flags=flags$d$DateTime[storeflags])
      if(is.null(flags$f)){
        flags$f = list(flaglist)
      }else{
        flags$f = c(flags$f, list(flaglist))
      }
      flagids = unique(unlist(lapply(flags$f, function(x) x$ID))) # unique flags
      updateSelectizeInput(session, "flag_name", choices=flagids, server=FALSE)
    }else{
      output$loadtext = renderText("Please select flagged points and/or enter a flag name/ID")
    }
  })
  # SAVE FLAGS
  observeEvent(input$flag_save,{
    # need DF with model fit columns and rows that are not in flags
    # and a list of the unique flag names
    #remove rows with stored flags
    flagged = flags$d %>% filter(f!=2) %>% spread(variable,value) %>% select_(.dots=input$vari) # not flagged data
    flagids = unique(unlist(lapply(flags$f, function(x) x$ID))) # unique flags
    if(!dir.exists("previously_flagged_data/")) dir.create("previously_flagged_data/")
    savefn = function(){ paste0("previously_flagged_data/",sub("(.*)\\..*", "\\1", input$file1$name),"-",Sys.Date(), ".Rda") }
    save(flagged,flagids,file=savefn())
    output$loadtext = renderText(paste0("Saved ",length(flagids)," flags."))
    updateTextInput(session, "flag_comments", value="")
    updateSelectizeInput(session, "flag_name", choices=flagids, server=FALSE)
  })
  # DOWNLOAD FLAGGED DATA
  observeEvent(input$flag_download,{
    filename1 = function(){ paste0(sub("(.*)\\..*", "\\1", input$file1$name),"-FLAGGED-",Sys.Date(),".csv") }
    flagIDs = do.call("rbind", lapply(flags$f, function(x) data_frame(DateTime=x$flags,Flag=x$ID)))
    flaggeddat = left_join(dataup(),flagIDs, by="DateTime")
    filename2 = function(){ paste0(sub("(.*)\\..*", "\\1", input$file1$name),"-FLAGMETA-",Sys.Date(),".csv") }
    flaggedmeta = do.call("rbind", lapply(flags$f, function(x) data_frame(Flag=x$ID,MinDate=min(x$flags),MaxDate=max(x$flags),Comments=x$comment)))
    write_csv(flaggeddat, filename1)
    write_csv(flaggedmeta, filename2)
    output$loadtext = renderText(paste0("Saved ",filename1," and ",filename2," to current working directory."))
  })
  # output$flag_download = downloadHandler(
  #   filename = function(){ paste0(sub("(.*)\\..*", "\\1", input$file1$name),"-",Sys.Date(),".csv") },
  #   content = function(file){
  #     flagIDs = do.call("rbind", lapply(flags$f, function(x) data_frame(DateTime=x$flags,Flag=x$ID)))
  #     flaggeddat = left_join(dataup(),flagIDs, by="DateTime")
  #     write_csv(flaggeddat, file)
  #   }
  # )

######### EXAMPLE CODE
  observeEvent(input$egdata,{ # generate new random eg data
    y = runif(200,-3,2)
    x = rnorm(200,y*y,2)
    ytest = rnorm(100,mean(y),sd(y))
    xtest = rnorm(100,mean(x),sd(x))
    mod = svm(cbind(x,y), nu=0.01, scale=TRUE, type='one-classification', kernel='radial')
    pred = predict(mod,cbind(xtest,ytest))
    pcol = rep("#E69F00",100)
    pcol[pred] = "#009E73"
    output$egplot = renderPlot({
      par(mar=c(4,4,0,0))
      plot(x,y,pch=1,xlim=range(c(x,xtest)),ylim=range(c(y,ytest)),bty="n",las=1,xlab="V1",ylab="V2")
      if(input$egshow){
        points(xtest,ytest,pch=21,bg=pcol,cex=1.1)
      }
      legend("topright",c("Training data","Testing - Good","Testing - Anomaly"),pch=21,pt.bg=c("#ffffff","#009E73","#E69F00"),pt.cex=c(1,1.1,1.1))
    })

  })

})
