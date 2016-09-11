
#render UI
flagui = fluidRow(
  wellPanel(
    fluidRow(
      column(width=6,h4(actionLink("taginstructions","Click here for instructions"))),
      column(width=6,div(style="display:inline-block", actionLink("QAQC","Reset and erase"), style="float:right"))
    ),
    fluidRow(column(width=12,
      conditionalPanel("input.taginstructions%2 == 1",
        HTML("<h3>Instructions:</h3>
        <i><b>TAGS</b> are interesting features that you recognize or recorded in field notes (e.g., a flood or certain type of disturbance).<br>
        <b>FLAGS</b> are warnings about possible data errors that should be noted in future analysis (note: a data point can only be flagged once).</i><br><br>
        <ul>
          <li>Highlight data by dragging and selecting on the graph.</li>
          <li><b>Remove NA values</b> by highlighting one value that is NA and clicking 'Mark and remove NA values' button. <i>Note:</i> you only need to highlight one NA value to mark all values that match it. This will permanently remove those data values from the data set.</li>
          <li><b>Add</b> or <b>Remove</b> flagged points by highlighting and clicking the 'Flag' and 'Un-flag' buttons below.</li>
          <li><b>Store</b> tags and flags by:<ol>
            <li><i>highlighting</i> the desired data,</li>
            <li><i>naming</i> the item (or choosing an appropriate previously-used flag/tag) and adding optional comments,</li>
            <li><i>clicking</i> 'Store selected flags' button.</li></ol>
          <li><b>Zoom</b> in on the graph by highlighting a time range and double clicking on the graph.</li>
          <li><b>When done:</b> Save the flagged data (suggestion: only do this once per file)</li>
        </ul>")
      )
    )),
    conditionalPanel("input.finishtagging==0",
      fluidRow(column(width=12,
        HTML(paste0("<h3>Tagging (optional) ","</h3>")), #,actionLink("skiptag","Skip/finish tagging")
        actionButton("tag_new", "Tag selected", icon=icon("flag"),
          style="color:#fff; background-color: #666666; border-color: #fff"),
        actionButton("tag_erase", "Un-tag selected", icon=icon("flag-o"),
          style="color:#fff; background-color: #CC79A7; border-color: #fff"),
        actionButton("flag_reset", "Reset zoom", icon=icon("repeat"),
          style="color: #fff; background-color: #ff7f50; border-color: #fff"),
        actionButton("tag_store", "Store selected tags", icon=icon("database"),
          style="color: #fff; background-color: #337ab7; border-color: #fff"),
        actionButton("na_rm", "Mark and remove NA values", icon=icon("times-circle"),
          style="color: #fff; background-color: #d55e00; border-color: #fff")
      )),br(),
      fluidRow(column(width=4,
        HTML("<i>IDs should be alphanumeric [A-Z, a-z, 0-9] and contain no spaces</i>"),
        selectizeInput("tag_name", "Tag name/ID (either choose or type to add):", choices = fnt$utags, options = list(create = TRUE))
      )),
      fluidRow(column(width=12,
        actionButton("finishtagging", "Finish tagging", style="color: #fff; background-color: #337ab7; border-color: #fff")
      ))
    ),

    conditionalPanel("input.finishtagging==1",
      fluidRow(column(width=12,
        HTML("<h3>Flagging</h3>"),
        actionButton("flag_new", "Flag selected", icon=icon("flag"),
          style="color:#fff; background-color: #E69F00; border-color: #fff"),
        actionButton("flag_erase", "Un-flag selected", icon=icon("flag-o"),
          style="color:#fff; background-color: #CC79A7; border-color: #fff"),
        actionButton("flag_clear", "Clear all unstored flags", icon=icon("eraser"),
          style="color: #fff; background-color: #009E73; border-color: #fff"),#d9534f
        actionButton("flag_reset", "Reset zoom", icon=icon("repeat"),
          style="color: #fff; background-color: #ff7f50; border-color: #fff"),
        actionButton("flag_store", "Store selected flags", icon=icon("database"),
          style="color: #fff; background-color: #337ab7; border-color: #fff")
      )),br(),
      fluidRow(
        column(width=4,
          HTML("<i>IDs should be alphanumeric [A-Z, a-z, 0-9] and contain no spaces</i>"),
          selectizeInput("flag_name", "Flag name/ID (either choose or type to add):", choices = fnt$uflags, options = list(create = TRUE))
        ),
        column(width=8,
          HTML("<br>"),
          textInput("flag_comments","Flag comment:",placeholder="Enter flag comments")
        )
      ),
      fluidRow(column(width=12,
        actionButton("flag_save", "Save stored flags/tags", icon=icon("folder"),
          style="color: #fff; background-color: #337ab7; border-color: #fff")
      ))
    ),
    h3(textOutput("loadtext"))
  )
)

output$flagplt = renderUI(
  fluidRow(column(width = 12,
    plotOutput("flagplot", height = "1500px", dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush",direction="x"))
  ))
)

# If qaqc button is hit
observeEvent(input$QAQC,{
  output$flagui = renderUI(flagui)
  dat = dataup()
  #### get the data that is in the training data columns
  dat = dat %>% select_(.dots=colnames(training$dat))
  dat$DateTimeUTC = as.POSIXct(dat$DateTimeUTC)
  dat = dat %>% filter(!duplicated(DateTimeUTC))

  # data automatically includes the change in each variable too...
  fittingdat = dat %>% select(-DateTimeUTC) %>% mutate_each(funs( delta=c(0,diff(.)) ))
  trainingdat = training$dat %>% select(-DateTimeUTC) %>% mutate_each(funs( delta=c(0,diff(.)) ))
  # fit model
  mod = svm(trainingdat, nu=0.01, scale=TRUE, type='one-classification', kernel='radial')
  # predict flags for uploaded data
  dat$f = 0
  dat$f[!predict(mod, fittingdat)] = 1 # flagged
  dat$f = factor(dat$f,levels=0:3)
  dat$t = 0.5 # tagged? 0.5=no
  d = dat %>% gather(variable, value, -DateTimeUTC, -f, -t)
  d$variable = factor(d$variable, levels=unique(d$variable))
  flags$d = d
  output$fiterr = renderUI(HTML(""))
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
      flags$d$t[flags$d$f != 0] = 4
      pltdat = flags$d %>% filter(!is.na(value))
      ggplot(pltdat, aes(DateTimeUTC, value, col=f)) +
        geom_point(shape=20,size=pltdat$t) +
        facet_grid(variable~.,scales='free_y') +
        theme(legend.position='top') + # , legend.text=c()
        scale_colour_manual(values=cbPalette, limits=levels(pltdat$f)) +
        coord_cartesian(xlim=ranges$x)
    }, height=150*length(unique(flags$d$variable)))
  }
})

######## TAG CONTROLS
# # ADD A TAG
observeEvent(input$tag_new,{
  newtags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
  if(any(newtags)) flags$d$t[newtags] = 4 # this is a TAG...
})
# # ERASE A TAG
observeEvent(input$tag_erase,{
  erasetags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
  if(any(erasetags)) flags$d$t[which(erasetags & flags$d$t==4)] = 0.5 # revert tags that were tags
})
# # STORE TAGS
observeEvent(input$tag_store,{
  storetags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
  if(any(storetags) & input$tag_name!=""){
    wtg = which(storetags & flags$d$t==4) # which tagged
    flags$d$f[wtg] = 3 # stored tag
    taglist = list(ID=input$tag_name, variable=input$plot_brush$panelvar1, source=site$id, by=USER$Name, tags=flags$d$DateTimeUTC[wtg])
    if(is.null(flags$t)){
      flags$t = list(taglist)
    }else{
      flags$t = c(flags$t, list(taglist))
    }
    fnt$utags = unique(c(fnt$utags,unlist(lapply(flags$t, function(x) x$ID)))) # unique flags
  }else{
    output$loadtext = renderText("Please select tagged points and/or enter a tag name/ID")
  }
})
# # REMOVE NA VALUES
observeEvent(input$na_rm,{
  navals = brushedPoints(df=flags$d, brush=input$plot_brush, allrows=TRUE)$selected_
  if(any(navals)){
    var = input$plot_brush$panelvar1
    wna = which(navals)
    nas = unique(flags$d$value[wna]) #the unique values in the NA brush
    flags$d$value[which(flags$d$value%in%nas & flags$d$variable == var)] = NA # assign all those matching values as NA
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
  if(any(eraseflags)){
    flags$d$f[eraseflags] = 0
    flags$d$t[eraseflags] = 0.5
  }
})
# CLEAR ALL UNSTORED FLAGS
observeEvent(input$flag_clear,{
  flags$d$t[flags$d$f==1] = 0.5
  flags$d$f[flags$d$f==1] = 0
  ranges$x = NULL
})
# STORE FLAGS
observeEvent(input$flag_store,{
  storeflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
  if(any(storeflags) & input$flag_name!=""){
    wflg = which(storeflags & flags$d$f==1)
    flags$d$f[wflg] = 2 # stored!
    flaglist = list(ID=input$flag_name, variable=input$plot_brush$panelvar1, comment=input$flag_comment, source=site$id, by=USER$Name, flags=flags$d$DateTimeUTC[wflg])
    if(is.null(flags$f)){
      flags$f = list(flaglist)
    }else{
      flags$f = c(flags$f, list(flaglist))
    }
    fnt$uflags = unique(c(fnt$uflags,unlist(lapply(flags$f, function(x) x$ID)))) # unique flags
  }else{
    output$loadtext = renderText("Please select flagged points and/or enter a flag name/ID")
  }
})


# SAVE FLAGS and TAGS
observeEvent(input$flag_save,{
  savefn = function(){ paste0("/",site$id,"-",Sys.Date()) }

  unflgd = flags$d %>% filter(f!=2) %>% select(-f, -t) %>% spread(variable, value)

  trainingdat = bind_rows(training$dat, unflgd) %>% distinct()
  #savefn = function(){ paste0(sub("(.*)\\..*", "\\1", input$file1$name),"-",Sys.Date(), ".Rda") }
  ffilePath = paste0(tempdir(),savefn(),".Rda")
  save(trainingdat, file=ffilePath)
  drop_upload(ffilePath, dest="SPtrainingdata", dtoken=dto)

  # add to master flags and tags
  tfilePath = paste0(tempdir(),"/flagsntags.Rda")
  utags = fnt$utags
  uflags = fnt$uflags
  save(utags, uflags, file=tfilePath)
  drop_upload(tfilePath, dest="SPflagsntags", dtoken=dto)
  atags = allfnt$atags
  aflags = allfnt$aflags
  if(length(flags$f)>0) aflags = c(aflags, flags$f)
  if(length(flags$t)>0) atags = c(atags, flags$t)
  if(!is.null(atags)|!is.null(aflags)){
    save(atags, aflags, file=ffilePath)
    drop_upload(ffilePath, dest="SPflagsntags", dtoken=dto)
  }

  # save formatted data in upload folder
  # IN THE FUTURE: Make correct format for cuahsi...
  ffilePath = paste0(tempdir(),savefn(),".csv")
  savealldat = flags$d %>% select(-t, -f)
  if(length(flags$f)>0){
    flagIDs = do.call("rbind", lapply(flags$f, function(x) data_frame(DateTimeUTC=x$flags,variable=x$variable,Flag=x$ID)))
    savealldat = left_join(savealldat, flagIDs, by=c("DateTimeUTC","variable")) # data with flag IDs
  }else{
    savealldat$Flag = NA
  }
  write_csv(savealldat, ffilePath)
  drop_upload(ffilePath, dest="SPreadyforupload", dtoken=dto)

  output$loadtext = renderText(paste0("The data were successfully saved. Thank you!"))
  updateTextInput(session, "flag_comments", value="")
})

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
