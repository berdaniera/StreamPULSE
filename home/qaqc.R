# training = reactiveValues(dat=NULL)
flags = reactiveValues(s=NULL,d=NULL,f=NULL) # placeholder for model flagged data
# # s is the site name
# # d is the data with a flag column (f)
#   # 0 is not flagged
#   # 1 is flagged
#   # 2 flagged and stored
# # f is the flag list for that site
#
# # master list of flags
item_file_download(sbmpath,names='SPflags.Rda',destinations=file.path(tmpwebfile,'SPflags.Rda'), overwrite_file=TRUE, session=asb)
load(file.path(tmpwebfile,'SPflags.Rda')) # master list of flags
allflags = reactiveValues(uflags=flaglist) # list of all flags
metafiles = item_list_files(sbmpath)$fname

# select a site - dropdown, just like download page
output$qaqcsite = renderUI({
  sites = sub("^(.*_.*)_[0-9]*-.*\\.Rda","\\1",dnld$ff) # the sites included in the download data
  sitenms = sort(sitenames[match(sites, allsites$SITEID)]) # the site names
  tagList(
    span(HTML("<h4>Choose a site to begin:</h4>")),
    div(style="display:inline-block; vertical-align:top;",
      selectizeInput("toqaqc", NULL, sitenms, multiple = FALSE, options = NULL)),
    div(style="display:inline-block; vertical-align:top;",
      actionButton("qaqcit","Flag this data", class="color: #fff; background-color: #337ab7; border-color: #fff")),
    span(HTML("<p>If you have administrative privileges for this site then you can edit the master flags. Otherwise, you can create your own flags and save the flagged data for your own use. If you believe you should have administrative privileges, please <a href='mailto:aaron.berdanier@gmail.com'>email Aaron</a>.</p>"))
  )
})

observeEvent(input$qaqcit, {
  stid = allsites$SITEID[which(sitenames==input$toqaqc)] # get siteid from name
  flags$s = stid
  # load data from that site
  f = grep(stid, dnld$ff, value=TRUE)
  gf = item_file_download(sbrpath, names=f, destinations=file.path(tdatf,f), overwrite_file=TRUE, session=asb) # save locally
  for(f in gf) load(f)
  file.remove(dir(tdatf, full.names=TRUE)) # remove locally
  lf = ls()
  objs = grep(stid, lf, value=TRUE)
  tmpl = lapply(objs, function(o){
    get(o) %>% gather(variable, value, -DateTime_UTC) %>% filter(!is.na(value))
  })
  tmpd = Reduce(function(df1,df2) bind_rows(df1,df2), tmpl) %>% distinct() %>% arrange(variable, DateTime_UTC) %>% mutate(Flag=0)# stack them up
  tmpd$Flag = factor(tmpd$Flag,levels=0:2)

  # check user credentials
  credentials = FALSE
  if(input$userName %in% names(users$cred)){ # user has credentials
    if(stid %in% users$cred[[which(names(users$cred)==input$userName)]]){ # user has correct credentials
      credentials = TRUE
      # load existing flags if they exist
      if(paste0(stid,"_flags.Rda")%in%metafiles){
        item_file_download(sbmpath,names=paste0(stid,"_flags.Rda"),destinations=file.path(tmpwebfile,paste0(stid,"_flags.Rda")), overwrite_file=TRUE, session=asb)
        load(file.path(tmpwebfile,paste0(stid,"_flags.Rda"))) # master list of flags for that site
        # cut data to after last flags
        mxdt = max(unlist(lapply(get(paste0(stid,"_flags")), function(o) o$flags))) # max date from previous flags
        tmpd = tmpd %>% filter(DateTime_UTC > mxdt)
        flags$f = get(paste0(stid,"_flags"))
      }
      #flaglist = list(siteID = site, flagID=input$flag_name, by=USER$Name, variable=input$plot_brush$panelvar1, comment=input$flag_comment, flags=flags$d$DateTimeUTC[wflg])
      #fl = list(list(flags="a"), list(flags=c("1","3","4")), list(fx="A"))
    }
  }
  flags$d = tmpd # store the temp data in the flag list
  if(credentials){ # save and download
    qaqcsaves = HTML(paste(
        actionButton("flag_save", "Save stored flags", icon=icon("folder"), style="color: #fff; background-color: #337ab7; border-color: #fff"),
        downloadButton("flag_dnld", "Download flagged data")
      ))
  }else{
    qaqcsaves = downloadButton("flag_dnld", "Download flagged data")
  }

  #render UI
  qaqcui = column(width=12,
    # fluidRow(
    #   selectizeInput("flag_variables", "Variables to display:", placeholder="ID: alphanumeric [A-Z,a-z,0-9] no spaces",
    #     width="100%", selected=unique(flags$d$variable), choices = unique(flags$d$variable), options = list(create = TRUE))
    # ),
    fluidRow(
      column(width=6,h4(actionLink("taginstructions","Click here for instructions"))),
      column(width=6,div(actionLink("resetall","Reset and clear"), style="float:right"))
    ),
    fluidRow(column(width=12,
      conditionalPanel("input.taginstructions%2 == 1",
        HTML("<h3>Instructions:</h3>
        <i>Include notes about possible data issues or interesting features. Note: a data point can only be marked with one flag at a time.</i>
        <ul>
          <li>Highlight data by dragging and selecting on the graph. <i>Zoom in</i> on the graph by highlighting a time range and double clicking on the graph. <i>Zoom out</i> by double clicking on a non-highlighted area.</li>
          <li><b>Add</b> or <b>Remove</b> marked points by highlighting and clicking the 'Flag' and 'Un-flag' buttons below.</li>
          <li><b>Remove NA values</b> by highlighting one value that is NA and clicking 'Mark and remove NA values' button. You only need to highlight one value to mark all values that match it. <font color='red'>Caution:</font> this will remove those values from the data set.</li>
          <li><b>Store</b> a flag by:<ol>
            <li><i>highlighting</i> the desired data,</li>
            <li><i>naming</i> the item (or choosing an appropriate previously-used flag) and adding optional comments, and</li>
            <li><i>clicking</i> the 'Store' button.</li></ol>
          <li><b>When done:</b> Save or download the flagged data.</li>
        </ul>")
      )
    )),
    fluidRow(column(width=12,
      actionButton("flag_new", "Flag selected", icon=icon("flag"),
        style="color:#fff; background-color: #E69F00; border-color: #fff"),
      actionButton("flag_erase", "Un-flag selected", icon=icon("flag-o"),
        style="color:#fff; background-color: #CC79A7; border-color: #fff"),
      actionButton("flag_clear", "Clear all unstored flags", icon=icon("eraser"),
        style="color: #fff; background-color: #009E73; border-color: #fff"),#d9534f
      actionButton("flag_store", "Store selected flags", icon=icon("database"),
        style="color: #fff; background-color: #337ab7; border-color: #fff"),
      actionButton("na_rm", "Mark and remove NA values", icon=icon("times-circle"),
        style="color: #fff; background-color: #d55e00; border-color: #fff")
    )),br(),
    fluidRow(
      column(width=4,
        HTML("<i>Flag IDs should be alphanumeric [A-Z,a-z,0-9] and contain no spaces</i>"),
        selectizeInput("flag_name", "Flag name/ID (either choose or type to add):",
          width="100%", selected=NULL, choices = allflags$uflags, options = list(create = TRUE))
      ),
      column(width=8,
        br(),
        textInput("flag_comments","Flag comment:",placeholder="Enter flag comments",width="100%")
      )
    ),
    fluidRow(column(width = 12, qaqcsaves ))
  )
  # render the flagging UI
  output$qaqcinterface = renderUI(qaqcui)
  output$qaqcplt = renderUI(
    fluidRow(column(width = 12,
      br(),
      plotOutput("flagplot", height = "1500px", dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush",direction="x"))
    ))
  )
})

# Function for getting rows that are in brush
brushedLogic = function(df,pb) c(df[pb$mapping$panelvar1]==pb$panelvar1 & df[pb$mapping$x]>pb$xmin & df[pb$mapping$x]<pb$xmax)

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
      ggplot(flags$d, aes(DateTime_UTC, value, col=Flag)) +
        geom_point(shape=20,size=1) +
        facet_grid(variable~.,scales='free_y') +
        theme(legend.position='none') + # , legend.text=c()
        scale_colour_manual(values=cbPalette, limits=levels(flags$d$Flag)) +
        coord_cartesian(xlim=ranges$x)
    }, height=200*length(unique(flags$d$variable)))
  }
})

######## FLAG CONTROLS
observeEvent(input$resetall, {
  flags$d$Flag[which(flags$d$Flag!=0)] = 0
  if(exists(paste0(flags$s,"_flags"))){
    flags$f = get(paste0(flags$s,"_flags"))
  }else{
    flags$f = NULL
  }
})
# # ADD A NEW FLAG
observeEvent(input$flag_new,{
  newflags = brushedLogic(flags$d, input$plot_brush)
  if(any(newflags)) flags$d$Flag[newflags] = 1
})
# # ERASE A FLAG
observeEvent(input$flag_erase,{
  eraseflags = brushedLogic(flags$d, input$plot_brush)
  if(any(eraseflags)) flags$d$Flag[eraseflags] = 0
})
# CLEAR ALL UNSTORED FLAGS
observeEvent(input$flag_clear,{
  flags$d$Flag[flags$d$Flag==1] = 0
  ranges$x = NULL
})
# STORE FLAGS
observeEvent(input$flag_store,{
  storeflags = brushedLogic(flags$d, input$plot_brush)
  #storeflags = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
  if(any(storeflags) & input$flag_name!=""){
    wflg = which(storeflags & flags$d$Flag==1)
    flags$d$Flag[wflg] = 2 # stored!
    flaglist = list(siteID = flags$s, flagID=input$flag_name, by=input$userName, variable=input$plot_brush$panelvar1, comment=input$flag_comment, flags=flags$d$DateTime_UTC[wflg])
    if(is.null(flags$f)){
      flags$f = list(flaglist)
    }else{
      flags$f = c(flags$f, list(flaglist))
    }
    allflags$uflags = unique(c(allflags$uflags,unlist(lapply(flags$f, function(x) x$flagID)))) # unique flags
  }else{
    output$qaqctext = renderText("Please select flagged points and/or enter a flag name/ID")
  }
})
# # REMOVE NA VALUES
observeEvent(input$na_rm,{
  navals = brushedLogic(flags$d, input$plot_brush)
  # navals = brushedPoints(df=flags$d, brush=input$plot_brush, allRows=TRUE)$selected_
  if(any(navals)){
    var = input$plot_brush$panelvar1
    nas = unique(flags$d$value[navals]) #the unique values in the NA brush
    flags$d$value[which(flags$d$value %in% nas & flags$d$variable == var)] = NA # assign all those matching values as NA
  }
})


# SAVE FLAGS
observeEvent(input$flag_save,{
  # add to master flag list
  flaglist = allflags$uflags
  save(flaglist, file.path(tmpwebfile,'SPflags.Rda'))
  item_replace_files(sbmpath,files=file.path(tmpwebfile,'SPflags.Rda'), session=asb)

  # save the flag data
  assign(paste0(flags$s,"_flags"),flags$f)
  save(paste0(flags$s,"_flags"), file.path(tmpwebfile,paste0(flags$s,"_flags.Rda")))
  # if it existed, replace it, if not, add it
  if(paste0(flags$s,"_flags.Rda")%in%metafiles){
    item_replace_files(sbmpath,files=file.path(tmpwebfile,paste0(flags$s,"_flags.Rda")), session=asb)
  }else{
    item_append_files(sbmpath,files=file.path(tmpwebfile,paste0(flags$s,"_flags.Rda")), session=asb)
  }

  # clear it out
  file.remove(dir(tempwebfile, full.names=TRUE)) # remove locally
  ranges$x = NULL
  output$qaqctext = renderText(paste0("Your flags were successfully saved. Thank you!"))
  updateTextInput(session, "flag_comments", value="")
})

output$flagdnld = downloadHandler(
  filename = function() { paste0('StreamPULSE_',Sys.Date(),'_flagged.csv') },
  content = function(file) {
    zip(file, "where the files are")
    sitestodl = allsites$SITEID[which(sitenames%in%input$todnld)] # get siteid from name
    for(sf in sitestodl){
      f = grep(stid, dnld$ff, value=TRUE)
      gf = item_file_download(sbrpath, names=f, destinations=file.path(tdatf,f), overwrite_file=TRUE, session=asb) # save locally
      for(f in gf) load(f) # load that site locally
    }
    file.remove(dir(tdatf, full.names=TRUE)) # remove locally
    lf = ls()
    objs = unlist(sapply(sitestodl, function(sf) grep(sf, lf, value=TRUE)))
    tmpl = lapply(objs, function(o){
      get(o) %>% mutate(Site=sub("^(.*_.*)_[0-9]*-.*","\\1",o)) %>%
      gather(variable, value, -DateTime_UTC, -Site) %>% filter(!is.na(value))
    })
    tmpd = Reduce(function(df1,df2) bind_rows(df1,df2), tmpl) %>% distinct() %>% arrange(Site, variable, DateTime_UTC) # stack them up
    write_csv(tmpd, file)
  }
)
