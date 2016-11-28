viz = reactiveValues(a=NULL)

output$viz_site = renderUI({
  sites = sub("^(.*_.*)_[0-9]*-.*\\.Rda","\\1",dnld$ff) # the sites included in the download data
  sitenms = sort(sitenames[match(sites, allsites$SITEID)]) # the site names
  tagList(
    span(HTML("<h4>Choose a site:</h4>")),
    div(style="display:inline-block; vertical-align:top;",
      selectizeInput("toviz", NULL, sitenms, multiple = FALSE, options = NULL)),
    div(style="display:inline-block; vertical-align:top;",
      actionButton("viz_it","View", class="color: #fff; background-color: #337ab7; border-color: #fff"))
  )
})

getvizdat = function(sitev){
  stid = allsites$SITEID[which(sitenames==sitev)] # get siteid from name
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
  Reduce(function(df1,df2) bind_rows(df1,df2), tmpl) %>% distinct() %>% arrange(variable, DateTime_UTC)
}

observeEvent(input$viz_it, {
  viz$a = getvizdat(input$toviz)
  vars = unique(viz$a$variable)
  datt = as.Date(viz$a$DateTime_UTC)
  updateDateRangeInput(session, "ddate", start=max(datt)-14, end=max(datt), min=min(datt), max=max(datt))
  updateSelectizeInput(session, "ts_vars", choices=vars, selected=vars[1])
  updateSelectizeInput(session, "pp_vars_x", choices=vars, selected=vars[1])
  updateSelectizeInput(session, "pp_vars_y", choices=vars, selected=vars[2])
})

observe({
  dfrom = as.POSIXct(paste(input$ddate[1],"00:00:01"),format="%Y-%m-%d %T",tz="UTC")
  dto = as.POSIXct(paste(input$ddate[2],"23:59:00"),format="%Y-%m-%d %T",tz="UTC")
  if( !is.null(viz$a) ){
    if( length(which(viz$a$DateTime_UTC>dfrom & viz$a$DateTime_UTC<dto)) ){
      vdat = viz$a %>% filter(DateTime_UTC>dfrom & DateTime_UTC<dto) %>% spread(variable, value)
      if(input$aggregate!="15M"){
        vdat$DateTime_UTC = snap_ts(vdat$DateTime_UTC,input$aggregate)
        vdat = vdat %>% group_by(DateTime_UTC) %>% summarise_each(funs(mean(.,na.rm=T))) %>% ungroup()
      }
      if(input$viztab=="pp" & input$pp_vars_x%in%colnames(vdat)){
        vdat = vdat %>% select_(.dots=c(input$pp_vars_x,input$pp_vars_y))
        output$viz_plot = renderPlot({
          par(mar=c(4,4,1.5,1.5), cex=1, las=1, bg=NA, mgp=c(2.5,1,0))
          plot(vdat[[1]],vdat[[2]],bty="l",pch=19,xlab=colnames(vdat)[1], adj=1, ylab="")
          title(colnames(vdat)[2],adj=0, line=0.5, cex.main=1, font.main=1)
        }, width=400, height=400)
      }else if(input$viztab=="ts" & input$ts_vars[1]%in%colnames(vdat)){
        vdat = vdat %>% select_(.dots=c("DateTime_UTC",input$ts_vars))
        output$viz_plot = renderPlot({
          par(mfcol=c(ncol(vdat)-1, 1), mar=c(4,3.5,1.5,0.5), mgp=c(2.5,1,0), cex=1, las=1, bg=NA)
          for(c in 2:ncol(vdat)){
            plot(vdat[[c]]~vdat$DateTime_UTC,bty="l", type="l", lwd=2, xlab="Date", adj=1, ylab="")
            title(colnames(vdat)[c],adj=0, line=0.5, cex.main=1, font.main=1)
            #if(c == ncol(vdat)) axis.POSIXct(1,vdat$DateTime_UTC,xlab="Date")
          }
        }, height=(ncol(vdat)-1)*200)
      }
    }
  }
})
