spin = reactiveValues(d=NULL) # placeholder for input data
if(useSB){
  load(file.path(tmpwebfile,'colnms.Rda'))
}else{# On AWS
  s3load(object='meta/colnms.Rda',bucket='streampulse') # load list of colnms data
}
coln = reactiveValues(ms=colnms,all=acolnms) # placeholder for column names

getsitedeets = function(){
  output$uploadhandle = renderUI({
    HTML(paste0("<h3 class='box-title'>We need some details about your site.</h3>",
      textInput("sitenm","Site name:"),
      "<h3 class='box-title'>Coordinates, decimal degrees (e.g., 36.00, -78.97)</h3>",
      numericInput("sitelat","Latitude (N/S):"),
      numericInput("sitelng","Longitude (E/W):"),
      "<h3 class='box-title'>Data sharing</h3>",
      "Our core sites are sharing data with the public through a <a href='https://www.cuahsi.org/'>CUAHSI</a> database. ",
      "You can choose to <i>also participate in public data sharing</i> <u>or</u> <i>keep your data private within StreamPULSE</i> ",
      "(which means that other StreamPULSE members can access your data).",
      selectizeInput("datasharing","Data sharing agreement", choices=c("Public","StreamPULSE only")),
      actionButton("addleveraged","Continue", width="100%", style="color: #fff; background-color: #337ab7; border-color: #fff")
    ))
  })
}

#item_replace_files("58189ef0e4b0bb36a4c82012",files='SPsites.csv', session=asb)
item_file_download("58189ef0e4b0bb36a4c82012",names='SPsites.csv',destinations=file.path(tmpwebfile,'SPsites.csv'), overwrite_file=TRUE, session=asb)
allsites = read_csv(file.path(tmpwebfile,'SPsites.csv')) # csv

observeEvent(input$addleveraged, {
  newsite = tibble(SITEID=spin$d$site, NAME=input$sitenm, LNG=input$sitelng, LAT=input$sitelat, SHARING=input$datasharing, CONTACT=input$userName)
  write_csv(newsite, path=file.path(tmpwebfile,'SPsites.csv'), append=TRUE)
  item_replace_files("58189ef0e4b0bb36a4c82012", files=file.path(tmpwebfile,'SPsites.csv'), session=asb)
  definecolumns(c("",colnames(spin$d$data)))
})

definecolumns = function(cn){
  output$uploadhandle = renderUI({
    HTML(paste0("<h3 class='box-title'>Match your columns with the codes below.</h3>",
    "If you do not have a given column, just leave it blank. ",
    "<i>Note:</i> You will only need to do this once (unless the column names in your files change). ",
    "<i>Your columns not here?</i> Email <a href='mailto:aaron.berdanier@gmail.com'>Aaron</a>.<br>",
      selectizeInput("DateTime_UTC", "DateTime_UTC", choices=cn),
      selectizeInput("DO_mgL", "DO_mgL", choices=cn),
      selectizeInput("satDO_mgL", "satDO_mgL", choices=cn),
      selectizeInput("WaterTemp_C", "WaterTemp_C", choices=cn),
      selectizeInput("WaterPres_kPa", "WaterPres_kPa", choices=cn),
      selectizeInput("AirTemp_C", "AirTemp_C", choices=cn),
      selectizeInput("AirPres_kPa", "AirPres_kPa", choices=cn),
      selectizeInput("Depth_m", "Depth_m", choices=cn),
      selectizeInput("Discharge_m3s", "Discharge_m3s", choices=cn),
      selectizeInput("Velocity_ms", "Velocity_ms", choices=cn),
      selectizeInput("pH", "pH", choices=cn),
      selectizeInput("fDOM_mV", "fDOM_mV", choices=cn),
      selectizeInput("fDOM_frac", "fDOM_frac", choices=cn),
      selectizeInput("Turbidity_mV", "Turbidity_mV", choices=cn),
      selectizeInput("Turbidity_NTU", "Turbidity_NTU", choices=cn),
      selectizeInput("Nitrate_mgL", "Nitrate_mgL", choices=cn),
      selectizeInput("SpecCond_mScm", "SpecCond_mScm", choices=cn),
      selectizeInput("SpecCond_uScm", "SpecCond_uScm", choices=cn),
      selectizeInput("Light_lux", "Light_lux", choices=cn),
      selectizeInput("Light_PAR", "Light_PAR", choices=cn),
      selectizeInput("CO2_ppm", "CO2_ppm", choices=cn),
      actionButton("definecols","Set columns", width="100%", style="color: #fff; background-color: #337ab7; border-color: #fff")
    ))
  })
}

sitedb = read_csv("sitelist.csv",col_types=cols())
coresites = paste(sitedb$REGIONID,sitedb$SITEID,sep="_")

sbprocess = function(ff){
  ffregex = "[A-Z]{2}_.*_[0-9]{4}-[0-9]{2}-[0-9]{2}_[A-Z]{2}.[a-zA-Z]{3}" # core sites
  ffregex2 = "[A-Z]{2}_.*_[0-9]{4}-[0-9]{2}-[0-9]{2}.csv" # leveraged sites
  if(!all(sapply(ff$name, grepl, pattern=paste(ffregex,ffregex2,sep="|")))) return(list(err="<font style='color:#FF0000;'><i>Please format your filenames as: REGIONID_SITEID_YYYY-MM-DD_LOGGERID.xxx</i></font>"))  # check if all uploaded
  updfiles = read_csv("upfiles.txt",col_names="n",col_types=cols()) # files already uploaded
  if(all(ff$name %in% updfiles$n)) return(list(err="<font style='color:#FF0000;'><i>All of those files were already uploaded.</i></font>"))  # check if all uploaded
  if(any(ff$name %in% updfiles$n)) ff = ff[-which(ff$name %in% updfiles$n),] # remove files already uploaded
  x = strsplit(ff$name,"_")
  site = unique(sapply(x, function(y) paste0(y[1],"_",y[2])))
  dnld_date = unique(sapply(x, function(y) y[3]))
  if(length(site)>1) return(list(err="<font style='color:#FF0000;'><i>Please only select data from a single site.</i></font>"))  # check for a single site
  # do the processing
  if(grepl(site, coresites)){ # if it is a core site, get gmtoff
    lat = sitedb$LAT[grep(site,coresites)]
    lng = sitedb$LNG[grep(site,coresites)]
    gmtoff = get_gmtoff(lat, lng, dnld_date, dst=FALSE)
    data = try(sp_in(ff, gmtoff), silent=TRUE)  # transform original data
    if(class(data)=="try-error") return(list(err="<font style='color:#FF0000;'><i>There was an error processing your data, please email <a href='aaron.berdanier@gmail.com'>Aaron</a> with a copy of the files you wish to upload.</i></font>"))
  }else{ # leveraged site
    if(nrow(ff)>1) return(list(err="<font style='color:#FF0000;'><i>Please merge your data files prior to upload.</i></font>"))  # check for a single site
    site = paste0(site,"_L") # designate as leveraged site
    data = sp_in_lev(ff)
  }
  # upload zip file to SB
  tupf = tempfile()
  dir.create(tupf)
  file.copy(ff$datapath, paste0(tupf,"/",ff$name))
  ff$datapath = paste0(tupf,"/",ff$name)
  tzipf = paste0(tupf,"/",site,"_",Sys.Date(),".zip") # temporary data folder
  zip(tzipf, ff$datapath, extras="-j")
  item_append_files(sbopath, files=tzipf, session=asb) # into the originals folder
  write_csv(tibble(ff$name), path="upfiles.txt", append=TRUE) # add the uploaded files to the uplist
  file.remove(dir(tupf, full.names=TRUE)) # remove locally
  list(site=site, dates=dnld_date, data=data)
}

# Load data
observeEvent(input$uploadFile, {
  ff = input$uploadFile
  xx = capture.output( spin$d <- sbprocess(input$uploadFile) ) # get the data
  output$spinupstatus = renderUI(HTML(paste(xx,collapse="<br>")))
  if("err" %in% names(spin$d)){
    output$uploadhandle = renderUI( HTML(spin$d$err) )
  }else{
    site = spin$d$site
    # check if the site has column name definitions or if any columns have changed
    if(!(site %in% names(coln$all))) coln$all[[site]]=NULL
    if( site%in%names(coln$ms) & length(colnames(spin$d$data))==length(coln$all[[site]]) ){
      if(all(coln$all[[site]]==colnames(spin$d$data))){
        allcol = TRUE # all columns available and match
      }else{ allcol = FALSE }
    }else{ allcol = FALSE }
    if(grepl("[A-Za-z]*_.*_L",site)){ leveraged = TRUE }else{ leveraged = FALSE }

    if(allcol){
      output$uploadhandle = renderUI({ HTML(paste0(
          actionButton("uploadaws", paste("Upload data for",site), width="100%",
            style="color: #fff; background-color: #337ab7; border-color: #fff"),
          "<br><br><center>Need to redefine your columns? ",
          actionLink("redefinecols", paste("Click here to manually reassign columns for",site)),"</center>"
        ))
      })
    }else{
      if(leveraged & !site%in%allsites$SITEID){ # need to get site info
        getsitedeets()
      }else{ # core site or site data already in...
        definecolumns(c("",colnames(spin$d$data)))
      }
    }
  }
})

observeEvent(input$definecols,{
  newv = c("DateTime_UTC",
  "DO_mgL",
  "satDO_mgL",
  "WaterTemp_C",
  "AirTemp_C",
  "WaterPres_kPa",
  "AirPres_kPa",
  "Depth_m",
  "Discharge_m3s",
  "Velocity_ms",
  "pH",
  "fDOM_mV",
  "fDOM_frac",
  "Turbidity_mV",
  "Turbidity_NTU",
  "Nitrate_mgL",
  "SpecCond_mScm",
  "SpecCond_uScm",
  "Light_lux",
  "Light_PAR",
  "CO2_ppm")
  oldv = c(input$DateTime_UTC,
  input$DO_mgL,
  input$satDO_mgL,
  input$WaterTemp_C,
  input$AirTemp_C,
  input$WaterPres_kPa,
  input$AirPres_kPa,
  input$Depth_m,
  input$Discharge_m3s,
  input$Velocity_ms,
  input$pH,
  input$fDOM_mV,
  input$fDOM_frac,
  input$Turbidity_mV,
  input$Turbidity_NTU,
  input$Nitrate_mgL,
  input$SpecCond_mScm,
  input$SpecCond_uScm,
  input$Light_lux,
  input$Light_PAR,
  input$CO2_ppm)
  coln$ms[[spin$d$site]] = tibble(new=newv[which(oldv!="")],old=oldv[which(oldv!="")])
  colnms = coln$ms
  coln$all[[spin$d$site]] = colnames(spin$d$data)
  acolnms = coln$all
  # tfn = tempfile()
  save(colnms, acolnms, file=file.path(tmpwebfile,'colnms.Rda'))
  if(useSB){
    item_replace_files(sbwpath, files=file.path(tmpwebfile,'colnms.Rda'), session=asb)
  }else{  # On AWS
    put_object(file=file.path(tmpwebfile,'colnms.Rda'), object="meta/colnms.Rda",bucket="streampulse")
  }
  site = spin$d$site
  output$uploadhandle = renderUI({ HTML(paste0(
      actionButton("uploadaws", paste("Upload data for",site), width="100%",
        style="color: #fff; background-color: #337ab7; border-color: #fff")#,
      # "<br><br><center>Need to redefine your columns? ",
      # actionLink("redefinecols", paste("Click here to manually reassign columns for",site)),"</center>"
    ))
  })
})


observeEvent(input$redefinecols,{
  definecolumns(c("",colnames(spin$d$data)))
})

observeEvent(input$uploadaws, {
  colnms = coln$ms[[spin$d$site]]
  dd = spin$d$data %>% rename_(.dots=setNames(colnms$old, colnms$new)) %>% select_(.dots=colnms$new)
  newpoints = sum(!is.na(dd[,-1]))
  # ddate = as.character(sort(as.Date(spin$d$dates), decreasing=TRUE))[1] # get latest date
  sitedate = paste0(spin$d$site,"_",Sys.Date(),"_",gsub(".*file(.*)","\\1",tempfile()))
  assign(sitedate, dd)
  if(useSB){
    save(list=c(sitedate), file=paste0(tdatf,"/",sitedate,".Rda")) # save locally
    item_append_files(sbrpath, files=paste0(tdatf,"/",sitedate,".Rda"), session=asb) # store in SB
    file.remove(dir(tdatf, full.names=TRUE)) # remove locally
  }else{  # On AWS
    tfn = tempfile()
    save(list=c(sitedate), file=tfn)
    put_object(file=tfn, object=paste0("raw/",sitedate,".Rda"), bucket="streampulse")
  }
  cat(newpoints, sep="\n", file="datapoints.txt", append=TRUE) # add new data points to list
  output$uploadhandle = renderUI(HTML(paste("Thanks! Added",newpoints,"new data points for",spin$d$site)))
})



# # Load merged data
# dataup = reactive({
#   inFile = input$file1
#   if (is.null(inFile))
#     return(NULL)
#   # read in raw data
#   read_csv(inFile$path)
# })
#
# observe({
#   if(!is.null(dataup())){
#     dat = dataup()
#     colnames(dat)[1] = "DateTimeUTC"
#     site$daterange = range(dat$DateTime, na.rm=T)
#     filename = sub("(.*)\\..*", "\\1", input$file1$name)
#     site$id = paste0(unlist(strsplit(filename,"_"))[-3],collapse="_")
#     str1 = paste("Data source:<b>",site$id,"</b><br>Date range:",site$daterange[1],"to",site$daterange[2])
#     #drop_dir(dtoken=dto) # view all data in dropbox
#     # reset all ui stuff
#     output$flagui = renderUI(HTML("<br>"))
#     output$flatplt = renderUI(HTML("<br>"))
#     output$loadtext = renderText(paste0(""))
#     flags$d = NULL
#
#     # check if training data exists
#     traindir = drop_dir("SPtrainingdata",dtoken=dto) # all data in training directory
#     getdb = grep(site$id, traindir$path, value=TRUE) # find files in dropbox that match
#     if(length(getdb) != 0){  # load training data if it exists
#       drop_get(path=last(getdb),local_file=file.path(tempdir(),"training.Rda"),dtoken=dto,overwrite=TRUE) # getting rda file
#       load(file.path(tempdir(),"training.Rda")) # load the Rda file
#       training$dat = trainingdat
#
#       output$filestatsupload = renderUI({
#         # maybe put in a header view of the file - preview
#         str2 = "<i>File looks good:</i> "
#         wellPanel(HTML(paste0(str1, '<h4>', str2, actionLink("QAQC", "Click here to flag and tag the data."), " <u>Note:</u> this step can take a few minutes... please be patient.</h4>")))
#       })
#
#       allfnt$aflags = NULL
#       allfnt$atags = NULL
#       afnts = drop_get(path=paste0("SPflagsntags/",site$id,".Rda"), local_file=file.path(tempdir(),"ftvalues.Rda"), dtoken=dto, overwrite=TRUE) # get this sites' flags and tags
#       if(afnts){
#         load(file.path(tempdir(),"ftvalues.Rda")) # load the Rda file
#         allfnt$aflags = aflags
#         allfnt$atags = atags
#       }
#     }else{  # if not, confirm that this is a new site
#       output$filestatsupload = renderUI({
#         # choose the date column, and the data columns
#         wellPanel(
#           HTML(paste0(str1,"<h4>This looks like a new data stream... Please select the correct date and data columns (you can select multiple data columns and exclude extra columns, e.g., 'RECORDS'). You will only need to do this once.</h4>")),
#           fluidRow(
#             column(4,selectizeInput('choosedate', '1. Choose Date/time column (UTC)', choices=colnames(dat))),
#             column(4,selectizeInput('choosedata', '2. Choose data variable columns', choices=colnames(dat), multiple = TRUE, selected=colnames(dat))),
#             column(4,HTML(paste0("<h4>",actionButton("newtrainingdata","Click here ",style="color: #fff; background-color: #337ab7; border-color: #fff")," to confirm.</h4>")))
#           )
#         )
#       })
#     }
#   }
# })
#
# observeEvent(input$newtrainingdata,{
#   str1 = paste("Data source:<b>",site$id,"</b><br>Date range:",site$daterange[1],"to",site$daterange[2])
#   # the current data is the training data...
#   datacols = c(input$choosedate,input$choosedata)
#   training$dat = dataup() %>% select_(.dots=datacols) # exclude date time
#   colnames(training$dat)[1] = "DateTimeUTC"
#   output$filestatsupload = renderUI({
#     # maybe put in a header view of the file - preview
#     str2 = "<i>File looks good:</i> "
#     wellPanel(HTML(paste0(str1, '<h4>', str2, actionLink("QAQC", "Click here to flag and tag it."), " <u>Note:</u> this step can take a few minutes... please be patient.</h4>")))
#   })
# })
