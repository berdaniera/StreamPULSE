awssave = function(ff){
  state = c("AZ","FL","NC","WI","PR")
  stlat = c(34, 30, 37, 43, 18)
  stlng = c(-111.5, -82.5, -79, -89.5, -66)
  x = strsplit(ff$name,"_")
  site = unique(sapply(x, function(y) paste0(y[1],"_",y[2])))
  dnld_date = unique(sapply(x, function(y) y[3]))
  if(length(site)>1) return(list(err="<font style='color:#FF0000;'><i>Please only select data from a single site.</i></font>"))  # check for a single site, error message if not
  sttt = substr(site,1,2)
  if(!any(grepl(sttt,state))){ # if it is a core site, get gmtoff
    lat <- stlat[grep(sttt,state)]
    lng <- stlng[grep(sttt,state)]
    gmtoff <- get_gmtoff(lat, lng, dnld_date, dst=FALSE)
  }else{ offset = 0
    gmtoff <- tibble(dnld_date,offs=0)
  }
  #  return(list(err="<font style='color:#FF0000;'><i>Region not recognized, please contact Aaron at aaron.berdanier@gmail.com.</i></font>"))  # check for a single site, error message if not
  sapply(1:nrow(ff), function(x) put_object(file=ff$datapath[x], object=paste0("original/",ff$name[x]), bucket="streampulse"))  # upload original data
  data = sp_in(ff, gmtoff)  # transform original data
  list(site=site, dates=dnld_date, data=data)
}

spin = reactiveValues(d=NULL) # placeholder for input data
# colnms = acolnms = list()
# s3save(colnms, acolnms, object="meta/colnms.Rda",bucket="streampulse")
s3load(object='meta/colnms.Rda',bucket='streampulse') # load list of colnms data
coln = reactiveValues(ms=colnms,all=acolnms) # placeholder for column names

definecolumns = function(cn){
  output$uploadhandle = renderUI({
    HTML(paste0("<h3 class='box-title'>Match your columns with the codes below.</h3>If you do not have a given column, just leave it blank. <i>Note:</i> You only need to do this once (unless the column names in your files change).<br>",
      selectizeInput("DateTime_UTC", "DateTime_UTC", choices=cn),
      selectizeInput("DO_mgL", "DO_mgL", choices=cn),
      selectizeInput("fDOsat_frac", "fDOsat_frac", choices=cn),
      selectizeInput("WaterTemp_C", "WaterTemp_C", choices=cn),
      selectizeInput("AirTemp_C", "AirTemp_C", choices=cn),
      selectizeInput("WaterPres_kPa", "WaterPres_kPa", choices=cn),
      selectizeInput("AirPres_kPa", "AirPres_kPa", choices=cn),
      selectizeInput("pH", "pH", choices=cn),
      selectizeInput("fDOM_mV", "fDOM_mV", choices=cn),
      selectizeInput("fDOM_frac", "fDOM_frac", choices=cn),
      selectizeInput("Turbidity_mV", "Turbidity_mV", choices=cn),
      selectizeInput("Turbidity_NTU", "Turbidity_NTU", choices=cn),
      selectizeInput("Nitrate_mgL", "Nitrate_mgL", choices=cn),
      selectizeInput("SpecCond_mscm", "SpecCond_mscm", choices=cn),
      selectizeInput("Depth_m", "Depth_m", choices=cn),
      selectizeInput("Light_lux", "Light_lux", choices=cn),
      selectizeInput("Light_par", "Light_par", choices=cn),
      selectizeInput("CO2_ppm", "CO2_ppm", choices=cn),
      actionButton("definecols","Set columns and upload data")
    ))
  })
}

# Load data
observe({ if(!is.null(input$awsFile)){
  xx = capture.output( spin$d <- awssave(input$awsFile) ) # get the data
  output$spinupstatus = renderUI(HTML(paste(xx,collapse="<br>")))
  if("err" %in% names(spin$d)){
    output$uploadhandle = renderUI( HTML(spin$d$err) )
  }else{
    site = spin$d$site
    if(!(site %in% names(coln$all))) coln$all[[site]]=NULL
    # check if the site has column name definitions or if any columns have changed
    if( !(site %in% names(coln$ms)) | !all(coln$all[[site]]==colnames(spin$d$data)) ){
      definecolumns(c("",colnames(spin$d$data)))
    }else{
      output$uploadhandle = renderUI( actionButton("uploadaws","Upload data") )
    }
  }
} })

observeEvent(input$definecols,{
  newv = c("DateTime_UTC",
  "DO_mgL",
  "fDOsat_frac",
  "WaterTemp_C",
  "AirTemp_C",
  "WaterPres_kPa",
  "AirPres_kPa",
  "pH",
  "fDOM_mV",
  "fDOM_frac",
  "Turbidity_mV",
  "Turbidity_NTU",
  "Nitrate_mgL",
  "SpecCond_mscm",
  "Depth_m",
  "Light_lux",
  "Light_par",
  "CO2_ppm")
  oldv = c(input$DateTime_UTC,
  input$DO_mgL,
  input$fDOsat_frac,
  input$WaterTemp_C,
  input$AirTemp_C,
  input$WaterPres_kPa,
  input$AirPres_kPa,
  input$pH,
  input$fDOM_mV,
  input$fDOM_frac,
  input$Turbidity_mV,
  input$Turbidity_NTU,
  input$Nitrate_mgL,
  input$SpecCond_mscm,
  input$Depth_m,
  input$Light_lux,
  input$Light_par,
  input$CO2_ppm)
  coln$ms[[spin$d$site]] = data.frame(new=newv[which(oldv!="")],old=oldv[which(oldv!="")])
  colnms = coln$ms
  coln$all[[spin$d$site]] = colnames(spin$d$data)
  acolnms = coln$all
  tfn = tempfile()
  save(colnms, acolnms, file=tfn)
  put_object(file=tfn, object="meta/colnms.Rda",bucket="streampulse")
  # s3save(colnms,acolnms,object="meta/colnms.Rda",bucket="streampulse")
})

observeEvent(input$uploadaws, {
  colnms = coln$ms[[spin$d$site]]
  dd = spin$d$data %>% rename_(.dots=setNames(colnms$old, colnms$new)) %>% select_(.dots=colnms$new)
  newpoints = sum(!is.na(dd[,-1]))
  ddate = as.character(sort(as.Date(spin$d$dates), decreasing=TRUE))[1] # get latest date
  sitedate = paste0(spin$d$site,"_",ddate)
  paste0(spin$d$site,"_",ddate,".Rda")
  assign(sitedate, dd)
  s3save(get(sitedate), object=paste0("raw/",sitedate,".Rda"), bucket="streampulse")
  cat(newpoints, sep="\n", file="datapoints.txt", append=TRUE) # add new data points to list
  output$uploadhandle = renderUI(HTML(paste("Thanks! Added",newpoints,"new data points.")))
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
