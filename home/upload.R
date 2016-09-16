# function for HOBO data
getHobodat = function(ff){
  f1 = read_csv(ff, skip=1)
  f1 = f1[,-grep("Coupler|File|Stopped",colnames(f1))]
  m = regexpr("\\,.*",colnames(f1),perl=T) # parse column names
  uu = sapply(strsplit(regmatches(colnames(f1),m)," "), function(x) x[2]) # get units
  colnames(f1) = gsub("\\ |\\/","",c("N","DateTimeUTC",paste(unlist(strsplit(colnames(f1)[3],","))[1],uu[2]),"Temp"))
  tzoff = ifelse(grepl("-",uu[1]), sub("-","+",uu[1]), sub("+","-",uu[1])) # switch time zone to match R conventions
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )AM(.*)$", "\\10\\2AM\\3", paste(f1$DateTimeUTC,tzoff)) # convert 12AM to 0AM (midnight)
  dada = sub("^(.*\\ )12(:.{2}:.{2}\\ )PM(.*)$", "\\112\\2AM\\3", dada) # convert 12PM to 12AM (noon)
  f1$DateTimeUTC = parse_datetime(dada,"%D %T %p %Z")
  if("AbsPreskPa" %in% colnames(f1)) f1 = f1 %>% mutate(AbsPresPa = AbsPreskPa*1000) %>% select(-AbsPreskPa)
  f1 %>% filter(apply(f1,1,function(x) all(!is.na(x)))) %>% select(-N)
}

getCSdat = function(ff){
  tzoff = 4 # EST
  if(grepl("WI_", ff)) tzoff = 5
  if(grepl("AZ_|WY_", ff)) tzoff = 6
  hh = read_csv(ff, skip=1, n_max=2)
  f1 = read_csv(ff, skip=4, col_names=colnames(hh),col_types=cols(TIMESTAMP = "c"))
  f1$DateTimeUTC = parse_datetime(paste(f1$TIMESTAMP,tzoff),"%m/%d/%Y %T %Z")
  ccn = colnames(f1)
  f1 %>% select_(.dots=c("DateTimeUTC",ccn[-grep("DateTimeUTC",ccn)])) %>% rename(LocalDateTime = TIMESTAMP)
}

# function to read streampulse file
streampulseFile = function(inFile){
  datalogger = sub("(.*_)(.*)\\..*", "\\2", inFile$name)
  #upload raw to aws
  put_object(file=inFile$datapath, object=inFile$name, bucket="streampulserawdata")
  if(datalogger == "CS"){ # cs data logger
    getCSdat(inFile$datapath)
  }else if(grepl("H",datalogger)){ # hobo data logger
    getHobodat(inFile$datapath)
  }else{
    read_csv(inFile$path)
  }
}

# Load new data
# NEED TO MODIFY TO BRING IN OUR DATA FILES in the corect format
dataup = reactive({
  inFile = input$file1
  print(inFile)
  if (is.null(inFile))
    return(NULL)
  # read in raw data
  streampulseFile(inFile)
})


observe({
  if(!is.null(dataup())){
    dat = dataup()

    dat$DateTimeUTC = as.POSIXct(dat$DateTimeUTC)
    if("LocalDateTime" %in% colnames(dat)) dat = select(dat, -LocalDateTime)

    site$daterange = range(dat$DateTimeUTC)
    filename = sub("(.*)\\..*", "\\1", input$file1$name) #"DF_AaronsSiteNameXXX_201601225_C.csv"
    site$id = paste0(unlist(strsplit(filename,"_"))[-3],collapse="_")
    str1 = paste("Data source:<b>",site$id,"</b><br>Date range:",site$daterange[1],"to",site$daterange[2])
    #drop_dir(dtoken=dto) # view all data in dropbox
    # reset all ui stuff
    output$flagui = renderUI(HTML("<br>"))
    output$flatplt = renderUI(HTML("<br>"))
    output$loadtext = renderText(paste0(""))
    flags$d = NULL

    # check if training data exists
    traindir = drop_dir("SPtrainingdata",dtoken=dto) # all data in training directory
    getdb = grep(site$id, traindir$path, value=TRUE) # find files in dropbox that match
    if(length(getdb) != 0){  # load training data if it exists
      drop_get(path=last(getdb),local_file=file.path(tempdir(),"training.Rda"),dtoken=dto,overwrite=TRUE) # getting rda file
      load(file.path(tempdir(),"training.Rda")) # load the Rda file
      training$dat = trainingdat

      output$filestatsupload = renderUI({
        # maybe put in a header view of the file - preview
        str2 = "<i>File looks good:</i> "
        wellPanel(HTML(paste0(str1, '<h4>', str2, actionLink("QAQC", "Click here to flag and tag the data."), " <u>Note:</u> this step can take a few minutes... please be patient.</h4>")))
      })

      allfnt$aflags = NULL
      allfnt$atags = NULL
      afnts = drop_get(path=paste0("SPflagsntags/",site$id,".Rda"), local_file=file.path(tempdir(),"ftvalues.Rda"), dtoken=dto, overwrite=TRUE) # get this sites' flags and tags
      if(afnts){
        load(file.path(tempdir(),"ftvalues.Rda")) # load the Rda file
        allfnt$aflags = aflags
        allfnt$atags = atags
      }
    }else{  # if not, confirm that this is a new site
      output$filestatsupload = renderUI({
        # choose the date column, and the data columns
        wellPanel(
          HTML(paste0(str1,"<h4>This looks like a new data stream... Please select the correct date and data columns (you can select multiple data columns and exclude extra columns, e.g., 'RECORDS'). You will only need to do this once.</h4>")),
          fluidRow(
            column(4,selectizeInput('choosedate', '1. Choose Date/time column (UTC)', choices=colnames(dat))),
            column(4,selectizeInput('choosedata', '2. Choose data variable columns', choices=colnames(dat), multiple = TRUE)),
            column(4,HTML(paste0("<h4>",actionButton("newtrainingdata","Click here ",style="color: #fff; background-color: #337ab7; border-color: #fff")," to confirm.</h4>")))
          )
        )
      })
    }
  }
})

observeEvent(input$newtrainingdata,{
  str1 = paste("Data source:<b>",site$id,"</b><br>Date range:",site$daterange[1],"to",site$daterange[2])
  # the current data is the training data...
  datacols = c(input$choosedate,input$choosedata)
  training$dat = dataup() %>% select_(.dots=datacols) # exclude date time
  colnames(training$dat)[1] = "DateTimeUTC"
  output$filestatsupload = renderUI({
    # maybe put in a header view of the file - preview
    str2 = "<i>File looks good:</i> "
    wellPanel(HTML(paste0(str1, '<h4>', str2, actionLink("QAQC", "Click here to flag and tag it."), " <u>Note:</u> this step can take a few minutes... please be patient.</h4>")))
  })
})
