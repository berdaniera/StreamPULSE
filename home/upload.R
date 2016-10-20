# Load new data
dataup = reactive({
  inFile = input$file1
  if (is.null(inFile))
    return(NULL)
  # read in raw data
  read_csv(inFile$path)
})

observe({
  if(!is.null(dataup())){
    dat = dataup()
    colnames(dat)[1] = "DateTimeUTC"
    site$daterange = range(dat$DateTime, na.rm=T)
    filename = sub("(.*)\\..*", "\\1", input$file1$name)
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
            column(4,selectizeInput('choosedata', '2. Choose data variable columns', choices=colnames(dat), multiple = TRUE, selected=colnames(dat))),
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
