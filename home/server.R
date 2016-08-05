library(DT)
require(shiny)
require(shinydashboard)
library('aws.s3')

mda <- data.frame(Level=c("Level 0","Level 1A","Level 1B","Level 2","Level 3"),
                  Description=c("Unprocessed sensor data","Unprocessed data with quality flags","Quality filtered data from flags in level 1A (and gap-filled?)","Derived variables","Model output"),
                  Storage=c("CUAHSI HIS","ScienceBase","ScienceBase","ScienceBase","ScienceBase"))

updatestab <- data.frame(Date=c('8 July 16','1 July 16'),Up=c("Dashboard online, accepting file uploads","Dashboard created"))

s3load('streampulseusers.Rdata','streampulse') # load user dataset

Logged <- FALSE
LoginPass <- 0 #0: not attempted, -1: failed, 1: passed
username <- ""

login <- box(title = "Login",status="primary",solidHeader=TRUE,#collapsible=TRUE,
             textInput("userName", "Username",), # make this orange
             passwordInput("passwd", "Password"),
             actionButton("Login", "Log in"),
             div(align="right",a(href="mailto:abb30@duke.edu","Email to create an account.")))

loginfail <- box(title = "Login",status="danger",solidHeader=TRUE,#collapsible=TRUE,
                 textInput("userName", "Username"), # make this orange
                 passwordInput("passwd", "Password"),
                 div(align="center",em(h5("Username or password incorrect. Please try again."))),
                 actionButton("Login", "Log in"),
                 div(align="right",a(href="mailto:abb30@duke.edu","Email to create an account.")))

# Lambda in S3

# FLAGGING SCRIPT
# data <- cars
# flags <- reactiveValues(f=FALSE)
# # ADD A NEW FLAG
# observeEvent(input$flag_new,{
#   newflags <- brushedPoints(cars,input$plot_brush, "speed", "dist",allRows=TRUE)$selected_
#   if(any(newflags)) flags$f[newflags] <- TRUE
# })
# # CLEAR FLAGS
# observeEvent(input$flag_clear,{ flags$f <- FALSE })
# # SUBMIT FLAGS
# observeEvent(input$flag_send,{
# # SAVE THE FLAGS SOMEHOW
# })
# # PLOT THE DATA
# output$flag_plot <- renderPlot({
#   d <- data
#   plot(d$speed, d$dist)
#   if(any(flags$f)) points(d$speed[flags$f],d$dist[flags$f],col="red",pch=19)
# })



server <- function(input, output, session) {
  USER <<- reactiveValues(Logged = Logged, LoginPass = LoginPass, Name = username)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          username <- isolate(input$userName)
          password <- isolate(input$passwd)
          if (username %in% users$user & password == 'streams') { #users$pass
            USER$Name <<- unlist(strsplit(username,"@"))[1] # or just username
            USER$Logged <<- TRUE
            USER$LoginPass <<- 1
          }
          USER$LoginPass <<- -1
        }
      }
    }
  })

  dataup <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath, header = TRUE, sep = ',')
  })


  observe({
    if(!is.null(dataup())){
      data <- dataup()
      output$filestatsupload <- renderUI({
        str1 <- paste("Number of rows:",dim(data)[1],"Number of columns:",dim(data)[2])
        str2 <- "<i>File looks good, ready to upload:</i> "
        HTML(paste0(str1, '<br/>', str2, actionButton("awsupload", "Upload it")))
      })
      # generate upload button
      #output$awsupload <- renderUI( actionButton("awsupload", "Upload it") )
      # upload to aws s3
      observeEvent(input$awsupload,{
        bc <- get_bucket('streampulse')
        currentfiles <- unlist(bc)[names(unlist(bc))%in%c('Key')]
        fname <- unlist(strsplit(isolate(input$file$name),"[.]"))[1]
        ffname <- paste0("Data-level0-",USER$Name,"-",fname)
        cnt <- 1
        if(length(grep(ffname,currentfiles))>0) cnt <- length(grep(ffname,currentfiles))+1
        fileName <- paste0(ffname,"-",cnt,".csv")
        filePath <- file.path(tempdir(), fileName)
        write.csv(data, filePath, row.names = FALSE, quote = TRUE)
        saved <- tryCatch(put_object(filePath,object=fileName,bucket="streampulse"), warning = function(e) FALSE)
        output$uploadstatus <- renderText({
          if (!saved) {
            "File upload failed"
          }else{
            paste("File upload to streampulse successful! Stored as:",fileName)
          }
        })
      })
    }
  })



  output$useBox <- renderValueBox({
    valueBox(length(users$user), "Users", icon = icon("users"))
  })

  output$obsBox <- renderValueBox({
    valueBox(paste0(0,"K"), "Observations", icon = icon("bar-chart"), color="yellow")
  })

  output$modBox <- renderValueBox({
    valueBox(0, "Models run", icon = icon("cloud"))
  })

  output$updatetable <-  renderDataTable({updatestab},
                                          options = list(paging = FALSE,searching=FALSE,ordering=FALSE),escape=FALSE,
                                          style="bootstrap",selection="none")

  output$table <- renderDataTable({mda},
                                  options = list(paging = FALSE,searching=FALSE,ordering=FALSE),escape=FALSE,
                                  style="bootstrap",selection="none")

  observe({
    if (USER$Logged == TRUE) {
      userstr <- span("Login successful as ",strong(USER$Name))
      logoutstr <- div(align="right",a(href="/","Log out"))
      loginpass <- box(title = "Logged in!",status="success",solidHeader=TRUE,userstr,logoutstr)
      output$loginbox <- renderUI(loginpass)
      output$Upload <- renderMenu( menuItem("Upload", tabName = "upload", icon = icon("cloud-upload")) )
      output$Cleaner <- renderMenu( menuItem("Cleaner", icon = icon("magic"), tabName = "clean",
                                             badgeLabel = "coming soon", badgeColor = "yellow") )
      output$Visualizer <- renderMenu( menuItem("Vizualizer", tabName = "view", icon = icon("line-chart")) )
      output$Modeler <- renderMenu( menuItem("Modeler", tabName = "model", icon = icon("cubes")) )
      output$SOPs <- renderMenu( menuItem("SOPs", tabName = "sop", icon = icon("file-text-o")) )
      output$fileup <- renderUI( fileInput('file', NULL,
                                           accept = c('text/csv','text/comma-separated-values','text/plain','.csv','application/octet-stream')) )
    } else {
      if(USER$LoginPass >= 0) {
        output$loginbox <- renderUI(login)
      } else {
        output$loginbox <- renderUI(loginfail)
      }
    }
  })
}
