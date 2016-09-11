library(DT)
require(shiny)
require(shinydashboard)
library('aws.s3')
library(rdrop2)
library(e1071)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
cbPalette = c("#333333", "#E69F00", "#337ab7", "#009E73", "#56B4E9", "#009E73", "#666666", "#739E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#plot(seq(1:length(cbPalette)),pch=20,col=cbPalette,cex=5)
dto = readRDS("droptoken.rds")

mda <- data.frame(Level=c("Level 0","Level 1A","Level 1B","Level 2","Level 3"),
                  Description=c("Unprocessed sensor data","Unprocessed data with quality flags","Quality filtered data from flags in level 1A (and gap-filled?)","Derived variables","Model output"),
                  Storage=c("CUAHSI HIS","ScienceBase","ScienceBase","ScienceBase","ScienceBase"))

updatestab <- data.frame(Date=c('2016-09-06','2016-09-03','2016-07-08','2016-07-01'),Up=c("QA/QC interface linked with data upload","SOP documents linked in","Dashboard online, accepting file uploads","Dashboard created"))

#s3load('streampulseusers.Rdata','streampulse') # load user dataset
drop_get(path="streampulseusers.Rdata",local_file=file.path(tempdir(),"spusers.Rdata"),dtoken=dto,overwrite=TRUE) # getting rda file
load(file.path(tempdir(),"spusers.Rdata")) # load the Rda file

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


server <- function(input, output, session) {
  USER = reactiveValues(Logged = FALSE, Name = "")
  training = reactiveValues(dat=NULL)
  flags = reactiveValues(d=NULL,f=NULL,t=NULL) # placeholder for model flagged data
  # d is the data with a flag column (f)
    # 0 is not flagged
    # 1 is automatically flagged
    # 2 is a stored flag
    # 3 is a stored TAG
  # f is the flag list
  # t is the tag list

  # master list of flags and tags
  gettgs = drop_get(path="SPflagsntags/flagsntags.Rda", local_file=file.path(tempdir(),"flagsntags.Rda"), dtoken=dto, overwrite=TRUE) # getting rda file
  if(gettgs){
    load(file.path(tempdir(),"flagsntags.Rda")) # load the Rda file
    fnt = reactiveValues(uflags=uflags,utags=utags)
  }else{
    fnt = reactiveValues(uflags="",utags="")
  }

  # Site name
  site = reactiveValues(id=NULL,daterange=NULL)
  # all flags and tags for a site
  allfnt = reactiveValues(aflags=NULL,atags=NULL)

  output$useBox <- renderValueBox({ valueBox(length(users$user), "Users", icon = icon("users")) })
  output$obsBox <- renderValueBox({ valueBox(paste0(0,"K"), "Observations", icon = icon("bar-chart"), color="yellow") })
  output$modBox <- renderValueBox({ valueBox(0, "Models run", icon = icon("cloud")) })
  output$updatetable <- renderDataTable({updatestab},
    options = list(paging = FALSE,searching=FALSE,ordering=FALSE),escape=FALSE,style="bootstrap",selection="none")
  output$table <- renderDataTable({mda},
    options = list(paging = FALSE,searching=FALSE,ordering=FALSE),escape=FALSE,style="bootstrap",selection="none")

  output$loginbox <- renderUI(login)
  observeEvent(input$Login, {
    if(!USER$Logged){ # not logged in, check if it passes
      username = input$userName
      password = input$passwd
      if (username %in% users$user & password == 'streams') { #users$pass
        USER$Name = unlist(strsplit(username,"@"))[1] # or just username
        USER$Logged = TRUE
      }else{ # failed login
        output$loginbox <- renderUI(loginfail)
      }
    }

    if(USER$Logged){ # passed login
      output$loginbox <- renderUI(loginpass)
      source("upload.R",local=TRUE)
      source("flag.R",local=TRUE)
      userstr <- span("Login successful as ",strong(USER$Name))
      logoutstr <- div(align="right",a(href="/","Log out"))
      loginpass <- box(title = "Logged in!",status="success",solidHeader=TRUE,userstr,logoutstr)
      output$Upload <- renderMenu(
        menuItem("Upload", icon = icon("cloud-upload"),
          menuSubItem("Sensor data", tabName="upload"),
          menuSubItem("Drop samples")
      ))
      output$Visualizer <- renderMenu( menuItem("Vizualizer", tabName = "view", icon = icon("line-chart")) )
      output$Modeler <- renderMenu( menuItem("Modeler", tabName = "model", icon = icon("cubes")) )
      output$SOPs <- renderMenu( menuItem("SOPs", tabName = "sop", icon = icon("file-text-o")) )
      output$fileup <- renderUI( fileInput('file1', NULL,
                                           accept = c('text/csv','text/comma-separated-values','text/plain','.csv','application/octet-stream')) )
    }
  })

}
