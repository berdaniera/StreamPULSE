library(DT)
require(shiny)
require(shinydashboard)
library('aws.s3')
# library(rdrop2)
library(e1071)
library(dplyr)
library(tidyr)
library(readr)
library(sbtools)
library(ggplot2)
cbPalette = c("#333333", "#E69F00", "#337ab7", "#009E73", "#56B4E9", "#009E73", "#666666", "#739E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ac = function(x) as.character(x)
# dto = readRDS("droptoken.rds")
if(is_logged_in()) session_logout()
asb = authenticate_sb(Sys.getenv("SB_LOGIN"), Sys.getenv("SB_PASS")) # login with renviron data
source("spFns.R")

tdatf = tempfile() # temporary data folder
dir.create(tdatf)
tmpwebfile = tempfile() # temporary web folder
dir.create(tmpwebfile)
sbopath = '58189f15e4b0bb36a4c82017' # original data files
sbrpath = '58189f03e4b0bb36a4c82014' # raw data files
sbwpath = "580f9ec1e4b0f497e796009b" # web file path
item_file_download(sbwpath, dest_dir=tmpwebfile, overwrite_file=TRUE, session=asb)
load(file.path(tmpwebfile,'spusers.Rda'))

useSB = TRUE
if(!useSB){
  s3load('meta/streampulseusers.Rda',bucket='streampulse') # load user dataset
}

# drop_get(path="streampulseusers.Rdata",local_file=file.path(tempdir(),"spusers.Rdata"),dtoken=dto,overwrite=TRUE) # getting rda file
# load(file.path(tempdir(),"spusers.Rdata")) # load the Rda file

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
  # training = reactiveValues(dat=NULL)
  # flags = reactiveValues(d=NULL,f=NULL,t=NULL) # placeholder for model flagged data
  # # d is the data with a flag column (f)
  #   # 0 is not flagged
  #   # 1 is automatically flagged
  #   # 2 is a stored flag
  #   # 3 is a stored TAG
  # # f is the flag list
  # # t is the tag list
  #
  # # master list of flags and tags
  # gettgs = drop_get(path="SPflagsntags/flagsntags.Rda", local_file=file.path(tempdir(),"flagsntags.Rda"), dtoken=dto, overwrite=TRUE) # getting rda file
  # if(gettgs){
  #   load(file.path(tempdir(),"flagsntags.Rda")) # load the Rda file
  #   fnt = reactiveValues(uflags=uflags,utags=utags)
  # }else{
  #   fnt = reactiveValues(uflags="",utags="")
  # }
  #
  # # # Site name
  # site = reactiveValues(id=NULL,daterange=NULL)
  # # # all flags and tags for a site
  # allfnt = reactiveValues(aflags=NULL,atags=NULL)

  datatab <- data.frame(Level=c("Level 0","Level 1A","Level 1B","Level 2","Level 3","Level 4"),
                    Description=c("Unprocessed sensor data","Calibrated raw data","Quality-checked (and maybe gap-filled?) data","Derived variables","Raw model output","Derived model output"),
                    Storage=c("ScienceBase","CUAHSI (public)","ScienceBase","ScienceBase","ScienceBase","ScienceBase"),
                    Example=c("Water pressure (kPa)","Water depth (m)","Flagged water depth (m)","Discharge (m3/s)","Metabolic flux (gO2/m2/d)","Metabolic flux (kgO2/d)"))
  updatestab <- data.frame(Date=c('2016-10-25','2016-09-06','2016-09-03','2016-07-08','2016-07-01'),
    Up=c("Download option available for raw data","QA/QC interface linked with data upload","SOP documents linked in","Dashboard online, accepting file uploads","Dashboard created"))

  output$useBox <- renderValueBox({ valueBox(length(users$user), "Users", icon = icon("users")) })
  nobs <- floor(sum(read_csv("datapoints.txt",col_names="n",col_types=cols()))/1000)
  # nobs <- 0
  output$obsBox <- renderValueBox({ valueBox(paste0(nobs,"K"), "Observations", icon = icon("bar-chart"), color="yellow") })
  output$modBox <- renderValueBox({ valueBox(0, "Models run", icon = icon("cloud")) })
  output$updatetable <- renderDataTable({updatestab},
    options = list(paging = FALSE,searching=FALSE,ordering=FALSE),escape=FALSE,style="bootstrap",selection="none")
  output$datatable <- renderDataTable({datatab},
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
      source("upload.R",local=TRUE)
      source("download.R",local=TRUE)
      # source("flag.R",local=TRUE)
      userstr <- span("Login successful as ",strong(USER$Name))
      logoutstr <- div(align="right",a(href="/","Log out"))
      loginpass <- box(title = "Logged in!",status="success",solidHeader=TRUE,userstr,logoutstr)
      output$loginbox <- renderUI(loginpass)
      output$Upload <- renderMenu(
        menuItem("Upload", icon = icon("cloud-upload"),
          menuSubItem("Sensor data", tabName="upload"),
          menuSubItem("Grab samples (coming soon)")
      ))
      output$Download <- renderMenu( menuItem("Download", tabName = "download", icon = icon("line-chart")) )
      output$Modeler <- renderMenu( menuItem("Modeler", tabName = "model", icon = icon("cubes")) )
      output$SOPs <- renderMenu( menuItem("SOPs", tabName = "sop", icon = icon("file-text-o")) )
      output$fileup <- renderUI( fileInput('file1', NULL, accept = c('text/csv','text/comma-separated-values','text/plain','.csv')) )
    }
  })

}
