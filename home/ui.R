library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
options(shiny.maxRequestSize=30*1024^2) # 30 MB upload max

header <- dashboardHeader(title = "StreamPULSE Hub")

sidebar <- dashboardSidebar(sidebarMenu(
  menuItem("Overview",tabName = "main", icon = icon("tachometer"),selected=TRUE),
  menuItemOutput("Upload"),
  menuItemOutput('Cleaner'),
  menuItemOutput("Visualizer"),
  menuItemOutput('Modeler'),
  menuItemOutput('SOPs')
))

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "main",
            fluidRow(
              valueBoxOutput("obsBox"),
              valueBoxOutput("useBox"),
              valueBoxOutput("modBox")
            ),
            fluidRow(
              # sparklines by site - other visualizations
              # QA/QC leaderboard
              # any updates box
              uiOutput('loginbox'),
              box(title="Updates",dataTableOutput("updatetable"))
            )
    ),
    tabItem(tabName = "upload",
            h2("Data upload and tagging/flagging"),
            HTML(paste("<b>Choose a file to upload.</b>",
              "Please only choose one file at a time. And be sure to check out the <a href='https://docs.google.com/document/d/1rF3Eo2AKlI_ewJubfQlu66-9GDCGCeTL6I6A9WhFcPA/edit?usp=sharing'>data upload SOP</a>. You can perform the upload/QAQC procedure multiple times.<br>",
              "For now, we are equipped to accept raw '.csv' data files from Campbell Scientific CR1000 and Hobo dataloggers.",
              "<u>If you have data from another datalogger</u>, you can either <a href='mailto:aaron.berdanier@gmail.com'>email Aaron</a> a file sample <i>or</i> ensure that the data file has only one header row and is comma separated.")),
            br(),br(),
            uiOutput('fileup'),
            uiOutput('filestatsupload'),
            br(),
            textOutput('uploadstatus'),
            uiOutput('flagui'),
            uiOutput('flagplt')
    ),
    tabItem(tabName = "view",
            h2("Data view and download"),
            p("Here we will have 1. instructions for how to download data on your own from R and 2. an interface for selecting and downloading data."),
            box(title="Data levels",dataTableOutput("table"))
    ),
    tabItem(tabName = "model",
            h2("modeler interface"),
            p("Later this fall we will have an area for visualizing model output."),
            p("We may also implement a section to 'request' model runs with specific attributes.")
    ),
    tabItem(tabName = "sop",
      HTML('<iframe src="https://drive.google.com/embeddedfolderview?id=0B7rFlnRNN7_dUW8tc1h5REZfQk0#list" style="width:100%; height:600px; border:0;"></iframe>')
    )
  ))

ui <- dashboardPage(header, sidebar, body, title="StreamPULSE")
