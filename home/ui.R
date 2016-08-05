library(shiny)
library(shinydashboard)
library(DT)

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
            h2("Data upload"),
            span(strong("Choose a file to upload. "),"You can select and upload multiple files at once. You can also perform the upload multiple times."),
            uiOutput('fileup'),
            uiOutput('filestatsupload'),
            #uiOutput('awsupload'),
            br(),
            textOutput('uploadstatus')
    ),
    tabItem(tabName = "clean",
            h2("Clean it up!"),
            p("Cleaning within timeseries can partially be automated, to check for sensor errors."),
            p("We will also have an interface for investigator cleaning: manual flagging and checking."),
            p("From here, level 1 data will be automatically uploaded to ScienceBase."),
            h3("Should be up and running before mid-August (expect automated flagging by end of July)")
#             FLAGGING SCRIPT
#             fluidRow(
#               column(width = 6,
#                      plotOutput("flag_plot", height=300,brush = brushOpts(id = "plot_brush",direction="x"))
#               ),
#               column(width = 4,
#                      wellPanel(actionButton("flag_new", "Flag"),actionButton("flag_clear", "Reset"))
#               )
#             )
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
    )
  ))

ui <- dashboardPage(header, sidebar, body, title="StreamPULSE")
