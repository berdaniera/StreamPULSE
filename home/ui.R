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
              uiOutput('loginbox'),
              box(title="Updates",dataTableOutput("updatetable"))
            )
    ),
    tabItem(tabName = "upload",
            h2("Data upload and tagging/flagging"),
            fluidRow(
                box(title = "Upload merged data file to CUAHSI", width=4, status = "primary",
                  HTML(paste("Please only choose one file at a time. And be sure to check out the <a href='https://docs.google.com/document/d/1rF3Eo2AKlI_ewJubfQlu66-9GDCGCeTL6I6A9WhFcPA/edit?usp=sharing'>data upload SOP</a>. You can perform the upload/QAQC procedure multiple times.<br>",
                    "Your file should have a) only one header row, b) the first column with a UTC Date-Time, and c) data columns separated with commas.",
                    "For help, check out <a href='https://github.com/berdaniera/StreamPULSE/tree/master/spfns'>these R functions</a>.")),
                  br(),br(),
                  uiOutput('fileup'),
                  uiOutput('filestatsupload'),
                  br(),
                  textOutput('uploadstatus')
                ),
                box(title = "Upload raw data files for backup", width=4, status = "success",
                  HTML(paste("Here you can upload your raw datalogger files for permanent backup on a StreamPULSE cloud server. You can select multiple files to upload at once.")),
                  br(),br(),
                  uiOutput('filebackup'),
                  br(),
                  textOutput('backupstatus')
                ),
                column(4,
                  actionLink("showeg","How does data flagging work? [+/-]"),
                  conditionalPanel("input.showeg%2 == 1",
                    HTML("The model is a One-class <a href='https://en.wikipedia.org/wiki/Support_vector_machine'>Support Vector Machine</a> with a radial kernel. It fits a decision boundary based on the multivariate relationship between the training data (for more information, see the <a href='https://docs.google.com/document/d/1333al425SOjlUsJA61WbpScuQ2KZxCuTtkkcdPZJVLI/edit?usp=sharing'>QAQC SOP</a>). In this example, the training data are shown as open circles. Randomly generated test data are overlaid in colors, based on whether the trained model classified them as 'good' or as 'anomalous'.<br><br>"),
                    actionButton("egdata","Generate example training and test data"),
                    checkboxInput("egshow","Show test data on graph"),
                    plotOutput("egplot")
                  )
                )
            ),
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
