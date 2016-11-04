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
  menuItemOutput("Download"),
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
      h2("Data upload"),
      fluidRow(
        column(6,
        box(title = "Upload raw data files", width=12, status = "success",
          HTML(paste("Only select files from a single site with each upload (although you can select multiple files from different download dates at the same site).",
          "Before uploading, ensure that files match the <b>REGIONID_SITEID_YYYY-MM-DD_LOGGERID.xxx</b> format.",
          "<br><br><u>Primary (core/ancillary) sites</u> can upload their raw datalogger files (CS.dat and hobo .csv) or a pre-processed file.",
          "<br><u>Leveraged sites</u> need to upload <a href='https://docs.google.com/document/d/1rF3Eo2AKlI_ewJubfQlu66-9GDCGCeTL6I6A9WhFcPA/edit?usp=sharing'>pre-processed data</a>. ",
          "<b>See the + tab below</b>.")),
          br(),br(),
          fileInput('uploadFile', NULL, multiple=TRUE),
          br(),
          uiOutput('uploadhandle')
        ),
        box(title="Data formatting for leveraged sites", width=12,
          includeMarkdown('leverageup.md'), collapsible=TRUE, collapsed=TRUE
        )
        ),
        box(title="Upload status", width=6,
          uiOutput('spinupstatus')
        )
          # box(title = "Upload merged data file to CUAHSI", width=4, status = "primary",
          #   HTML(paste("Please only choose one file at a time. And be sure to check out the <a href='https://docs.google.com/document/d/1rF3Eo2AKlI_ewJubfQlu66-9GDCGCeTL6I6A9WhFcPA/edit?usp=sharing'>data upload SOP</a>. You can perform the upload/QAQC procedure multiple times.<br>",
          #     "Your file should have a) only one header row, b) the first column with a UTC Date-Time, and c) data columns separated with commas.",
          #     "For help, check out <a href='https://github.com/berdaniera/StreamPULSE/tree/master/spfns'>these R functions</a>.")),
          #   br(),br(),
          #   uiOutput('fileup'),
          #   uiOutput('filestatsupload'),
          #   br(),
          #   textOutput('uploadstatus')
          # ),
          # column(4,
          #   actionLink("showeg","How does data flagging work? [+/-]"),
          #   conditionalPanel("input.showeg%2 == 1",
          #     HTML("The model is a One-class <a href='https://en.wikipedia.org/wiki/Support_vector_machine'>Support Vector Machine</a> with a radial kernel. It fits a decision boundary based on the multivariate relationship between the training data (for more information, see the <a href='https://docs.google.com/document/d/1333al425SOjlUsJA61WbpScuQ2KZxCuTtkkcdPZJVLI/edit?usp=sharing'>QAQC SOP</a>). In this example, the training data are shown as open circles. Randomly generated test data are overlaid in colors, based on whether the trained model classified them as 'good' or as 'anomalous'.<br><br>"),
          #     actionButton("egdata","Generate example training and test data"),
          #     checkboxInput("egshow","Show test data on graph"),
          #     plotOutput("egplot")
          #   )
          # )
      )
      # uiOutput('flagui'),
      # uiOutput('flagplt')
    ),
    tabItem(tabName = "download",
      h2("Data download"),
      column(width=6,
        box(title = "Pull down the latest raw data", width=12, status = "success",
          p("Select the site(s) that you'd like to access. You can choose multiple sites. This will download a .csv file with the raw data."),
          uiOutput('datadnld')
        ),
        box(title="How to work with this data", width=12,
          includeMarkdown('dnldcode.md')
        )
      ),
      # box(title = "Pull down the latest raw data", width=6, status = "success",
      #   p("Select the site(s) that you'd like to access. You can choose multiple sites. This will download a .csv file with the raw data."),
      #   uiOutput('datadnld')
      # ),
      column(width=6,
        box(title="Data levels", width=12, dataTableOutput("datatable")),
        box(title="Original data files", width=12,
          HTML(paste("Our original data files are stored on ScienceBase and can be <a href='https://www.sciencebase.gov/catalog/item/57b1cefde4b0fc09fab1e218'>accessed here</a>."))
        )
      )
    ),
    tabItem(tabName = "model",
      h2("Modeler interface"),
      p("Next year we will have an area for working with model output.")
    ),
    tabItem(tabName = "sop",
      HTML('<iframe src="https://drive.google.com/embeddedfolderview?id=0B7rFlnRNN7_dUW8tc1h5REZfQk0#list" style="width:100%; height:600px; border:0;"></iframe>')
    )
  ))

ui <- dashboardPage(header, sidebar, body, title="StreamPULSE")
