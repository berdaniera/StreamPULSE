library(shiny)
options(shiny.maxRequestSize=30*1024^2) # 30 MB upload max

shinyUI(fluidPage(title="Data flagging",
  #tags$head( tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lato:300');")) ),
  wellPanel(div(align="center","Timeseries data quality assurance and flagging",style="font-family:sans-serif; font-size:20px; color: #fff; text-align: center;"),
    style="background-image: url('strip.png'); border-color: #2e6da4;"),
  sidebarPanel(
    HTML("<i>Data requirements:</i> One column named 'DateTime' with an R-readable date or time format and at least one data column."),
    fileInput('file1', '1. Upload data to test for anomalies.',
                accept=c('text/csv','text/comma-separated-values,text/plain','.csv','.dat')),
    strong("2. Training data:"),
    tabsetPanel(
      tabPanel("The uploaded data", value="new",
        # choose training data
        br(),
        # select columns to include in model
        selectizeInput("vari", "Choose variables for training model:", choices=NULL, multiple=TRUE),
        # enter filters for training data
        textInput("filt", "Enter data filter to apply for training data (optional):",placeholder="Temp<20&DateTime>'2016-01-01'"),
        p("This is a conditional statement. For help, ",actionLink("conditionals","click here.")),
        conditionalPanel("input.conditionals%2 == 1","Need to add some description of conditional statements here."),
        br()
      ),
      tabPanel("Previously-trained data", value="old",
        br(),
        selectizeInput("predat", "Choose existing flagged dataset:", choices="No data found...", multiple=FALSE)
      ),
    id="traindat",type="tabs"),
    # choose nu value for SVM model
    sliderInput("nu", "3. Select an error threshold for training data:", 0, 0.2, 0.01),
    p("Tip: As the training data are filtered to only include 'good' data, the error threshold can be lower (since we expect fewer errors in the training data)."),
    htmlOutput("fiterr"), # check if there is an error in model
    uiOutput("fitbutton"), # load model fit button if data is uploaded
    HTML("<br><center><font color='#666666'><i>Note: The model can take some time to fit... please be patient.</i></font></center>"),
    # add conditional panel to explain conditional statements
    # add option to show example, which pops up conditional panel for other plot....
    br(),
    actionLink("showeg","How does this work?"),
    conditionalPanel("input.showeg%2 == 1",
      HTML("The model is a One-class <a href='https://en.wikipedia.org/wiki/Support_vector_machine'>Support Vector Machine</a> with a radial kernel. It fits a decision boundary based on the multivariate relationship between the training data. In this example, the training data are shown as open circles. Randomly generated test data are overlaid in colors, based on whether the trained model classified them as 'good' or as 'anomalous'.<br><br>"),
      actionButton("egdata","Generate example training and test data"),
      checkboxInput("egshow","Show test data on graph"),
      plotOutput("egplot")
    )
  ),

  mainPanel(
    fluidRow(
      wellPanel(
        fluidRow(column(width=12,
          HTML("<b>Instructions:</b><br><ul>
            <li>Highlight data by dragging and selecting on the graph.</li>
            <li><b>Add</b> or <b>remove</b> flagged points by highlighting and clicking the buttons below.</li>
            <li><b>Store</b> flags by:<ol>
              <li><i>highlighting</i> the desired flagged data,</li>
              <li><i>naming</i> the flag (and adding optional comments),</li>
              <li><i>clicking</i> 'Store selected flags' button.</li></ol>
            <li><b>Zoom</b> in on the graph by highlighting a time range and double clicking on the graph.</li>
            <li><b>When done:</b> Save and/or download the flagged data</li>
          </ul>")
        )),
        fluidRow(column(width=12,
          actionButton("flag_new", "Flag selected", icon=icon("flag"),
            style="color:#fff; background-color: #E69F00; border-color: #fff"),
          actionButton("flag_erase", "Un-flag selected", icon=icon("flag-o"),
            style="color:#fff; background-color: #CC79A7; border-color: #fff"),
          actionButton("flag_clear", "Clear all unstored flags", icon=icon("eraser"),
            style="color: #fff; background-color: #009E73; border-color: #fff"),#d9534f
          actionButton("flag_reset", "Reset zoom", icon=icon("repeat"),
            style="color: #fff; background-color: #ff7f50; border-color: #fff"),
          span("..."),
          actionButton("flag_store", "Store selected flags", icon=icon("database"),
            style="color: #fff; background-color: #337ab7; border-color: #fff")
        )),br(),
        fluidRow(
          column(width=4,selectizeInput("flag_name", "Flag name/ID (either choose or type to add):", choices = NULL, options = list(create = TRUE))),
          column(width=8,textInput("flag_comments","Flag comment:",placeholder="Enter flag comments"))
        ),
        fluidRow(column(width=12,
          actionButton("flag_save", "Save stored flags and clean data for future model fits", icon=icon("folder"),
            style="color: #fff; background-color: #337ab7; border-color: #fff"),
          actionButton("flag_save", "Download flagged data and metadata as .csv", icon=icon("download"))
          # downloadButton('flag_download', 'Download flagged data as .csv')
        )),
        textOutput("loadtext")
      )
    ),
    fluidRow(
      column(width = 12,
             plotOutput("flagplot", dblclick = "plot_dblclick", brush = brushOpts(id = "plot_brush",direction="x"))
      )
    )
  ),
  HTML("<center><font color='#aaa'><i>Built by <a href='http://github.com/berdaniera/'>Aaron</a>. Powered by <a href='http://shiny.rstudio.com/'>Shiny.</a></i></font></center>")

))
