library(shiny)
library(ggplot2)
load("sites.Rda")

# Define UI for miles per gallon application
shinyUI(fluidPage(
  fluidRow(
    column(width=6,
      h2("StreamPULSE")
    ),
    column(width=2,
      br(),
      HTML("Enter your email for updates about our data and analysis products:")
    ),
    column(width=3,
      textInput("email","",placeholder="Your email address",width='100%')
    ),
    column(width=1,
      br(),
      actionButton("contact","Add",icon=icon("envelope-o"),width='100%')
    )
  ),
  fluidRow(column(width=6,
    wellPanel(
      HTML("<font size='4'>Recent data</font>  (Choose sites to view)"),
      selectizeInput('site', '', choices=spsites, multiple=TRUE, selected=spsites)
    ),
    fluidRow(
      HTML("<i><center>Highlight and double click to zoom.</center></i>"),
      plotOutput("flagplot", height="600px", dblclick="plot_dblclick", brush=brushOpts(id="plot_brush",direction="x"))
    )
  ),
  column(width=6,
    wellPanel(
      HTML("<font size ='4'>Metabolism models</font>  (Choose sites to view)"),
      selectizeInput('powsite', '', choices=as.character(powsites), multiple=TRUE, selected=as.character(powsites[1:5]))
    ),
    fluidRow(
      HTML("<i><center>95% kernel density of daily metabolism estimates</center></i>"),
      plotOutput("powplot", height="600px")
    )
  )),
  title="StreamPULSE"
))
