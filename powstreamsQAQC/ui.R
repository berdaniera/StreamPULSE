library(shiny)
load("powstreamseg.Rda")
ss = as.character(unique(dd$site))

shinyUI(fluidPage(
  titlePanel("Example data filtering and flagging"),
  sidebarLayout(

    sidebarPanel(
      selectInput("site", "Choose a site:", choices=ss),
      p("Number of observation days:", textOutput("numobs",inline=TRUE), "Error points:", textOutput("numerr",inline=TRUE)),
      sliderInput("err", "Select error threshold:", 0, 0.2, 0.01),
      checkboxInput("all", "Pool across all sites?", TRUE),
      checkboxInput("filt_er", "Filter for er<0", TRUE),
      checkboxInput("filt_gpp", "Filter for gpp>0", TRUE)
    ),

    mainPanel(
      plotOutput("plot"),
      plotOutput("pairplot",width="400px")
    )
  )
))
