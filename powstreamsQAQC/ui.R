library(shiny)
load("powstreamseg.Rda")
ss = as.character(unique(dd$site))

shinyUI(fluidPage(
  titlePanel("Example data filtering and flagging"),
  sidebarLayout(

    sidebarPanel(
      HTML("<b>How this works:</b><br>
        I randomly chose 100 sites with models from 'gpp_estBest'.<br><br>
        The algorithm looks for anomalies in the multivariate relationship between gpp, er, and k600 across sites.
        It considers the training data as 'good' with some allowable error (threshold variable). Filters restrict the 'good' training data.
        If dy/dt is included, it also looks for anomalies in the change in each variable through time.<br><br>
        For each site, I train with data from that site as well as the 9 'closest' sites in average discharge.<br><br>
      "),
      selectInput("site", "Choose a site:", choices=ss),
      p("Number of observation days:", textOutput("numobs",inline=TRUE), "Error points:", textOutput("numerr",inline=TRUE)),
      sliderInput("err", "Select error threshold (amt. of expected error in data set):", 0, 0.2, 0.025),
      checkboxInput("delt", "Include dy/dt", TRUE),
      checkboxInput("filt_er", "Filter for er < 0", TRUE),
      checkboxInput("filt_gpp", "Filter for gpp > 0", TRUE)
    ),

    mainPanel(
      plotOutput("plot"),
      plotOutput("pairplot",width="400px")
    )
  )
))
