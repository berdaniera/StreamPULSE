library(shiny)
library(tidyr)
library(ggplot2)
library(e1071)
cbPalette <- c("#333333", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ui = fluidPage(
  fluidRow(
    selectizeInput(
      'cleanIn', 'Choose a dataset:',
      choices = c("PuertoRicoSample1","PuertoRicoSample2","PuertoRicoSample2-drift")
    ),
    actionButton("cleanButton", "Load data"),
    textOutput("loadtext")
  ),
  fluidRow(
    wellPanel(actionButton("flag_new", "Flag selected"),
              actionButton("flag_erase", "Erase selected"),
              actionButton("flag_clear", "Clear all", 
                           style="color: #fff; background-color: #009E73"),#d9534f
              actionButton("flag_send", "Approve flags", 
                           style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              textInput("flag_comments","",width='500px',placeholder="Enter flag comments"))
  ),
  fluidRow(
    column(width = 12,
           plotOutput("flag_plot", brush = brushOpts(id = "plot_brush",direction="x"))
    )
  )
)