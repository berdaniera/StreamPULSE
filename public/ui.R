shinyUI(fluidPage(
  uiOutput("sitename"),
  div(style="display:inline-block; vertical-align:top;",
    p("Change date range: ")),
  div(style="display:inline-block; vertical-align:top;",
    dateRangeInput("daterange", label=NULL)),
  div(style="display:inline-block; vertical-align:top;",
    actionButton("update","View")),
  plotOutput("pT"),
  plotOutput("pD"),
  plotOutput("pL")
))
