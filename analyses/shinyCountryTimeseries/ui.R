# ui.R
library(shiny)
library(ggvis)

shinyUI(fluidPage(
    titlePanel("Plotting Ebola"),

    tags$head(includeScript("google-analytics.js")),

    sidebarLayout(
        sidebarPanel("Plot Options",
              checkboxInput("all_countries", "Show all countries", value=T),
                   conditionalPanel(
                     condition= "input.all_countries == false",
                     uiOutput("countriesList")),
                 h3("Scales"),
				 checkboxInput("absolute", "Absolute time base"),
                 checkboxInput("log_scale", "Use log scales", value=F)
                 ),

        mainPanel(p("These graphs show incidence and death rates of Ebola in countries where infections have been reported."),
                  p("Data was all taken from Caitlin River's",
                  a('"ebola" repository', href = 'https://github.com/cmrivers/ebola'),
                  "and this interface is forked from ", a("Daniel Chen's","https://github.com/chendaniely/ebola"),"app."),

                  ggvisOutput("plot_cases"),
                  ggvisOutput("plot_deaths")
                  )
        )
))
