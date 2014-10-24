# ui.R
library(shiny)

shinyUI(fluidPage(
    titlePanel("Plotting Ebola"),

    tags$head(includeScript("google-analytics.js")),

    sidebarLayout(
        sidebarPanel("Interactive plot components",

                     uiOutput("countriesList"),
                     "Scale for cases:",
                     uiOutput("plot_cases_controls"),
                     "Scale for deaths:",
                     uiOutput("plot_deaths_controls")
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
