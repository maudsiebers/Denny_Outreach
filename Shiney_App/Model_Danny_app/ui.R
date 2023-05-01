#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# install.packages('shinycustomloader')
library(shinycustomloader)
navbarPage(
  'Lake Prediction game',
  tabPanel('Chlorophyll a',
           fluidPage(fluidRow(
             column(
               sliderInput("Temp",
                           "Temperature multiplication by:",
                           min = 0,
                           max = 4,
                           value = 1,
                           step = 0.25),
               width = 3
             ),
             column(
               sliderInput("N",
                           "Nitrogen multiplication by:",
                           min = 0,
                           max = 4,
                           value = 1,
                           step = 0.25)
               ,
               width = 3
             ),
             column(
               sliderInput("P",
                           "Phosphorus multiplication by:",
                           min = 0,
                           max = 4,
                           value = 1,
                           step = 0.25)
               ,
               width = 3
             ),
             column(
               sliderInput("S",
                           "Silica multiplication by:",
                           min = 0,
                           max = 4,
                           value = 1,
                           step = 0.25)
               ,
               width = 3
             )
           )),
           actionButton("run_model", label = "Run Model", width = "100%"),
           withLoader( plotOutput("CHLA"))
  ),
  tabPanel('Oxygen',
           fluidPage(fluidRow(
    plotOutput('Oxy')
  ))),
  collapsible = TRUE
)
