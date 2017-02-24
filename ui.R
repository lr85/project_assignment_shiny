#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Survival in the disaster of the Titanic"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("selectClass", label = h3("Segment by passenger class"), 
                   choices = list("Overall", "Full Class Segmentation" = "Full Segmentation", 
                                  "Only 1st Class" = "1st", 
                                  "Only 2nd Class" = "2nd",
                                  "Only 3rd Class"= "3rd",
                                  "Only Crew" = "Crew"), selected ="Overall"),
       h3("Segment by age"),
       checkboxInput("checkboxAdult", label = "Adult", value = FALSE),
       checkboxInput("checkboxChild", label = "Child", value = FALSE),
       h3("Segment by sex"),
       checkboxInput("checkboxMale", label = "Male", value = FALSE),
       checkboxInput("checkboxFemale", label = "Female", value = FALSE)
       ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Documentation",
                           h3("How to use this App"),
                           textOutput("doc")
                  ),
                  tabPanel("Application",
                           h3("Reactive information about the chosen segmentation"),
                           textOutput("message_sex"), 
                           textOutput("message_age"),
                           textOutput("message_class"),
                           h3("Reactive plot based on the chosen segmentation"),
                           plotOutput("distPlot"),
                           h3("Reactive results computed on the chosen segmentation"),
                           tableOutput("table_result")
                           )
                  )
    )
  )
))
