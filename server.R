#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # import useful libraries
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  
  # prepare the dataset by first transforming the Titanic data (from the R package
  # "datasets") from table to dataset
  titanic_data <- as.data.frame(Titanic)
  # create flag variables on whether or not use a certain dimension to segment the
  # analysis
  use_sex <- reactive({
    ifelse(!input$checkboxMale & !input$checkboxFemale,"FALSE","TRUE")
  })
  use_age <- reactive({
    ifelse(!input$checkboxChild & !input$checkboxAdult,"FALSE","TRUE")
  })
  use_class <- reactive({
    ifelse(input$selectClass == "Overall","FALSE","TRUE")
  })
  # define some reactive getters that can be useful throughout the script
  get_sex <- reactive({
    c(ifelse(input$checkboxMale, "Male", ""),ifelse(input$checkboxFemale,"Female",""))
  })
  
  get_age <- reactive({
    c(ifelse(input$checkboxChild, "Child", ""),ifelse(input$checkboxAdult,"Adult",""))
  })
  
  
  # the following function filters the dataset according to the selection of the
  # user
  determine_output = reactive({
    output_data <- titanic_data
    
    # determine the output data to be plotted according to the user input
    if(use_sex()==TRUE){
      output_data <- filter(output_data, Sex %in% get_sex())
    }
    
    if(use_age()==TRUE){
      output_data <- filter(output_data, Age %in% get_age())
    }

    if(input$selectClass=="1st"){
      output_data <- filter(output_data, Class == "1st")
    }
    if(input$selectClass=="2nd"){
      output_data <- filter(output_data, Class == "2nd")
    }
    if(input$selectClass=="3rd"){
        output_data <- filter(output_data, Class == "3rd")
    }
    if(input$selectClass=="Crew"){
        output_data <- filter(output_data, Class == "Crew")
    }
    
    output_data
  })
  
  
  #the following function determines the number of people survived and dead
  get_result <- reactive({
    result <- determine_output()
    
    if(use_class()== TRUE & use_age() == TRUE & use_sex() == TRUE){
      result <- result %>% 
        group_by(Class,Age,Sex,Survived) %>%
        summarize(Freq = sum(Freq))
    }
    
    if(use_class()== TRUE & use_age() == FALSE & use_sex() == FALSE){
      result <- result %>% 
        group_by(Class,Survived) %>%
        summarize(Freq = sum(Freq))
    }
    
    if(use_class()== TRUE & use_age() == TRUE & use_sex() == FALSE){
      result <- result %>% 
        group_by(Class,Age,Survived) %>%
        summarize(Freq = sum(Freq))
    }
    
    if(use_class()== TRUE & use_age() == FALSE & use_sex() == TRUE){
      result <- result %>% 
        group_by(Class,Sex,Survived) %>%
        summarize(Freq = sum(Freq))
    }
    
    if(use_class()== FALSE & use_age() == TRUE & use_sex() == TRUE){
      result <- result %>% 
        group_by(Age,Sex,Survived) %>%
        summarize(Freq = sum(Freq))
    }
    
    if(use_class()== FALSE & use_age() == FALSE & use_sex() == FALSE){
      result <- result %>% 
        group_by(Survived) %>%
        summarize(Freq = sum(Freq))
    }
    
    # reshape the table of results for better readability
    result <- spread(result, Survived, Freq)
    names(result)[names(result) == "No"] <- "Dead"
    names(result)[names(result) == "Yes"] <- "Survived"
    result
  })
  
  
  
  # interactively generate a conditional plot according to the choices of the user
  conditional_plot <- reactive({
    g <- ggplot()
    output_data <- determine_output()
    
    # now determine the plot to display
    ##  no segmentation at all
    if(use_class()==FALSE){
      g <- ggplot(output_data, aes(x=Survived,y=Freq))+ 
        geom_bar(stat='identity')
    } else{
      g <- ggplot(output_data, aes(x=Class,y=Freq, fill = Survived))+
        geom_bar(stat='identity')
    }
      
    ## Adding segmentation by sex to the plot
    if(use_sex()==TRUE & use_age()==FALSE){
      g <- g + facet_grid(Sex ~ .) 
    }
    
    
    ## Adding segmentation by age to the plot
    if(use_age()==TRUE & use_sex() == FALSE){
      g <- g + facet_grid(Age ~ .) 
    }
    
    ## Adding segmentation by age and sex to the plot
    if(use_age()==TRUE & use_sex() == TRUE){
      g <- g + facet_grid(Sex ~ Age) 
    }
    
    g
    
  })
  
  
  # render the output plot
  output$distPlot <- renderPlot({
    # case when no dimensions are selected
    conditional_plot()
  })
  
  
  
  # render the output message regarding the segmentation by sex
  output$message_sex <- renderText({
    str_sex <- NULL
    if(use_sex()==FALSE){
      str_sex <- "You are not segmenting by sex"
    }else{
      str_sex <- paste(get_sex(), collapse = " ")
      str_sex <- paste("Segmented by sex:",str_sex)
    }
    
    str_sex
    
  })
  
  # render the output message regarding the segmentation by age
  output$message_age <- renderText({
    str_age <- NULL
    if(use_age()==FALSE){
      str_age <- "You are not segmenting by age"
    }else{
      str_age <- paste(get_age(), collapse = " ")
      str_age <- paste("Segmented by age:",str_age)
    }
    str_age
    
  })
  
  
  # render output message regarding the segmentation by passenger class
  output$message_class <- renderText({
    str_class <- NULL
    if(use_class()==FALSE){
      str_class <- "You are not segmenting by passenger class"
    }else{
      str_class <- paste("Segmented by passenger class:", input$selectClass)
    }
    str_class
    
  })
  
  # render the table with the result for the segmentation defined by the user
  output$table_result <- renderTable({
    get_result()
  })
  
  
  # render the output text about the documentation
  
  output$doc <- renderText({
    doc <- 'This application features a reactive data exploration analysis
    on the disaster of the Titanic. The app allows the user to select different
    segmentation dimensions, then it reactively generates the graphs and the table 
    results that show the number of survived and not survived people according 
    to the chosen segmentation. To start using the app, just click on the next 
    tab "Application", choose your segmentation dimensions on the sidebar and
    explore the results. The source code for this project can be found at the 
    following link: https://github.com/lr85/project_assignment_shiny
    '
      
      
    doc
    
    
  })
  
  
})
