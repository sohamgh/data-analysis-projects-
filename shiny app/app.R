#install.packages("shiny")
#install.packages("DT")
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("DT")
library("maps")
options(scipen = 999)
forest_data <- read.csv("data/WBI_Forest_Area_Cleaned.csv", stringsAsFactor = FALSE)
##cleaning
forest_data <-  forest_data %>% 
  gather(key = 'year' , value = 'value', YR1992:YR2016)%>%
  filter(Series.Name != "Land area (sq. km)") %>%
  select(-Series.Code , -X) %>%
  mutate(year = as.numeric(substr(year, 3, nchar(year))))


my_ui <- fluidPage(
  sidebarLayout(
    # interaction panel
    sidebarPanel(
      ##select the features to display
      radioButtons("type", "Data Type",
                   unique(forest_data$Series.Name)),
      # Select the year range to display the observations for
      sliderInput(inputId = "Years", label = "Years",
                  min = 1992, max = 2016, value = c(min, max), step = 1,
                  animate = animationOptions(interval = 100))
    ), 
    # display panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Table", textOutput("selected_var2") ,DT::dataTableOutput("mytable")),
                  tabPanel("Data Plot", textOutput("selected_var1") ,plotOutput("plot"))
      )
    )
  )
  
)


my_server <-  function(input, output){
  output$plot <- renderPlot({
    
    world_map <- map_data("world") %>%
      mutate(Country.Code = iso.alpha(region , 3))
    ##filtering based on user input 
    forest_data_new <-  forest_data %>% 
      filter(year == input$Years[1] | year == input$Years[2]) %>% 
      filter(Series.Name == input$type) %>%
      spread(year, value)
    #adding change column
    forest_data_new <-  forest_data_new %>%
      mutate(change = forest_data_new[, 5] - forest_data_new[, 4])
    
    world_forest_map <- left_join(world_map, forest_data_new, by = "Country.Code") 
    ##finding the 5 bins based on quantiles 
    bin_values <- quantile(world_forest_map$change , probs = c(0, 0.2, 0.4, 0.6, 0.8, 1) , na.rm = T)
    bin_values_rounded <-  round(bin_values)
    world_forest_map <- world_forest_map %>% 
      mutate(`Percentage change` = cut(change, breaks=bin_values, labels=c(paste(bin_values_rounded[1],"to",bin_values_rounded[2]), 
                                                                           paste(bin_values_rounded[2],"to",bin_values_rounded[3]), 
                                                                           paste(bin_values_rounded[3],"to",bin_values_rounded[4]), 
                                                                           paste(bin_values_rounded[4],"to",bin_values_rounded[5]), 
                                                                           paste(bin_values_rounded[5],"to",bin_values_rounded[6]))))
    
    
    ggplot(data = world_forest_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = `Percentage change`)) +
      scale_fill_brewer(palette = "RdYlGn") +
      labs(title = paste("Change in" , input$type , "between the years" ,input$Years[1] , "and" ,input$Years[2] ) , x = "", y = "" , fill = "change") +
      coord_quickmap() +
      theme(legend.position = "bottom")
    
  })
  output$mytable <-  renderDataTable({
    ## make table based on user input
    forest_data_new <-  forest_data %>% 
      filter(year >= input$Years[1] | year <= input$Years[2]) %>% 
      filter(Series.Name == input$type) %>%
      select(Country.Name , year, value) %>%
      spread(year, value)
    DT::datatable(forest_data_new, options = list(lengthMenu = c(15, 30, 45), pageLength = 20))
  })
  output$selected_var1 <- renderText({ 
    paste("In the map below each color represents the magnitude of the change in", input$type,"between the years" ,input$Years[1] , "and" ,input$Years[2], ". Red and Orange usually depicts a decrease while all Shades of green depict an increase. The magnitude of the brackets have been determined by quantiles, although there may be better ways to do that. ")
  })
  output$selected_var2 <- renderText({ 
    paste("The table below conatins data on", input$type,"from the years" ,input$Years[1] , "to" ,input$Years[2])
  })
}

shinyApp(ui = my_ui , server = my_server)