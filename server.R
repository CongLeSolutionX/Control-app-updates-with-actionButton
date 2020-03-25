library("tidyverse")

# loading wdi dataset 
wdi_data <- read_csv("data/wdi_data.csv")
# loading dataset of contries and continent
countries_and_continents <- read_csv("data/countries.csv") %>%
  filter(name %in% wdi_data$country)
# reading indicators 
indicators <- read_csv("data/indicators.csv")

function(input, output, session) {
  #---- Select inputs -------
  updateSelectInput(session,
                    "selected_continent",
                    choices = unique(countries_and_continents$continent))
  
  observeEvent(input$selected_continent,
               {
                 countries_in_continent <- countries_and_continents %>%
                   filter(continent == input$selected_continent) %>%
                   pull(name)
                 
                 updateSelectInput(session,
                                   "selected_country",
                                   choices = countries_in_continent)
                 
               })
  
  updateSelectInput(session,
                    "selected_indicator",
                    choices = setNames(indicators$indicator_code, indicators$indicator_name))
  
  #------ The indicator chart based on the dataset wdi -----
  output$wdi_indicator_chart <- renderPlot({
    # for debug 
    print(input$update_chart)
    #----- not display the chart until the user click on the action button ----
    if(input$update_chart == 0){
      return()
    }
    # --- get the indicator-----
    selected_indicator_name <- indicators %>%
      filter(indicator_code == isolate(input$selected_indicator)) %>%
      pull(indicator_name)
    # --- get the data ------
    wdi_data %>%
      filter(
        country == isolate(input$selected_country)
      ) %>%
      filter(indicator == isolate(input$selected_indicator)) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = year, y = value)) +
      geom_path() +
      labs(
        title = paste("Individuals using the Internet (% of population)", "in", isolate(input$selected_country)),
        subtitle = "Data source: WDI Package, see data/world-bank.R for details"
      )
  })
  
}