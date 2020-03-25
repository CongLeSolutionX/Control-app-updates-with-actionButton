library("DT")

navbarPage(
  "Control updates with actionButton | World Bank Data",
  tabPanel(
    "Selected indicator",
    fluidPage(fluidRow(
      column(
        selectInput("selected_continent",
                    "Select a continent",
                    choices = NULL),
        width = 3
      ),
      column(
        selectInput("selected_country",
                    "Select a country",
                    choices = NULL),
        width = 4
      ),
      column(
        selectInput("selected_indicator",
                    "Select an indicator",
                    choices = NULL,
                    width = "100%"),
        width = 5
      )
    )),
    actionButton("update_chart", label = "update_chart", width = "100%"),
    plotOutput("wdi_indicator_chart")
  ),
  collapsible = TRUE
)