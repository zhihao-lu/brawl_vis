library(tidyverse)
library(lubridate)
library(googlesheets4)
library(shiny)
ui <- fluidPage(
  checkboxGroupInput(inputId = "maptype",label="Map Type",
                     choices = c("gemGrab","bounty","heist","brawlBall","hotZone","siege")
  ),
  checkboxGroupInput(inputId = "gametype",label="Game Type",
                     choices = c("soloRanked", "teamRanked")
  ),
  plotOutput("plot")
)

server <- function(input, output) {
  #options(gargle_oauth_cache = ".secrets")
  #gs4_auth()
  options(
    gargle_oauth_cache = ".secrets",
    gargle_oauth_email = TRUE
  )
  data <- read_sheet("https://docs.google.com/spreadsheets/d/1-b6HCSTrTdS12dLWhqq3VzWGPrWf9sbN1r1L4vWNzAI/edit#gid=2008828887", sheet = "Sheet3")
  print("hi")
  data_new <- mutate(data, day = date(ymd_hms(DateTime)))
  #data_filter <- filter(data, Event == input$maptype)
  output$plot <- renderPlot({
    ggplot(filter(data_new, Event == input$maptype, Mode == input$gametype)) +
      geom_bar(aes(x = day, fill = Result))
  })
}

shinyApp(ui = ui, server = server)