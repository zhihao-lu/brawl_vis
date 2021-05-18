library(tidyverse)
library(lubridate)
library(googlesheets4)
library(shiny)
library(shinydashboard)


ui <- dashboardPage(dashboardHeader(),
                    dashboardSidebar(),
                    dashboardBody(
                      fluidRow(
                        valueBoxOutput("winRateBox"),
                        valueBoxOutput("soloWinRateBox"),
                        valueBoxOutput("teamWinRateBox")
                    )))

                   

server <- function(input, output) {
  #options(gargle_oauth_cache = ".secrets")
  #gs4_auth()
  options(gargle_oauth_cache = ".secrets",
          gargle_oauth_email = TRUE)
  data <-
    read_sheet(
      "https://docs.google.com/spreadsheets/d/1-b6HCSTrTdS12dLWhqq3VzWGPrWf9sbN1r1L4vWNzAI/edit#gid=2008828887",
      sheet = "Sheet3"
    )
  
  #function to count win rate
  countWins <- function(data, event, mode) {
    if (!missing(event)) {
      data <- filter(data, Event == event)
    }

    if (!missing(mode)) {
      data <- filter(data, Mode == mode)
    }
    
    output <- data  %>%
      mutate(value = (Result == "victory")) %>%
      group_by(SD) %>%
      summarise(a = sum(sum(value) > 1))
    return(output)
  }
  
  winRate <- round(mean(countWins(data)$a) * 100,2)
  print(winRate)
  output$winRateBox <- renderValueBox({
    valueBox(
      paste0(winRate,"%"), paste0("Win rate out of ", nrow(countWins(data))," games played"),
      color = "purple"
    )
  })
  
  soloWinRate <- round(mean(countWins(data, mode = "soloRanked")$a) * 100,2)
  output$soloWinRateBox <- renderValueBox({
    valueBox(
      paste0(soloWinRate,"%"), paste0("Solo win rate out of ", nrow(countWins(data, mode = "soloRanked"))," games played"),
      color = "blue"
    )
  })

  teamWinRate <- round(mean(countWins(data, mode = "teamRanked")$a) * 100,2)
  output$teamWinRateBox <- renderValueBox({
    valueBox(
      paste0(teamWinRate,"%"), paste0("Team win rate out of ", nrow(countWins(data, mode = "teamRanked"))," games played"),
      color = "red"
    )
  })

  data_new <- mutate(data, day = date(ymd_hms(DateTime)))
  #data_filter <- filter(data, Event == input$maptype)
  output$plot <- renderPlot({
    ggplot(filter(data_new, Event == input$maptype, Mode == input$gametype)) +
      geom_bar(aes(x = day, fill = Result))
  })
}

shinyApp(ui = ui, server = server)