library(tidyverse)
library(lubridate)
library(googlesheets4)
library(shiny)
library(shinydashboard)
source("progress_bar.R")

BASIC_COLORS <- c("blue", "green", "aqua", "yellow", "red")


ui <- dashboardPage(dashboardHeader(),
                    dashboardSidebar(),
                    dashboardBody(
                      fluidRow(
                        valueBoxOutput("winRateBox"),
                        valueBoxOutput("soloWinRateBox"),
                        valueBoxOutput("teamWinRateBox")
                    ),
                    
                      fluidRow(
                        box(
                          width = 8, status = "info", solidHeader = FALSE,
                          title = "Wins and games played",
                          plotOutput("packagePlot", width = "100%", height = 700)
                        ), 
                        box(
                          width = 4,
                          status = "info",
                          title = "My best brawlers",
                          #tableOutput("brawlerTable"),
                          uiOutput("brawlerProgress"),
                          helpText("Played at least 2 times")
                        ),
                        box(
                          width = 4,
                          status = "info",
                          title = "My best maps",
                          uiOutput("mapProgress"),
                          footer = "Played at least 2 times"
                        )
                      )
                    )
                  )

                   

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
  
  # add NUMBER.png to the back
  badgeURL <- "https://cdn.brawlstats.com/ranked-ranks/ranked_ranks_l_"
  
  
  soloData <- filter(data, Mode == "soloRanked")
  teamData <- filter(data, Mode == "teamRanked")
  
  
  
  #current rank for each mode
  soloRank <- soloData$MeRank[[nrow(soloData)]]
  teamRank <- teamData$MeRank[[nrow(teamData)]]
  bestRank <- max(soloRank, teamRank)
  
  soloBadgeURL <- paste0(badgeURL, soloRank, ".png")
  teamBadgeURL <- paste0(badgeURL, teamRank, ".png")
  bestBadgeURL <- paste0(badgeURL, bestRank, ".png")
  
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
      div(paste0(winRate,"%"), tags$img(src = bestBadgeURL, height="100px", width="200px")), paste0("Win rate out of ", nrow(countWins(data))," games played"),
      color = "purple"
    )
  })
  
  soloWinRate <- round(mean(countWins(data, mode = "soloRanked")$a) * 100,2)
  output$soloWinRateBox <- renderValueBox({
    valueBox(
      div(paste0(soloWinRate,"%"), tags$img(src = soloBadgeURL, height="100px", width="200px")), paste0("Solo win rate out of ", nrow(countWins(data, mode = "soloRanked"))," games played"),
      color = "blue", icon = 
    )
  })

  teamWinRate <- round(mean(countWins(data, mode = "teamRanked")$a) * 100,2)
  output$teamWinRateBox <- renderValueBox({
    valueBox(
      div(paste0(teamWinRate,"%"), tags$img(src = teamBadgeURL, height="100px", width="200px")), paste0("Team win rate out of ", nrow(countWins(data, mode = "teamRanked"))," games played"),
      color = "red"
    )
  })
  
  
# Win rate chart
  plotData <- inner_join(countWins(data), select(data,Event, Mode, SD), by = "SD") %>% 
    distinct() %>% 
    mutate(Status = ifelse(a == 1, "win","lose"))
  
  output$packagePlot <- renderPlot({
    ggplot(plotData) +
    geom_bar(aes(x = Event, fill = Status)) +
    facet_wrap(~Mode)
  })
  
  
# Table for mode  
  # output$modeTable <- renderTable({
  #   data %>% 
  #   group_by(Event, Mode) %>% 
  #   summarise("Win Rate" = mean(countWins(data, Event, Mode)$a), "Games Played" = nrow(countWins(data, Event, mode = Mode ))) %>%
  #   mutate("Win Rate" = paste0(round(`Win Rate`,2), " (",`Games Played`,")")) %>%
  #   select(1:3) %>%
  #   spread(Mode, `Win Rate`) %>%
  #   arrange(desc(teamRanked))
  # })


  #My best brawler progress bar
  output$brawlerProgress <- renderUI({
    bb <- inner_join(countWins(data), select(data,Me, Mode, SD), by = "SD") %>% 
      distinct() %>% 
      group_by(Me) %>% 
      summarise("Win%" = mean(a), "Count" = n()) %>%
      mutate("Win%" = round(`Win%`*100,2)) %>%
      filter(Count > 1) %>%
      slice_max(`Win%`, n=5)
    
    
    
    tags$div(
      map(seq_len(min(5, nrow(bb))), ~ {
        progressGroup(paste0(bb$Me[.], "(",bb$Count[.],")"), bb$`Win%`[.], max = 100, color = BASIC_COLORS[.])
      })
    )
  })
  

  
  #My best map table
  output$mapProgress <- renderUI({
    bm <- inner_join(countWins(data), select(data,Map, Mode, SD, Event), by = "SD") %>% 
      distinct() %>% 
      group_by(Map) %>% 
      summarise("Win%" = mean(a), "Count" = n(), Event) %>%
      mutate("Win%" = round(`Win%`*100,2)) %>%
      distinct() %>%
      filter(Count > 1) %>%
      ungroup %>%
      slice_max(`Win%`, n=5) 
    
    tags$div(
      map(seq_len(min(5, nrow(bm))), ~ {
        progressGroup(paste0(bm$Map[.], "(",bm$Count[.],")"), bm$`Win%`[.], max = 100, color = BASIC_COLORS[.])
      })
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




