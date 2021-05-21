library(tidyverse)
library(lubridate)
library(googlesheets4)
library(shiny)
library(shinydashboard)
source("progress_bar.R")

BASIC_COLORS <- c("blue", "green", "aqua", "yellow", "red")

### TODO
#TABSET for chart for each game mode
#adjust how many brawlers and maps to see for best

ui <- dashboardPage(dashboardHeader(),
                    dashboardSidebar(  
                      sidebarMenu(
                        menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
                        menuItem("Maps", icon = icon("th"), tabName = "Maps")
                    )),
                    
                    dashboardBody(
                    tabItems(
                      
                      #overview tab
                      tabItem("Overview",
                      fluidRow(
                        
                        valueBoxOutput("soloWinRateBox"),
                        valueBoxOutput("teamWinRateBox"),
                        valueBoxOutput("winRateBox")
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
                      ),
                    fluidRow(
                      radioButtons("dateFilter", "Period",
                                   c("All" = 999999,
                                     "1 Day" = 1,
                                     "1 Week" = 7,
                                     "1 Month" = 30,
                                     "1 Year" = 365
                                     ), inline = TRUE)
                                    )
                      ),
                    
                    #### Maps
                    tabItem("Maps",
                      fluidRow(radioButtons("mapMode", "Game mode",
                                   c("Heist" = "heist",
                                     "Bounty" = "bounty",
                                     "Gem Grab" = "gemGrab",
                                     "Brawl Ball" = "brawlBall",
                                     "Siege" = "siege",
                                     "Hot Zone" = "hotZone")),
                      uiOutput("mapList")),
                      fluidRow(
                      tableOutput("brawlerByMap"))
                    #can use tabsets to sort by my own suggestions, youtubers, etc
                    )
                   
                    )
                  )
)
                   

server <- function(input, output) {
  
  #options(gargle_oauth_cache = ".secrets")
  #gs4_auth()
  options(gargle_oauth_cache = ".secrets",
          gargle_oauth_email = TRUE)
  raw_data <-
    read_sheet(
      "https://docs.google.com/spreadsheets/d/1-b6HCSTrTdS12dLWhqq3VzWGPrWf9sbN1r1L4vWNzAI/edit#gid=2008828887",
      sheet = "Sheet3"
    )
  
  #filter data based on dateFilter
  data <- reactive({

    filter(raw_data, between(ymd_hms(DateTime), now()-days(input$dateFilter), now()))
  
  })
 
  # add NUMBER.png to the back
  badgeURL <- "https://cdn.brawlstats.com/ranked-ranks/ranked_ranks_l_"
  
  
  soloData <- reactive({filter(data(), Mode == "soloRanked")})
  teamData <- reactive({filter(data(), Mode == "teamRanked")})
  
  
  
  #current rank for each mode
  soloRank <- reactive({soloData()$MeRank[[nrow(soloData())]]})
  teamRank <- reactive({teamData()$MeRank[[nrow(teamData())]]})
  bestRank <- reactive({max(soloRank(), teamRank())})
  
  soloBadgeURL <- reactive({paste0(badgeURL, soloRank(), ".png")})
  teamBadgeURL <- reactive({paste0(badgeURL, teamRank(), ".png")})
  bestBadgeURL <- reactive({paste0(badgeURL, bestRank(), ".png")})
  
  #function to count win rate
  countWins <- function(data, event, mode) {
    #data <- data()
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
  
  #function to return the full dataframe with countwins appended
  fullCountWins <- function(data, cw) {
    out <- left_join(cw, data, by = "SD")
    return(out)
  }
  
   winRate <- reactive({round(mean(countWins(data())$a) * 100,2)})
  
   output$winRateBox <- renderValueBox({
     validate(
       need(nrow(data()) > 0, "No games found!" )
     )
     valueBox(
       div(paste0(winRate(),"%"), tags$img(src = bestBadgeURL(), height="100px", width="200px")), paste0("Win rate out of ", nrow(countWins(data()))," games played"),
       color = "purple"
     )
   })
  
   soloWinRate <- reactive({round(mean(countWins(data(), mode = "soloRanked")$a) * 100,2)})
   output$soloWinRateBox <- renderValueBox({
     validate(
       need(soloWinRate(), "No games found!" )
     )
     valueBox(
       div(paste0(soloWinRate(),"%"), tags$img(src = soloBadgeURL(), height="100px", width="200px")), paste0("Solo win rate out of ", nrow(countWins(data(), mode = "soloRanked"))," games played"),
       color = "blue", icon =
     )
   })
  
   teamWinRate <- reactive({round(mean(countWins(data(), mode = "teamRanked")$a) * 100,2)})
   output$teamWinRateBox <- renderValueBox({
     validate(
       need(nrow(data()) > 0, "No games found!" )
     )
     valueBox(
       div(paste0(teamWinRate(),"%"), tags$img(src = teamBadgeURL(), height="100px", width="200px")), paste0("Team win rate out of ", nrow(countWins(data(), mode = "teamRanked"))," games played"),
       color = "red"
     )
   })
  

 # Win rate chart
   plotData <- reactive({inner_join(countWins(data()), select(data(),Event, Mode, SD), by = "SD") %>%
     distinct() %>%
     mutate(Status = ifelse(a == 1, "win","lose"))})

   output$packagePlot <- renderPlot({
     validate(
       need(nrow(data()) > 0, "No games found!" )
     )
     ggplot(plotData()) +
     geom_bar(aes(x = Event, fill = Status)) +
     facet_wrap(~Mode)
   })


# Table for mode
  output$modeTable <- renderTable({
    validate(
      need(nrow(data()) > 0, "No games found!" )
    )
    data() %>%
     group_by(Event, Mode) %>%
     summarise("Win Rate" = mean(countWins(data(), Event, Mode)$a), "Games Played" = nrow(countWins(data(), Event, mode = Mode ))) %>%
     mutate("Win Rate" = paste0(round(`Win Rate`,2), " (",`Games Played`,")")) %>%
     select(1:3) %>%
     spread(Mode, `Win Rate`) %>%
     arrange(desc(teamRanked))
   })


   #My best brawler progress bar
   output$brawlerProgress <- renderUI({
     validate(
       need(nrow(data()) > 0, "No games found!" )
     )
     bb <- inner_join(countWins(data()), select(data(),Me, Mode, SD), by = "SD") %>%
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
     validate(
       need(nrow(data()) > 0, "No games found!" )
     )
     bm <- inner_join(countWins(data()), select(data(),Map, Mode, SD, Event), by = "SD") %>%
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


  
  ##### maps tab
  
  # maps dropdown controls
  output$mapList <- renderUI({
    maps <- raw_data %>% 
      select(Event, Map) %>% 
      filter(Event == input$mapMode) %>% 
      distinct() %>% 
      select(Map)
    selectInput("map", "Choose map", maps)
  })

  mapData <- reactive({filter(raw_data, Map == input$map)})
  
  
  #strip friendly tag identification
  
  
  
  tagStrip <- function(tag) {
    return(str_remove_all(tag, "\\(.+\\)"))
  }
  # 
  # 
  # #brawlers collapsed
  # mapData <- raw_data %>%
  #             mutate(Friend2 = tagStrip(Friend2), Friend3 = tagStrip(Friend3)) %>%
  #             rowwise() %>%
  #             mutate(set = paste0(sort(c(Me, Friend2, Friend3)), collapse = ','))
  # #test1 <- fullCountWins(mapData, countWins(mapData, "bounty"))
  # 
  # most picked brawlers for this map
  output$brawlerByMap <- renderTable({
    validate(
      need(nrow(mapData()) > 0, "No games found!" )
    )
    

    bb <- fullCountWins(mapData() %>% gather(key = "Role", value = "Brawler", 5,7,9,11,13,15), countWins(mapData())) %>%
      select(-DateTime) %>%
      distinct() %>%
      mutate(Brawler = tagStrip(Brawler)) %>%
      group_by(Brawler) %>%
      summarise("Win Rate" = mean(a)*100, "Pick Rate" = n()) %>%
      arrange(desc(`Win Rate`), desc(`Pick Rate`)) %>%
      slice_max(`Win Rate`, n=5)

    fullCountWins(mapData(), countWins(mapData())) %>% select(Map, Me, Friend2, Friend3, Enemy1, Enemy2, Enemy3, a) %>% distinct()

    # tags$div(
    #   map(seq_len(min(5, nrow(bb))), ~ {
    #     progressGroup(paste0(bb$Brawler[.], "(",bb$`Pick Rate`[.],")"), bb$`Win Rate`[.], max = 100, color = BASIC_COLORS[.])
    #   })
    # )
  })


  

  #test1 %>% group_by(Brawler) %>% filter(Event == "bounty") %>% summarise(win = mean(a), n())
  
  # data_new <- mutate(data, day = date(ymd_hms(DateTime)))
  # #data_filter <- filter(data, Event == input$maptype)
  # output$plot <- renderPlot({
  #   ggplot(filter(data_new, Event == input$maptype, Mode == input$gametype)) +
  #     geom_bar(aes(x = day, fill = Result))
  # })
}

shinyApp(ui = ui, server = server)





