library(shiny)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
source("question1.R")
source("question2.R")
source("question3.R")


torneos_2010_2019 <- read.csv("torneos_2010_2019.csv")
datos_partidos <- read.csv("datos_partidos.csv")

data <- torneos_2010_2019[,c("tourney_type", "year", "singles_winner_name")]
years <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
columns <- c("winners_list", "counts_list", "year")




players <- unique(torneos_2010_2019$singles_winner_name)
c = order(players)
players = players[c]
surfaces <- unique(torneos_2010_2019$tourney_surface)




tourney_mapper <- torneos_2010_2019[,c("tourney_name","tourney_surface", "tourney_type")]
merged_df = merge(datos_partidos, tourney_mapper, by="tourney_name")





stats <- c("Match Duration", 
           "Aces",
           "Double Faults",
           "First Serve In",
           "First Serve Points Won",
           "Second Serve Points Won",
           "Break Points Saved",
           "First Serve Return Won",
           "Second Serve Return Won",
           "Break Points Converted",
           "Break Games Converted",
           "Service Points Won",
           "Return Points Won",
           "Total Points Won")



ui <- fluidPage(
  
  navbarPage("NavBar",
             
    tabPanel("Bars Chart", gui1()),
    
    tabPanel("Players Evolution",gui2(players, stats)),
    
    tabPanel("Stats behavior", gui3(stats))
    )
  )



server <- function(input, output) {
  
  ### Part 1.1
  
  output$stackedBarChart <- renderPlot({
    
    category = input$tourneyType
    
    data_final <- data.frame(matrix(nrow = 0, ncol = length(columns)))
    colnames(data_final) = columns

    counts_all <- counts_all(data, category)
    winners_show <- winners_show(counts_all)
    
    for (y in years) {
      
      total_tourneys <- total_tourneys(data, category, y)
      data_show <- data[data$tourney_type == input$tourneyType & data$year == y,]
      
      total_tourneys <- nrow(data_show)
      counts <- data_show %>% count(singles_winner_name, sort = TRUE)
      
      winners_list <- c()
      counts_list <- c()
      
      accumulated <- 0
      for (i in 1:nrow(counts)) {
        player <- counts$singles_winner_name[i]
        num <- counts$n[i]
        if (player %in% winners_show) {
          winners_list <- c(winners_list, player)
          counts_list <- c(counts_list, num)
          accumulated <- accumulated + num
        }
      }
      if (accumulated < nrow(data_show)) {
        winners_list <- c(winners_list, "Others")
        counts_list <- c(counts_list, nrow(data_show)-accumulated)
      }
      
      counts_list <- counts_list / total_tourneys
      year <- rep(c(toString(y)), length(winners_list))
      data_aux <- data.frame(winners_list, counts_list, year)
      data_final <- rbind(data_final, data_aux)
    }
    
    winners_show <- rev(c("Others", winners_show))
    
    ggplot(data_final, aes(fill=factor(winners_list, levels = winners_show), y=counts_list, x=year)) +
      geom_bar(position="stack", color = "black", stat="identity") +
      xlab("Year") + ylab("Percentage of titles won") + labs(fill = "Player")
    
    
    
  })
  
  ### Part 1.2
  output$barChart <- renderPlot({
    
    category = input$tourneyType
    year = input$year
    
    agg_win <- agg_win(data, category)
    counts_all <- counts_all(data, category)
    
    total_tourneys <- total_tourneys(data, category, year)
    
    winners_show <- winners_show(counts_all)
    
    rest_of_winners <- rest_of_winners(agg_win, year, winners_show, total_tourneys)
    
    if (nrow(rest_of_winners) == 0) {
      return()
    }
    
    ggplot(data=rest_of_winners, aes(x=singles_winner_name, y=n)) +
      geom_bar(stat="identity", fill="steelblue") +
      theme(axis.text.x = element_text(angle = 20)) +
      xlab("Player") + ylab("Percentage of titles won")
    
  })
  
  ### Part 2
  
  v<- reactiveValues(df = NULL,
                     plot = NULL)
  
  output$plot <- renderPlot({
    ps <- input$Players
    if(is.null(ps)) {return()}
    stat <-input$Stat
    surface <- input$Surface
    wl <- input$W
    df <- printable_df(ps,stat,wl,surface, datos_partidos, torneos_2010_2019)
    v$df <- df
    v$plot <-  plot <- ggplot(v$df, aes(Year, Stat, col = Player)) +
      ylab(stat) +
      geom_line() + 
      geom_point()
    if(is.null(v$plot)) return()
    v$plot
  })
  
  
  ### Part 3
  
  output$heatmap <- renderPlot({
    
    statq3 <-input$StatQ3
    surfaceq3 <- input$SurfaceQ3
    heatmp <- matrix_general(statq3, surfaceq3, merged_df)
    #Master 1000 does not have grass matches
    if (c("Grass") %in% surfaceq3 && length(surfaceq3)<2){
      tournaments = tournaments[c(1,2,4)]
    }

    ggplot(heatmp, aes(heatmp[,c("Group.2")], heatmp[,c("Group.1")], fill=heatmp[,c(statq3)])) + 
      geom_tile() + 
      scale_x_discrete(labels=rounds, name="Round") + 
      scale_y_discrete(labels=tournaments, name="Tournament")+ 
      scale_fill_distiller(name = statq3, palette = "YlGn",direction=1) + 
      theme(axis.text.x = element_text(size=16),
            axis.text.y = element_text(size=16),
            axis.title.x = element_text(size=20,margin = margin(r=0, t=30, b = 30, l = 0)),
            axis.title.y = element_text(size=20,margin = margin(r=30, t=0, b = 0, l = 30)),
            legend.key.height = unit(2,"cm"),
            legend.text =  element_text(size=14),
            legend.title =  element_text(size=16)) +
      coord_fixed()
  })
  

  
}

shinyApp(ui = ui, server = server)
