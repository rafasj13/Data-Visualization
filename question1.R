library(shiny)

#### Question 1




# question 1
counts_all <- function(data_show, category) {
  
  data_show_all <- data_show[data_show$tourney_type == category,]
  return(data_show_all %>% count(singles_winner_name, sort = TRUE))
  
}

agg_win <- function(data_show, category) {
  
  data_tourney <- data_show[data_show$tourney_type == category,]
  return(data_tourney %>% count(singles_winner_name, year, sort = TRUE))
  
}

total_tourneys <- function(data_aux, category, year) {
  
  data_show <- data_aux[data_aux$tourney_type == category & data_aux$year == year,]
  return(nrow(data_show))
  
}

winners_show <- function(counts_all) {
  
  if (nrow(counts_all) < 11) {
    return(counts_all$singles_winner_name)
  } else {
    return(counts_all[1:9,]$singles_winner_name)
  }
  
}

rest_of_winners <- function(agg_win, year, winners_show, total_tourneys) {
  
  winners_year <- agg_win[agg_win$year == year,]
  new <- winners_year[!winners_year$singles_winner_name %in% winners_show,]
  new$n <- new$n / total_tourneys
  return(new)
  
}

gui1 <- function(){
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 4,
      
      fluidRow(h3("Question 1"),align = "center"),
      fluidRow(h4("What is the part-to-whole relationship of the winners of all
                      tourneys of a certain category",align = "center"),
               h4("What is its evolution over the years?",align = "center")),
      
      selectInput(
        inputId = "tourneyType",
        label = "Type of tourney",
        choices = c("ATP 250", "ATP 500", "Masters 1000", "Grand Slam"),
        selected = "Masters 1000",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL
      ),
      selectInput(
        inputId = "year",
        label = "Year to show the content of 'Others' cathegory",
        choices = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019),
        selected = "Masters 1000",
        multiple = FALSE,
        selectize = TRUE,
        width = NULL,
        size = NULL)
      
    ),
    
    mainPanel(
      column(
        width = 12,
        fluidRow(plotOutput(outputId = "stackedBarChart")),
        
        fluidRow(plotOutput(outputId = "barChart"))
      )
    )
  )
}





