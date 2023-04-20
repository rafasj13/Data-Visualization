

#### Question 3

rounds <- c("Round of 32",
            "Round of 16",
            "Quarter-Finals",
            "Semi-Finals",
            "Finals")

tournaments <- c("ATP 250",
                 "ATP 500",
                 "Masters 1000",
                 "Grand Slam")

# mapper and stats list for questions 2 and 3
mapper <- function(stat){
  if (stat == "Match Duration"){return(c("match_duration","match_duration"))};
  if (stat == "Aces"){return(c("winner_pctg_aces","loser_pctg_aces"))};
  if (stat == "Double Faults"){return(c("winner_pctg_double_faults","loser_pctg_double_faults"))};
  if (stat == "First Serve In"){return(c("winner_pctg_first_serves_in","loser_pctg_first_serves_in"))};
  if (stat == "First Serve Points Won"){return(c("winner_pctg_first_serve_points_won","loser_pctg_first_serve_points_won"))};
  if (stat == "Second Serve Points Won"){return(c("winner_pctg_second_serve_points_won","loser_pctg_second_serve_points_won"))};
  if (stat == "Break Points Saved"){return(c("winner_pctg_break_points_saved","loser_pctg_break_points_saved"))};
  if (stat == "First Serve Return Won"){return(c("winner_pctg_first_serve_return_won","loser_pctg_first_serve_return_won"))};
  if (stat == "Second Serve Return Won"){return(c("winner_pctg_second_serve_return_won","loser_pctg_second_serve_return_won"))};
  if (stat == "Break Points Converted"){return(c("winner_pctg_break_points_converted","loser_pctg_break_points_converted"))};
  if (stat == "Break Games Converted"){return(c("winner_pctg_break_games_converted","loser_pctg_break_games_converted"))};
  if (stat == "Service Points Won"){return(c("winner_pctg_service_points_won","loser_pctg_service_points_won"))};
  if (stat == "Return Points Won"){return(c("winner_pctg_return_points_won","loser_pctg_return_points_won"))};
  if (stat == "Total Points Won"){return(c("winner_pctg_total_points_won","loser_pctg_total_points_won"))};
  
}


general_df <- function(stat, merged_df){
  
  #calculate total of the stat
  df <- merged_df[,c("tourney_round_name", "tourney_type","tourney_surface")]
  stat_cols <- mapper(stat)
  winn <- cbind(df, merged_df[,stat_cols[1]])
  loss <- cbind(df, merged_df[,stat_cols[2]])
  colnames(winn)[4] <- c(stat)
  colnames(loss)[4] <- c(stat)
  stat_round_df <- rbind(winn,loss)
  
  #filter the unecesary  rounds and tournaments
  rounds_filtered <- stat_round_df[stat_round_df$tourney_round_name %in% rounds,]
  rounds_tours_filtered <-rounds_filtered[rounds_filtered$tourney_type %in% tournaments,]
  return (rounds_tours_filtered)
}


surface_filterQ3 <- function(df, surface){
  if (!is.null(surface)){
    df_surface <- df[df$tourney_surface %in% surface,]
    return(df_surface)
  } else {return(df)}
}


matrix_general <- function(stat, surface, merged_df){
  
  df <- general_df(stat, merged_df)
  df_surface <- surface_filterQ3(df, surface)
  
  # calculate the mean for each tourny and round pair 
  df_grouped = aggregate(df_surface[c(stat)], list(df_surface$tourney_type, df_surface$tourney_round_name), FUN=mean)
  
  # encode rounds
  for (n in 1:5) {
    df_grouped[df_grouped==rounds[c(n)]] <- n
  }
  
  for (n in 1:4) {
    df_grouped[df_grouped==tournaments[c(n)]] <- n
  }
  
  return (df_grouped)
  
}


linebreaks <- function(n){HTML(strrep(br(), n))}


gui3 <- function(stats) {
  
  sidebarLayout(
    
    sidebarPanel(
      width = 4,
      fluidRow(h3("Question 3"),align = "center"),
      fluidRow(h4("Is there any hidden pattern for a certain stat given its corresponding round and tournament?",align = "center"),
               h4("Does the surface affects in that matter?",align = "center")),
      
      selectInput("StatQ3", label = "Select Statistic", choices = sort(stats)),
      
      checkboxGroupInput("SurfaceQ3", label = h4("Surface"),
                         choices = c("Hard","Clay", "Grass"),
                         selected = 0)
    ),
    
    mainPanel(
      column(
        width = 12,
        plotOutput(outputId = "heatmap", height = 600),
        
      )
    )
  )
}

server3 <- function(){
  
}
