

##### Question 2

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



#############################
######Filters
#############################

join_df <- function(player, stat, LW, datos_partidos){
  if(LW == "Loser"){
    return(loser_df(player,stat, datos_partidos))
  }
  if(LW == "Winner"){
    return(winner_df(player,stat, datos_partidos))
  }
  return(rbind(loser_df(player,stat, datos_partidos),winner_df(player,stat, datos_partidos)))
}

#Get the dataframe of the matches where the player is winning
#And generates a new dataframe with all the needed information
#(statistic, year of the match and tournament name)
winner_df <- function(player, stat, datos_partidos){
  winner_df <- datos_partidos[datos_partidos$winner_name == player,]
  stat_cols <- mapper(stat)
  stat_winner_col <- stat_cols[1]
  winner_df = winner_df[,c(stat_winner_col,"start_year","tourney_name")]
  colnames(winner_df) <- c(stat, "start_year", "tourney_name")
  winner_df
}

#Get the dataframe of the matches where the player is losing
#And generates a new dataframe with all the needed information
#(statistic, year of the match and tournament name)
loser_df <- function(player, stat, datos_partidos){
  loser_df <- datos_partidos[datos_partidos$loser_name == player,]
  stat_cols <- mapper(stat)
  stat_loser_col <- stat_cols[2]
  loser_df = loser_df[,c(stat_loser_col,"start_year","tourney_name")]
  colnames(loser_df) <- c(stat, "start_year", "tourney_name")
  loser_df
}


surface_filter <- function(df, surface, torneos_2010_2019){
  if (!is.null(surface)){
    t_surf <- torneos_2010_2019[torneos_2010_2019$tourney_surface %in% surface,]
    tournaments <-unique(t_surf[["tourney_name"]])
    df <- df[df$tourney_name %in% tournaments,]
  }
  df
}

#Compute the mean of the statistic and generates the new dataframe 
# For one player
compute_mean <- function(df,name,stat){
  if(nrow(df) == 0) {
    df <- data.frame(matrix(ncol = 2, nrow = 0))
    colnames(df) <- c("Year", name)
    return(df)
  }
  df_new <- df[c(stat,"start_year")]
  df_new = aggregate(df_new[,stat], list(df_new$"start_year"), FUN=mean)
  colnames(df_new) = c("Year", name)
  df_new
}

#Get DataFrame to plot for one player
# player: One player
# stat: statistic to compare
# LW: "Total" if you want any match, "Loser" if only when the player loses
#   and "Winner" if only matches where the players end winning
# surface: list of surfaces you want to compare
printable_df_1player <- function(player, stat, LW, surface, datos_partidos, torneos_2010_2019){
  df_player_lw = join_df(player, stat, LW, datos_partidos)
  df_player_lw_surface = surface_filter(df_player_lw,surface, torneos_2010_2019)
  compute_mean(df_player_lw_surface,player, stat)
}

#Applies printable_df_1player to all players of LPlayer
#LPlayer: list of players to compare
printable_df <- function(LPlayer, stat, LW, surface, datos_partidos, torneos_2010_2019){
  f <- function(player){
    a = printable_df_1player(player = player, stat, LW, surface, datos_partidos, torneos_2010_2019)
    if(nrow(a) == 0) {
      df <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(df) <- c("Year", "Stat", "Player")
      return(df)
    }
    colnames(a) <- c("Year","Stat")
    a$Player = player
    a
  }
  list = lapply(LPlayer, f)
  Reduce(function(x, y) rbind(x, y), list)  
}

gui2 <- function(players, stats){
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 4,
      fluidRow(h3("Question 2"),align = "center"),
      fluidRow(h4("How have the players evolved over the years?",align = "center"),
               h4("Does the surface affects in that matter?",align = "center"),
               h4("Is there a noticeable difference if the players end up losing the match? What if they end up winning?",align = "center")),
    
      radioButtons("W", label = h4("Stats as Winner, Loser or in General"), 
                   choices = list("Total" = "Total", "Winner" = "Winner", "Loser" = "Loser"),
                   selected = "Total"),
      
      checkboxGroupInput("Surface", label = h4("Particular Surface"), 
                         choices = list("Hard" = "Hard", "Clay" = "Clay", "Grass" = "Grass"),
                         selected = 0),
      
      selectInput(
        "Players",
        label = "Select Players",
        choices = players,
        multiple = TRUE
      ),
      
      selectInput("Stat", "Select Statistic", choices = sort(stats)),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      column(
        width = 12,
        plotOutput("plot")
      )
      
      # Output: Histogram ----
    )
  )
}
