# Function to simulate a roll of two six-sided dice


roll_dice <- function() {
  sample(1:6, 2, replace = T) %>% sum()
}




# Function to play a single round and return a detailed dataframe



simulate_craps_game <- function() {
  # Creating an empty df with relevant columns [this will get filled up as we progress]
  rolls_log <- data.frame(
    Roll_Number = integer(),
    Dice_Total = integer(),
    Phase = character(),
    Point_Value = integer(),
    Outcome_of_Roll = character(),
    Match_Outcome = character(),
    stringsAsFactors = FALSE
  )
  
  # Start the come-out roll
  come_out_roll <- roll_dice() #very first roll
  roll_number <- 1
  
  # Logging the 1st roll into the dataset rolls_log
  rolls_log <- rbind(
    rolls_log,
    data.frame(
      Roll_Number = roll_number,
      Dice_Total = come_out_roll,
      Phase = "Come-Out",
      Point_Value = NA, #point will always be NA for the come-out roll
      Outcome_of_Roll = ifelse(come_out_roll %in% c(7, 11), "Natural Win",
                               ifelse(come_out_roll %in% c(2, 3, 12), "Craps", "Point Set")),
      Match_Outcome = NA #keeping it empty for now will fill it up using if-else
    )
  )
  
  
  # now, Checking the outcome of the 1st roll
  if (come_out_roll %in% c(7, 11)) {
    rolls_log$Match_Outcome <- "Win"
    return(rolls_log) # that is spit out the table that we created with whatever value it has till now
  } else if (come_out_roll %in% c(2, 3, 12)) {
    rolls_log$Match_Outcome <- "Loss"
    return(rolls_log) # that is spit out the table that we created with whatever value it has till now
  } else {
    # Establishing the point
    point <- come_out_roll
    phase <- "Point" #i.e when we get 1,4,5,6,8,9,10
    
    # Keep rolling until point or 7 is rolled
    while (TRUE) {
      roll_number <- roll_number + 1
      current_roll <- roll_dice()
      
      # again Determining the outcome of the roll for subsequent rolls
      roll_outcome <- ifelse(current_roll == point, "Point Hit",
                             ifelse(current_roll == 7, "Seven-Out", "Continue"))
      
      # Log the roll for this case when getting 1,4,5,6,8,9,10
      rolls_log <- rbind(
        rolls_log,
        data.frame(
          Roll_Number = roll_number,
          Dice_Total = current_roll, #dice total for the current roll
          Phase = phase,
          Point_Value = point,# here it is no longer NA as point is established
          Outcome_of_Roll = roll_outcome,
          Match_Outcome = NA #again keeping it empty for now, will fill it up using if-else
        )
      )
      
      # Check for match-ending conditions
      if (current_roll == point) {
        rolls_log$Match_Outcome <- "Win"
        return(rolls_log) # that is spit out the table that we created with whatever value it has till now
      } else if (current_roll == 7) {
        rolls_log$Match_Outcome <- "Loss"
        return(rolls_log) # that is spit out the table that we created with whatever value it has till now
      }
    } #while loop end
  } #else statement end
}







#Function to summarize a single crap game

summarize_craps_game <- function(game_log) {
  num_rolls <- nrow(game_log) #total throws in that game
  
  # Match outcome 
  match_outcome <- unique(game_log$Match_Outcome)
  
  # Point value (if point was set, otherwise NA[i.e for 7,11,2,3,12])
  point_value <- unique(na.omit(game_log$Point_Value)) #ignoring the first NA and taking the subsequent value
  if (length(point_value) == 0) point_value <- NA #winning or losing in the first roll
  
  # Return summarized information as a dataframe
  summary <- data.frame(
    Number_of_Rolls = num_rolls,
    Match_Outcome = match_outcome,
    Point_Value = point_value,
    stringsAsFactors = FALSE
  )
  
  return(summary)
}







#Function to simulate N number of games



run_craps_simulation <- function(N) {
  # creating an empty dataframe to store game summaries
  simulation_results <- data.frame(
    Number_of_Rolls = integer(),
    Match_Outcome = character(),
    Point_Value = integer(),
    stringsAsFactors = FALSE
  )
  
  # Loop through the number of games
  for (i in 1:N) {
    # Running a single game and saving it
    game_log <- simulate_craps_game()
    
    # Summarizing that played game and saving it
    game_summary <- summarize_craps_game(game_log)
    
    # Add the game number column to the summary table
    game_summary <- cbind(Game_Number = i, game_summary)
    
    # Adding the summary to the results table
    simulation_results <- rbind(simulation_results, game_summary)
  } #end of for loop
  
  return(simulation_results)
}


