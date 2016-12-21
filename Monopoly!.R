# arbitrary seed for rng

set.seed(2016-10-18)

# size of each trial, number of trials to run
num_rolls<-1000
trials <- 1000

#dimensions of board, locations of community chest & chance squares
max_position<-40
chest <- c(2, 17, 33)
chance <- c(7, 22, 36)

#set up a matrix to track all positions on all trials
positions <- matrix(nrow = num_rolls+1)

#we start on square 0, the Go square
places <- 0
square <- 0
speeding <- 0

#this for loop keeps track of each trial
for (i in seq_len(trials)) {
  places <- 1

#this for loop keeps track of each roll within a trial  
for (i in seq_len(num_rolls)) {
  #roll 2 dice. move that many spaces to a new square.
  #if the new square is on a space greater than 40 (boardwalk), use the mod function to wrap around the board
  thisroll <- sample(1:6, 1)
  thatroll <- sample(1:6, 1)
  square <- ((square + thisroll + thatroll)%%40)
  #count number of doubles in a row
  if (thisroll == thatroll){
    speeding <- speeding + 1
  }
  else {
    speeding <- 0
  }
  #go to jail if you roll 3 doubles in a row
  if (speeding == 3){
    square <- 10
    speeding <- 0
  }
  else {
    speeding <- speeding
  }
  #go to jail square
  if (square == 30){
    square <- 10
  }
  #community chest squares
  else if (square %in% chest){
    card <- sample(1:16, 1)
    if (card == 1){
      square <- 0
    }
    else if (card == 2){
      square <- 10
    }
    else {
      square <- square
    }
  }
  #chance squares
  else if(square %in% chance){
    card <- sample(1:16, 1)
    if (card == 1){
      square <- 0
    }
    else if (card == 2){
      square <- 24
    }
    else if (card == 3){
      square <- 11
    }
    else if (card == 4){
      if (28 > square & square > 11){
        square <- 28
      }
      else {
        square <- 12
      }
    }
    else if (card == 5){
      if (15 > square & square > 4){
        square <- 15
      }
      else if (25 > square & square > 14){
        square <- 25
      }
      else if (35 > square & square > 24){
        square <- 25
      }
      else {
        square <- 5
      }
    }
    else if (card == 6){
      if (15 > square & square > 4){
        square <- 15
      }
      else if (25 > square & square > 14){
        square <- 25
      }
      else if (35 > square & square > 24){
        square <- 25
      }
      else {
        square <- 5
      }
    }
    else if (card == 7){
      square <- square-3
    }
    else if (card == 8){
      square <- 10
    }
    else if (card == 9){
      square <- 5
    }
    else if (card == 10){
      square <- 39
    }
    else {
      square <- square
    }
  }
  else {
    square <- square
  }
  #for each roll, add the new square to a vector of places that this trial has gone to
  places <- c(places, square)
}
  #for each trial, tack the vector "places" on to a matrix of positions that each trial has landed on.
  positions <- cbind(positions, places)
}

#get rid of the pesky label row and column in our positions matrix
positions <- positions[-1,-1]

#count that number of times each square appears in our matrix
count_per_position <- tabulate(positions+1, max_position)
count_per_position

library(dplyr)

position_probs <- data_frame(position = seq_len(max_position), probability = count_per_position / trials / num_rolls)

position_probs

library(ggplot2)

ggplot(position_probs, aes(position, probability)) +
  geom_line()

position_probs %>%
  arrange(desc(probability)) %>%
  print(n=40)
