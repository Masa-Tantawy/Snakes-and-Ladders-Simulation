#Snakes and Ladders

library(readxl)
library(tidyverse)
library(ggthemes)
library(ggplot2)
library(scico)
library(gganimate)
library(ggpubr)
library("gridExtra")
library(shape)


# A function to create the board
create_board= function(size=10){
  df<-as.data.frame(matrix(rep(0), ncol= size, nrow= size))
  count = 1
  if( (size %%2) ==0 ){
    for(row in sort(1:size, decreasing=T)){
      for (col in 1:size){
        if((row %% 2) == 0){# even lines
          df[row,col]= count
        }else{ # odd lines
          
          df[row,size+1-col]= count
        }
        count=count+1
      }
    }
  }
  else{
    for(row in sort(1:size, decreasing=T)){
      for (col in 1:size){
        if((row %% 2) == 0){# even lines
          df[row,size+1-col]= count
        }else{ # odd lines
          df[row,col]= count
        }
        count=count+1
      }
    }
  }
  return(df)
}

# A function to create custom snakes and ladders according to dimension
custom_sl<-function(n){
  m=n^2
  s<-sample(m-1,n*2,replace=FALSE)
  sl<-data.frame(start=s[1:(n)],
                 end=s[(n+1):length(s)])
  sl$type<-NA
  for(i in 1:nrow(sl)){
    if (sl$s[i]<sl$e[i]){
      sl$type[i]='ladder'
    }else if (sl$s[i]>sl$e[i]){
      sl$type[i]='snake'
    }else if (sl$s[i]==sl$e[i]){
      sl[-i,]
    }
  }
  ladders<-data.frame(sl[sl$type=='ladder',1:2]);ladders<-na.omit(ladders)
  snakes<-data.frame(sl[sl$type=='snake',1:2]);snakes<-na.omit(snakes)
  list(ladders,snakes)
}

# A function to find the coordinates of the tile in the board
find_tile= function (tile, size){
  df= create_board(size)
  
  for(i in 1:size){
    for (j in 1: size){
      if( df[i,j]== tile){
        x= size+1 - i; y=j
        return(list(x,y));}
    }
  }
  
}

# A function to plot the board
plot_board= function(size=10, Snake, Ladder, p1=0,p2=0){ 
  # Step 1: Plotting only the numbers on a plain board
  x=c(0,size); y=c(0,size); title= "Snakes and Ladder"
  plot(x,y, xaxt='n', pch=19, yaxt='n', xlab="", ylab="", xlim= c(0.4,size-0.4), ylim= c(0.4,size-0.4),
       main= title, col='black')
  abline(v = c(1:size-1)); abline(h = c(1:size-1))
  count=1
  for( row in 1:size){
    for (col in 1:size){
      if( (row %% 2) == 0 ) # even lines
        text(size+0.4- col, row- 0.4, labels = count, cex= 0.85)
      else{ # odd lines
        text(col-0.6, row- 0.4, labels = count, cex= 0.85)
      }
      count=count+1
    }
  }
  #Step 2: Plotting the ladders
  library(shape)
  for(i in 1: dim(Ladder)[1]){
    start_y = find_tile(Ladder$s[i], size)[[1]]; start_x= find_tile(Ladder$s[i], size)[[2]]
    end_y = find_tile(Ladder$e[i], size)[[1]]; end_x = find_tile(Ladder$e[i], size)[[2]]
    
    Arrows(start_x -0.3 , start_y -0.3 , end_x-0.65, end_y-0.65,
           col= "blue", lty= "solid", lwd= 2, arr.length =0.3, arr.width = 0.2, arr.type= "triangle")
  }
  #Step 3: Plotting the snakes
  library(shape)
  for(i in 1: dim(Snake)[1]){
    start_y = find_tile(Snake$s[i], size)[[1]]; start_x= find_tile(Snake$s[i], size)[[2]]
    end_y = find_tile(Snake$e[i], size)[[1]]; end_x = find_tile(Snake$e[i], size)[[2]]
    
    Arrows(start_x -0.3 , start_y -0.3 , end_x-0.65, end_y-0.65,
           col= "red", lty= "dashed", lwd= 2, arr.length =0.3, arr.width = 0.2, arr.type= "triangle")
  }
  #Step 4: Plotting the positions of th 2 players
  par(new=TRUE)
  if(p1 !=0){
    y= find_tile(p1, size)[[1]]; x= find_tile(p1, size)[[2]]
    plot(x-0.2,y-0.2, xaxt='n', pch=19, yaxt='n', xlab="", ylab="", xlim= c(0.4,size-0.4), ylim= c(0.4,size-0.4),
         col='purple', cex=2.5 )
  }
  if(p2 !=0){
    par(new=TRUE)
    y= find_tile(p2, size)[[1]]; x= find_tile(p2, size)[[2]]
    plot(x-0.2,y-0.2, xaxt='n', pch=19, yaxt='n', xlab="", ylab="", xlim= c(0.4,size-0.4), ylim= c(0.4,size-0.4),
         col='green', cex=2.5 )
  }
  
}

# Playing against the computer
against_computer = function(sides=6, dimension=10, difficulity=1){
  # Checking on the dimension
  if(dimension > 10){
    print("Please enter a dimension not greater than 10!")
    return()
  }
  if(dimension == 10){
    if(difficulity ==1){
      ladders <- data.frame(start=c(1,4,9,21,28,36,51,71,80,94), 
                            end=c(38,14,31,42,84,44,67,91,100,99)) # 10 ladders
      snakes <- data.frame(start=c(98,95,93,64,62,56,49,47,16), 
                           end=c(78,75,73,60,19,53,11,26,6)) # 9  Snakes
      
    }
    if(difficulity == 2){
      
      ladders <- data.frame(start=c(1,4,9,21,28,36,51,71,80), 
                            end=c(38,14,31,42,84,44,67,91,100)) # 9 ladders 
      snakes <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16), 
                           end=c(78,75,73,24,60,19,53,11,26,6)) # 10 snakes
    }
    if(difficulity == 3 ){
      ladders <- data.frame(start=c(1,4,9,21,36,51,71,80), 
                            end=c(38,14,31,42,44,67,91,100)) # 8 ladders
      snakes <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16,82), 
                           end=c(78,75,73,24,60,19,53,11,26,6,65)) # 11 snakes
    }
    
  }
  else{
    x= custom_sl(dimension); ladders= as.data.frame(x[1]); snakes= as.data.frame(x[2])
  }
  
  p1=0; p2=0 # Both players start at position zero
  t1=0; t2=0; l1=0; l2=0; s1=0;s2=0; # Turns, ladders and snakes taken by each player
  
  # A matrix to record the data of the game
  winnings= matrix(rep(0), ncol= 3, nrow= 1) 
  colnames(winnings)=c("Turns","Ladders", "Snakes")
  
  # Plotting the board
  plot_board(dimension, snakes, ladders, p1,p2)
  
  # Playing the game
  while (p1 < dimension**2 && p2<dimension**2){
    cat ("Your Turn\n")
    roll <- readline(prompt="Press Enter to roll the die.\t")
    if(roll ==""){
      r1=sample(sides, 1,replace=T)
    }
    else{
      print ("Incorrect input. Sorry to see you go!")
      break;
    }
    cat('Dice Roll = ',r1,"\n")
    p1 = p1 + r1 # Recording the new position
    t1 = t1+1 # Incrementing the turn
    if (any(ladders$s %in% p1)) { # Checking for ladders
      p1 <- ladders$e[ladders$s %in% p1]
      l1 <- l1 + 1
    }else if (any(snakes$s %in% p1)) { # Checking for snakes
      p1 <- snakes$e[snakes$s %in% p1]
      s1 <- s1 + 1}
    
    plot_board(dimension, snakes, ladders, p1,p2)
    cat('Your Current Status (orange): ',p1,"\t and Opponent (green):",p2,"\n")
    
    if (p1>(dimension**2)-1){ # Checking if a player won 
      cat ('\n You won  the game after',t1,"turns. Wohooo!!! \n")
      winnings[,1]=t1; winnings[,2]=l1; winnings[,3]=s1 # Recording the data of this game
      break
    }
    
    cat("Opponent's Turn\n")
    r2=sample(sides, 1,replace=T)
    cat('Dice Roll = ',r2,"\n")
    p2=p2 + r2
    t2=t2+1
    if (any(ladders$s %in% p2)) {
      p2 <- ladders$e[ladders$s %in% p2]
      l2 <- l2 + 1
    }else if (any(snakes$s %in% p2)) {
      p2 <- snakes$e[snakes$s %in% p2]
      s2 <- s2 + 1}
    
    plot_board(dimension, snakes, ladders, p1,p2)
    cat('Your Current Status (orange): ',p1,"\t and Opponent (green):",p2,"\n")
    
    if (p2>(dimension**2)-1){
      cat ('\n You lost! The Winner of the game is the opponent after',t2,"turns \n")
      winnings[,1]=t2; winnings[,2]=l2; winnings[,3]=s2
      break
    }
  }
  #print(winnings)
  return(winnings)
}

# Playing only 1 game 
#single game has plot board and prints every turn
single_game = function(sides=6, dimension=10, difficulity = 1){
  # Checking on the dimension
  if(dimension > 10){
    print("Please enter a number not greater than 10!")
    return()
  }
  if(dimension == 10){
    if(difficulity ==1){
      ladders <- data.frame(start=c(1,4,9,21,28,36,51,71,80,94), 
                             end=c(38,14,31,42,84,44,67,91,100,99)) # 10 ladders
      snakes <- data.frame(start=c(98,95,93,64,62,56,49,47,16), 
                            end=c(78,75,73,60,19,53,11,26,6)) # 9  Snakes
      
    }
    if(difficulity == 2){
      
      ladders <- data.frame(start=c(1,4,9,21,28,36,51,71,80), 
                            end=c(38,14,31,42,84,44,67,91,100)) # 9 ladders 
      snakes <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16), 
                           end=c(78,75,73,24,60,19,53,11,26,6)) # 10 snakes
    }
    if(difficulity == 3 ){
      ladders <- data.frame(start=c(1,4,9,21,36,51,71,80), 
                            end=c(38,14,31,42,44,67,91,100)) # 8 ladders
      snakes <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16,82), 
                           end=c(78,75,73,24,60,19,53,11,26,6,65)) # 11 snakes
    }
    
  }
  else{
    x= custom_sl(dimension); ladders= as.data.frame(x[1]); snakes= as.data.frame(x[2])
  }
  
  # Plotting the board
  plot(1,type = "n")
  plot_board(dimension, snakes, ladders)
  
  # Playing the game
  p1=0; p2=0 # Both players start at position zero
  t1=0; t2=0; l1=0; l2=0; s1=0;s2=0; # Turns, ladders and snakes taken by each player
  # A matrix to record the data of the game
  winnings= matrix(rep(0), ncol= 3, nrow= 1) 
  colnames(winnings)=c("Turns","Ladders", "Snakes")
  
  while (p1 < (dimension)**2 && p2<(dimension)**2){
    cat ("Player 1 Turn\n")
    r1=sample(sides, 1,replace=T)
    cat('Dice Roll = ',r1,"\n")
    p1 = p1 + r1 # Recording the new position
    t1 = t1+1 # Incrementing the turn
    if (any(ladders$s %in% p1)) { # Checking for ladders
      p1 <- ladders$e[ladders$s %in% p1]
      l1 <- l1 + 1
    }else if (any(snakes$s %in% p1)) { # Checking for snakes
      p1 <- snakes$e[snakes$s %in% p1]
      s1 <- s1 + 1}
    cat('Current Status of Player 1: ',p1,"and Player 2:",p2,"\n")
    
    if ((p1>(dimension**2)-1)){ # Checking if a player won 
      cat ('\n Winner of the game is Player 1 with',t1,"turns \n")
      winnings[,1]=t1; winnings[,2]=l1; winnings[,3]=s1 # Recording the data of this game
      break
    }
    
    cat("Player 2 Turn\n")
    r2=sample(sides, 1,replace=T)
    cat('Dice Roll = ',r2,"\n")
    p2=p2 + r2
    t2=t2+1
    if (any(ladders$s %in% p2)) {
      p2 <- ladders$e[ladders$s %in% p2]
      l2 <- l2 + 1
    }else if (any(snakes$s %in% p2)) {
      p2 <- snakes$e[snakes$s %in% p2]
      s2 <- s2 + 1}
    
    #p2=check_sl(p2)
    cat('Current Status of Player 1 : ',p1,"and Player 2:",p2,"\n")
    
    if ((p2>(dimension**2)-1)){
      cat ('\n Winner of the game is Player 2 with',t2,"turns \n")
      winnings[,1]=t2; winnings[,2]=l2; winnings[,3]=s2
      break
    }
  }
  #print(winnings)
  return(winnings)
}

#game simulate to be called inside play has no plot board to reduce time to multiple simulations and no printing
game_simulate = function(sides=6, dimension=10, difficulity = 1){
  # Checking on the dimension
  if(dimension > 10){
    print("Please enter a number not greater than 10!")
    return()
  }
  if(dimension == 10){
    if(difficulity ==1){
      ladders <- data.frame(start=c(1,4,9,21,28,36,51,71,80,94), 
                            end=c(38,14,31,42,84,44,67,91,100,99)) # 10 ladders
      snakes <- data.frame(start=c(98,95,93,64,62,56,49,47,16), 
                           end=c(78,75,73,60,19,53,11,26,6)) # 9  Snakes
      
    }
    if(difficulity == 2){
      
      ladders <- data.frame(start=c(1,4,9,21,28,36,51,71,80), 
                            end=c(38,14,31,42,84,44,67,91,100)) # 9 ladders 
      snakes <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16), 
                           end=c(78,75,73,24,60,19,53,11,26,6)) # 10 snakes
    }
    if(difficulity == 3 ){
      ladders <- data.frame(start=c(1,4,9,21,36,51,71,80), 
                            end=c(38,14,31,42,44,67,91,100)) # 8 ladders
      snakes <- data.frame(start=c(98,95,93,87,64,62,56,49,47,16,82), 
                           end=c(78,75,73,24,60,19,53,11,26,6,65)) # 11 snakes
    }
    
  }
  else{
    x= custom_sl(dimension); ladders= as.data.frame(x[1]); snakes= as.data.frame(x[2])
  }
  
  # Playing the game
  p1=0; p2=0 # Both players start at position zero
  t1=0; t2=0; l1=0; l2=0; s1=0;s2=0; # Turns, ladders and snakes taken by each player
  # A matrix to record the data of the game
  winnings= matrix(rep(0), ncol= 3, nrow= 1) 
  colnames(winnings)=c("Turns","Ladders", "Snakes")
  
  while (p1 < (dimension)**2 && p2<(dimension)**2){
    r1=sample(sides, 1,replace=T)
    p1 = p1 + r1 # Recording the new position
    t1 = t1+1 # Incrementing the turn
    if (any(ladders$s %in% p1)) { # Checking for ladders
      p1 <- ladders$e[ladders$s %in% p1]
      l1 <- l1 + 1
    }else if (any(snakes$s %in% p1)) { # Checking for snakes
      p1 <- snakes$e[snakes$s %in% p1]
      s1 <- s1 + 1}
    
    if ((p1>(dimension**2)-1)){ # Checking if a player won 
      winnings[,1]=t1; winnings[,2]=l1; winnings[,3]=s1 # Recording the data of this game
      break
    }
    
    #cat("Player 2 Turn\n")
    r2=sample(sides, 1,replace=T)
    p2=p2 + r2
    t2=t2+1
    if (any(ladders$s %in% p2)) {
      p2 <- ladders$e[ladders$s %in% p2]
      l2 <- l2 + 1
    }else if (any(snakes$s %in% p2)) {
      p2 <- snakes$e[snakes$s %in% p2]
      s2 <- s2 + 1}
    
    if ((p2>(dimension**2)-1)){
      winnings[,1]=t2; winnings[,2]=l2; winnings[,3]=s2
      break
    }
  }
  return(winnings)
}


# Playing N games
play = function(N, sides=6, dimension=10,difficulity = 1){
  # Creating a matrix to store the data
  wins=matrix(rep(0), ncol= 3, nrow= N)
  colnames(wins)=c("Turns","Ladders", "Snakes")
  
  # Simulating playing N times
  for(i in 1:N){
    result = game_simulate(sides, dimension, difficulity)
    wins[i,] = result
  }
  # Printing the data of the simulation
  cat("Average of ", N, "games \n")
  print(colMeans(wins))
  
  #Plotting the simulation results
  wins<-as.data.frame(wins)
  g1<-ggplot(wins, aes(Turns)) + 
    geom_histogram(binwidth = 2, fill = "steelblue", color = "#132A45")+
    labs(title = "Number of Turns",
         y = "number of games")
  g2<-ggplot(wins, aes(Snakes)) + 
    geom_histogram(binwidth = 1, fill = "steelblue", color = "#132A45") + 
    labs(title = "Number of Snakes",
         x = "snakes/game",
         y = "number of games")
  g3<-ggplot(wins, aes(Ladders)) + 
    geom_histogram(binwidth = 1, fill = "steelblue", color = "#132A45") + 
    labs(title = "Number of Ladders",
         x = "ladders/game",
         y = "number of games")
  grid.arrange(g1, g2, g3, 
               ncol = 1, nrow = 3)
  
  return(wins)
}


N=10000
set.seed(1954)
#play(1000,ladders,snakes)


