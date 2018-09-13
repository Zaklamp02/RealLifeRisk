###############################################################################
# GLOBAL.R
###############################################################################
#
#   MODUS OPERANDI
#
#
###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################

#-------------------------------------------#
# 0. SET UP ENVIRONMENT
#-------------------------------------------#
#
# !!! Most of these settings will probably need
# to be moved to (individual?) text files.
# That way they are a bit easier to adjust
# as we type along.
#
#-------------------------------------------#

# Load basic packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(png)
library(DT)
library(magick)

# Load key files
unitDef   <- read.csv(file.path(getwd(),'www','maak_units.csv'),header=T,skip=1,stringsAsFactors = F)   # defines all units 
playerDef <- read.csv(file.path(getwd(),'www','maak_spelers.csv'),header=T,skip=1,stringsAsFactors = F) # defines all players

# Basic game settings
scrnRes  = c(1600,600)                                                                     # screen resolution
gridSize = c(20,9)                                                                         # size of game board 
sprRes   = rep(floor(scrnRes[1]*0.73/gridSize[1]),2)                                       # resolution of sprites/units (scaled to match screen resolution)
gridRes  = gridSize*sprRes                                                                 # resolution of gameboard
yNames   = c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))[1:gridSize[2]]      # names of Y axis                                                    # names of Y axis
xNames   = as.character(1:gridSize[1])                                                     # names of X axis
uNames   = unitDef$Unit                                                                    # unit names (abbreviated)
pNames   = playerDef$Player                                                                # names of players (abbreviated)

# Basic variables
startIni = 25                                                                              # RANDOMLY initialize the board with 15 units
turn     = 1                                                                               # can be anything
year     = 1945                                                                            # define the current year
yearStart= 1945                                                                            # define when the game begins
yearCycle= 5                                                                               # defines how many turns constitute a year
turnBonus = 500
yearBonus = 1500

# Basic board definition
boardDef = data.frame(xPos     = rep(xNames,each=gridSize[2]),                             # all possible X coördinates (in grid)
                      yPos     = rep(yNames,gridSize[1]),                                  # all possible Y coördinates (in grid)
                      xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],  # all possible X coördinates (in pixels)
                      yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],       # all possible Y coördinates (in pixels)
                      water    = rep('no',gridSize[1]*gridSize[2]),                        # boolean if location is 'water'
                      stringsAsFactors=F)                                                  # make sure to encode as 'char' instead of 'factor'

# Create initial board state
boardState <- data.frame(xPos=sample(1:length(xNames),startIni,replace=T),
                         yPos=sample(1:length(yNames),startIni,replace=T))
boardState$xRes <- boardState$xPos * sprRes[1]-sprRes[1]
boardState$yRes <- boardState$yPos * sprRes[2]-sprRes[2]
boardState$xPos <- xNames[boardState$xPos]
boardState$yPos <- yNames[boardState$yPos]
boardState$player <- sample(pNames,startIni,replace=T)
boardState$unit   <- sample(uNames,startIni,replace=T)
boardState$sprite <- paste0(boardState$unit,boardState$player)
boardState$quantity <- sample(1:4,startIni,replace=T)

# Load images & sprites
spr <- list()                                                                              # this will hold all sprites
spr[['board']] = readPNG(file.path(getwd(),'www','base_map.png'),native=T)                 # read gameboard
for(i in 1:nrow(unitDef)){                                                                 # loop over units
  sprTmp <- image_read(file.path(getwd(),'www',paste0('spr_',unitDef$Label[i],'.jpeg')))    # read sprite for current unit
  for(j in 1:nrow(playerDef)){                                                             # loop over players
    spr[[paste0(unitDef$Unit[i],playerDef$Player[j])]] <- sprTmp %>%                       # make copy of unit sprite
      image_fill(color=playerDef$Color[j],fuzz=20) %>%                                     # set background of sprite to player color
      image_scale(paste0("x",sprRes[1]))
  }
}


#-------------------------------------------#
# 1. CHECK ACTION
#-------------------------------------------#
#
# The check action function is a key function that does exactly what the name implies. It checks whether the action is specified
# in a valid way, BEFORE it is executed. So it basically serves as a 'try/catch' mechanism. In most cases, you ALWAYS want
# to call this function BEFORE executingan action. If the action is correctly specified, it will return 'success'.
#
#-------------------------------------------#

check_action <- function(tbs,player,unit,quantity,x1,y1,path){
  out      <- list()                                                                                     # allocate variable to hold output
  out$type <- 'fout'                                                                                     # assume the worst

  # Perform GENERAL checks
  if(! player %in% pNames){                                                                              # check if player name exists
    out$msg  <- 'speler bestaat niet'                                                                    # if so, update message
  } else if(! unit %in% uNames){                                                                         # check if unit type exists, use else-if to create chain of checks
    out$msg  <- 'unit bestaat niet'                                                                      # if so, update message
  } else if(!x1 %in% boardDef$xPos | !y1 %in% boardDef$yPos){                                            # etc. etc. etc.
    out$msg  <- 'startcoördinaat bestaat niet'
  
  # Check SPECIAL actions
  } else if(is.na(path)){
    if(unitDef$Cost[unitDef$Unit==unit]*as.numeric(quantity) > playerDef$Gold[playerDef$Player==player] ){
      out$msg  <- 'Onvoldoende geld'
    } else {
      out$type <- 'succes'
      out$msg  <- paste0("Koop ",quantity,unit,' op ',x1,y1)
    }
    
  # Check MOVE actions
  } else if(!is.na(path)) {
    pathSep <- unlist(strsplit(path,NULL))                                                               # separate path into individual components
    destE2  <- sum(pathSep %in% c("R","W")) - sum(pathSep %in% c("L","E"))                               # calculate horizontal movement (in squares)
    destN2  <- sum(pathSep %in% c("U","N")) - sum(pathSep %in% c("D","S"))                               # calculate vertical movement (in squares)
    x2      <- ifelse(which(x1==xNames)+destE<=0,NA,xNames[which(x1==xNames)+destE])                     # find target x coördinate
    y2      <- ifelse(which(y1==yNames)+destN<=0,NA,yNames[which(y1==yNames)+destN])                     # find target y coördinate
    
    if(nrow(tbs[tbs$xPos==x1 & tbs$yPos==y1,])<=0){                                                      # check if ANY units exist on square
      out$msg  <- 'Je hebt hier geen units'                                            
    } else if(!tbs$player[tbs$xPos==x1 & tbs$yPos==y1] %in% player){                                     # check if PLAYER has units on square
      out$msg  <- 'Je hebt hier geen units'
    } else if(!tbs$unit[tbs$xPos==x1 & tbs$yPos==y1 & tbs$player==player] %in% unit){                    # check if player has TARGET UNIT on square
      out$msg  <- 'Unit niet op startcoördinaat'
    } else if(tbs$quantity[tbs$xPos==x1 & tbs$yPos==y1 & tbs$player==player & tbs$unit==unit] < quantity){ # check if player has sufficient QUANTITY of units
      out$msg  <- 'Onvoldoende units op startcoördinaat'
    } else if(nchar(path) > unitDef$Speed[unitDef$Unit==unit]){                                          # check if unit can move far enough
      out$msg  <- 'Verplaatsing te ver'
    } else if(is.na(x2) | is.na(y2)){                                                                    # check if target coördinate is valid
      out$msg  <- 'Doelcoördinaat buiten kaart'
    } else {
      out$type <- 'succes'
      out$msg  <- paste0("Verplaats ",quantity,unit,' naar ',y2,x2)
    }
  }

  return(out)
}

#-------------------------------------------#
# 2. EXECUTE MOVE
#-------------------------------------------#
#
# Function does exactly what the name implies. It performs a 'move' action. It will take the specified unit,
# move it from the start location along a specified path to its destination. It also does some basic checks to make sure
# the boardState remains valid in edge cases. A Path is defined as a series of up/down/left/right movements. These can
# be denoted as either u-d-l-r or n-o-z-w (noord oost zuid west)
#
# IMPORTANT: Make sure to check_action first!!!
#
#-------------------------------------------#

execute_move <- function(tbs,player,unit,quantity,x1,y1,path){
  msg <- 'unit succesvol verplaatst naar: '
  
  if(unit == 'ALL'){
    rowFrom = which(tbs$xPos==x1 & tbs$yPos==y1 & tbs$player==player)          # find the row where unit is currently specified
  } else {
    rowFrom = which(tbs$xPos==x1 & tbs$yPos==y1 & tbs$unit==unit & tbs$player==player) # find the row where unit is currently specified
    
    if(quantity < tbs$quantity[rowFrom]){                                      # if there are more units than player wants to move ...
      tbs <- rbind(tbs,tbs[rowFrom,])                                          # ... then make a copy of the unit ...
      tbs$quantity[nrow(tbs)] <- tbs$quantity[nrow(tbs)] - quantity            # ... whith a quantity equal to the units remaining ...
      tbs$quantity[rowFrom]   <- quantity                                      # ... and set quantity of the 'moving row' equal to quantity specified
    }
  }

  path <- toupper(path)                                                        # make sure all characters are in upper case
  
  for(i in 1:nchar(path)){                                                     # loop over the entire path
    nextLoc <- substr(path,i,i)                                                # find current step in the path
    
    if(nextLoc == 'U' | nextLoc == 'N'){                                       # then move unit up
      yInd <- which(yNames %in% tbs$yPos[rowFrom])                             # find index of location on Y axis
      if(yInd+1 <= length(yNames)){                                            # then position is valid, so execute step
        tbs$yPos[rowFrom] <- yNames[yInd+1]                                    # update location (coördinate)
        tbs$yRes[rowFrom] <- tbs$yRes[rowFrom] + sprRes[2]                     # update location (in pixels)
      }
    } else if(nextLoc == 'D' | nextLoc == 'Z'){
      yInd <- which(yNames %in% tbs$yPos[rowFrom])                             # now do the same for 'down/south' movements
      if(yInd >= 2){                                            
        tbs$yPos[rowFrom] <- yNames[yInd-1]                   
        tbs$yRes[rowFrom] <- tbs$yRes[rowFrom] - sprRes[2]
      }
    } else if(nextLoc == 'L' | nextLoc == 'W'){
      xInd <- which(xNames %in% tbs$xPos[rowFrom])                             # now do the same for 'left/west' movements           
      if(xInd >= 2){                                             
        tbs$xPos[rowFrom] <- xNames[xInd-1]                   
        tbs$xRes[rowFrom] <- tbs$xRes[rowFrom] - sprRes[1]
      }
    } else if(nextLoc == 'R' | nextLoc == 'O'){                                # now do the same for 'right/east' movements
      xInd <- which(xNames %in% tbs$xPos[rowFrom])                      
      if(xInd+1 <= length(xNames)){                                     
        tbs$xPos[rowFrom] <- xNames[xInd+1]                   
        tbs$xRes[rowFrom] <- tbs$xRes[rowFrom] + sprRes[1]
      }
    }
    
    if( length(unique(tbs$player[tbs$xPos==tbs$xPos[rowFrom] & tbs$yPos==tbs$yPos[rowFrom]]))>1 ){ # if there are opponents on the current square
      msg <- gsub('succesvol verplaatst naar','gestopt op',msg)                # update the message to reflect move has not been completely completed
      break                                                                    # stop further movement
    }
  }
  
  tbs <- setNames(aggregate(tbs[, c("quantity")],                              # remove any duplicate rows, and return boardstate
                            tbs[, names(tbs)[!names(tbs) %in% c("quantity")]], # this will take all rows that are identical
                            FUN = sum, na.rm = TRUE),                          # ... and collapse them while summing the quantity
                  c(names(tbs)[!names(tbs) %in% c("quantity")],"quantity"))    # finally, make sure column names are correct
  
  msg <- paste(msg,tbs$xPos[rowFrom],tbs$yPos[rowFrom])                        # add destination coördinates to output message

  return(list(tbs=tbs,msg=msg))
}


#-------------------------------------------#
# 3. CREATE UNIT
#-------------------------------------------#
#
# This function creates units on locations where the corresponding unit does not already exist. 
# This will usually happen for two reasons. OR: a unit is moved to a location where a player
# does not already have units of this type. OR: a unit is bought/obtained so it needs to be created.
#
#-------------------------------------------#

create_unit <- function(tbs,player,unit,quantity,x,y){
  tbs <- rbind(tbs,tbs[1,])                                             # randomly duplicate first row as a placeholder
  tbs[nrow(tbs),'xPos']      <- x                                              # now set all variables to correct values
  tbs[nrow(tbs),'yPos']      <- y
  tbs[nrow(tbs),'xRes']      <- as.numeric(x)*sprRes[1]-sprRes[1]
  tbs[nrow(tbs),'yRes']      <- which(LETTERS %in% y) * sprRes[2]-sprRes[2]
  tbs[nrow(tbs),'unit']      <- unit
  tbs[nrow(tbs),'player']    <- player
  tbs[nrow(tbs),'sprite']    <- paste0(unit,player)
  tbs[nrow(tbs),'quantity']  <- as.numeric(quantity)

  tbs <- simplify_board(tbs)
  
  return(tbs)
}

buy_unit <- function(tbs,player,unit,quantity,x,y){
  tbs <- create_unit(tbs,player,unit,quantity,x,y)
  playerDef$Gold[playerDef$Player==player] <<- playerDef$Gold[playerDef$Player==player] - unitDef$Cost[unitDef$Unit==unit]*as.numeric(quantity) 
  
  return(tbs)
}

#-------------------------------------------#
# 4. BATTLE ENGInE
#-------------------------------------------#
#
# This function resolves fights on the board
# a 'Fight' is defined as any square that is
# occupied by units from more than 1 player
#
# The order of fighting is determined by
# the relative attack power of partaking units
# The fight is stopped when highest relative
# attacking power <1, meaning there is no unit
# left that can take out another unit.
#
#-------------------------------------------#

battle_engine <- function(tbs, x, y){
  
  fight <- tbs[tbs$xPos == x & tbs$yPos == y,]                                                # select all units on square
  done  <- F                                                                               # used later on to define the end of the battle
  turn  <- 0                                                                               # used later to check how many turns were taken
  fighters <- NULL                                                                         # convenience variable
  msg   <- paste0("Uitslag gevecht ",y,x)                                                  # starts an output message
  
  for(i in 1:nrow(fight)){                                                                 # loop over all units in the fight
    pcurrent <- fight$player[i]                                                            # find current player
    ocurrent <- fight$unit[fight$player != pcurrent]                                       # find possible opponents
    ucurrent <- unitDef[unitDef$Unit==fight$unit[i],unitDef$Unit] * fight$quantity[i]      # find relative strength against all possible opponents
    ucurrent[!names(ucurrent) %in% ocurrent] <- 0                                          # set strengths against non-existing opponents to 0
    fighters <- rbind(fighters, cbind(fight[i,c('player','unit','quantity')],ucurrent))    # add this information to current fighter sheet
  }
  
  while(done==F){                                                                          # initiate a round of fighting the fight
    turn    <- turn+1                                                                      # check how long we have been fighting
    loc     <- which(fighters[,-(1:3)]==max(fighters[,-(1:3)]),arr.ind = T)                # find location of highest attack power
    loc     <- loc[sample(nrow(loc),1),]                                                   # in case of multiple equals, choose randomly
    
    # allocate convenience variables [SHOULD IMPROVE EFFICIENCY!!!!]
    rattack <- loc[1]
    pattack <- fighters$player[loc[1]]                                                     # find attacking player
    uattack <- fighters$unit[loc[1]]
    qattack <- fighters$quantity[loc[1]]
    sattack <- fighters[loc[1],3+loc[2]]
    rdefend <- which(fighters$player != pattack & fighters$unit==names(fighters)[3+loc[2]])# find defending unit
    rdefend <- ifelse(length(rdefend==1),rdefend,sample(rdefend,1))                        # in case of multiple defending units, choose randmoly
    pdefend <- fighters$player[rdefend]
    udefend <- fighters$unit[rdefend]
    qdefend <- fighters$quantity[rdefend]
    sdefend <- fighters[rdefend,uattack]
    
    msg <- paste(msg,paste(qattack,uattack,'van',pattack,'vs',qdefend,udefend,'van',pdefend), sep = '<br/>')
    
    # now actually start evaluating the fight
    ldefend <- min(fighters$quantity[rdefend],floor(sattack))
    lattack <- min(fighters$quantity[rattack],floor(sdefend))
    
    fighters$quantity[rdefend] <- fighters$quantity[rdefend] - ldefend
    fighters$quantity[rattack] <- fighters$quantity[rattack] - lattack
    
    fighters[rattack,unitDef$Unit] <- fighters[rattack,unitDef$Unit] * (sattack-ldefend)/sattack
    fighters[rattack,3+loc[2]]     <- 0
    
    fighters[rdefend,unitDef$Unit] <- fighters[rdefend,unitDef$Unit] * (sdefend-lattack)/sdefend
    fighters[rdefend,uattack]      <- 0
    
    playerDef$Gold[playerDef$Player==pattack] <<- playerDef$Gold[playerDef$Player==pattack] + ldefend*unitDef$KillBonus[unitDef$Unit==udefend]
    playerDef$Gold[playerDef$Player==pdefend] <<- playerDef$Gold[playerDef$Player==pdefend] + lattack*unitDef$KillBonus[unitDef$Unit==uattack]

    fighters <- fighters[fighters$quantity>0,]
    
    if(nrow(fighters)==0){                                                              # then no players left
      done=T
    } else if(length(unique(fighters$player[fighters$quantity>0]))==1){                 # then only 1 player left
      done=T
    } else if(all(fighters[,-c(1:3)]==0)){                                              # then 2+ players left, but all units have 0 attacking power vs eachother
      done=T                                                                            # maybe then you want to reset attacking power?
    } else if(turn>50){
      done=T                                                                            # force a break if there is no winner after 50 turns
    }
  }
  
  tbs <- rbind(tbs[tbs$xPos != x | tbs$yPos != y,],tbs[rownames(fighters),])            # remove vanquished units from board
  tbs <- simplify_board(tbs)                                                            # make sure to remove potential duplicates (probably unnecessary here)

  return(list(tbs=tbs,msg=msg))
}

#-------------------------------------------#
# 5. PARSE ACTION
#-------------------------------------------#
#
# This function takes in an action (written in
# any acceptable format), and decomposes it
# into relevant action components. It is the
# basic introduction into performing any action
# 
# IMPORTANT: This function will only break up
# an action into components. This should be 
# followed by a 'check_action' function, to 
# evaluate whether an action is actually valid.
#
#-------------------------------------------#

parse_action <- function(action){
  out <- list()
  if(is.character(action)){                                    # then input is in character format
    act <- unlist(strsplit(action,'[.]'))                      # try to split on '.' (CAN IMPROVE LATER!!!)
    if(length(act)<3){                                         # then too few elements
      out$type <- 'fout'                                       # log short message
      out$msg  <- 'te weinig elementen'                        # log long message
    } else if(length(act)>4){                                  # then too many elements
      out$type <- 'fout'
      out$msg  <- 'teveel elementen'
    } else{                                                    # then either 3 or 4 elements
      out$type     <- 'succes'                                 # log that parsing was successful
      out$msg      <- paste('actie bevat', length(act),'elementen')
      out$player   <- toupper(act[1])                          # log all elements
      out$unit     <- toupper(gsub('\\d','', act[2]))
      out$quantity <- as.numeric(gsub('\\D','', act[2]))
      out$y1       <- toupper(gsub('\\d','', act[3]))
      out$x1       <- toupper(gsub('\\D','', act[3]))
      out$path     <- toupper(gsub('\\d','', act[4]))
    }
  } else {
    out$type <- 'fout'
    out$msg  <- 'onbekend formaat'
  }
  return(out)
}

#-------------------------------------------#
# 6. UPDATE SCORE BUTTONS
#-------------------------------------------#
#
# Minor function to automatically update
# all score buttons on screen. May be improved
# in the future by using reactive global variables
#
#-------------------------------------------#

update_score_buttons <- function(session){                                     # function will only work for 1 session at a time
  for(i in 1:nrow(playerDef)){                                                 # for every player ...
    updateActionButton(session,paste0("p",i,"Score"),label=paste(playerDef$Label[i],playerDef$Gold[i])) # ... update the gold-button
  }
  updateActionButton(session,"turn",paste("turn",turn))
  updateActionButton(session,"year",paste("year",year))
}

#-------------------------------------------#
# 7. SIMPLIFY BOARD
#-------------------------------------------#
#
# Minor function to remove duplicate rows
# The function will merge identical rows 
# (i.e. same unit, player ánd coördinate)
# and collapse them, summing the quantity
#
#-------------------------------------------#

simplify_board <- function(tbs){
  tbs$xRes <- match(tbs$xPos,xNames) * sprRes[1]-sprRes[1]                     # double-check x pixel coördinates
  tbs$yRes <- match(tbs$yPos,yNames) * sprRes[2]-sprRes[2]                     # check y pixel coördinates
  
  # remove duplicates
  tbs <- setNames(aggregate(tbs[, c("quantity")],                              # remove any duplicate rows, and return boardstate
                            tbs[, names(tbs)[!names(tbs) %in% c("quantity")]], # this will take all rows that are identical
                            FUN = sum, na.rm = TRUE),                          # ... and collapse them while summing the quantity
                  c(names(tbs)[!names(tbs) %in% c("quantity")],"quantity"))    # finally, make sure column names are correct
  return(tbs)
}

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
# UNUSED FUNCTIONS
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#
# Playground for functions that are currently
# not used, but will be at a later time.
#
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#

# EVALUATE ACTION
evaluate_action <- function(tbs,action){
  
  a <- parse_action(tbs, action)
  
  aType <- check_action(tbs,a[[1]],a[[2]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]])
  
  # Possible action types: unit_move, unit_create, special
  if(aType == 'unit_move'){
    do_move(tbs,a[[1]],a[[2]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]])
  } else if( aType == 'unit_create'){
    create_unit(tbs,a[[1]],a[[2]],a[[3]],a[[5]],a[[4]])
  }
  
}


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# X. ARCHIVE
#   
#        _____
#      //  +  \     this is where code
#     ||  RIP  |      goes to die
#     ||       |      
#     ||       |      
#    \||/\/\//\|/     
#
# 