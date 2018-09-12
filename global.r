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
startIni = 15                                                                              # RANDOMLY initialize the board with 15 units

# Basic board definition
boardDef = data.frame(xPos     = rep(xNames,each=gridSize[2]),                             # all possible X coördinates (in grid)
                      yPos     = rep(yNames,gridSize[1]),                                  # all possible Y coördinates (in grid)
                      xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],  # all possible X coördinates (in pixels)
                      yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],       # all possible Y coördinates (in pixels)
                      water    = rep('no',gridSize[1]*gridSize[2]),                        # boolean if location is 'water'
                      stringsAsFactors=F)                                                  # make sure to encode as 'char' instead of 'factor'

# Create initial board state
boardState <- data.frame(xPos=sample(1:length(xNames),8),
                         yPos=sample(1:length(yNames),8))
boardState$xRes <- boardState$xPos * sprRes[1]-sprRes[1]
boardState$yRes <- boardState$yPos * sprRes[2]-sprRes[2]
boardState$xPos <- xNames[boardState$xPos]
boardState$yPos <- yNames[boardState$yPos]
boardState$player <- sample(pNames,8,replace=T)
boardState$unit   <- sample(uNames,8,replace=T)
boardState$sprite <- paste0(boardState$unit,boardState$player)
boardState$quantity <- sample(1:4,8,replace=T)

# Load images & sprites
spr <- list()                                                                              # this will hold all sprites
spr[['board']] = readPNG(file.path(getwd(),'www','base_map.png'),native=T)                 # read gameboard
for(i in 1:nrow(unitDef)){                                                                 # loop over units
  sprTmp <- image_read(file.path(getwd(),'www',paste0('spr_',unitDef$Label[i],'.png')))    # read sprite for current unit
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

check_action <- function(boardState,player,unit,quantity,x1,y1,x2,y2){
  
  # Assume action is correctly specified
  out <- list()

  # Basic checks
  if(! player %in% pNames){
    out$type <- 'fout'
    out$msg  <- 'speler bestaat niet'
  } else if(! unit %in% uNames){
    out$type <- 'fout'
    out$msg  <- 'unit bestaat niet'
  } else if(! x1 %in% boardDef$xPos){
    out$type <- 'fout'
    out$msg  <- 'start bestaat niet'
  } else if(! x2 %in% boardDef$xPos & y2 %in% boardDef$yPos){
    out$type <- 'fout'
    out$msg  <- 'doel bestaat niet'
  } else {
    out$type <- 'succes'
    out$msg  <- 'actie begrepen'
  }
  
  # Advanced checks
#  if(!player %in% boardState[boardState$xPos==x1 & boardState$yPos==y1,'player']){
#    message = 'Player does not exist on start coördinate'
#  } else if(!unit %in% boardState[boardState$xPos==x1 & boardState$yPos==y1 & boardState$player==player,'unit']){
#    message = 'Unit does not exist on start coördinate'
#  } else if(quantity < boardState[boardState$xPos==x1 & boardState$yPos==y1 & boardState$player==player,'quantity']){
#    message = 'Not enough units on start coördinate'
#  } else if(boardDef[boardDef$xPos==x2 & boardDef$yPos==y2,'water']=='yes'){
#    message = 'Destination coördinate is water!'
#  }

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

execute_move <- function(tbs,player,unit,quantity,x1,y1,x2,path){
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

  return(list(bs=tbs,msg=msg))
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

create_unit <- function(boardState,player,unit,quantity,x,y){
  boardState <- rbind(boardState,boardState[1,])                                             # randomly duplicate first row as a placeholder
  boardState[nrow(boardState),'xPos']      <- x                                              # now set all variables to correct values
  boardState[nrow(boardState),'yPos']      <- y
  boardState[nrow(boardState),'xRes']      <- as.numeric(x)*sprRes[1]-sprRes[1]
  boardState[nrow(boardState),'yRes']      <- which(LETTERS %in% y) * sprRes[2]-sprRes[2]
  boardState[nrow(boardState),'unit']      <- unit
  boardState[nrow(boardState),'player']    <- player
  boardState[nrow(boardState),'sprite']    <- paste0(unit,player)
  boardState[nrow(boardState),'quantity']  <- as.numeric(quantity)
  
#  spr <- unitDef[unitDef$unit==unit,'sprite']                                                # look up correct sprite pointer in unitDef
#  spr <- sub('p1',tolower(player),spr)                                                       # substitute correct player identifier in sprite pointer
#  boardState[nrow(boardState),'sprite']    <- spr                                            # actually log sprite pointer in dataframe
  
  return(boardState)
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

battle_engine <- function(bs, x, y){
  
  fight <- bs[bs$xPos == x & bs$yPos == y,]                                                # select all units on square
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
  
  bs <- rbind(bs[bs$xPos != x | bs$yPos != y,],bs[rownames(fighters),])

  return(list(boardState=bs,msg=msg))
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
      out$msg      <- ifelse(length(act)==3,'speciale actie','verplaatsing')
      out$player   <- act[1]                                   # log all elements
      out$unit     <- gsub('\\d','', act[2])
      out$quantity <- gsub('\\D','', act[2])
      out$y1       <- gsub('\\d','', act[3])
      out$x1       <- gsub('\\D','', act[3])
      out$y2       <- gsub('\\d','', act[4])
      out$x2       <- gsub('\\D','', act[4])
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

update_score_buttons <- function(session){                              # function will only work for 1 session at a time
  for(i in 1:nrow(playerDef)){                                          # for every player ...
    updateActionButton(session,paste0("p",i,"Score"),label=paste(playerDef$Label[i],playerDef$Gold[i])) # ... update the gold-button
  }
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
evaluate_action <- function(boardState,action){
  
  a <- parse_action(boardState, action)
  
  aType <- check_action(boardState,a[[1]],a[[2]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]])
  
  # Possible action types: unit_move, unit_create, special
  if(aType == 'unit_move'){
    do_move(boardState,a[[1]],a[[2]],a[[3]],a[[4]],a[[5]],a[[6]],a[[7]])
  } else if( aType == 'unit_create'){
    create_unit(boardState,a[[1]],a[[2]],a[[3]],a[[5]],a[[4]])
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