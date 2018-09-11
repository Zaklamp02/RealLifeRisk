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

# Basic game settings
scrnRes  = c(1600,600)                                                                     # screen resolution
gridSize = c(20,10)                                                                        # size of game board 
sprRes = rep(floor(scrnRes[1]*0.73/gridSize[1]),2)                                         # resolution of sprites/units (scaled to match screen resolution)
gridRes  = gridSize*sprRes                                                                 # resolution of gameboard
yNames   = c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))[1:gridSize[2]]                                                          # names of Y axis
xNames   = as.character(1:gridSize[1])                                                     # names of X axis
uNames   = c("T","P","A")                                                                  # unit names (abbreviated)
pNames   = c("P1","P2","P3")                                                               # names of players (abbreviated)

# Load images & sprites
base_map          = readPNG(file.path(getwd(),'www','base_map.png'),         native=T)     # read some files
spr_p1_tank       = readPNG(file.path(getwd(),'www','spr_p1_tank.png'),      native=T)     # should be automated later on based on 'unitDef'
spr_p1_platoon    = readPNG(file.path(getwd(),'www','spr_p1_platoon.png'),   native=T) 
spr_p1_artillery  = readPNG(file.path(getwd(),'www','spr_p1_artillery.png'), native=T)
spr_p2_tank       = readPNG(file.path(getwd(),'www','spr_p2_tank.png'),      native=T) 

# Basic board definition
boardDef = data.frame(xPos     = rep(xNames,each=gridSize[2]),                             # all possible X coördinates (in grid)
                      yPos     = rep(yNames,gridSize[1]),                                  # all possible Y coördinates (in grid)
                      xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],  # all possible X coördinates (in pixels)
                      yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],       # all possible Y coördinates (in pixels)
                      water    = rep('no',gridSize[1]*gridSize[2]),                        # boolean if location is 'water'
                      stringsAsFactors=F)                                                  # make sure to encode as 'char' instead of 'factor'

# Basic unit definition
unitDef = read.csv(file.path(getwd(),'www','unitDef.csv'),header=T,skip=1,stringsAsFactors = F)

# Basic player definition
playerDef = data.frame(player = c("P1","P2","P3"),
                       label = c("Herten","Sperwers","Vossen"),
                       gold   = c(15000,15000,15000),
                       color  = c("blue","yellow","red"),
                       stringsAsFactors=F)

# Create initial board state                                                               # (this is probably about the ugliest 15 lines of code you will ever see)
boardState = data.frame(xPos     = rep(xNames,each=gridSize[2]),                           # start by creating WAY too much  
                        yPos     = rep(yNames,gridSize[1]),                                # and anyhow, this should come from a text file
                        xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],
                        yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],
                        player   = NA,                                                     # oh, now we're creating empty variables as well?
                        unit     = NA,
                        sprite   = NA,
                        quantity = rep(0,gridSize[1]*gridSize[2]),
                        stringsAsFactors=F)

boardState[1,c('player','unit','quantity','sprite')] <- c('P1','T',1,'spr_p1_tank')        # now we start to actually draw some units
boardState[8,c('player','unit','quantity','sprite')] <- c('P1','P',1,'spr_p1_platoon')
boardState[12,c('player','unit','quantity','sprite')] <- c('P2','T',1,'spr_p2_tank')

boardState$quantity <- as.numeric(boardState$quantity)                                     # this should not be necessary
boardState <- boardState[!is.na(boardState$player),]                                       # and finish by undoing ALL the work we just did

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
# 2. DO MOVE
#-------------------------------------------#
#
# Function does exactly what the name implies. It performs a 'move' action. It will take the specified unit,
# remove it from the start location and add it to the destination. It also does some basic checks to make sure
# the boardState remains valid in edge cases. Make sure to check_action first!!!
#
#-------------------------------------------#

do_move <- function(boardState,player,unit,quantity,x1,y1,x2,y2){
  
  rowFrom = which(boardState$xPos==x1 & boardState$yPos==y1 & boardState$unit==unit & boardState$player==player) # find the row where unit is currently specified
  
  # Get unit to destination
  if(length(boardState[boardState$xPos==x2 & boardState$yPos==y2 & boardState$unit==unit])==1){ # check if unit already exists on target
    rowTo = boardState[boardState$xPos==x2 & boardState$yPos==y2 & boardState$unit==unit]       # if yes, merge with target
    boardState[rowTo,'quantity'] <- boardState[rowTo,'quantity'] + quantity                     # so only update the quantity
  } else {                                                                                      # if not, a new row needs to be created
    boardState <- create_unit(boardState,player,unit,quantity,x2,y2)
  }
  
  # Remove units from start location
  if(boardState[rowFrom,'quantity'] > quantity){                                                # check if any units should remain
    boardState[rowFrom,'quantity'] <- boardState[rowFrom,'quantity'] - quantity                 # if yes, only update quantity
  } else {                                                                                      # if no, remove row
    boardState <- boardState[-rowFrom,]
  }
  
  return(boardState)
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
  boardState[nrow(boardState),'quantity']  <- quantity
  boardState[nrow(boardState),'player']    <- player
  
#  spr <- unitDef[unitDef$unit==unit,'sprite']                                                # look up correct sprite pointer in unitDef
#  spr <- sub('p1',tolower(player),spr)                                                       # substitute correct player identifier in sprite pointer
#  boardState[nrow(boardState),'sprite']    <- spr                                            # actually log sprite pointer in dataframe
  
  return(boardState)
}

#-------------------------------------------#
# 4. DO BATTLE
#-------------------------------------------#
#
# This function resolves fights on the board
# a 'Fight' is defined as any square that is
# occupied by units from more than 1 player
#
# !!! 
# currently this function RANDOMLY selects
# a winner out of competing players. I.e.
# there is no logic/rules at all!
#
#-------------------------------------------#

do_battle <- function(boardState,x,y){
  
  r <- boardState$xPos==x & boardState$yPos==y                                               # get (logical) of all rows in boardState involved in the action
  fight <- boardState[r,]                                                                    # select relevant data
  p <- unique(fight$player)                                                                  # find which players are involved
  winner <- sample(p,1)                                                                      # select random player as the winner
  boardState <- boardState[!r | boardState$player==winner,]                                  # delete all data from other users on selected square
    
  return(boardState)
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

# PARSE ACTION
# idea is that you give this an action in any acceptable definition,
# which is then parsed and returned in an intelligent manner

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
      out$type     <- 'succes'                                   # log that parsing was successful
      out$msg      <- ifelse(length(act)==3,'speciale actie','verplaatsing')
      out$player   <- act[1]                                     # log all elements
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

## B ENGINE 2.0
battle_engine <- function(tbs, x, y){
  
  fight <- tbs[tbs$xPos == x & tbs$yPos == y,]                                            # select all units on square
  done  <- F
  turn  <- 1
  fighters <- NULL
  msg   <- paste0("Uitslag gevecht ",y,x)

  for(i in 1:nrow(fight)){                                                                # loop over all units in the fight
    pcurrent <- fight$player[i]                                                           # find current player
    ocurrent <- fight$unit[fight$player != pcurrent]                                      # find possible opponents
    ucurrent <- unitDef[unitDef$Unit==fight$unit[i],unitDef$Unit] * fight$quantity[i] # find relative strength against all possible opponents
    ucurrent[!names(ucurrent) %in% ocurrent] <- 0                                         # set strengths against non-existing opponents to 0
    fighters <- rbind(fighters, cbind(fight[i,c('player','unit','quantity')],ucurrent))   # add this information to current fighter sheet
  }

  while(done==F){                                                                         # initiate a round of fighting the fight
    loc = which(fighters[,-(1:3)]==max(fighters[,-(1:3)]),arr.ind = T)                    # find location of highest attack power
    loc = loc[sample(nrow(loc),1),]                                                       # in case of multiple equals, choose randomly
    pattack = fighters$player[loc[1]]                                                     # find attacking player
    udefend = which(fighters$player != pattack & fighters$unit==names(fighters)[3+loc[2]])# find defending unit
    udefend = ifelse(length(udefend==1),udefend,sample(udefend,1))                        # in case of multiple defending units, choose randmoly

    # now, actually perform the attack
    q <- fighters$quantity[udefend]        # get quantity of defending units
    msg <- paste(msg,paste(fighters$quantity[loc[1]],fighters$unit[loc[1]],'van',pattack,'vs',q,names(fighters[3+loc[2]]),'van',fighters$player[udefend]), sep = '<br/>')

    # attacker vs defender
    fighters$quantity[udefend] <- fighters$quantity[udefend] - fighters[loc[1],3+loc[2]] # select

    # defender vs attacker
    fighters$quantity[loc[1]] <- fighters$quantity[loc[1]] - floor(q * fighters[udefend,names(fighters) %in% fighters$unit[loc[1]]])

    # clean up quantities a bit
    fighters$quantity[fighters$quantity<0] <- 0

    # reduce attacking power of attacker
    fighters[loc[1],names(fighters) %in% fight$unit] = floor(fighters[loc[1],names(fighters) %in% fight$unit] * (fighters[loc[1],3+loc[2]]-q)/fighters[loc[1],3+loc[2]])
    fighters[loc[1],loc[2]+3] <- 0 # set remaining attack power against defending unit to 0 (to avoid infinite attacks)

    tbs$quantity[tbs$xPos == x & tbs$yPos == y] <- fighters$quantity # update boardState

    turn = turn+1
    if(length(unique(fighters$player[fighters$quantity>0]))==1){                     # if only 1 player left with units, break
      done=T
    } else if(all(fighters[,-c(1:3)]==0)){                                           # if none of the remaining units has any attacking power vs other units, break
      done=T
    } else if(turn>50){
      done=T                                                                         # force a break if there is no winner after 50 turns
    }
  }
  tbs <- tbs[tbs$quantity>0,]

  return(list(boardState=tbs,msg=msg))
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