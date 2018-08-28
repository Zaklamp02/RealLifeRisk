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
#
#-------------------------------------------#

# Load basic packages
library(shiny)
library(shinydashboard)
library(png)
library(DT)

# Load images & sprites
base_map          = readPNG(file.path(getwd(),'www','base_map.png'),         native=T)     # read some files
spr_p1_tank       = readPNG(file.path(getwd(),'www','spr_p1_tank.png'),      native=T)     # should be automated later on based on 'unitDef'
spr_p1_platoon    = readPNG(file.path(getwd(),'www','spr_p1_platoon.png'),   native=T) 
spr_p1_artillery  = readPNG(file.path(getwd(),'www','spr_p1_artillery.png'), native=T)
spr_p2_tank       = readPNG(file.path(getwd(),'www','spr_p2_tank.png'),      native=T) 

# Basic game settings
gridSize = c(12,6)                                                                         # size of game board
sprRes   = c(100,100)                                                                      # resolution of sprites/units
gridRes  = gridSize*sprRes                                                                 # resolution of gameboard
yNames   = LETTERS[1:gridSize[2]]                                                          # names of Y axis
xNames   = as.character(1:gridSize[1])                                                     # names of X axis
uNames   = c("T","P","A")                                                                  # unit names (abbreviated)
pNames   = c("P1","P2","P3")                                                               # names of players (abbreviated)

# Board definition
boardDef = data.frame(xPos     = rep(xNames,each=gridSize[2]),                             # all possible X coördinates (in grid)
                      yPos     = rep(yNames,gridSize[1]),                                  # all possible Y coördinates (in grid)
                      xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],  # all possible X coördinates (in pixels)
                      yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],       # all possible Y coördinates (in pixels)
                      water    = rep('no',gridSize[1]*gridSize[2]),                        # boolean if location is 'water'
                      stringsAsFactors=F)                                                  # make sure to encode as 'char' instead of 'factor'

# Basic unit definition
unitDef = data.frame(unit = c("T","P","A"),                                                # abbreviated unit names
                     label = c("Tank","Platoon","Artillery"),                              # full unit names
                     attack = c(5,1,20),                                                   # unit attack power
                     hp = c(50,20,30),                                                     # unit hit points
                     sprite=c('spr_p1_tank','spr_p1_platoon','spr_p1_artillery'),          # basic filepointer to unit sprite (player should be adjusted later)
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

check_action <- function(boardState,player,unit,quantity,y1,x1,y2,x2){
  
  # Assume action is correctly specified
  message = 'success'
  
  # Basic checks
  if(! player %in% pNames){
    message = 'Invalid player'
  } else if(! unit %in% uNames){
    message = 'Invalid unit'
  } else if(! x1 %in% boardDef$xPos){
    message = 'Invalid start coördinate'
  } else if(! x2 %in% boardDef$xPos & y2 %in% boardDef$yPos){
    message = 'Invalid destination coördinate'
  } 
  
  # Advanced checks
  if(!player %in% boardState[boardState$xPos==x1 & boardState$yPos==y1,'player']){
    message = 'Player does not exist on start coördinate'
  } else if(!unit %in% boardState[boardState$xPos==x1 & boardState$yPos==y1 & boardState$player==player,'unit']){
    message = 'Unit does not exist on start coördinate'
  } else if(quantity < boardState[boardState$xPos==x1 & boardState$yPos==y1 & boardState$player==player,'quantity']){
    message = 'Not enough units on start coördinate'
  } else if(boardDef[boardDef$xPos==x2 & boardDef$yPos==y2,'water']=='yes'){
    message = 'Destination coördinate is water!'
  }

  return(message)
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

do_move <- function(boardState,player,unit,quantity,y1,x1,y2,x2){
  
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
  
  spr <- unitDef[unitDef$unit==unit,'sprite']                                                # look up correct sprite pointer in unitDef
  spr <- sub('p1',tolower(player),spr)                                                       # substitute correct player identifier in sprite pointer
  boardState[nrow(boardState),'sprite']    <- spr                                            # actually log sprite pointer in dataframe
  
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