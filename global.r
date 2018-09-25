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
wd        <- getwd()
unitDef   <- read.csv(file.path(wd,'www','maak_units.csv'),header=T,skip=1,stringsAsFactors = F)   # defines all units 
playerDef <- read.csv(file.path(wd,'www','maak_spelers.csv'),header=T,skip=1,stringsAsFactors = F) # defines all players
playerDef <- cbind(playerDef,t(col2rgb(playerDef$Color)))                                   # add player color in RGB

# Basic game settings
scrnRes  <- c(1600,600)                                                                     # screen resolution
gridSize <- c(20,8)                                                                         # size of game board 
sprRes   <- rep(floor(scrnRes[1]*0.73/gridSize[1]),2)                                       # resolution of sprites/units (scaled to match screen resolution)
gridRes  <- gridSize*sprRes                                                                 # resolution of gameboard
yNames   <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))[1:gridSize[2]]      # names of Y axis                                                    # names of Y axis
xNames   <- as.character(1:gridSize[1])                                                     # names of X axis
uNames   <- unitDef$Unit                                                                    # unit names (abbreviated)
pNames   <- playerDef$Player                                                                # names of players (abbreviated)

# Basic variables
startIni  <- 15                                                                             # RANDOMLY initialize the board with 15 units
turn      <- 1                                                                              # can be anything
year      <- 1945                                                                           # define the current year
yearStart <- 1945                                                                           # define when the game begins
yearCycle <- 5                                                                              # defines how many turns constitute a year
turnBonus <- 500
yearBonus <- 1500
mapAlpha  <- 0.3                                                                            # alpha value for player/map occupation tiles
gameID    <- paste0(LETTERS[sample(1:26,1)],LETTERS[sample(1:26,1)],LETTERS[sample(1:26,1)],sample(111:999,1))

# Basic board definition
boardDef <- data.frame(xPos     = rep(xNames,each=gridSize[2]),                             # all possible X coördinates (in grid)
                       yPos     = rep(yNames,gridSize[1]),                                  # all possible Y coördinates (in grid)
                       xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],  # all possible X coördinates (in pixels)
                       yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],       # all possible Y coördinates (in pixels)
                       water    = rep('no',gridSize[1]*gridSize[2]),                        # boolean if location is 'water'
                       stringsAsFactors=F)                                                  # make sure to encode as 'char' instead of 'factor'

# Create initial board state
boardState <- data.frame(xPos=sample(1:length(xNames),startIni,replace=T),
                         yPos=sample(1:length(yNames),startIni,replace=T))
boardState$xRes     <- boardState$xPos * sprRes[1]-sprRes[1]
boardState$yRes     <- boardState$yPos * sprRes[2]-sprRes[2]
boardState$xPos     <- xNames[boardState$xPos]
boardState$yPos     <- yNames[boardState$yPos]
boardState$player   <- sample(pNames,startIni,replace=T)
boardState$unit     <- sample(uNames,startIni,replace=T)
boardState$quantity <- sample(1:4,startIni,replace=T)

# Load images & sprites
spr <- list()                                                                              # this will hold all sprites
spr[['board']] <- readPNG(file.path(wd,'www','base_map.png'),native=F)                     # read gameboard
for(i in 1:nrow(unitDef)){                                                                 # loop over units
  sprTmp <- image_read(file.path(wd,'www',paste0('spr_',unitDef$Label[i],'.jpeg')))        # read sprite for current unit
  for(j in 1:nrow(playerDef)){                                                             # loop over players
    spr[[paste0(unitDef$Unit[i],playerDef$Player[j])]] <- sprTmp %>%                       # make copy of unit sprite
      image_fill(color=playerDef$Color[j],fuzz=20) %>%                                     # set background of sprite to player color
      image_scale(paste0("x",sprRes[1]))
  }
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
  if(is.character(action)){                                                    # then input is in character format
    act <- unlist(strsplit(action,'[.]'))                                      # try to split on '.' (CAN IMPROVE LATER!!!)
    if(length(act)<3){                                                         # then too few elements
      out$type <- 'fout'                                                       # log short message
      out$msg  <- 'te weinig elementen'                                        # log long message
    } else if(length(act)>4){                                                  # then too many elements
      out$type <- 'fout'
      out$msg  <- 'teveel elementen'
    } else{                                                                    # then either 3 or 4 elements
      out$type     <- 'succes'                                                 # log that parsing was successful
      out$msg      <- paste('actie bevat', length(act),'elementen')
      out$player   <- toupper(act[1])                                          # log all elements
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
# 2. CHECK ACTION
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
    destE  <- sum(pathSep %in% c("R","W")) - sum(pathSep %in% c("L","E"))                                # calculate horizontal movement (in squares)
    destN  <- sum(pathSep %in% c("U","N")) - sum(pathSep %in% c("D","S"))                                # calculate vertical movement (in squares)
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
# 3. EXECUTE MOVE
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

execute_move <- function(tbs,tland,player,unit,quantity,x1,y1,path){
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
        tland <- updateMapOwner(tland,tbs$xRes[rowFrom],tbs$yRes[rowFrom],player) # update ownership of tile to new player
      }
    } else if(nextLoc == 'D' | nextLoc == 'Z'){
      yInd <- which(yNames %in% tbs$yPos[rowFrom])                             # now do the same for 'down/south' movements
      if(yInd >= 2){                                            
        tbs$yPos[rowFrom] <- yNames[yInd-1]                   
        tbs$yRes[rowFrom] <- tbs$yRes[rowFrom] - sprRes[2]
        tland <- updateMapOwner(tland,tbs$xRes[rowFrom],tbs$yRes[rowFrom],player)
      }
    } else if(nextLoc == 'L' | nextLoc == 'W'){
      xInd <- which(xNames %in% tbs$xPos[rowFrom])                             # now do the same for 'left/west' movements           
      if(xInd >= 2){                                             
        tbs$xPos[rowFrom] <- xNames[xInd-1]                   
        tbs$xRes[rowFrom] <- tbs$xRes[rowFrom] - sprRes[1]
        tland <- updateMapOwner(tland,tbs$xRes[rowFrom],tbs$yRes[rowFrom],player)
      }
    } else if(nextLoc == 'R' | nextLoc == 'O'){                                # now do the same for 'right/east' movements
      xInd <- which(xNames %in% tbs$xPos[rowFrom])                      
      if(xInd+1 <= length(xNames)){                                     
        tbs$xPos[rowFrom] <- xNames[xInd+1]                   
        tbs$xRes[rowFrom] <- tbs$xRes[rowFrom] + sprRes[1]
        tland <- updateMapOwner(tland,tbs$xRes[rowFrom],tbs$yRes[rowFrom],player)
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

  return(list(tbs=tbs,tland=tland,msg=msg))
}

#-------------------------------------------#
# 4 CREATE UNIT
#-------------------------------------------#
#
# Takes care of creating units. Consists
# of two basic functions;
# 
# create_unit <- does exactly what you think
# buy_unit    <- also does exactly what you think; wraps around create_unit
#
#-------------------------------------------#

create_unit <- function(tbs,player,unit,quantity,x,y){
  tbs                        <- rbind(tbs,tbs[1,])                            # randomly duplicate first row as a placeholder
  tbs[nrow(tbs),'xPos']      <- x                                             # now set all variables to correct values
  tbs[nrow(tbs),'yPos']      <- y
  tbs[nrow(tbs),'xRes']      <- as.numeric(x)*sprRes[1]-sprRes[1]
  tbs[nrow(tbs),'yRes']      <- which(LETTERS %in% y) * sprRes[2]-sprRes[2]
  tbs[nrow(tbs),'unit']      <- unit
  tbs[nrow(tbs),'player']    <- player
  tbs[nrow(tbs),'quantity']  <- as.numeric(quantity)
  tbs                        <- simplify_board(tbs)
  
  return(tbs)
}

buy_unit <- function(tbs,player,unit,quantity,x,y){
  tbs <- create_unit(tbs,player,unit,quantity,x,y)
  playerDef$Gold[playerDef$Player==player] <<- playerDef$Gold[playerDef$Player==player] - unitDef$Cost[unitDef$Unit==unit]*as.numeric(quantity) 
  
  return(tbs)
}

#-------------------------------------------#
# 5. EXECUTE FIGHT
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
  
  fight <- tbs[tbs$xPos == x & tbs$yPos == y,]                                             # select all units on square
  done  <- F                                                                               # used later on to define the end of the battle
  turn  <- 0                                                                               # used later to check how many turns were taken
  fighters <- NULL                                                                         # convenience variable
  msg   <- paste0("Uitslag gevecht ",y,x)                                                  # starts an output message
  
  for(i in 1:nrow(fight)){                                                                 # loop over all units in the fight
    pcurrent <- fight$player[i]                                                            # find current player
    ocurrent <- fight$unit[fight$player != pcurrent]                                       # find possible opponents
    ucurrent <- unitDef[unitDef$Unit==fight$unit[i],unitDef$Unit] * fight$quantity[i]      # find relative strength against all possible opponents
    fighters <- rbind(fighters, cbind(fight[i,c('player','unit','quantity')],ucurrent))    # add this information to current fighter sheet
  }
  
  while(done==F){                                                                          # initiate a round of fighting the fight
    
    for(i in 1:nrow(fighters)){                                                            # start by checking/cleaning
      pcurrent <- fighters$player[i]                                                       # find current player
      ocurrent <- fighters$unit[fighters$player != pcurrent]                               # find possible opponents
      fighters[i,!names(fighters) %in% c('player','unit','quantity',ocurrent)] <- 0        # set attack-power vs non-existing oponents to 0
    }

    if(max(fighters[,-(1:3)])==0){                                                         # check if there are any valid attacks, otherwise break
      msg <- paste(msg, paste('aanvalskracht uitgeput'), sep = '<br/>')                    # report back
      break
    }
    
    turn    <- turn+1                                                                      # check how long we have been fighting
    loc     <- which(fighters[,-(1:3)]==max(fighters[,-(1:3)]),arr.ind = T)                # find location of highest attack power
    loc     <- loc[sample(nrow(loc),1),]                                                   # in case of multiple equals, choose randomly
    
    # allocate convenience variables [SHOULD IMPROVE EFFICIENCY!!!!]
    rattack <- loc[1]                                                                      # attacking row number
    pattack <- fighters$player[loc[1]]                                                     # attacking player
    uattack <- fighters$unit[loc[1]]                                                       # attacking unit
    qattack <- fighters$quantity[loc[1]]                                                   # attacking quantity
    sattack <- fighters[loc[1],3+loc[2]]                                                   # attacking strength
    rdefend <- which(fighters$player != pattack & fighters$unit==names(fighters)[3+loc[2]])# defending row
    rdefend <- rdefend[order(fighters$quantity[rdefend],decreasing=T)[1]]                  # in case of multiple defending units, choose highest quantity, otherwise randmoly
    pdefend <- fighters$player[rdefend]
    udefend <- fighters$unit[rdefend]
    qdefend <- fighters$quantity[rdefend]
    sdefend <- fighters[rdefend,uattack]
    
    msg <- paste(msg,paste(qattack,uattack,'van',pattack,'vs',qdefend,udefend,'van',pdefend), sep = '<br/>')
    
    # now actually start evaluating the fight
    ldefend <- min(fighters$quantity[rdefend],floor(sattack*qattack))                      # calculate losses for defender
    lattack <- min(fighters$quantity[rattack],floor(sdefend*qdefend))                      # calculate losses for attacker
    
    fighters$quantity[rdefend] <- fighters$quantity[rdefend] - ldefend                     # apply losses or defender
    fighters$quantity[rattack] <- fighters$quantity[rattack] - lattack                     # apply losses for attacker
    
    fighters[rattack,unitDef$Unit] <- fighters[rattack,unitDef$Unit] * ifelse(sattack==0,1,(sattack-ldefend)/sattack) # reduce attacking power for attacker
    fighters[rattack,udefend]      <- ifelse(sum(fighters$unit==udefend & fighters$player != pattack)==1,0,fighters[rattack,udefend]) # reduce attackpower vs defending unit to 0, unless another same unit exists for a 3d player
    
    fighters[rdefend,unitDef$Unit] <- fighters[rdefend,unitDef$Unit] * ifelse(sdefend==0,1,(sdefend-lattack)/sdefend) # reduce attacking power for defender
    fighters[rdefend,uattack]      <- ifelse(sum(fighters$unit==uattack & fighters$player != pdefend)==1,0,fighters[rdefend,uattack])
    
    playerDef$Gold[playerDef$Player==pattack] <- playerDef$Gold[playerDef$Player==pattack] + ldefend*unitDef$KillBonus[unitDef$Unit==udefend] # update gold for defender
    playerDef$Gold[playerDef$Player==pdefend] <- playerDef$Gold[playerDef$Player==pdefend] + lattack*unitDef$KillBonus[unitDef$Unit==uattack] # update gold for attacker
    
    fighters <- fighters[fighters$quantity>0,]                                             # remove dead units
    
    if(nrow(fighters)==0){                                                                 # then no players left
      done=T
    } else if(length(unique(fighters$player[fighters$quantity>0]))==1){                    # then only 1 player left
      done=T
    } else if(all(fighters[,-c(1:3)]==0)){                                                 # then 2+ players left, but all units have 0 attacking power vs eachother
      done=T                                                                               # maybe then you want to reset attacking power?
    } else if(turn>50){
      done=T                                                                               # force a break if there is no winner after 50 turns
    }
  }
  
  fight <- fight[rownames(fighters),]                                                      # find surviving units
  
  if(nrow(fight)>=1){                                                                      # if there are any 
    fight$quantity <- fighters$quantity                                                    # update their quantity
  }

  tbs <- rbind(tbs[tbs$xPos != x | tbs$yPos != y,],fight)                                  # remove vanquished units from board
  tbs <- simplify_board(tbs)                                                               # make sure to remove potential duplicates (probably unnecessary here)
  
  return(list(tbs=tbs,msg=msg))
}

#-------------------------------------------#
# 6. UPDATE MAP OWNERSHIP
#-------------------------------------------#
#
# This function takes care of assigning land (tiles) to players. Basically any tile that is occupied by 1 player
# is assigned to that player. Once a new player occupies that land, it is re-assigned to the new player. 
# In case of disputed land (2+ players at the same time), land becomes unassigned
#
# This function should be called during all move actions (in each individual move step),
# during unit creation and during end-of-turn events
#
#-------------------------------------------#

updateMapOwner <- function(land,x1,y1,player){
  if(is.character(x1)){x1 <- pos2res(x1,xNames)}                               # if x is given as position, find corresponding pixel value
  if(is.character(y1)){y1 <- pos2res(y1,yNames)}                               # if y is given as position, find corresponding pixel value
  
  y1 <- gridRes[2]-sprRes[2]-y1                                                # our coördinates start bottom-left, but this image will be drawn from top-left rasterplot, which starts top-left
  y2 <- y1+sprRes[2]                                                           # ... so we need to make sure to 'flip' the y axis
  x2 <- x1+sprRes[1]
  
  if(length(player)>1){                                                        # in case of disputed land, ownership is undecided
    land[y1:y2,x1:x2,1:4] <- 0                                                 # so put everything to 0, including alpha
  } else {
    land[y1:y2,x1:x2,1] <- playerDef$red[playerDef$Player==player]/255         # otherwise, assing player color
    land[y1:y2,x1:x2,2] <- playerDef$green[playerDef$Player==player]/255
    land[y1:y2,x1:x2,3] <- playerDef$blue[playerDef$Player==player]/255
    land[y1:y2,x1:x2,4] <- mapAlpha
  }
  return(land)
}

# Basic land ownership definition
land <- array(rep(0,gridRes[2]*gridRes[1]*4),c(gridRes[2],gridRes[1],4))
for(i in 1:nrow(boardState)){
  land <- updateMapOwner(land,boardState$xRes[i],boardState$yRes[i],boardState$player[i])
}

# Initiate game variable
game <- NULL
game[[paste(turn)]][['bs']] <- boardState
game[[paste(turn)]][['land']] <- land

#-------------------------------------------#
# 7. RENDER GAME BOARD
#-------------------------------------------#
#
# Basic function to render the game board
#
#-------------------------------------------#
render_board <- function(tbs,tland){
  par(mar=rep(0,4))                                                            # set figure margins to 0
  plot.window(xlim=c(0,gridRes[1]),ylim=c(0,gridRes[2]))                       # create a window with correct size
  rasterImage(spr[['board']],0,0,gridRes[1],gridRes[2])                        # create 'base layer' of game map to start drawing on
  rasterImage(tland,0,0,gridRes[1],gridRes[2],interpolate=F)                   # create 'base layer' of game map to start drawing on
  
  for(i in 1:nrow(tbs)){                                                       # loop over all units in boardState
    rasterImage(spr[[paste0(tbs$unit[i],tbs$player[i])]],                      # cast sprite pointer (char) to variable, and draw
                tbs[i,'xRes'],                                                 # define correct x1,y1,x2,y2 coördinates (in pixels)
                tbs[i,'yRes'],
                tbs[i,'xRes']+sprRes[1],
                tbs[i,'yRes']+sprRes[2])
    text(x=tbs$xRes[i]+sprRes[1]*0.8,y=tbs$yRes[i]+sprRes[2]*0.8,tbs$quantity[i],cex=1.3, font=2)
  }
  
  abline(h=seq(0,gridRes[2],sprRes[2]), col="darkgrey", lwd=4)                 # horizontal lines. Draw these LAST to mask potential sprite-overlaps
  abline(v=seq(0,gridRes[1],sprRes[1]), col="darkgrey", lwd=4)                 # vertical lines to complete the grid/raster
  text(x=boardDef$xRes+sprRes[1]/10, y = gridRes[2]+sprRes[1]/10, labels = boardDef$xPos) # create coördinate labels along x axis
  text(x=-sprRes[1]/10, y = boardDef$yRes+sprRes[1]/10, labels = boardDef$yPos) # create coördinate labels along y axis
}

#-------------------------------------------#
# 8. SAVE TURN
#-------------------------------------------#
#
# Takes care of saving/storing end-of-turn results
# The core is pretty simple; just take the current
# states of variables, and store/save them.
#
# The function does also check if 'future' states
# exist, and removes these from the game. This
# is done to avoid creating parallel timelines.
#
#-------------------------------------------#

save_turn <- function(d){
  game[[paste(turn)]][['bs']] <<- d$bs                                         # store boardstate
  game[[paste(turn)]][['land']] <<- d$land                                     # store land ownership
  
  if(as.numeric(turn) < max(as.numeric(names(game)))){                         # if a parallel universe is started ...
    game[paste(seq(turn+1,max(as.numeric(names(game)))))] <<- NULL             # ... destroy the old universe
  }
  
  saveRDS(game,file.path(wd,'savegame',paste0("Game_",gameID,"_turn_",turn,".rds"))) # save entire game
  png(file.path(wd,'savegame',paste0("Game_",gameID,"_turn_",turn,'.png')),    # save png of boardState
      width=gridRes[1],height=gridRes[2])
  plot.new()
  render_board(d$bs,d$land)
  dev.off()
  
  if(turn==2){                                                                 # the first turn is not automatically saved, because it is not the result of an end-of-turn event
    png(file.path(wd,'savegame',paste0("Game_",gameID,"_turn_",turn-1,'.png')),# save png of boardState
        width=gridRes[1],height=gridRes[2])
    plot.new()
    render_board(game[['1']][['bs']],game[['1']][['land']])
    dev.off()
  }  
}

#-------------------------------------------#
# 10 SIMPLIFY BOARD
#-------------------------------------------#
simplify_board <- function(tbs){
  tbs$xRes <- match(tbs$xPos,xNames) * sprRes[1]-sprRes[1]                    # double-check x pixel coördinates
  tbs$yRes <- match(tbs$yPos,yNames) * sprRes[2]-sprRes[2]                    # check y pixel coördinates
  
  # remove duplicates
  tbs <- setNames(aggregate(tbs[, c("quantity")],                             # remove any duplicate rows, and return boardstate
                            tbs[, names(tbs)[!names(tbs) %in% c("quantity")]],# this will take all rows that are identical
                            FUN = sum, na.rm = TRUE),                         # ... and collapse them while summing the quantity
                  c(names(tbs)[!names(tbs) %in% c("quantity")],"quantity"))   # finally, make sure column names are correct
  return(tbs)
}

#-------------------------------------------#
# 11. UPDATE SCORE BUTTONS
#-------------------------------------------#
update_buttons <- function(session){                                          # function will only work for 1 session at a time
  for(i in 1:nrow(playerDef)){                                                # for every player ...
    updateActionButton(session,paste0("p",i,"Score"),label=paste(playerDef$Label[i],playerDef$Gold[i])) # ... update the gold-button
  }
  updateActionButton(session,"turn",paste("turn",turn))
  updateActionButton(session,"year",paste("year",year))
}

#-------------------------------------------#
# 12 POS2RES
#-------------------------------------------#
pos2res <- function(pos,names){                                               # calculate resolution in pixels from a position (origin: bottom-left)
  res = which(toupper(pos) == names)*sprRes[1]-sprRes[1]                      # make sure position is in uppercase
  return(res)
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