#-------------------------------------------#
# 1. PARSE ACTION
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
    
    if(length(act)==1 & act[1] %in% playerDef$Player){                         # then action is the pre-defined "Px." format
      out$type   <- 'fout'
      out$action <- ''
    } else if(length(act)<3){                                                  # then too few elements
      out$type   <- 'fout'                                                     # log short message
      out$action <- ''                                                         # log long message
    } else if(length(act)>4){                                                  # then too many elements
      out$type   <- 'fout'
      out$action <- 'teveel elementen'
    } else{                                                                    # then either 3 or 4 elements
      n <- as.numeric(unlist(regmatches(act[2], gregexpr("[[:digit:]]+", act[2]))))
      u <- unlist(regmatches(act[2], gregexpr("[[:alpha:]]+", act[2])))
      
      if(length(u) != length(n) & length(u) > 1){
        out$type   <- 'fout'
        out$action <- 'ongelijk aantal units vs aantallen'
      } else {
        out$type     <- 'succes'                                               # log that parsing was successful
        out$action   <- paste('actie bevat', length(act),'elementen')
        out$player   <- toupper(act[1])                                        # log all elements
        out$unit     <- toupper(u[1])
        out$quantity <- as.numeric(n[1])
        out$y1       <- toupper(gsub('\\D','', act[3]))
        out$x1       <- toupper(gsub('\\d','', act[3]))
        out$path     <- toupper(gsub('\\d','', act[4]))
        if(is.na(out$quantity)){out$quantity<-'1'}                             # can be valid for special actions; i.e. radar, bomb
        if(length(u)>1){                                                       # then multi-unit
          out$subunit <- list(u)
          out$subquantity <- list(n)
        }
      }
    }
  } else {
    out$type   <- 'fout'
    out$action <- 'onbekend formaat'
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

check_action <- function(tbs,player,unit,quantity,x1,y1,path,subunit=NULL,subquantity=NULL){
  out      <- list()                                                                                     # allocate variable to hold output
  out$type <- 'fout'                                                                                     # assume the worst
  
  # Perform GENERAL checks
  if(! player %in% pNames){                                                                              # check if player name exists
    out$action  <- paste(player,'bestaat niet')                                                          # if so, update message
  } else if(! unit %in% uNames){                                                                         # check if unit type exists, use else-if to create chain of checks
    out$action  <- paste(unit,'bestaat niet')                                                            # if so, update message
  } else if(!x1 %in% xNames | !y1 %in% yNames){                                                          # etc. etc. etc.
    out$action  <- paste0(x1,y1,' bestaat niet')
  } else if(!is.null(quantity) & quantity<1){
    out$action  <- "hoeveelheid kleiner dan 1"
    
    # Check SPECIAL actions
  } else if(is.na(path)){
    if(!is.null(subunit)){
      out$action  <- 'koop meerdere units verboden'
    } else if(unitDef$Cost[unitDef$Unit==unit]*as.numeric(quantity) > playerDef$Gold[playerDef$Player==player] ){
      out$action  <- 'Onvoldoende geld'
    } else {
      out$type <- 'succes'
      out$action  <- paste0("Koop ",quantity,unit,' op ',x1,y1)
    }
    
    # Check MOVE actions
  } else if(nchar(path) > length(gregexpr('U|R|D|L|N|O|Z|W', path)[[1]]) | nchar(path)==0){              # check if nchar(path)>0 & all characters are valid
    out$action  <- 'onjuiste beweging'
  } else if(!is.na(path) & unit!=unitDef$Unit[1] & unit!=unitDef$Unit[2]) {
    pathSep <- unlist(strsplit(path,NULL))                                                               # separate path into individual components
    destE  <- sum(pathSep %in% c("R","W")) - sum(pathSep %in% c("L","O"))                                # calculate horizontal movement (in squares)
    destN  <- sum(pathSep %in% c("U","N")) - sum(pathSep %in% c("D","Z"))                                # calculate vertical movement (in squares)
    x2      <- ifelse(which(x1==xNames)+destE<=0,NA,xNames[which(x1==xNames)+destE])                     # find target x coördinate
    y2      <- ifelse(which(y1==yNames)+destN<=0,NA,yNames[which(y1==yNames)+destN])                     # find target y coördinate
    
    if(nrow(tbs[tbs$xPos==x1 & tbs$yPos==y1,])<=0){                                                      # check if ANY units exist on square
      out$action  <- paste0('Je hebt hier geen units')                                            
    } else if(!any(tbs$player[tbs$xPos==x1 & tbs$yPos==y1] %in% player)){                                # check if PLAYER has units on square
      out$action  <- 'Je hebt hier geen units'
    } else if(!any(tbs$unit[tbs$xPos==x1 & tbs$yPos==y1 & tbs$player==player] %in% unit)){               # check if player has TARGET UNIT on square
      out$action  <- paste(unit,'niet op startcoördinaat')
    } else if(tbs$quantity[tbs$xPos==x1 & tbs$yPos==y1 & tbs$player==player & tbs$unit==unit] < quantity){ # check if player has sufficient QUANTITY of units
      out$action  <- paste0('Onvoldoende ',unit,' op ',x1,y1)
    } else if(nchar(path) > unitDef$Speed[unitDef$Unit==unit]){                                          # check if unit can move far enough
      out$action  <- 'Verplaatsing te ver'
    } else if(is.na(x2) | is.na(y2)){                                                                    # check if target coördinate is valid
      out$action  <- 'Doelcoördinaat buiten kaart'
    } else {
      out$type <- 'succes'
      out$action  <- paste0("Verplaats ",quantity,unit,' naar ',y2,x2)
    }
  }
  
  return(out)
}

check_action_all <- function(tbs,player,unit,quantity,x1,y1,path,subunit=NULL,subquantity=NULL){
  
  subunit  <- unlist(subunit)                # nice to have, cannot hurt
  subquantity <- unlist(subquantity)
  
  out <- check_action(tbs,player,unit,quantity,x1,y1,path)
  
  if(out$type=='succes' & !is.null(subunit)){
    for(i in 1:length(subunit)){
      out <- check_action(tbs,player,subunit[i],subquantity[i],x1,y1,path,subunit,subquantity) #strsplit(path,'')[[1]][1],NULL,NULL)
      if(out$type=='fout'){break}
    }
    out$action <- sub(paste0(subquantity[i],subunit[i]),'meerdere',out$action)
  }
  
  return(out)
}

parse_and_check <- function(tbs,action){
  prs <- parse_action(action)
  if(prs$type=="succes"){
    prs <- check_action_all(tbs,prs$player,prs$unit,prs$quantity,prs$x1,prs$y1,prs$path,prs$subunit,prs$subquantity)
  }
  return(prs)
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
        tland <- updateMapOwner(tland,tbs$xPos[rowFrom],tbs$yPos[rowFrom],player) # update ownership of tile to new player
      }
    } else if(nextLoc == 'D' | nextLoc == 'Z'){
      yInd <- which(yNames %in% tbs$yPos[rowFrom])                             # now do the same for 'down/south' movements
      if(yInd >= 2){                                            
        tbs$yPos[rowFrom] <- yNames[yInd-1]                   
        tbs$yRes[rowFrom] <- tbs$yRes[rowFrom] - sprRes[2]
        tland <- updateMapOwner(tland,tbs$xPos[rowFrom],tbs$yPos[rowFrom],player)
      }
    } else if(nextLoc == 'L' | nextLoc == 'W'){
      xInd <- which(xNames %in% tbs$xPos[rowFrom])                             # now do the same for 'left/west' movements           
      if(xInd >= 2){                                             
        tbs$xPos[rowFrom] <- xNames[xInd-1]                   
        tbs$xRes[rowFrom] <- tbs$xRes[rowFrom] - sprRes[1]
        tland <- updateMapOwner(tland,tbs$xPos[rowFrom],tbs$yPos[rowFrom],player)
      }
    } else if(nextLoc == 'R' | nextLoc == 'O'){                                # now do the same for 'right/east' movements
      xInd <- which(xNames %in% tbs$xPos[rowFrom])                      
      if(xInd+1 <= length(xNames)){                                     
        tbs$xPos[rowFrom] <- xNames[xInd+1]                   
        tbs$xRes[rowFrom] <- tbs$xRes[rowFrom] + sprRes[1]
        tland <- updateMapOwner(tland,tbs$xPos[rowFrom],tbs$yPos[rowFrom],player)
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

execute_move_all <- function(tbs,tland,player,unit,quantity,x1,y1,path,subunit=NULL,subquantity=NULL){
  subunit  <- unlist(subunit)                # nice to have, cannot hurt
  subquantity <- unlist(subquantity)
  
  out <- execute_move(tbs,tland,player,unit,quantity,x1,y1,path)  
  
  if(is.null(subunit) & is.null(subquantity)){
    
  } else if(is.null(subunit) | is.null(subquantity)){
    out$type <- 'fout'
    out$msg  <- 'subunit als subquantity onbekend'
  } else {
    for(i in 2:length(subunit)){
      out <- execute_move(out$tbs,out$tland,player,subunit[i],subquantity[i],x1,y1,path)
    }
  }
  
  return(out)
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
  if(quantity>0){                                                               # only create units if quantiy > 0
    tbs                        <- rbind(tbs,tbs[1,])                            # randomly duplicate first row as a placeholder
    tbs[nrow(tbs),'xPos']      <- x                                             # now set all variables to correct values
    tbs[nrow(tbs),'yPos']      <- y
    tbs[nrow(tbs),'xRes']      <- which(x==xNames)*sprRes[1]-sprRes[1]
    tbs[nrow(tbs),'yRes']      <- which(y==yNames)*sprRes[2]-sprRes[2]
    tbs[nrow(tbs),'unit']      <- unit
    tbs[nrow(tbs),'player']    <- player
    tbs[nrow(tbs),'quantity']  <- as.numeric(quantity)
    tbs                        <- simplify_board(tbs)
  }
  
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

battle_engine <- function(tbs, tland, x, y){
  
  fight <- tbs[tbs$xPos == x & tbs$yPos == y,]                                             # select all units on square
  fight <- fight[fight$quantity>0,]
  done  <- F                                                                               # used later on to define the end of the battle
  turn  <- 0                                                                               # used later to check how many turns were taken
  fighters <- NULL                                                                         # convenience variable
  msg   <- paste0("Uitslag gevecht ",x,y,': ',paste(sort(unique(fight$player)),collapse=" VS ")) # starts an output message
  
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
      msg <- paste(msg, paste('aanvalskracht uitgeput'), sep = b)                          # report back
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
    
    msg <- paste(msg,paste(qattack,uattack,'van',pattack,'vs',qdefend,udefend,'van',pdefend), sep = b)
    
    # now actually start evaluating the fight
    ldefend <- min(fighters$quantity[rdefend],floor(sattack))                              # calculate losses for defender
    lattack <- min(fighters$quantity[rattack],floor(sdefend))                              # calculate losses for attacker
    
    fighters$quantity[rdefend] <- fighters$quantity[rdefend] - ldefend                     # apply losses or defender
    fighters$quantity[rattack] <- fighters$quantity[rattack] - lattack                     # apply losses for attacker
    
    fighters[rattack,unitDef$Unit] <- fighters[rattack,unitDef$Unit] * ifelse(sattack==0,1,(sattack-ldefend)/sattack) # reduce attacking power for attacker
    fighters[rattack,udefend]      <- ifelse(sum(fighters$unit==udefend & fighters$player != pattack)==1,0,fighters[rattack,udefend]) # reduce attackpower vs defending unit to 0, unless another same unit exists for a 3d player
    
    fighters[rdefend,unitDef$Unit] <- fighters[rdefend,unitDef$Unit] * ifelse(sdefend==0,1,(sdefend-lattack)/sdefend) # reduce attacking power for defender
    fighters[rdefend,uattack]      <- ifelse(sum(fighters$unit==uattack & fighters$player != pdefend)==1,0,fighters[rdefend,uattack])
    
    playerDef$Gold[playerDef$Player==pattack] <- playerDef$Gold[playerDef$Player==pattack] + ldefend*unitDef$KillBonus[unitDef$Unit==udefend] # update gold for defender
    playerDef$Gold[playerDef$Player==pdefend] <- playerDef$Gold[playerDef$Player==pdefend] + lattack*unitDef$KillBonus[unitDef$Unit==uattack] # update gold for attacker
    
    fighters <- fighters[fighters$quantity>0,]                                             # remove dead units
    
    if(turn>50){
      done=T                                                                               # force a break if there is no winner after 50 turns
    } else if(nrow(fighters)==0){                                                          # then no players left
      done=T
    } else if(length(unique(fighters$player[fighters$quantity>0]))==1){                    # then only 1 player left
      done=T
    } else if(min(fighters$quantity * fighters[,-c(1:3)])<1){                              # then 2+ players left, but all units have 0 attacking power vs eachother
      for(i in 1:nrow(fighters)){                                                          # start by checking/cleaning
        pcurrent <- fighters$player[i]                                                     # find current player
        ocurrent <- fighters$unit[fighters$player != pcurrent]                             # find possible opponents
        ucurrent <- unitDef[unitDef$Unit==fighters$unit[i],unitDef$Unit] * fighters$quantity[i] # refresh attack powers
        fighters[i,] <- cbind(fighters[i,c('player','unit','quantity')],ucurrent)
        fighters[i,!names(fighters) %in% c('player','unit','quantity',ocurrent)] <- 0      # set attack-power vs non-existing oponents to 0
      }
    }
  }
  
  # use diff figh vs fighters to build message:
  msg2 <- paste0("Uitslag gevecht ",x,y,': ',paste(sort(relable(unique(fight$player),playerDef[,c('Player','Label')])),collapse=" VS "),b)
  
  for(p in unique(fight$player)){                                                          # find how many units were lost
    lossFull <- fight[!rownames(fight) %in% rownames(fighters) & fight$player==p,]         # units that are completely dead
    lossPart <- fight[rownames(fight) %in% rownames(fighters) & fight$player==p,]          # units that decreased in quantity
    lossPart$quantity <- lossPart$quantity - fighters$quantity[fighters$player==p]         # find difference in quantity
    lossPart <- lossPart[lossPart$quantity >= 1,]                                          # and ignore units that have no difference in quantity
    loss     <- rbind(lossFull, lossPart )
    
    msg2 <- paste0(msg2, '- Verliezen ',playerDef$Label[playerDef$Player==p],': ', paste0(loss$quantity,'x ',relable(loss$unit,unitDef[,c('Unit','Label')]), collapse = s), b)
  }
  
  fight <- fight[rownames(fighters),]                                                      # find surviving units
  
  if(nrow(fight)>=1){                                                                      # if there are any 
    fight$quantity <- fighters$quantity                                                    # update their quantity
  }
  if(nrow(fight)==0 | length(unique(fight$player))>1){                                     # then either 0 or >1 players
    tland <- updateMapOwner(tland,x,y)
  }
  
  tbs <- rbind(tbs[tbs$xPos != x | tbs$yPos != y,],fight)                                  # remove vanquished units from board
  tbs <- simplify_board(tbs)                                                               # make sure to remove potential duplicates (probably unnecessary here)
  
  return(list(tbs=tbs,tland=tland,msg=msg2,msgOld=msg))
}

#-------------------------------------------#
# 6. EXECUTE ACTION
#-------------------------------------------#
#
# Wrapper function to perform an entire action in one function.
# This function wraps parse_action, check_action, buy_unit, execute_move.
# It will also apply updateMapOwner where necessary.
#
#-------------------------------------------#

do_action <- function(session,tbs,tland,action){
  msg <- parse_action(action)
  
  if(msg$type=="succes"){
    chk        <- check_action(tbs,msg$player,msg$unit,msg$quantity,msg$x1,msg$y1,msg$path)
    msg$action <- chk$action
    msg$type   <- chk$type
  }
  
  if(msg$type=="succes"){                                                                       # IF the action is valid (i.e. success)
    if(is.na(msg$path)){                                                                        # for now, assume a unit needs to be bought
      
      if(msg$unit == unitDef$Unit[1]){                                                          # then we want a radar scan
        msg$outcome <- paste0('radarscan uitgevoerd op ',msg$x1,msg$y1)
        msg$radar   <- radar_scan(tbs,tland,msg$x1,msg$y1,msg$player)
      } else if(msg$unit == unitDef$Unit[2]){
        bomb        <- bomb(tbs,tland,msg$x1,msg$y1,msg$player)
        tbs         <- bomb$tbs
        tland       <- bomb$tland
        msg$outcome <- paste0('bombardement uitgevoerd op ',msg$x1,msg$y1)
        msg$bomb    <- bomb$msg
      } else {                                                                                   # then assume we want to move stuff
        tbs    <- buy_unit(tbs,msg$player,msg$unit,msg$quantity,msg$x1,msg$y1)
        tland  <- updateMapOwner(tland,msg$x1,msg$y1,msg$player)
        msg$outcome <- paste0(msg$quantity," ",unitDef$Label[unitDef$Unit==msg$unit],"s gekocht op ",msg$y1,msg$x1)
      }
    } else {                                                                                    # if path is supplied, action must be a move
      move   <- execute_move_all(tbs,tland,msg$player,msg$unit,msg$quantity,msg$x1,msg$y1,msg$path,msg$subunit,msg$subquantity) # THEN actually perform the move
      tbs    <- move$tbs
      tland  <- move$tland
      msg$outcome <- move$msg
    }
  }
  
  update_buttons(session)
  return(list(tbs=tbs,tland=tland,msg=msg))
}

#-------------------------------------------#
# 7. RADAR SCAN
#-------------------------------------------#
#
# A radar scan does not affect the board.
# It only generates a message, containing
# board information around a central square.
# The radius of the radar scan can be changed
# in the game settings.
#
#-------------------------------------------#

radar_scan <- function(tbs,tland,x,y,player){
  
  x1 <- ifelse(which(x==xNames)<=scanRadius, 1, which(x==xNames)-scanRadius)                # find leftmost coördinate                                      
  x2 <- ifelse(which(x==xNames)+scanRadius>=length(xNames), length(xNames), which(x==xNames)+scanRadius) # find rightmost coördinate 
  y1 <- ifelse(which(y==yNames)<=scanRadius, 1, which(y==yNames)-scanRadius)                # find highest coördinate
  y2 <- ifelse(which(y==yNames)+scanRadius>=length(yNames), length(yNames), which(y==yNames)+scanRadius) # find lowest coördinate
  
  msg <- ''
  for(x in x1:x2){                                                                          # loop over horizontal coördinates
    for(y in y1:y2){                                                                        # loop over vertical coördinates
      msg <- paste(msg,xNames[x],yNames[y],': ',sep='')                                     # log current coördinate
      if(any(tbs$xPos==xNames[x] & tbs$yPos==yNames[y])){                                   # check if units exist on coördinate
        hit <- which(tbs$xPos==xNames[x] & tbs$yPos==yNames[y])                             # if so, find indexes ...
        for(i in hit){                                                                      # ... and loop over them
          msg <- paste0(msg, tbs$quantity[i],'x ',tbs$unit[i],' ',playerDef$Label[playerDef$Player==tbs$player[i]]) # log unit, quantity & player
        } 
      } else if(any(tland[gridSize[2]-y+1,x,1:3]!=0)){                                      # else, check if coördinate is owned by player
        clr <- tland[gridSize[2]-y+1,x,1:3] * 255                                           # log color
        msg <- paste0(msg,playerDef$Label[playerDef$red==clr[1] & playerDef$green==clr[2] & playerDef$blue==clr[3]]) # log player belonging to color
      } else {                                                                              # then no relevant information
        msg <- paste(msg,'X')
      }
      msg <- paste0(msg,s)                                                                  # suffix each coördinate with "//"
    }
  }
  
  playerDef$Gold[playerDef$Player==player] <<- playerDef$Gold[playerDef$Player==player] - unitDef$Cost[unitDef$Unit==unitDef$Unit[1]] # update player score
  
  return(msg)
}

#-------------------------------------------#
# 8. BOMB
#-------------------------------------------#
#
# A bomb is a special action that completely
# wipes out all units around a central tile.
# The blast radius can be adjusted in settings.
#
# A bombardment also generates a message informing
# the user and all affected players about the 
# results, i.e. all players that lost units.
#
#-------------------------------------------#

bomb <- function(tbs,tland,xt,yt,player){
  x1 <- ifelse(which(xt==xNames)<=bombRadius, 1, which(xt==xNames)-bombRadius)                # find leftmost coördinate                                      
  x2 <- ifelse(which(xt==xNames)+bombRadius>=length(xNames), length(xNames), which(xt==xNames)+bombRadius) # find rightmost coördinate 
  y1 <- ifelse(which(yt==yNames)<=bombRadius, 1, which(yt==yNames)-bombRadius)                # find highest coördinate
  y2 <- ifelse(which(yt==yNames)+bombRadius>=length(yNames), length(yNames), which(yt==yNames)+bombRadius) # find lowest coördinate
  
  msg <- paste0('Bom van ',playerDef$Label[playerDef$Player==player],'. Verliezen:  ',b)
  for(x in x1:x2){
    for(y in y1:y2){
      msg <- paste0(msg,xNames[x],yNames[y],': ')
      if(any(tbs$xPos==xNames[x] & tbs$yPos==yNames[y])){
        for(i in which(tbs$xPos==xNames[x] & tbs$yPos==yNames[y])){
          msg <- paste(msg, paste0(tbs$quantity[i],'x ',tbs$unit[i],' ',playerDef$Label[playerDef$Player==tbs$player[i]]),sep=',')
        }
        tbs <- tbs[!(tbs$xPos==xNames[x] & tbs$yPos==yNames[y]),]                             # actually kill units
        msg <- sub(",","",msg)                                                                # remove trailing ','
      } else {
        msg <- paste0(msg,'X')
      }
      tland <- updateMapOwner(tland,xNames[x],yNames[y])                                      # free up map ownership
      msg <- paste0(msg,s)                                                                    # suffix all vertical coördinates with a line break
    }
  }
  
  playerDef$Gold[playerDef$Player==player] <<- playerDef$Gold[playerDef$Player==player] - unitDef$Cost[unitDef$Unit==unitDef$Unit[2]] # update player score
  
  return(list(tbs=tbs,tland=tland,msg=msg))
}

#-------------------------------------------#
# 9. SPECIAL
#-------------------------------------------#

parse_special_msg <- function(msg){
  
  maxl <- max(nchar(playerDef$Player))
  
  p <- unlist(strsplit(substr(msg,1,maxl),'[.]'))                                      # try to split on '.' (CAN IMPROVE LATER!!!)
  
  if(!toupper(p) %in% playerDef$Player){   # then message is for one player
    p <- "ALL"
  } else {
    p <- toupper(p)
    msg <- sub(paste0(p,"."),"",msg)
  }

  return(list(p=p,msg=msg))
}

parse_special <- function(action){
  act <- unlist(strsplit(action,'[.]'))                                      # try to split on '.' (CAN IMPROVE LATER!!!)

  p <- toupper(act[1])
  u <- unlist(regmatches(act[2], gregexpr("[[:alpha:]]+", act[2])))
  n <- as.numeric(unlist(regmatches(act[2], gregexpr("[[:digit:]]+", act[2]))))
  
  if(grepl("-",act[2])){
    n <- -n
  }
  
  msg <- paste(n,u,p)
  if(trimws(msg)=='NA'){
    msg <- "Speciale Actie"
  }
  
  return(list(msg=msg,n=n,u=u,p=p))
}

add_gold <- function(action){
  
  act <- parse_special(action)

  if(act$p %in% playerDef$Player){
    if(toupper(act$u) == "GOUD"){
      if(is.numeric(act$n)){
       playerDef$Gold[playerDef$Player==act$p] <<- playerDef$Gold[playerDef$Player==act$p] + act$n 
      }
    }
  }
  
  return(act$msg)
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