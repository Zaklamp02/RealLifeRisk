#-------------------------------------------#
# 1. END TURN
#-------------------------------------------#
#
# This is a key function. end_turn() will take care of ending a turn.
# It will execute all open actions, evaluating their effect and update the board.
# This includes checking for battle scenarios and resolving them, as well as doing basic housekeeping tasks.
# It will then apply player bonusses where needed, update key variables (turn, year, etc.)
# If required, the function will also save the game state (in .rds, .png and/or .csv)
#
#---------------------------------------------#

end_turn <- function(session,tbs,tland,input,output){
  
  #-------------------------------------------#
  # 1.1. CHECK/DO/FREE ACTIONS
  for(i in playerDef$Player){                                                               # clear up actions list for next turn
    for(j in 1:3){
      actions[[paste0(i,j)]] <<- ''
    }
  }
  
  for(i in 1:nrow(playerDef)){
    for(j in 1:3){
      shinyjs::enable(paste0(playerDef$Player[i],"a",j))
      updateTextInput(session, paste0(playerDef$Player[i],"a",j), value = paste0(playerDef$Player[i],"."))
      updateActionButton(session,paste0(playerDef$Player[i],"a",j,"submit"),"")
    }
  }
  
  #-------------------------------------------#
  # 1.2. CHECK/EXECUTE BATTLES
  msgBattle <- ''
  for(x in unique(tbs$xPos)){                                                              # loop over all x coördinates
    for(y in unique(tbs$yPos)){                                                            # loop over all y coördinates
      if(any(tbs$xPos==x & tbs$yPos==y) ){                                                 # check if ANY units occupy the current square
        if(length(unique(tbs$player[tbs$xPos==x & tbs$yPos==y]))>1){                       # check if >1 PLAYER occupies the current square
          battleOutcome <- battle_engine(tbs,tland,x,y)                                    # if so, trigger battle engine
          tbs <- battleOutcome$tbs
          tland <- battleOutcome$tland
          msgBattle <- paste(msgBattle,battleOutcome$msg,sep = '<br/>')
        }
      }
    }
  }
  
  #-------------------------------------------#
  # 1.3. HOUSEKEEPING A
  tbs$unit[tbs$unit=="E"] <- "P"                                                           # at the end of turn, change paratroopers to platoons
  tbs           <- simplify_board(tbs)                                                     # check board if necessary
  for(i in 1:nrow(playerDef)){
    playerDef$Land[i] <<- sum(tland[,,1]  == playerDef$red[i]/255 & tland[,,2]  == playerDef$green[i]/255 & tland[,,3]  == playerDef$blue[i]/255)  
  }
  
  #-------------------------------------------#
  # 1.4. REPORTING
  output$battleResult <- renderUI({HTML(msgBattle)})
  
  output[[paste0('report',playerDef$Player[1])]] <- renderUI({HTML(generate_report(session,tbs,tland,playerDef$Player[1],input,output,msgBattle))})
  output[[paste0('report',playerDef$Player[2])]] <- renderUI({HTML(generate_report(session,tbs,tland,playerDef$Player[2],input,output,msgBattle))})
  output[[paste0('report',playerDef$Player[3])]] <- renderUI({HTML(generate_report(session,tbs,tland,playerDef$Player[3],input,output,msgBattle))})
  output[[paste0('report',playerDef$Player[4])]] <- renderUI({HTML(generate_report(session,tbs,tland,playerDef$Player[4],input,output,msgBattle))})
  
  #-------------------------------------------#
  # 1.5. HOUSEKEEPING B
  turn           <<- turn + 1                                                              # increase turn counter
  newTurn        <<- TRUE                                                                  # signals remote clients that a new turn has started
  year           <<- yearStart + floor(turn/yearCycle)                                     # increase year if necessary
  playerDef$Gold <<- playerDef$Gold + turnBonus                                            # add turn bonus
  playerDef$Gold <<- playerDef$Gold + ifelse(floor(turn/yearCycle)-floor((turn-1)/yearCycle)==1,yearBonus,0) # in case of new year, add year bonus

  update_buttons(session)                                                                  # this will simply overwrite the current value
  
  #-------------------------------------------#
  # 1.6. SAVE RESULTS
  save_turn(session,tbs,tland)                                                             # log turn
  
  return(list(tbs=tbs,tland=tland,output=output,msgBattle=msgBattle))
}

#-------------------------------------------#
# 2. SAVE TURN
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

save_turn <- function(session,tbs,tland,saveFiles=F){
  game[[paste(turn)]][['bs']] <<- tbs                                                     # store boardstate
  game[[paste(turn)]][['land']] <<- tland                                                 # store land ownership
  game[[paste(turn)]][['msglog']] <<- msglog
  
  if(as.numeric(turn) < max(as.numeric(names(game)))){                                    # if a parallel universe is started ...
    game[paste(seq(turn+1,max(as.numeric(names(game)))))] <<- NULL                        # ... destroy the old universe
  }
  
  if(saveFiles==T){
    saveRDS(game,file.path(wd,'savegame',paste0("Game_",gameID,"_turn_",turn,".rds")))    # save entire game
    png(file.path(wd,'savegame',paste0("Game_",gameID,"_turn_",turn,'.png')),             # save png of boardState
        width=gridRes[1],height=gridRes[2])
    plot.new()
    render_board(tbs,tland)
    render_state(tbs,tland)
    dev.off()
    
    if(turn==2){                                                                          # the first turn is not automatically saved, because it is not the result of an end-of-turn event
      png(file.path(wd,'savegame',paste0("Game_",gameID,"_turn_",turn-1,'.png')),         # save png of boardState
          width=gridRes[1],height=gridRes[2])
      plot.new()
      render_board(game[['1']][['bs']],game[['1']][['land']])
      dev.off()
    }  
  }
}

#-------------------------------------------#
# 3. RENDER GAME BOARD
#-------------------------------------------#
#
# Basic function to render the game board. This is split into two separate functions.
# render_board() does most of the background, while render_state() draws units etc.
# This saves having to redraw the complete board on every update. The idea would be to place
# both plots on top of eachother to generate 1 complete image
#
#-------------------------------------------#

render_board <- function(tbs,tland){
  terrain <- parse_terrain(boardDef)
  par(mar=rep(0,4))                                                                        # set figure margins to 0
  plot.window(xlim=c(0,gridRes[1]),ylim=c(0,gridRes[2]))                                   # create a window with correct size
  rasterImage(spr[['board']],0,0,gridRes[1],gridRes[2])                                    # create 'base layer' of game map to start drawing on
  rasterImage(terrain$terrain,0,0,gridRes[1],gridRes[2],interpolate=F)                     # create layer for cities and water
  plot(terrain$p,add=T,density=10,col='darkgrey')                                          # add texture to cities and water
  text(x=seq(0,gridRes[1]-sprRes[1],sprRes[1])+sprRes[1]/5, y = gridRes[2]+sprRes[1]/4, labels = xNames, font=2) # create coördinate labels along x axis
  text(x=-sprRes[1]/4, y = seq(0,gridRes[2]-sprRes[2],sprRes[2])+sprRes[1]/5, labels = yNames, font=2) # create coördinate labels along y axis
}

render_state <- function(tbs,tland){
  par(mar=rep(0,4))                                                                        # set figure margins to 0
  plot.window(xlim=c(0,gridRes[1]),ylim=c(0,gridRes[2]))                                   # create a window with correct size
  rasterImage(tland,0,0,gridRes[1],gridRes[2],interpolate=F)                               # create 'base layer' of game map to start drawing on
  
  for(i in 1:nrow(tbs)){                                                                   # loop over all units in boardState
    hits <- which(tbs[i,'xRes'] == tbs$xRes & tbs[i,'yRes'] == tbs$yRes)
    
    if(length(hits)==1){
      rasterImage(spr[[paste0(tbs$unit[i],tbs$player[i])]],                                # cast sprite pointer (char) to variable, and draw
                  tbs[i,'xRes'],                                                           # define correct x1,y1,x2,y2 coördinates (in pixels)
                  tbs[i,'yRes'],
                  tbs[i,'xRes']+sprRes[1],
                  tbs[i,'yRes']+sprRes[2])
      text(x=tbs$xRes[i]+sprRes[1]*0.8,y=tbs$yRes[i]+sprRes[2]*0.8,tbs$quantity[i],cex=1.5, font=2)
    } else {
      hitLoc <- which(hits==i)
      nc <- ceiling(sqrt(length(hits))) 
      x  <- (hitLoc-1) %% nc + 1
      y  <- ceiling(hitLoc/nc)
      sprResAdj <- sprRes / nc
      rasterImage(spr[[paste0(tbs$unit[i],tbs$player[i])]],                                # cast sprite pointer (char) to variable, and draw
                  tbs[i,'xRes'] + sprResAdj[1]*(x-1),
                  tbs[i,'yRes'] + sprRes[2] - sprResAdj[2]*(y),
                  tbs[i,'xRes'] + sprResAdj[1]*x,                                          # define correct x1,y1,x2,y2 coördinates (in pixels)
                  tbs[i,'yRes'] + sprRes[2] - sprResAdj[2]*(y-1))
    }
  }
    
  abline(h=seq(0,gridRes[2],sprRes[2]), col="darkgrey", lwd=2)                             # horizontal lines. Draw these LAST to mask potential sprite-overlaps
  abline(v=seq(0,gridRes[1],sprRes[1]), col="darkgrey", lwd=2)                             # vertical lines to complete the grid/raster
}

#-------------------------------------------#
# 4. UPDATE MAP OWNERSHIP
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

updateMapOwner <- function(land,x,y,player=NULL){
  
  x <- which(x == xNames)
  y <- gridSize[2] - which(y == yNames) + 1
  
  if(length(player)>1 | is.null(player)){                                                  # in case of disputed land, ownership is undecided
    land[y,x,1:4] <- 0                                                                     # so put everything to 0, including alpha
  } else {
    land[y,x,1] <- playerDef$red[playerDef$Player==player]/255                             # otherwise, assing player color
    land[y,x,2] <- playerDef$green[playerDef$Player==player]/255
    land[y,x,3] <- playerDef$blue[playerDef$Player==player]/255
    land[y,x,4] <- mapAlpha
  }
  return(land)
}


#-------------------------------------------#
# 5. SIMPLIFY BOARD
#-------------------------------------------#
simplify_board <- function(tbs){
  tbs$xRes <- match(tbs$xPos,xNames) * sprRes[1]-sprRes[1]                                 # double-check x pixel coördinates
  tbs$yRes <- match(tbs$yPos,yNames) * sprRes[2]-sprRes[2]                                 # check y pixel coördinates
  
  # remove duplicates
  tbs <- setNames(aggregate(tbs[, c("quantity")],                                          # remove any duplicate rows, and return boardstate
                            tbs[, names(tbs)[!names(tbs) %in% c("quantity")]],             # this will take all rows that are identical
                            FUN = sum, na.rm = TRUE),                                      # ... and collapse them while summing the quantity
                  c(names(tbs)[!names(tbs) %in% c("quantity")],"quantity"))                # finally, make sure column names are correct
  return(tbs)
}

#-------------------------------------------#
# 6. UPDATE SCORE BUTTONS
#-------------------------------------------#
update_buttons <- function(session){                                                       # function will only work for 1 session at a time
  for(i in 1:nrow(playerDef)){                                                             # for every player ...
    updateActionButton(session,paste0(playerDef$Player[i],"Score"),label=paste(playerDef$Label[i],playerDef$Gold[i])) # ... update the gold-button
  }
  updateActionButton(session,"turn",paste("beurt",turn))
  updateActionButton(session,"year",paste("jaar",year))
}

#-------------------------------------------#
# 7. POS2RES
#-------------------------------------------#
pos2res <- function(pos,names){                                                            # calculate resolution in pixels from a position (origin: bottom-left)
  res = which(toupper(pos) == names)*sprRes[1]-sprRes[1]                                   # make sure position is in uppercase
  return(res)
}

#-------------------------------------------#
# 8. PRINT_TO_PRINTER
#-------------------------------------------#
print_to_printer <- function(txt,filename="tmp.txt"){     
  filename <- ifelse(grepl(".txt",filename),filename,paste0(filename,".txt"))
  writeLines(txt,file.path(getwd(),'savegame',filename))
  system("cmd.exe", input = paste0("notepad /P ", file.path(getwd(),'savegame',filename)),
         show.output.on.console = F, wait = F, minimized = T)   
}

#-------------------------------------------#
# 9. PARSE TERRAIN
#-------------------------------------------#
#
# Function used mainly to identify cities
# and water from a user-specified map. Cities
# and water have to be specified as C/c or W/w.
# The function returns the resulting map in
# two formats; a grid and a raster. The first
# is used to draw colors, the second to draw
# textures.
#
#-------------------------------------------#

parse_terrain <- function(boardDef){              
  terrain <- array(rep(0,gridSize[2]*gridSize[1]*4),c(gridSize[2],gridSize[1],4))          # create empty array of size board * 4
  city    <- data.frame(which(boardDef=="C" | boardDef=="c",arr.ind=T))                    # find row/column locations of cities
  water   <- data.frame(which(boardDef=="W" | boardDef=="w",arr.ind=T))                    # find row/column locations of water
  r       <- raster(ncols=gridSize[1],nrows=gridSize[2],xmn=0,xmx=gridRes[1],ymn=0,ymx=gridRes[2]) # create raster of size board
  
  city$city <- seq(1,nrow(city),by=1)
  
  for(i in 1:nrow(city)){
    a <- rep(FALSE,nrow(city))
    for(j in c(-1,0,1)){                                                   # look up/within/down
      for(k in c(-1,0,1)){                                                 # look left/within/right
        a <- a | (city$row[i]+j==city$row & city$col[i]+k==city$col)       # check if current tile is a city (i.e. neighbour)
      }
    }
    city$city[a] <- min(city$city[a],city$city[i])                         # assign lowest common number to all city tiles
  }
  for(i in unique(city$city)){
    city$city[city$city==i] <- which(unique(city$city)==i)
  }
  
  for(i in 1:nrow(city)){                                                                  # loop through all city tiles
    x <- city[i,1]                                                                         # find x coördinate
    y <- city[i,2]-1                                                                       # find y coördinate
             
    terrain[x,y,1] <- col2rgb('orange')[1]/255                                             # set color (rgb[1]) terrain[x,y,1] <- col2rgb(rainbow(max(city$city))[city$city[i]])[1]/255 set color (rgb[1])
    terrain[x,y,2] <- col2rgb('orange')[2]/255                                             # set color (rgb[2])
    terrain[x,y,3] <- col2rgb('orange')[3]/255                                             # set color (rgb[3])
    terrain[x,y,4] <- 0.5                                                                  # set alpha
    
    r[city[i,1],city[i,2]-1] <- 1                                                          # set raster value
  }
  
  for(i in 1:nrow(water)){                                                                 # do the same for water
    x <- water[i,1]
    y <- water[i,2]-1
    
    terrain[x,y,1] <- col2rgb('blue')[1]/255
    terrain[x,y,2] <- col2rgb('blue')[2]/255
    terrain[x,y,3] <- col2rgb('blue')[3]/255
    terrain[x,y,4] <- 0.5
    
    r[water[i,1],water[i,2]-1] <- 1
  }
  
  p <- rasterToPolygons(r, fun=function(x){x==1}, dissolve=TRUE)                           # this will create polygons where raster = 1
  
  return(list(p=p,terrain=terrain,city=city,water=water))
}

#-------------------------------------------#
# 10. RELABLE
#-------------------------------------------#
# 
# Useful function to for instance relable 
# unit abbreviations to full unit names.
#
#-------------------------------------------#

relable <- function(v, m){
  # v = vector of abbreviations
  # m = nx2 matrix of abbreviations and corresponding lables
  for(i in 1:nrow(m)){
    v[v==m[i,1]] <- gsub(m[i,1],m[i,2],v[v==m[i,1]])
  }
  
  return(v)
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