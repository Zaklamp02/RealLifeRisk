###############################################################################
# SERVER.R
###############################################################################
#
#   MODUS OPERANDI
#
#   This function is the 'heart' of the Shiny App. It contains all the internal
#   logic of the app. So this is the part that 'notices' when you click a button,
#   and calculates the result. It is where the board is drawn & updated, and
#   basically the place where the whole game is recorded & evaluated.
#
################################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################

#-------------------------------------------#
# 0. CREATE A NEW SERVER FUNCTION
#-------------------------------------------#
server <- function(input, output, session) {
  
  #-------------------------------------------#
  # 1. DEFINE USER VARIABLES
  #-------------------------------------------#
  d   <- reactiveValues(bs=boardState,land=land)                                              # reactive values are special, and (should) do downstream cascading if they are updated
  sel <- reactiveValues(row=1)                                                                # i.e. if you update this value, all outputs that use this value get updated too
  
  #-------------------------------------------#
  # 2.a. OBSERVE: MOVE BUTTONS
  #-------------------------------------------#                                               # observeEvent listens for an event, and then executes a code chunk
  observeEvent(input$btn_up,{                                                                 # in this case, listen to 'button up' event
    d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] + sprRes[2]                                   # if 'button up' is pressed, move the unit up by 1x the sprite resolution
    d$bs[sel$row,'yPos'] = LETTERS[which(LETTERS %in% d$bs[sel$row,'yPos'])+1]                # also update the Y coördinate
    d$land <- updateMapOwner(d$land,d$bs$xRes[sel$row],d$bs$yRes[sel$row],d$bs$player[sel$row])
  })
  
  observeEvent(input$btn_down,{                                                               # create similar interactions for the other buttons
    d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] - sprRes[2]
    d$bs[sel$row,'yPos'] = LETTERS[which(LETTERS %in% d$bs[sel$row,'yPos'])-1]
    d$land <- updateMapOwner(d$land,d$bs$xRes[sel$row],d$bs$yRes[sel$row],d$bs$player[sel$row])
  })
  
  observeEvent(input$btn_left,{
    d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] - sprRes[1]
    d$bs[sel$row,'xPos'] = as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
    d$land <- updateMapOwner(d$land,d$bs$xRes[sel$row],d$bs$yRes[sel$row],d$bs$player[sel$row])
  })
  
  observeEvent(input$btn_right,{
    d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] + sprRes[1]
    d$bs[sel$row,'xPos'] = as.character(as.numeric(d$bs[sel$row,'xPos'])+1)
    d$land <- updateMapOwner(d$land,d$bs$xRes[sel$row],d$bs$yRes[sel$row],d$bs$player[sel$row])
  })
  
  #-------------------------------------------#
  # 2.b. OBSERVE: FORWARD/BACKWARD
  #-------------------------------------------#
  observeEvent(input$backward,{                                                               # this will take the game 1 turn backward
    if(turn>=2){                                                                              # you cannot go back before turn 2... dôh
      d$bs   <- game[[paste(turn-1)]][['bs']]                                                 # update boardState from game variable
      d$land <- game[[paste(turn-1)]][['land']]                                               # do the same for land ownership
      turn   <<- turn-1                                                                       # update the global turn variable
      year   <<- yearStart + floor(turn/yearCycle)  
      update_buttons(session)                                                                 # make sure the 'turn' and 'year' buttons are correct
    }
  })
  
  observeEvent(input$forward,{                                                                # this will take the game 1 turn forward
    if(as.numeric(turn)<max(as.numeric(names(game)))){                                        # you cannot go forward if there is no turn+1 available... dôh
      d$bs   <- game[[paste(turn+1)]][['bs']]                                                 # same old same old
      d$land <- game[[paste(turn+1)]][['land']]
      turn   <<- turn+1
      year   <<- yearStart + floor(turn/yearCycle)
      update_buttons(session)
    }
  })
  
  #-------------------------------------------#
  # 2.c. OBSERVE: ACTION SUBMISSION
  #-------------------------------------------#
  observeEvent(input$p1a1submit,{
    act <- parse_action(input$p1a1)
    
    if(act$type=="succes"){
      chk <- check_action(d$bs,act$player,act$unit,act$quantity,act$x1,act$y1,act$path)
      act$type  <- chk$type
      act$msg   <- chk$msg
    }

    if(act$type=="succes"){                                                                    # IF the action is valid (i.e. success)
      if(is.na(act$path)){
        d$bs    <- buy_unit(d$bs,act$player,act$unit,act$quantity,act$x1,act$y1)
        act$msg <- 'unit gekocht'
        d$land  <- updateMapOwner(d$land,act$x1,act$y1,act$player)
      } else {
        move    <- execute_move(d$bs,d$land,act$player,act$unit,act$quantity,act$x1,act$y1,act$path) # THEN actually perform the move
        act$msg <- move$msg
        d$bs    <- move$tbs
        d$land  <- move$tland
      }
    }
    
    output$endOfTurnResult <- renderText(paste(act$msg))
    updateActionButton(session,"p1a1submit",label=act$type)
    update_buttons(session)
  })
  
  #-------------------------------------------#
  # 2.d. OBSERVE: END OF TURN
  #-------------------------------------------#
  observeEvent(input$btn_endTurn,{                                                            # observe if the 'end turn' button is pressed 
    msg <- ''
    
    #-------------------------------------------#
    # 2.d.1. CHECK/EXECUTE BATTLES
    for(x in unique(d$bs$xPos)){                                                              # loop over all x coördinates
      for(y in unique(d$bs$yPos)){                                                            # loop over all y coördinates
        if(any(d$bs$xPos==x & d$bs$yPos==y) ){                                                # check if ANY units occupy the current square
          if(length(unique(d$bs$player[d$bs$xPos==x & d$bs$yPos==y]))>1){                     # check if >1 PLAYER occupies the current square
            battleOutcome <- battle_engine(d$bs,x,y)                                          # if so, trigger battle engine
            d$bs <- battleOutcome$tbs
            msg <- paste(msg,battleOutcome$msg,sep = '<br/>')
          } 
        }
      }
    }
    
    #-------------------------------------------#
    # 2.d.2. HOUSEKEEPING
    d$bs$unit[d$bs$unit=="E"] <- "P"                                                          # at the end of turn, change paratroopers to platoons
    d$bs           <- simplify_board(d$bs)                                                    # check board if necessary
    output$battleResult <- renderUI({HTML(msg)})
    turn           <<- turn + 1                                                               # increase turn counter
    year           <<- yearStart + floor(turn/yearCycle)                                      # increase year if necessary 
    playerDef$Gold <<- playerDef$Gold + turnBonus                                             # add turn bonus
    playerDef$Gold <<- playerDef$Gold + ifelse(floor(turn/yearCycle)-floor((turn-1)/yearCycle)==1,yearBonus,0) # in case of new year, add year bonus
    
    update_buttons(session)                                                                   # this will simply overwrite the current value
    
    #-------------------------------------------#
    # 2.d.3. SAVE RESULTS
    save_turn(d)                                                                              # log turn
    
  })
  
  #-------------------------------------------#
  # 2.e. OBSERVE: MOUSE CLICKS
  #-------------------------------------------#
  observe({                                                                                   # tbh: I'm not sure between 'Observe' and 'ObserveEvent'
    if(is.null(input$board_click$x)) return(NULL)                                             # if nothing was clicked yet, do nothing
    
    xTmp <- input$board_click$x - input$board_click$x %% sprRes[1]                            # find nearest x coördinate
    yTmp <- input$board_click$y - input$board_click$y %% sprRes[2]                            # find nearest y coördinate

    if(any(d$bs$xRes == xTmp & d$bs$yRes == yTmp)) {                                          # check if units exist on location
      sel$row <- sort(which(d$bs$xRes==xTmp & d$bs$yRes==yTmp),decreasing=T)[1]               # select only the sprite on top (i.e. the unit that you see)
      updateActionButton(session,"btn_sel",label=paste(d$bs[sel$row,c('player','unit')],collapse="-")) # if unit is found, update the button-label
    }
  })
  
  #-------------------------------------------#
  # 3.a RENDER: GAME BOARD
  #-------------------------------------------#
  output$game_board <- renderPlot({                                                           # this is the actual fun stuff: the game board!
    render_board(d$bs,d$land)
  }, height = gridRes[2], width = gridRes[1])                                                 # make sure plot is rendered at same size as plot itself
  
  #-------------------------------------------#
  # 3.b. RENDER: BOARD STATE TABLE
  #-------------------------------------------#
  output$tbl_boardState <- DT::renderDataTable({                                              # render a very basic table
    d$bs                                                                                      # just draw the full current boardState
  })
  
  #-------------------------------------------#
  # 3.c. RENDER: OTHER
  #-------------------------------------------#
  output$set_txt1 <- renderText(paste("X resolution",gridSize[1]," Y Resolution",gridSize[2]))
  output$p1Score <- renderUI({actionButton("p1Score",paste(playerDef$Label[1],playerDef$Gold[1]),width="100%",style=paste("background-color:",playerDef$Color[1]))}) # show score Player 1
  output$p2Score <- renderUI({actionButton("p2Score",paste(playerDef$Label[2],playerDef$Gold[2]),width="100%",style=paste("background-color:",playerDef$Color[2]))}) # show score Player 2
  if(nrow(playerDef)>=3){output$p3Score <- renderUI({actionButton("p3Score",paste(playerDef$Label[3],playerDef$Gold[3]),width="100%",style=paste("background-color:",playerDef$Color[3]))})} # show score Player 3 (if exists)
  if(nrow(playerDef)>=4){output$p4Score <- renderUI({actionButton("p4Score",paste(playerDef$Label[4],playerDef$Gold[4]),width="100%",style=paste("background-color:",playerDef$Color[4]))})} # show score Player 4 (if exists)
  output$testhtml <- renderUI({HTML(paste("test", "str2", sep = '<br/>')) })
  
  #-------------------------------------------#
  # 4. OBSERVE: STATUS SCREEN
  #-------------------------------------------#
  output$saveImg <- downloadHandler(
    filename = paste0("Game_",gameID,"_turn_",turn,'.png'),
    content = function(file) {
      png(file,width=gridRes[1],height=gridRes[2])
      plot.new()
      render_board(d$bs,d$land)
      dev.off()
    }
  )
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