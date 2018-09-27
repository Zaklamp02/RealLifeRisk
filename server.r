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
    d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
  })
  
  observeEvent(input$btn_down,{                                                               # create similar interactions for the other buttons
    d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] - sprRes[2]
    d$bs[sel$row,'yPos'] = LETTERS[which(LETTERS %in% d$bs[sel$row,'yPos'])-1]
    d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
  })
  
  observeEvent(input$btn_left,{
    d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] - sprRes[1]
    d$bs[sel$row,'xPos'] = as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
    d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
  })
  
  observeEvent(input$btn_right,{
    d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] + sprRes[1]
    d$bs[sel$row,'xPos'] = as.character(as.numeric(d$bs[sel$row,'xPos'])+1)
    d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
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
  observe({updateActionButton(session,"p1a1submit",label=paste(lazy_check(d$bs,input$p1a1)[2]))})
  observe({updateActionButton(session,"p1a2submit",label=paste(lazy_check(d$bs,input$p1a2)[2]))})
  observe({updateActionButton(session,"p1a3submit",label=paste(lazy_check(d$bs,input$p1a3)[2]))})
  observe({updateActionButton(session,"p2a1submit",label=paste(lazy_check(d$bs,input$p2a1)[2]))})
  observe({updateActionButton(session,"p2a2submit",label=paste(lazy_check(d$bs,input$p2a2)[2]))})
  observe({updateActionButton(session,"p2a3submit",label=paste(lazy_check(d$bs,input$p2a3)[2]))})
  observe({updateActionButton(session,"p3a1submit",label=paste(lazy_check(d$bs,input$p3a1)[2]))})
  observe({updateActionButton(session,"p3a2submit",label=paste(lazy_check(d$bs,input$p3a2)[2]))})
  observe({updateActionButton(session,"p3a3submit",label=paste(lazy_check(d$bs,input$p3a3)[2]))})
  
  observeEvent(input$p1a1submit,{
    result     <- do_action(session,d$bs,d$land,input$p1a1)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p1a1submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p1a1")
  })
  observeEvent(input$p1a2submit,{
    result     <- do_action(session,d$bs,d$land,input$p1a2)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p1a2submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p1a2")
  })
  observeEvent(input$p1a3submit,{
    result     <- do_action(session,d$bs,d$land,input$p1a3)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p1a3submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p1a3")
  })
  observeEvent(input$p2a1submit,{
    result     <- do_action(session,d$bs,d$land,input$p2a1)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p2a1submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p2a1")
  })
  observeEvent(input$p2a2submit,{
    result     <- do_action(session,d$bs,d$land,input$p2a2)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p2a2submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p2a2")
  })
  observeEvent(input$p2a3submit,{
    result     <- do_action(session,d$bs,d$land,input$p2a3)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p2a3submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p2a3")
  })
  observeEvent(input$p3a1submit,{
    result     <- do_action(session,d$bs,d$land,input$p3a1)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p3a1submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p3a1")
  })
  observeEvent(input$p3a2submit,{
    result     <- do_action(session,d$bs,d$land,input$p3a2)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p3a2submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p3a2")
  })
  observeEvent(input$p3a3submit,{
    result     <- do_action(session,d$bs,d$land,input$p3a3)
    d$bs       <- result$tbs
    d$land     <- result$tland
    updateActionButton(session,"p3a3submit",label=paste(result$msg$type,result$msg$check,sep=' , '))
    shinyjs::toggleState("p3a3")
  })
  
  #-------------------------------------------#
  # 2.d. OBSERVE: END OF TURN
  #-------------------------------------------#
  observeEvent(input$btn_endTurn,{                                                            # observe if the 'end turn' button is pressed 
    eot    <- end_turn(session,d$bs,d$land,output)
    d$bs   <- eot$tbs
    d$land <- eot$tland
    output <- eot$output
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