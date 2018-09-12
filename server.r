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
  d   <- reactiveValues(bs=boardState)                                                         # reactive values are special, and (should) do downstream cascading if they are updated
  sel <- reactiveValues(row=1)                                                                 # i.e. if you update this value, all outputs that use this value get updated too
  
  #-------------------------------------------#
  # 2.a. OBSERVE: MOVE BUTTONS
  #-------------------------------------------#                                                # observeEvent listens for an event, and then executes a code chunk
  observeEvent(input$btn_up,{                                                                  # in this case, listen to 'button up' event
    d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] + sprRes[2]                                    # if 'button up' is pressed, move the unit up by 1x the sprite resolution
    d$bs[sel$row,'yPos'] = LETTERS[which(LETTERS %in% d$bs[sel$row,'yPos'])+1]                 # also update the Y coördinate
  })
  
  observeEvent(input$btn_down,{                                                                # create similar interactions for the other buttons
    d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] - sprRes[2]
    d$bs[sel$row,'yPos'] = LETTERS[which(LETTERS %in% d$bs[sel$row,'yPos'])-1]
  })
  
  observeEvent(input$btn_left,{
    d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] - sprRes[1]
    d$bs[sel$row,'xPos'] = as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
  })
  
  observeEvent(input$btn_right,{
    d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] + sprRes[1]
    d$bs[sel$row,'xPos'] = as.character(as.numeric(d$bs[sel$row,'xPos'])+1)
  })
  
  #-------------------------------------------#
  # 2.b. OBSERVE: ACTION SUBMISSION
  #-------------------------------------------#
  observeEvent(input$a1submit,{                                                               # observe if a new 'action 1'  is submitted (using drop-down buttons)
  })
  
  observeEvent(input$p1a1submit,{
    act <- parse_action(input$p1a1)
    
    if(act$type=="succes"){
      chk <- check_action(d$bs,act$player,act$unit,act$quantity,act$x1,act$y1,act$x2,act$y2)
      act$type  <- chk$type
      act$msg   <- chk$msg
    }

    if(act$type=="succes"){                                                                    # IF the action is valid (i.e. success)
      if(is.na(act$x2) & is.na(act$y2)){
        d$bs <- create_unit(d$bs,act$player,act$unit,act$quantity,act$x1,act$y1)
        act$msg <- 'unit gekocht'
      } else {
        move <- execute_move(d$bs,act$player,act$unit,act$quantity,act$x1,act$y1,act$x2,act$y2)     # THEN actually perform the move
        act$msg <- move$msg
        d$bs <- move$bs
      }
    }
    
    output$endOfTurnResult <- renderText(paste(act$msg))
    updateActionButton(session,"p1a1submit",label=act$type)
  })
  
  #-------------------------------------------#
  # 2.c. OBSERVE: END OF TURN
  #-------------------------------------------#
  observeEvent(input$btn_endTurn,{                                                            # observe if the 'end turn' button is pressed 
    msg <- ''                                                                                 # this will capture the outcome of the turn
    
    #-------------------------------------------#
    # 2.c.1. CHECK/EXECUTE BATTLES
    
    for(x in unique(d$bs$xPos)){                                                              # loop over all x coördinates
      for(y in unique(d$bs$yPos)){                                                            # loop over all y coördinates
        if(any(d$bs$xPos==x & d$bs$yPos==y) ){                                                # check if ANY units occupy the current square
          if(length(unique(d$bs$player[d$bs$xPos==x & d$bs$yPos==y]))>1){                     # check if >1 PLAYER occupies the current square
            battleOutcome <- battle_engine(d$bs,x,y)                                          # if so, trigger battle engine
            msg2 <- battleOutcome$msg
            d$bs <- battleOutcome$boardState
            msg <- paste(msg,'battle at: ',x,y,' winner: ',unique(d$bs$player[d$bs$xPos==x & d$bs$yPos==y]),sep='') # log the outcome
            output$battleResult <- renderUI({HTML(msg2)})
          }
        }
      }
    }
    update_score_buttons(session)                                                             # this will simply overwrite the current value
    output$endOfTurnResult <- renderText({msg})                                               # report back on the outcome of the turn
  })
  
  #-------------------------------------------#
  # 2.d. OBSERVE: MOUSE CLICKS
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
    
    par(mar=rep(0,4))                                                                         # set figure margins to 0
    plot.window(xlim=c(0,gridRes[1]),ylim=c(0,gridRes[2]))                                    # create a window with correct size
    rasterImage(base_map,0,0,gridRes[1],gridRes[2])                                           # create 'base layer' of game map to start drawing on

    for(i in 1:nrow(d$bs)){                                                                   # loop over all units in boardState
      if(!is.na(d$bs$sprite[i])){                                                             # only draw units with a defined sprite  
        rasterImage(eval(parse(text = d$bs$sprite[i])),                                       # cast sprite pointer (char) to variable, and draw
                    d$bs[i,'xRes'],                                                           # define correct x1,y1,x2,y2 coördinates (in pixels)
                    d$bs[i,'yRes'],
                    d$bs[i,'xRes']+sprRes[1],
                    d$bs[i,'yRes']+sprRes[2])
      }
    }
    
    abline(h=seq(0,gridRes[2],sprRes[2]), col="darkgrey", lwd=4)                              # horizontal lines. Draw these LAST to mask potential sprite-overlaps
    abline(v=seq(0,gridRes[1],sprRes[1]), col="darkgrey", lwd=4)                              # vertical lines to complete the grid/raster
    text(x=boardDef$xRes+sprRes[1]/10, y = gridRes[2]+sprRes[1]/10, labels = boardDef$xPos)   # create coördinate labels along x axis
    text(x=-sprRes[1]/10, y = boardDef$yRes+sprRes[1]/10, labels = boardDef$yPos)             # create coördinate labels along y axis
    
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
  
#  output$p1Score <- renderUI({actionButton("p1Score",paste(playerDef$label[1],playerDef$gold[1]),width="100%",style=paste("background-color:",playerDef$color[1]))}) # show score Player 1
  output$p2Score <- renderUI({actionButton("p2Score",paste(playerDef$label[2],playerDef$gold[2]),width="100%",style=paste("background-color:",playerDef$color[2]))}) # show score Player 2
  if(nrow(playerDef)>2){output$p3Score <- renderUI({actionButton("p3Score",paste(playerDef$label[3],playerDef$gold[3]),width="100%",style=paste("background-color:",playerDef$color[3]))})} # show score Player 3 (if exists)
  if(nrow(playerDef)>3){output$p4Score <- renderUI({actionButton("p4Score",paste(playerDef$label[4],playerDef$gold[4]),width="100%",style=paste("background-color:",playerDef$color[4]))})} # show score Player 4 (if exists)
  output$testhtml <- renderUI({HTML(paste("test", "str2", sep = '<br/>')) })
  
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