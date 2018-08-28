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
  # 1. DEFINE REACTIVE DATA
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
    msg <- check_action(d$bs,input$a1p1,input$a1p2,input$a1p3,input$a1p4,input$a1p5,input$a1p6,input$a1p7) # if so, check if action is valid
    if(msg=='success'){                                                                       # IF the action is valid (i.e. success)
      d$bs <- do_move(d$bs,input$a1p1,input$a1p2,input$a1p3,input$a1p4,input$a1p5,input$a1p6,input$a1p7) # THEN actually perform the move
    }
    output$a1result <- renderText({msg})                                                      # report back on the outcome of the action
  })
  
  #-------------------------------------------#
  # 2.c. OBSERVE: END OF TURN
  #-------------------------------------------#
  observeEvent(input$btn_endTurn,{                                                            # observe if the 'end turn' button is pressed 
    msg <- ''                                                                                 # this will capture the outcome of the turn
    
    #-------------------------------------------#
    # 2.c.1. CHECK/EXECUTE BATTLES
    for(x in d$bs$xPos){                                                                      # loop over all x coördinates
      for(y in d$bs$yPos){                                                                    # loop over all y coördinates
        if( any(d$bs$xPos==x & d$bs$yPos==y) ){                                               # check if ANY units occupy the current square
          if(length(unique(d$bs$player[d$bs$xPos==x & d$bs$yPos==y]))>1){                     # check if >1 PLAYER occupies the current square
            d$bs <- do_battle(d$bs,x,y)                                                       # if so, trigger battle engine
            msg <- paste(msg,'battle at: ',x,y,' winner: ',unique(d$bs$player[d$bs$xPos==x & d$bs$yPos==y]),sep='') # log the outcome
          }
        }
      }
    }
    output$endOfTurnResult <- renderText({msg})                                               # report back on the outcome of the turn
  })
  
  #-------------------------------------------#
  # 2.d. OBSERVE: MOUSE CLICKS
  #-------------------------------------------#
  # !!! This function should be drastically improved:
  # -> use boardDef instead of boardState
  # -> check with coördinate is clicked, instead of which unit is nearest
  # -> be robust against multiple units on 1 square
  observe({                                                                                   # tbh: I'm not sure between 'Observe' and 'ObserveEvent'
    if(is.null(input$board_click$x)) return(NULL)                                             # if nothing was clicked yet, do nothing
    sel$row <- boardState$xRes == max(boardState$xRes[boardState$xRes <= input$board_click$x]) & boardState$yRes == max(boardState$yRes[boardState$yRes <= input$board_click$y]) # find coördinate closest to click (towards bottom-left)
    usel <- paste(d$bs[sel$row,c('player','unit')],collapse="-")                              # find which unit corresponds to selected coördinate
    updateActionButton(session,"btn_sel",label=usel)                                          # if unit is found, update the button-label
  })
  
  #-------------------------------------------#
  # 3.a RENDER: GAME BOARD
  #-------------------------------------------#
  output$game_board <- renderPlot({                                                           # this is the actual fun stuff: the game board!
    par(mar=rep(0,4))                                                                         # set figure margins to 0
    plot.new()                                                                                # make sure we start on a new plot
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