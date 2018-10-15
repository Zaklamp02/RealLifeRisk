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
  
  shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';") # by default, sidebar navigation is blocked
  
  #-------------------------------------------#
  # 1. DEFINE USER VARIABLES
  #-------------------------------------------#
  usr <- ifelse(isolate(session$clientData$url_hostname)=="127.0.0.1","master","slave")       # check if client is master (gamehost) or slave (remote player)
  d   <- reactiveValues(bs=boardState,land=land)                                              # reactive values are special, and (should) do downstream cascading if they are updated
  sel <- reactiveValues(row=1)                                                                # i.e. if you update this value, all outputs that use this value get updated too
  t5s <- reactiveTimer(5000)                                                                  # this can be used as a reactive timer of 2seconds
  
  if(usr=="master"){                                                                          # this means game host is viewing (i.e. master)
    shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'visible';")  # if so, enable sidebar navigation
    updateTabItems(session, "sidebar", selected = "Main")                                                 # if so, make main tab active
  }
  
  #-------------------------------------------#
  # 2.a. OBSERVE: MOVE BUTTONS
  #-------------------------------------------#                                               # observeEvent listens for an event, and then executes a code chunk
  observeEvent(input$btn_up,{                                                                 # in this case, listen to 'button up' event
    if(which(d$bs[sel$row,'yPos']==yNames)<length(yNames)){
      d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] + sprRes[2]
      d$bs[sel$row,'yPos'] = yNames[which(d$bs[sel$row,'yPos']==yNames)+1]                    #   as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
  })
  
  observeEvent(input$btn_down,{                                                               # create similar interactions for the other buttons
    if(which(d$bs[sel$row,'yPos']==yNames)>1){
      d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] - sprRes[2]
      d$bs[sel$row,'yPos'] = yNames[which(d$bs[sel$row,'yPos']==yNames)-1]                    #   as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
  })
  
  observeEvent(input$btn_left,{
    if(which(d$bs[sel$row,'xPos']==xNames)>1){
      d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] - sprRes[1]
      d$bs[sel$row,'xPos'] = xNames[which(d$bs[sel$row,'xPos']==xNames)-1]                    #   as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
  })
  
  observeEvent(input$btn_right,{
    if(which(d$bs[sel$row,'xPos']==xNames)<length(xNames)){
      d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] + sprRes[1]
      d$bs[sel$row,'xPos'] = xNames[which(d$bs[sel$row,'xPos']==xNames)+1]                    #   as.character(as.numeric(d$bs[sel$row,'xPos'])-1)
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
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
  lapply(                                                                                                               # start creating observeEvents for player actions
    X = 1:nrow(playerDef),                                                                                              # create one set for each player
    FUN = function(i){
      #-------------------------------------------#
      # 2.c.1. monitor non-submitted actions
      observe({
        updateActionButton(session,paste0(playerDef$Player[i],'a1submit'),                                              # check if player is writing an action
                                  label=paste(parse_and_check(d$bs,input[[paste0(playerDef$Player[i],'a1')]])$action))  # if so, evaluate the action and report back on validity
      })
      observe({updateActionButton(session,paste0(playerDef$Player[i],'a2submit'),
                                  label=paste(parse_and_check(d$bs,input[[paste0(playerDef$Player[i],'a2')]])$action))
      })
      observe({updateActionButton(session,paste0(playerDef$Player[i],'a3submit'),
                                  label=paste(parse_and_check(d$bs,input[[paste0(playerDef$Player[i],'a3')]])$action))
      })
      #-------------------------------------------#
      # 2.c.2. observe remote action submissions
      observe({t5s()                                                                                                    # refresh this part every 5 seconds
        if(!actions[[paste0(playerDef$Player[i],"1")]]==''){                                                            # check if remote/slave player has updated their action
          updateTextInput(session,paste0(playerDef$Player[i],"a1"),NULL,actions[[paste0(playerDef$Player[i],"1")]])     # if so, copy it to our own action input field
        }
      })
      observe({t5s()                                                                                                    # refresh this part every 5 seconds
        if(!actions[[paste0(playerDef$Player[i],"2")]]==''){                                                            # check if remote/slave player has updated their action
          updateTextInput(session,paste0(playerDef$Player[i],"a2"),NULL,actions[[paste0(playerDef$Player[i],"2")]])     # if so, copy it to our own action input field
        }
      })
      observe({t5s()                                                                                                    # refresh this part every 5 seconds
        if(!actions[[paste0(playerDef$Player[i],"3")]]==''){                                                            # check if remote/slave player has updated their action
          updateTextInput(session,paste0(playerDef$Player[i],"a3"),NULL,actions[[paste0(playerDef$Player[i],"3")]])     # if so, copy it to our own action input field
        }
      })
      #-------------------------------------------#
      # 2.c.3. observe actual action submissions
      observeEvent(input[[paste0(playerDef$Player[i],"a1submit")]], {                                                   # set up listener for action 1
        result <- do_action(session,d$bs,d$land,input[[paste0(playerDef$Player[i],"a1")]])                              # if pressed, execute action
        d$bs <- result$tbs                                                                                              # update boardState
        d$land <- result$tland                                                                                          # update land ownership
        updateActionButton(session,paste0(playerDef$Player[i],"a1submit"),label=paste(result$msg$type,result$msg$action,sep=" , "))   # update action button
        shinyjs::toggleState(paste0(playerDef$Player[i],"a1"))                                                          # lock action button
        msglog[[paste0(playerDef$Player[i],"a1")]] <<- result$msg                                                       # log outcome
      })
      observeEvent(input[[paste0(playerDef$Player[i],"a2submit")]], {                                                   # do the same for action 2 ...
        result <- do_action(session,d$bs,d$land,input[[paste0(playerDef$Player[i],"a2")]])
        d$bs <- result$tbs
        d$land <- result$tland
        updateActionButton(session,paste0(playerDef$Player[i],"a2submit"),label=paste(result$msg$type,result$msg$action,sep=" , "))
        shinyjs::toggleState(paste0(playerDef$Player[i],"a2"))
        msglog[[paste0(playerDef$Player[i],"a2")]] <<- result$msg
      })
      observeEvent(input[[paste0(playerDef$Player[i],"a3submit")]], {
        result <- do_action(session,d$bs,d$land,input[[paste0(playerDef$Player[i],"a3")]])
        d$bs <- result$tbs
        d$land <- result$tland
        updateActionButton(session,paste0(playerDef$Player[i],"a3submit"),label=paste(result$msg$type,result$msg$action,sep=" , "))
        shinyjs::toggleState(paste0(playerDef$Player[i],"a3"))
        msglog[[paste0(playerDef$Player[i],"a3")]] <<- result$msg
      })
    }
  )
  
  
  
  #-------------------------------------------#
  # 2.d. OBSERVE: END OF TURN
  #-------------------------------------------#
  observeEvent(input$btn_endTurn,{                                                            # observe if the 'end turn' button is pressed 
    eot    <- end_turn(session,d$bs,d$land,input,output)
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
  output$plot_board <- renderPlot({                                                           # the gameboard consists of 2 plots, the board itself + the current state
    render_board(d$bs,d$land)                                                                 # this avoids having to redraw the base board every time the status changes
  }, height = gridRes[2], width = gridRes[1]) 
  
  output$plot_state <- renderPlot({                                                           # this will draw the actual game state, i.e. units+land
    render_state(d$bs,d$land)
  }, height = gridRes[2], width = gridRes[1], bg = "transparent")                             # make sure the second plot is drawn without a background, so they can overlap

  #-------------------------------------------#
  # 3.b. RENDER: BOARD STATE TABLE
  #-------------------------------------------#
  output$tbl_boardState <- DT::renderDataTable({                                              # render a very basic table
    d$bs                                                                                      # just draw the full current boardState
  })
  
  #-------------------------------------------#
  # 3.c. RENDER: OTHER
  #-------------------------------------------#
  output$p1Score <- renderUI({actionButton("p1Score",paste(playerDef$Label[1],playerDef$Gold[1]),width="100%",style=paste("background-color:",playerDef$Color[1]))}) # show score Player 1
  output$p2Score <- renderUI({actionButton("p2Score",paste(playerDef$Label[2],playerDef$Gold[2]),width="100%",style=paste("background-color:",playerDef$Color[2]))}) # show score Player 2
  if(nrow(playerDef)>=3){output$p3Score <- renderUI({actionButton("p3Score",paste(playerDef$Label[3],playerDef$Gold[3]),width="100%",style=paste("background-color:",playerDef$Color[3]))})} # show score Player 3 (if exists)
  if(nrow(playerDef)>=4){output$p4Score <- renderUI({actionButton("p4Score",paste(playerDef$Label[4],playerDef$Gold[4]),width="100%",style=paste("background-color:",playerDef$Color[4]))})} # show score Player 4 (if exists)

  #-------------------------------------------#
  # 4. OBSERVE: EXPORTS
  #-------------------------------------------#
  output$saveImg <- downloadHandler(
    filename = paste0("Game_",gameID,"_turn_",turn,'.png'),
    content = function(file) {
      png(file,width=gridRes[1],height=gridRes[2])
      plot.new()
      render_board(d$bs,d$land)
      render_state(d$bs,d$land)
      dev.off()
    }
  )
  
  lapply(                                                                                                               # start creating observeEvents for player actions
    X = 1:nrow(playerDef),                                                                                              # create one set for each player
    FUN = function(i){
      observeEvent(input[[paste0("printReport",playerDef$Player[i])]],{
        print_to_printer(gsub("<br/>","\n",game[[paste(turn)]][[paste0('report',playerDef$Player[i])]]),paste("report",playerDef$Player[i],"turn",turn,sep="_"))
      })
    }
  )
  
  #-------------------------------------------#
  # 5. OBSERVE: REMOVE PLAYER LOGIC
  #-------------------------------------------#
  px  <- NULL                                                                                 # variable to identify remote player
  observe({                                                                      
    t5s()                                                                                     # every 5 seconds ...
    if(input$turnSlave < turn & usr=="slave"){                                                # then a new turn has started
      d$bs <<- game[[paste(turn)]]$bs
      d$land <<- game[[paste(turn)]]$land
    }
    updateActionButton(session,"turnSlave",paste("turn:",turn))                               # update global variables
    updateActionButton(session,"yearSlave",paste("year:",year))
    updateActionButton(session,"scoreSlave",paste("score:",playerDef$Gold[playerDef$Player==px$Player]))
    output$reportSlave <- renderUI({HTML(game[[paste(turn)]][[paste0('report',px$Player)]])})
  })

  observeEvent(input$pxpwsubmit,{                                                             # user submits password
    if(input$pxpw %in% playerDef$Password){
      px <<- playerDef[input$pxpw == playerDef$Password,]
      updateTextInput(session,"pxpw",value=paste0("Welkom ",px$Label,"!"))
      output$pxa1 <- renderUI(textInput("pxa1",NULL,NULL))
      output$pxa2 <- renderUI(textInput("pxa2",NULL,NULL))
      output$pxa3 <- renderUI(textInput("pxa3",NULL,NULL))
      output$pxa1submit <- renderUI(actionButton("pxa1submit","",icon=icon("cog"),width="100%"))
      output$pxa2submit <- renderUI(actionButton("pxa2submit","",icon=icon("cog"),width="100%"))
      output$pxa3submit <- renderUI(actionButton("pxa3submit","",icon=icon("cog"),width="100%"))
      shinyjs::show(id = "pxgameoverviewdiv")
      shinyjs::show(id = "pxactioninputdiv")
      shinyjs::show(id = "pxreportdiv")
      shinyjs::hide(id = "pxpw")
      shinyjs::hide(id = "pxpwsubmit")
    }
  })
  
  lapply(                                                                                      # start creating observeEvents for player actions
    X = 1:3,                                                                                   # create one set for each player
    FUN = function(i){
      observe({
        updateActionButton(session,paste0('pxa',i,'submit'),label=paste(parse_and_check(d$bs,paste0(px$Player,'.',input[[paste0('pxa',i)]]))$action))
        observeEvent(input[[paste0('pxa',i,'submit')]],{
          updateActionButton(session,paste0("pxa",i,"submit"),"Actie doorgegeven")
          shinyjs::disable(paste0('pxa',i))
          shinyjs::disable(paste0('pxa',i,'submit'))
          actions[[paste0(px$Player,i)]] <<- paste0(px$Player,'.',input[[paste0("pxa",i)]])
        })
      })
    }
  )
  
  #-------------------------------------------#
  # X. EXIT
  #-------------------------------------------#
  session$onSessionEnded(function() {
    stopApp()
  })
  
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