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
  t5s <- reactiveTimer(5000) 
  
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
      d$bs[sel$row,'yPos'] = yNames[which(d$bs[sel$row,'yPos']==yNames)+1]              
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
  })
  
  observeEvent(input$btn_down,{                                                               # create similar interactions for the other buttons
    if(which(d$bs[sel$row,'yPos']==yNames)>1){
      d$bs[sel$row,'yRes'] = d$bs[sel$row,'yRes'] - sprRes[2]
      d$bs[sel$row,'yPos'] = yNames[which(d$bs[sel$row,'yPos']==yNames)-1]                
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
  })
  
  observeEvent(input$btn_left,{
    if(which(d$bs[sel$row,'xPos']==xNames)>1){
      d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] - sprRes[1]
      d$bs[sel$row,'xPos'] = xNames[which(d$bs[sel$row,'xPos']==xNames)-1]           
      d$land <- updateMapOwner(d$land,d$bs$xPos[sel$row],d$bs$yPos[sel$row],d$bs$player[sel$row])
    }
  })
  
  observeEvent(input$btn_right,{
    if(which(d$bs[sel$row,'xPos']==xNames)<length(xNames)){
      d$bs[sel$row,'xRes'] = d$bs[sel$row,'xRes'] + sprRes[1]
      d$bs[sel$row,'xPos'] = xNames[which(d$bs[sel$row,'xPos']==xNames)+1]              
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
  observe({                                                                                    # this will automatically update if ANYTHING inside {} changes value
    updateActionButton(                                                                        # if so, update the label in the submission button
      session,                                                                                 # required parameter
      'hostactionsubmit',                                                                      # ID of the submission button
      label=paste(parse_and_check(d$bs,input$hostaction)$action))                              # Value of the submit button. Automatically refreshes as input$hosaction changes, because of observe{}
  })
  
  lapply(                                                                                      # start creating observeEvents for player actions
    X = 1:nrow(playerDef),                                                                     # create one set for each player
    FUN = function(i){
      #-------------------------------------------#
      # 2.c.1. monitor non-submitted actions
      observe({
        updateActionButton(                                                                    # exactly the same logic as for 'hostactionsubmit'
          session,paste0(playerDef$Player[i],'a1submit'),                                      # check if player is writing an action
          label=paste(parse_and_check(d$bs,input[[paste0(playerDef$Player[i],'a1')]])$action)  # if so, evaluate the action and report back on validity
        )
      })
      observe({
        updateActionButton(
          session,paste0(playerDef$Player[i],'a2submit'),
          label=paste(parse_and_check(d$bs,input[[paste0(playerDef$Player[i],'a2')]])$action)
        )
      })
      observe({
        updateActionButton(
          session,paste0(playerDef$Player[i],'a3submit'),
          label=paste(parse_and_check(d$bs,input[[paste0(playerDef$Player[i],'a3')]])$action)
        )
      })
      
      #-------------------------------------------#
      # 2.c.2. observe remote action submissions
      observe({t5s()                                                                                                    # refresh this part every 5 seconds
        if(!actions[[paste0(playerDef$Player[i],"1")]]==''){                                                            # check if remote/slave player has updated their action
          updateTextInput(session,paste0(playerDef$Player[i],"a1"),NULL,actions[[paste0(playerDef$Player[i],"1")]])     # if so, copy it to our own action input field
          actions[[paste0(playerDef$Player[i],"1")]] <- ''                                                              # and LOCALLY remove the action so that we could edit it without it being continuously overwritten
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
        d$bs   <- result$tbs                                                                                            # update boardState
        d$land <- result$tland                                                                                          # update land ownership
        updateActionButton(session,paste0(playerDef$Player[i],"a1submit"),label=paste(result$msg$type,result$msg$action,sep=" , ")) # update action button
        shinyjs::disable(paste0(playerDef$Player[i],"a1"))                                                              # lock action button
        msglog[[paste0(playerDef$Player[i],"a1")]] <<- result$msg                                                       # log outcome
      })
      
      observeEvent(input[[paste0(playerDef$Player[i],"a2submit")]], {                                                   # do the same for action 2 ...
        result <- do_action(session,d$bs,d$land,input[[paste0(playerDef$Player[i],"a2")]])
        d$bs   <- result$tbs
        d$land <- result$tland
        updateActionButton(session,paste0(playerDef$Player[i],"a2submit"),label=paste(result$msg$type,result$msg$action,sep=" , "))
        shinyjs::disable(paste0(playerDef$Player[i],"a2"))
        msglog[[paste0(playerDef$Player[i],"a2")]] <<- result$msg
      })
      
      observeEvent(input[[paste0(playerDef$Player[i],"a3submit")]], {
        result <- do_action(session,d$bs,d$land,input[[paste0(playerDef$Player[i],"a3")]])
        d$bs   <- result$tbs
        d$land <- result$tland
        updateActionButton(session,paste0(playerDef$Player[i],"a3submit"),label=paste(result$msg$type,result$msg$action,sep=" , "))
        shinyjs::disable(paste0(playerDef$Player[i],"a3"))
        msglog[[paste0(playerDef$Player[i],"a3")]] <<- result$msg
      })
      
      #-------------------------------------------#
      # 2.c.3. observe print buttons
      observeEvent(input[[paste0("printReport",playerDef$Player[i])]],{
        print_to_printer(gsub("<br/>","\n",game[[paste(turn)]][[paste0('report',playerDef$Player[i])]]),paste("report",playerDef$Player[i],"turn",turn,sep="_"))
      })
    }
  )
  
  observe({
    updateActionButton(session,"hostspecialsubmit",parse_special(input$hostspecial)$msg)
  })
  observeEvent(input$hostspecialsubmit,{
    msg <- add_gold(input$hostspecial)
    updateTextInput(session,"hostspecial",NULL,"")
  })
  
  observeEvent(input$hostmsgsubmit,{
    msg <- parse_special_msg(input$hostmsg)
    if(msg$p=="ALL"){
      for(p in unique(playerDef$Player)){
        msglog[[paste0(p)]]$special <<- paste0(msglog[[p]]$special,b,msg$msg)
      }
    } else {
      msglog[[paste0(msg$p)]]$special <<- paste0(msglog[[msg$p]]$special,b,msg$msg)
    }
    updateTextInput(session,"hostmsg",NULL,"")
  })
  
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
  observeEvent(input$board_click,{
    output$board_hover_box <- NULL                                                            # this will remove panels created by earlier double-clicks    if(is.null(input$board_click$x)) return(NULL)                                             # if nothing was clicked yet, do nothing
    
    xTmp <- input$board_click$x - input$board_click$x %% sprRes[1]                            # find nearest x coördinate
    yTmp <- input$board_click$y - input$board_click$y %% sprRes[2]                            # find nearest y coördinate
    nr <- which(d$bs$xRes == xTmp & d$bs$yRes == yTmp)                                        # find rows matching clicked coördinate

    if(length(nr)==1) {                                                                       # exactly 1 units exists at location
      sel$row <- nr                                                                           # select only the sprite on top (i.e. the unit that you see)
      updateActionButton(session,"btn_sel",label=paste(d$bs[sel$row,c('player','unit')],collapse="-")) # if unit is found, update the button-label
    } else if (length(nr)>1){                                                                 # then multiple units exist
      nc  <- ceiling(sqrt(length(nr)))                                                        # check nr of columns
      adj <- sprRes / nc                                                                      # calculate sprite size
      x   <- ceiling((input$board_click$x - xTmp) / adj[1])                                   # check unit closest to click (X)
      y   <- nc-ceiling((input$board_click$y - yTmp) / adj[2])+1                              # check unit closest to click (Y)
      sel$row <- nr[y*nc-nc + x]                                                              # combine X and Y and log selected unit
      updateActionButton(session,"btn_sel",label=paste(d$bs[sel$row,c('player','unit')],collapse="-")) # if unit is found, update the button-label
    }
  })
  
  observeEvent(input$board_dblclick,{
    xTmp <- input$board_dblclick$x - input$board_dblclick$x %% sprRes[1]                      # find nearest x coördinate
    yTmp <- input$board_dblclick$y - input$board_dblclick$y %% sprRes[2]                      # find nearest y coördinate
    tile <- which(d$bs$xRes==xTmp & d$bs$yRes==yTmp)
    
    if(length(tile)>=1){
      output$board_hover_box <- renderUI({
        loc    <- input$board_dblclick
        left_pct <- (xTmp + sprRes[1] - loc$domain$left)/(loc$domain$right - loc$domain$left) # find relative position (%) to left of frame
        top_pct  <- (loc$domain$top - yTmp - sprRes[2])/(loc$domain$top - loc$domain$bottom)  # find relative position (%) to top of frame
        left_px  <- loc$range$left + left_pct * (loc$range$right - loc$range$left)            # find absolute position (px) to left of frame
        top_px   <- loc$range$top + top_pct * (loc$range$bottom - loc$range$top)              # find absolute position (px) to top of frame
        
        wellPanel(                                                                            # create a wellpanel
          style = paste0("position:absolute; background-color: rgba(245, 245, 245, 0.85); padding:5px;", "left:", left_px, "px; top:", top_px, "px; "),
          DT::renderDataTable({
            hoverdata <- d$bs[tile,c('unit','quantity','player')]
            hoverdata$unit <- paste0('<img src="',paste0(relable(hoverdata$unit,unitDef[,c('Unit','Sprite')])),'" height="',sprRes[2]/2,'"></img>')
            DT::datatable(hoverdata,
                          escape=F,                               # makes sure that images are treated as html (not strings)
                          rownames = FALSE,                       # hide rownames
                          colnames = c("", "", ""),               # 'hide' colnames
                          selection = 'none',                     # prevent row selection
                          class='compact',                        # reduces whitespace
                          options=list(dom='t',ordering=F, bSort = FALSE) # removes a lot of the header
            )
          })                                                                                  # TXT ONLY: p(HTML(paste(d$bs[tile,'quantity'],'x',d$bs[tile,'unit'],d$bs[tile,'player'], collapse=b)))
        )
      })
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
  observe({updateTextInput(session,"ip_address",value=paste0(ip,":",session$clientData$url_port))})
  
  lapply(                                                                                      # this will create player reports on the START of game
    X = 1:nrow(playerDef),                                                                     # create one set for each player
    FUN = function(i){
      output[[paste0('report',playerDef$Player[i])]] <- renderUI({HTML(isolate(generate_report(session,d$bs,d$land,playerDef$Player[i],input,output)))}) # generate report, using isolate to ensure this only runs once
      outputOptions(output,paste0('report',playerDef$Player[i]),suspendWhenHidden=F)           # this ensures updates are prioritized, even if output is on a hidden tab
    }
  )
  
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
  
  #-------------------------------------------#
  # 5. PLAYER X
  #-------------------------------------------#
  px <- NULL                                                                                   # variable to identify remote player
  
  #-------------------------------------------#
  # 5.a. observe password input
  observeEvent(input$pxpwsubmit,{                                                              # user submits password
    if(input$pxpw %in% playerDef$Password){                                                    # if client is not logged in, but password is correct:
      px <<- playerDef[input$pxpw == playerDef$Password,]                                      # log current player
      output$pxa1 <- renderUI(textInput("pxa1",NULL,NULL))                                     # render action input for action 1
      output$pxa2 <- renderUI(textInput("pxa2",NULL,NULL))                                     # repeat for action 2
      output$pxa3 <- renderUI(textInput("pxa3",NULL,NULL))                                     # repeat for action 3
      output$pxa1submit <- renderUI(actionButton("pxa1submit","",icon=icon("cog"),width="100%")) # render action submit for action 1
      output$pxa2submit <- renderUI(actionButton("pxa2submit","",icon=icon("cog"),width="100%")) # repeat for action 2
      output$pxa3submit <- renderUI(actionButton("pxa3submit","",icon=icon("cog"),width="100%")) # repeat for action 3
      output$reportSlave <- renderUI({HTML(game[[paste(turn)]][[paste0('report',px$Player)]])}) # fetch report
      shinyjs::show(id = "pxgameoverviewdiv")                                                  # this is hidden if not logged in
      shinyjs::show(id = "pxactioninputdiv")                                                   # this is hidden if not logged in
      shinyjs::show(id = "pxreportdiv")                                                        # this is hidden if not logged in
      shinyjs::hide(id = "pxpw")                                                               # remove login input
      shinyjs::hide(id = "pxpwsubmit")                                                         # remove login submit button
    }
  })
  
  #-------------------------------------------#
  # 5.b. observe action input
  lapply(                                                                                      # start creating observeEvents for player actions
    X = 1:3,                                                                                   # create one set for each player
    FUN = function(i){
      observe({
        updateActionButton(session,paste0('pxa',i,'submit'),label=paste(parse_and_check(d$bs,paste0(px$Player,'.',input[[paste0('pxa',i)]]))$action))
      })
      observeEvent(input[[paste0('pxa',i,'submit')]],{                                         # if player submits an action:
        shinyjs::disable(paste0('pxa',i))                                                      # disable changing/editing the action
        shinyjs::disable(paste0('pxa',i,'submit'))                                             # disable the submit button itself
        updateActionButton(session,paste0("pxa",i,"submit"),"Actie doorgegeven")               # let user know action was submitted
        updateTextInput(session,paste0("pxa",i),NULL,input[[paste0("pxa",i)]])                 # this just replaces the text removed by 'disable'
        actions[[paste0(px$Player,i)]] <<- paste0(px$Player,'.',input[[paste0("pxa",i)]])      # log the action in the global action variable
      })
    }
  )
  
  #-------------------------------------------#
  # 5.d. observe new turn
  observe({
    t5s()
    if(usr=='slave' & (newTurn==T | input$turnSlave < turn)){                                                                           # then game host has started a new turn
      d$bs <<- game[[paste(turn)]]$bs                                                         # fetch boardstate
      d$land <<- game[[paste(turn)]]$land                                                     # fetch landstate
      updateActionButton(session,"turnSlave",paste("turn:",turn))                             # update buttons
      updateActionButton(session,"yearSlave",paste("year:",year))
      output$reportSlave <- renderUI({HTML(game[[paste(turn)]][[paste0('report',px$Player)]])}) # fetch report
      for(i in 1:3){                                                                          
        updateTextInput(session,paste0("pxa",i),value="")                                     # clear action input
        updateActionButton(session,paste0("pxa",i,"submit"),"")                               # clear action submission
        shinyjs::enable(paste0('pxa',i))                                                      # release action input
        shinyjs::enable(paste0('pxa',i,'submit'))                                             # release action submission
      }
      newTurn <- F                                                                            # put newTurn to False (for client only!)
    }
  })
  
  #-------------------------------------------#
  # X. EXIT
  #-------------------------------------------#
  session$onSessionEnded(function() {                                                         # define what we should do when we close a browser
    if(usr=='master'){                                                                        # check if client is host
      stopApp()                                                                               # if so, kill game process
    }
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