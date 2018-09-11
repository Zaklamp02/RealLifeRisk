###############################################################################
# UI.R
###############################################################################
#
#   MODUS OPERANDI
#   This function creates the 'UI' component for the Shiny App. The UI
#   contains the basic outline of the app. I.e. it defines which elements
#   exist in the app (sidebar, tabs, buttons, plots, tables).
#   Once they are defined in UI.R, SERVER.R will define the actual behavior
#   of the app (i.e. listeners, actions, processing, drawing, etc.)
#
###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################

#-------------------------------------------#
# 0. START CREATING A PAGE
#-------------------------------------------#
ui <- dashboardPage(
  
  #-----------------------------------------#
  # 1. CREATE A HEADER
  #-----------------------------------------#
  dashboardHeader(                                                                                 # for now; use an empty header
    titleWidth=0
  ),
  
  #-----------------------------------------#
  # 2. CREATE A SIDEBAR
  #-----------------------------------------#
  dashboardSidebar(collapsed=TRUE,                                                                 # create a sidebar, but auto-hide
                   sidebarMenu(                                                                    # create a menu to navigate between tabs
                     menuItem("Main",tabName="Main"),                                              # create buttons for each tab
                     menuItem("P1",tabName="P1"),
                     menuItem("Settings",tabName="Settings")
                   )
  ),
  
  #-----------------------------------------#
  # 3. CREATE THE MAIN BODY
  #-----------------------------------------#
  dashboardBody(                                                                                   # start building the main body
    tabItems(                                                                                      # separate contents into several tabs
      
      #-------------------------------------#
      # 3.a. CREATE MAIN TAB
      #-------------------------------------#
      tabItem(tabName="Main",                                                                      # main tab containing map, controls, etc.
              
              #-----------------------------#
              # 3.a.1. GAME BOARD
              fluidRow(                                                                            # lay-out is controlled by defining rows ('fluidRow') and columns ('column')
                column(9,                                                                          # columns can have a width of 1-12, which is relative to the 'box' the column exists in
                       plotOutput("game_board", click="board_click", height=gridRes[2]+20)         # within the row/column we just defined, create an object that can hold a plot (which will later be created by server.r)
                ),
                
                #---------------------------#
                # 3.a.2 SCORE OVERVIEW
                column(3,                                                                          # note that we are still in the same row, but we are defining a new column (of width=3, which together with the previous one makes exactly 12)
                       fluidRow(
                         column(3,uiOutput("p1Score"), style='padding:0px;'),                      # these will show the current scores
                         column(3,uiOutput("p2Score"), style='padding:0px;'),
                         column(3,uiOutput("p3Score"), style='padding:0px;'),
                         column(3,uiOutput("p4Score"), style='padding:0px;')
                       ),

                       #---------------------------#
                       # 3.a.3 MOVE BUTTONS
                       fluidRow(                                                                   # within this column we make a new row; you can repeat this indefinitely
                         column(4),                                                                # now we create a new column that acts just as whitespace
                         column(4,actionButton("btn_up", label = " up ",width='100%'))             # ... and another column that holds an 'up' button
                       ),
                       fluidRow(                                                                   # ... add a bunch more rows, columns and buttons
                         column(4,actionButton("btn_left", label = "left",width='100%')),
                         column(4,actionButton("btn_sel", label = "NA-NA",width='100%')),
                         column(4,actionButton("btn_right", label = "right",width='100%'))
                       ),
                       fluidRow(
                         column(4),
                         column(4,actionButton("btn_down", label = "down",width='100%'))
                       ),
                       fluidRow(
                         column(4),
                         column(4,actionButton("btn_endTurn", label = "End Turn", width='100%'))
                       ),
                       fluidRow(
                         box(title="End of Turn Results", status = "primary", width="100%", height=gridRes[2]-170,
                             verbatimTextOutput("endOfTurnResult"),
                             uiOutput("battleResult")
                         )
                       )
                )
              ),
              
              #------------------------------#
              # 3.a.4 ACTION INPUT CONTROLS
              fluidRow(
                column(3,
                       box(title = "Player 1", status = "primary", width="100%",
                           fluidRow(
                             column(8,textInput("p1a1",NULL,"P1.")),
                             actionButton("p1a1submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p1a2",NULL,"P1.")),
                             actionButton("p1a2submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p1a3",NULL,"P1.")),
                             actionButton("p1a3submit","",icon=icon("cog"))
                           )
                       )
                ),
                column(3,
                       box(title = "Player 2", status = "primary", width="100%",
                           fluidRow(
                             column(8,textInput("p2a1",NULL,"P2.")),
                             actionButton("p2a1submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p2a2",NULL,"P2.")),
                             actionButton("p2a2submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p2a3",NULL,"P2.")),
                             actionButton("p2a3submit","",icon=icon("cog"))
                           )
                       )
                ),
                column(3,
                       box(title = "Player 3", status = "primary", width="100%",
                           fluidRow(
                             column(8,textInput("p3a1",NULL,"P3.")),
                             actionButton("p3a1submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p3a2",NULL,"P3.")),
                             actionButton("p3a2submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p3a3",NULL,"P3.")),
                             actionButton("p3a3submit","",icon=icon("cog"))
                           )
                       )
                ),
                column(3,
                       box(title = "Player 4", status = "primary", width="100%",
                           fluidRow(
                             column(8,textInput("p4a1",NULL,"P4.")),
                             actionButton("p4a1submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p4a2",NULL,"P4.")),
                             actionButton("p4a2submit","",icon=icon("cog"))
                           ),
                           fluidRow(
                             column(8,textInput("p4a3",NULL,"P4.")),
                             actionButton("p4a3submit","",icon=icon("cog"))
                           )
                       )
                )
              )
                
      ),
      
      #-----------------------------------------#
      # 3.b CREATE TAB FOR PLAYER 1
      #-----------------------------------------#
      tabItem(tabName="P1",
              
              #------------------------------#
              # 3.b.3 ACTION INPUT CONTROLS
              fluidRow(
                column(2, selectInput("p1a1p1", NULL, selected = 1, choices = pNames)),
                column(2, selectInput("p1a1p2", NULL, selected = 1, choices = uNames)),
                column(1, selectInput("p1a1p3", NULL, selected = 1, choices = yNames)),
                column(1, selectInput("p1a1p4", NULL, selected = 1, choices = xNames)),
                column(1, selectInput("p1a1p5", NULL, selected = 1, choices = yNames)),
                column(1, selectInput("p1a1p6", NULL, selected = 1, choices = xNames)),
                column(2, actionButton("p1act_1_submit", label = "action 1")),
                column(2, textOutput("p1act_1_result"))
              ),
              fluidRow(
                column(2, selectInput("p1a2p1", NULL, selected = 1, choices = pNames)),
                column(2, selectInput("p1a2p2", NULL, selected = 1, choices = uNames)),
                column(1, selectInput("p1a2p3", NULL, selected = 1, choices = yNames)),
                column(1, selectInput("p1a2p4", NULL, selected = 1, choices = xNames)),
                column(1, selectInput("p1a2p5", NULL, selected = 1, choices = yNames)),
                column(1, selectInput("p1a2p6", NULL, selected = 1, choices = xNames)),
                column(2, actionButton("p1act_2_submit", label = "action 2")),
                column(2, textOutput("p1act_2_result"))
              ),
              fluidRow(
                column(2, selectInput("p1a3p1", NULL, selected = 1, choices = pNames)),
                column(2, selectInput("p1a3p2", NULL, selected = 1, choices = uNames)),
                column(1, selectInput("p1a3p3", NULL, selected = 1, choices = yNames)),
                column(1, selectInput("p1a3p4", NULL, selected = 1, choices = xNames)),
                column(1, selectInput("p1a3p5", NULL, selected = 1, choices = yNames)),
                column(1, selectInput("p1a3p6", NULL, selected = 1, choices = xNames)),
                column(2, actionButton("p1act_3_submit", label = "action 3")),
                column(2, textOutput("p1act_3_result"))
              )
      ),
      
      #-----------------------------------------#
      # 3.c CREATE SETTINGS TAB
      #-----------------------------------------#
      tabItem(tabName="Settings",
              
              fluidRow(
                column(2, textOutput("set_txt1"))
              ),
              fluidRow(
                DT::dataTableOutput("tbl_boardState")
              ),
              fluidRow(
                tags$audio(src = "Nijmegen.mp3", type = "audio/mp3", autoplay = 1, controls = NA)
              )
      )
    )
  )
)

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