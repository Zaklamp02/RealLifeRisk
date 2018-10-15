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
  dashboardSidebar(
    collapsed=TRUE,                                                                                # create a sidebar, but auto-hide
    sidebarMenu(                                                                                   # create a menu to navigate between tabs
      id="sidebar",                
      tags$head(tags$style(".inactiveLink {pointer-events: none;cursor: default;}")),              # by default, disable sidebar
      menuItem("Player",tabName="Player"),                                                         # create buttons for each tab
      menuItem("Main",tabName="Main"),                                                             # order is important!
      menuItem("Settings",tabName="Settings")
    )
  ),
  
  #-----------------------------------------#
  # 3. CREATE THE MAIN BODY
  #-----------------------------------------#
  dashboardBody(                                                                                   # start building the main body
    useShinyjs(),
    tabItems(                                                                                      # separate contents into several tabs
      
      #-------------------------------------#
      # 3.a. CREATE MAIN TAB
      #-------------------------------------#
      tabItem(tabName="Main",                                                                      # main tab containing map, controls, etc.
              #-----------------------------#
              # 3.a.1. GAME BOARD
              column(9,                                                                            # columns can have a width of 1-12, which is relative to the 'box' the column exists in
                     fluidRow(
                       div(id="container",                                                         # create custom container to hold 2 plots on top of eachother
                           style="position:relative;",                                             # no idea if this is necessary
                           div(plotOutput("plot_board", click = NULL), style="position:absolute; top:0; left:0;"), # create plot 1
                           div(plotOutput("plot_state", click = "board_click"), style="position:absolute; top:0; left:0;") # create plot 2
                       ), style = paste0("height: ",gridRes[2]+20,"px;")                           # specify window height to help layout
                     ),
                     
                     #------------------------------#
                     # 3.a.4 ACTION INPUT CONTROLS
                     fluidRow(
                       lapply(1:nrow(playerDef), function(i) {
                         column(12/nrow(playerDef),
                                box(title = playerDef$Label[i], status = "primary", width="100%",
                                    fluidRow(
                                      column(6,textInput(paste0(playerDef$Player[i],"a1"),NULL,paste0(playerDef$Player[i],"."))),
                                      actionButton(paste0(playerDef$Player[i],"a1submit"),"",icon=icon("cog"))
                                    ),
                                    fluidRow(
                                      column(6,textInput(paste0(playerDef$Player[i],"a2"),NULL,paste0(playerDef$Player[i],"."))),
                                      actionButton(paste0(playerDef$Player[i],"a2submit"),"",icon=icon("cog"))
                                    ),
                                    fluidRow(
                                      column(6,textInput(paste0(playerDef$Player[i],"a3"),NULL,paste0(playerDef$Player[i],"."))),
                                      actionButton(paste0(playerDef$Player[i],"a3submit"),"",icon=icon("cog"))
                                    )
                                )
                         )
                       })
                     )
              ),
              
              #---------------------------#
              # 3.a.2 GAME OVERVIEW
              column(3,                                                                          # note that we are still in the same row, but we are defining a new column (of width=3, which together with the previous one makes exactly 12)
                     fluidRow(
                       actionButton("btn_endTurn", label = "Einde Beurt", style=paste("background-color:darkgrey"),width="98%"),
                       actionButton("backward","<<",width="19%"),
                       actionButton("turn",paste('beurt',turn),width="29%"),
                       actionButton("year",paste('jaar',year),width="29%"),
                       actionButton("forward",">>",width="19%")
                     ),

                     #---------------------------#
                     # 3.a.3 MOVE BUTTONS
                     fluidRow(                                                                   # within this column we make a new row; you can repeat this indefinitely
                       column(4),                                                                # now we create a new column that acts just as whitespace
                       column(4,actionButton("btn_up", label = "Noord",width='100%'))             # ... and another column that holds an 'up' button
                     ),
                     fluidRow(                                                                   # ... add a bunch more rows, columns and buttons
                       column(4,actionButton("btn_left",  label = "West",  width='100%')),
                       column(4,actionButton("btn_sel",   label = "NA-NA", width='100%')),
                       column(4,actionButton("btn_right", label = "Oost",  width='100%'))
                     ),
                     fluidRow(
                       column(4),
                       column(4,actionButton("btn_down", label = "Zuid",width='100%'))
                     ),

                     fluidRow(
                       do.call(tabBox, c(id='tab',width='100%',height=gridRes[2]+30,lapply(0:nrow(playerDef), function(i) {
                         if(i==0){tabPanel(title="All",uiOutput("battleResult"))} 
                         else {tabPanel(title=playerDef$Label[i], 
                                        fluidRow(uiOutput(paste0('report',playerDef$Player[i]))), 
                                        fluidRow(actionButton(paste0("printReport",playerDef$Player[i]),"Print Report", width="100%")))}
                       })))
                     )
              )
      ),
      
      #-----------------------------------------#
      # 3.b CREATE TAB FOR REMOTE PLAYER
      #-----------------------------------------#
      tabItem(
        tabName="Player",
        column(6,
               fluidRow(
                 column(6,textInput("pxpw",NULL,NULL)),
                 column(6,actionButton("pxpwsubmit","Login",icon=icon("cog")))
               ),
               fluidRow(
                 hidden(
                   div(id="pxgameoverviewdiv",
                       box(id="pxgameoverviewbox", width="100%",
                           actionButton("turnSlave",paste('beurt',turn)),
                           actionButton("yearSlave",paste('jaar',year)),
                           actionButton("scoreSlave",paste('goud',year))
                       )
                   )
                 )
               ),
               hidden(
                 div(id="pxactioninputdiv",
                     box(id="pxactioninputbox",title="Action Inputs", status = "primary", width="100%",
                         fluidRow(
                           column(6,uiOutput("pxa1")),
                           column(6,uiOutput("pxa1submit"))
                         ),
                         fluidRow(
                           column(6,uiOutput("pxa2")),
                           column(6,uiOutput("pxa2submit"))                     
                         ),
                         fluidRow(
                           column(6,uiOutput("pxa3")),
                           column(6,uiOutput("pxa3submit"))                     
                         )
                     ))
               )
        ),
        column(6,
               hidden(
                 div(id="pxreportdiv",
                     box(id="pxreportbox",title="Reports",status="primary", width="100%",
                         uiOutput('reportSlave')
                     ))
               )
        )
      ),
      
      #-----------------------------------------#
      # 3.c CREATE SETTINGS TAB
      #-----------------------------------------#
      tabItem(
        tabName="Settings",
        
        column(4,
               fluidRow(
                 box(title="Game Settings", status = "primary", width="100%",
                     textInput("ip_address","IP Adres",paste0(ip,":4414")),
                     textInput("turn_bonus","Bonus per turn",turnBonus),
                     textInput("year_bonus","Bonus per jaar",yearBonus),
                     textInput("year_cycle","Beurten per jaar",yearCycle),
                     textInput("start_year","Start jaar",year),
                     textInput("start_turn","Start beurt",turn)
                 ),
                 box(title="Settings", status = "primary", width="100%",
                     column(6,textInput("scrn_res_x","Scherm Resolutie (x)",scrnRes[1])),
                     column(6,textInput("scrn_res_y","Scherm Resolutie (y)",scrnRes[2])),
                     column(6,textInput("grd_size_x","Speelbord dim (x)",gridRes[1])),
                     column(6,textInput("grd_size,y","Speelbord dim (y)",gridRes[2]))
                 )
               )
        ),
        column(8,
               column(12,
                      fluidRow(
                        DT::dataTableOutput("tbl_boardState")
                      )
               ),
               column(12,
                      
                      column(6,
                             fluidRow(
                               box(title="Save/Load", status = "primary", width="100%",
                                   fluidRow(style='padding:10px;',
                                            actionButton("save","Save Game",icon('save')),
                                            actionButton("save","Save CSV",icon('table')),
                                            downloadButton("saveImg","Save PNG", icon('image')),
                                            actionButton("saveGif","Save Gif",icon('film'))
                                   ),
                                   fluidRow(style='padding:10px;',
                                            fileInput("file1", NULL, multiple = FALSE, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                   )
                               )
                             )
                      ),
                      column(6,
                             fluidRow(
                               box(title="Audio", status = "primary", width="100%",
                                   tags$audio(src = "Nijmegen.mp3", type = "audio/mp3", autoplay = 1, controls = NA)
                               )
                             )
                      )
               )
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
#    tags$head(
#      tags$style(HTML(".ra2_background_style{
#                      background-repeat: no-repeat; background-size: cover;
#                      background-image: url('menu_background.png'); 
#                      }"))),
#    class="ra2_background_style",

