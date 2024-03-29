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
  skin="black",                                                                                    # nice basic outline for color scheme
  
  #-----------------------------------------#
  # 1. CREATE A HEADER
  #-----------------------------------------#
  dashboardHeader(                                                                                 # for now; use an empty header
    titleWidth=0,                                                                                  # effectively places sidebar toggle at the left
    tags$li(
      class = "dropdown",
      tags$style(".main-header {max-height: 20px}"),                                               # reduce header height
      tags$style(".main-header .logo {height: 20px;}"),                                            # ... also rescale logo area
      tags$style(".sidebar-toggle {height: 20px; padding-top: 1px !important;}"),                  # ... also rescale the sidebar toggle
      tags$style(".navbar {min-height:20px !important}")                                           # ... also rescale navbar area  
    ) 
  ),
  
  #-----------------------------------------#
  # 2. CREATE A SIDEBAR
  #-----------------------------------------#
  dashboardSidebar(
    collapsed=TRUE,                                                                                # create a sidebar, but auto-hide
    sidebarMenu(                                                                                   # create a menu to navigate between tabs
      id="sidebar",                                                                                # gives the sidebar an ID (e.g. to hide/show it later)
      tags$head(tags$style(".inactiveLink {pointer-events: none;cursor: default;}")),              # by default, disable sidebar
      menuItem("Player",tabName="Player"),                                                         # create buttons for each tab
      menuItem("Main",tabName="Main"),                                                             # ... order is important!
      menuItem("CheatSheet",tabName="CheatSheet"),
      menuItem("Laad game",tabName="Laadgame"),
      menuItem("Credits",tabName="Credits")
    )
  ),
  
  #-----------------------------------------#
  # 3. CREATE THE MAIN BODY
  #-----------------------------------------#
  dashboardBody(                                                                                   # start building the main body
    useShinyjs(),                                                                                  # this adds a lot of useful javascript options
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),              # this will add all our custom styling options
    tags$style(type="text/css", ".shiny-html-output { padding-left:20px;} .nav-tabs-custom {margin-bottom: 0px;}"), # this helps with the layout of html text in tabboxes
    tags$head(tags$style(HTML('.wrapper {height: auto !important; position:relative; overflow-x:hidden; overflow-y:hidden}'))),
    tags$head(tags$style(".creditsbg {background:url('menu_main.png'); background-repeat: no-repeat; background-size: cover; opacity: 0.8;}")),
    tags$style(HTML(paste0(".nav-tabs-custom > .nav-tabs > li > a[data-value='Leiding'] {background-color:grey;}",
                           ".nav-tabs-custom > .nav-tabs > li > a[data-value='",playerDef$Label[1],"'] {background-color:",playerDef$hexcol[1],";}",
                           ".nav-tabs-custom > .nav-tabs > li > a[data-value='",playerDef$Label[2],"'] {background-color:",playerDef$hexcol[2],";}",
                           ".nav-tabs-custom > .nav-tabs > li > a[data-value='",playerDef$Label[3],"'] {background-color:",playerDef$hexcol[3],";}",
                           ".nav-tabs-custom > .nav-tabs > li > a[data-value='",playerDef$Label[4],"'] {background-color:",playerDef$hexcol[4],";}"
                          ))),
    
    tabItems(                                                                                      # separate contents into several tabs
      
      #-------------------------------------#
      # 3.a. CREATE MAIN TAB
      #-------------------------------------#
      tabItem(tabName="Main",                                                                      # main tab containing map, controls, etc.
              
              #-----------------------------#
              # 3.a.1. GAME BOARD
              column(8,                                                                            # columns can have a width of 1-12, which is relative to the 'box' the column exists in
                     fluidRow(
                       div(id="container",                                                         # create custom container to hold 2 plots on top of eachother
                           style="position:relative;",                                             # no idea if this is necessary
                           div(plotOutput("plot_board", click = NULL), style="position:absolute; top:0; left:0;"), # create plot 1
                           div(plotOutput("plot_state", click = "board_click", dblclick = "board_dblclick", hover="board_hover"),
                               uiOutput("board_hover_box"), style="position:absolute; top:0; left:0;") # create plot 2
                       ), style = paste0("height: ",gridRes[2]+20,"px;")                           # specify window height to help layout
                     )
              ),
              
              #---------------------------#
              # 3.a.2 GAME OVERVIEW
              column(4,                                                                            # note that we are still in the same row, but we are defining a new column (of width=3, which together with the previous one makes exactly 12)
                     fluidRow(
                       actionButton("btn_endTurn", label = "Einde Beurt", style=paste("background-color:darkgrey"),width="98%"),
                       actionButton("backward","<<",width="19%"),
                       actionButton("turn",paste('beurt',turn),width="29%"),
                       actionButton("year",paste('jaar',year),width="29%"),
                       actionButton("forward",">>",width="19%")
                     ),

                     #---------------------------#
                     # 3.a.3 MOVE BUTTONS
                     fluidRow(                                                                     # within this column we make a new row; you can repeat this indefinitely
                       column(4),                                                                  # now we create a new column that acts just as whitespace
                       column(4,actionButton("btn_up", label = "Noord",width='100%'))              # ... and another column that holds an 'up' button
                     ),
                     fluidRow(                                                                     # ... add a bunch more rows, columns and buttons
                       column(4,actionButton("btn_left",  label = "West",  width='100%')),
                       column(4,actionButton("btn_sel",   label = "NA-NA", width='100%')),
                       column(4,actionButton("btn_right", label = "Oost",  width='100%'))
                     ),
                     fluidRow(
                       column(4),
                       column(4,actionButton("btn_down", label = "Zuid",width='100%'))
                     ),

                     #------------------------------#
                     # 3.a.4 ACTION INPUT CONTROLS
                     fluidRow(
                       do.call(tabBox, c(id='tab2',width='100%', height="580px",lapply(0:nrow(playerDef), function(i) {
                         if(i==0){                                                                 # first create a tab for non-players  
                           tabPanel(                                                               # create a tabPanel    
                             title="Leiding",                                                      # give it a name
                             helpText("Algemeen actie invoer veld. Basis-structuur: [team].[unit].[locatie]
                                      Dus bijvoorbeeld: PAN.PLT3.H4 = Herten kopen 3 peletons op H3. Van bestaande units kan ook een verplaatsing worden aangegeven.
                                      SPW.PLT4.J1.NNO = Sperwers verplaatsen 4 peletons van J1 noord.noord.oost."),
                             fluidRow(                                                          
                               column(6,textInput('hostaction',NULL,"")),                          # create action input field
                               actionButton("hostactionsubmit","Actie",icon=icon("cog"))           # create action submission button that will also provide feedback on action validity
                             ),
                             helpText("Hieronder kun je extra goud geven, bijvoorbeeld 'PAN.GOUD.1000"),
                             fluidRow(                                                          
                               column(6,textInput('hostspecial',NULL,"")),                         # create action input field
                               actionButton("hostspecialsubmit","Speciale Actie",icon=icon("cog")) # create action submission button that will also provide feedback on action validity
                             ),
                             helpText("Hier kun je berichten sturen. Die komen op het eerstvolgende overzicht. Zoals 'gelukkig nieuwjaar'
                                      Je kunt ook berichten naar een specifiek team sturen, bijvoorbeeld 'PAN.dit is een testbericht'"),
                             fluidRow(                                                          
                               column(6,textInput('hostmsg',NULL,"")),                             # create action input field
                               actionButton("hostmsgsubmit","Stuur Bericht",icon=icon("cog"))      # create action submission button that will also provide feedback on action validity
                             ),
                             hr(),
                             div(style = 'overflow-y:scroll; overflow-x: hidden; height:310px;',
                                 uiOutput("battleResult"),
                                 htmlOutput("testTxt"),
                                 actionButton("printReportAll","Print All Reports", width="100%")
                             )                            
                           )
                         } else {                                                                  # now start looping over players (as defined in playerDef)
                           tabPanel(                                                               # create tabPanel per player
                             title=playerDef$Label[i],                                             # give tab a name
                             fluidRow(
                               column(6,textInput(paste0(playerDef$Player[i],"a1"),NULL,paste0(playerDef$Player[i],"."))), # create action inputs, similar to before
                               actionButton(paste0(playerDef$Player[i],"a1submit"),"",icon=icon("cog"))                    # create action submission button, similar to before
                             ),
                             fluidRow(
                               column(6,textInput(paste0(playerDef$Player[i],"a2"),NULL,paste0(playerDef$Player[i],"."))), # repeat to create 3 sets of input/submission buttons
                               actionButton(paste0(playerDef$Player[i],"a2submit"),"",icon=icon("cog"))                    # ... could improve readability?
                             ),
                             fluidRow(
                               column(6,textInput(paste0(playerDef$Player[i],"a3"),NULL,paste0(playerDef$Player[i],"."))),
                               actionButton(paste0(playerDef$Player[i],"a3submit"),"",icon=icon("cog"))
                             ),
                             hr(),
                             div(style = 'overflow-y:scroll; overflow-x:hidden; height:310px',
                                 fluidRow(uiOutput(paste0('report',playerDef$Player[i]))), 
                                 fluidRow(actionButton(paste0("printReport",playerDef$Player[i]),"Print Report", width="100%"))
                             )                            
                           )
                          }
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
                           actionButton("yearSlave",paste('jaar',year))
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
      # 3.c CREATE CHEATSHEET TAB
      #-----------------------------------------#
      tabItem(
        tabName="CheatSheet",
        
        column(8,
               column(12,
                      fluidRow(
                        box(title="Uitleg acties", status = "primary", width="100%",
                            DT::dataTableOutput("cheatsheet")
                        ),
                        box(title="Unit overzicht", status = "primary", width="100%",
                            DT::dataTableOutput("cheatsheet_units")
                        )
                      )
               )
        ),
        column(4,
               fluidRow(
                 box(title="Game Settings", status = "primary", width="100%",
                     disabled(textInput("ip_address","IP Adres","Unknown IP")),
                     disabled(textInput("turn_bonus","Bonus per turn",turnBonus)),
                     disabled(textInput("year_bonus","Bonus per jaar",yearBonus)),
                     disabled(textInput("year_cycle","Beurten per jaar",yearCycle)),
                     disabled(textInput("start_year","Start jaar",year)),
                     disabled(textInput("start_turn","Start beurt",turn))
                 ),
                 box(title="Settings", status = "primary", width="100%",
                     disabled(column(6,textInput("scrn_res_x","Scherm Resolutie (x)",scrnRes[1]))),
                     disabled(column(6,textInput("scrn_res_y","Scherm Resolutie (y)",scrnRes[2]))),
                     disabled(column(6,textInput("grd_size_x","Speelbord dim (x)",gridRes[1]))),
                     disabled(column(6,textInput("grd_size,y","Speelbord dim (y)",gridRes[2])))
                 ),
                 box(title="Audio", status = "primary", width="100%",
                     tags$audio(src = "Nijmegen.mp3", type = "audio/mp3", autoplay = 1, controls = NA)
                 )
               )
        )
      ),

      #-----------------------------------------#
      # 3.d CREATE LOAD GAME TAB
      #-----------------------------------------#
      tabItem(
        tabName="Laadgame",
        fluidRow(
          box(title="Save/Load", status = "primary", width="100%",
            fluidRow(style='padding:10px;',
                     fileInput("loadGameFile", NULL, multiple = FALSE, accept = c(".rda"))
            )
          )
        )
      ),
      
      #-----------------------------------------#
      # 3.e CREATE CREDITS TAB
      #-----------------------------------------#
      tabItem(
        tabName="Credits",
        htmlOutput("credits", class='creditsbg')
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