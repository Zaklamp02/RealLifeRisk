###############################################################################
# APP.R
###############################################################################
#
#   MODUS OPERANDI
#
#   THis is basically a wrapper function for the Shiny App. This function
#   simply sources important files, and actually starts the app. Note that
#   the app is hosted on the local ip at port 4414. This means any device
#   on the network can access the app at 'website': ipaddress:4414
#
###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################

source('global.R')
source('ui.R')
source('server.R')

app <- shinyApp(ui,server)
shiny::runApp(app,host="0.0.0.0")

###############################################################################
#   TO DO
###############################################################################
#
#   GENERAL
#   - improve end of turn sequence
#   - add save/continue / turn counters
#
#   GLOBAL
#   - create game_tracker function (to log game progression/statistics)
#   - create game_score function (to keep track of/assign scores)
#   - improve check_action
#
#   SERVER
#   - improve plot efficiency (find alternative to ggplot?)
#   - fix entire 'settings' screen
#
#   UI
#   - creat themes
#   - add music
#   - add score to header
#
#   NETWORKING
#   - improve update speed/stability
#   - fix report-updating
#
#   DATA WRANGLING
#   - profileVis
#
###############################################################################

###############################################################################
#   !!! FOR CODE PROFILING ONLY !!!
###############################################################################
#
#  The code below will run the app in profiling mode. Once you close the app,
#  a report is generated that can help debug the app and find (processing)
#  bottlenecks. Should only be used for debugging/testing
#
library(profvis)
profvis({
  runApp()
})
#
###############################################################################

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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