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
shiny::runApp(app,host="0.0.0.0",port=4414)

###############################################################################
#   TO DO
###############################################################################
#
#   GLOBAL
#   - create actual battle engine
#   - create path_finder function (to find optimal route for move actions)
#   - create game_tracker function (to log game progression/statistics)
#   - create game_score function (to keep track of/assign scores)
#   - improve check_action
#   - shift key settings to .txt/.xls files
#
#   SERVER
#   - improve plot efficiency (find alternative to ggplot?)
#
#   UI
#   - improve app layout
#   - add login screen
#   - add music
#
#   DATA WRANGLING
#   - start using pipes
#   - profile pipes
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