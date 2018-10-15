###############################################################################
# GLOBAL.R
###############################################################################
#
#   MODUS OPERANDI
#
#
###############################################################################
# @author: S.V. den Boer | Aug 27, 2018
###############################################################################

#-------------------------------------------#
# 0. SET UP ENVIRONMENT
#-------------------------------------------#
#
# !!! Most of these settings will probably need
# to be moved to (individual?) text files.
# That way they are a bit easier to adjust
# as we type along.
#
#-------------------------------------------#

# Load basic packages
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinyjs)
library(png)
library(DT)
library(magick)
library(xlsx)

source("global_reports.R")
source("global_actions.R")
source("global_other.R")

# Load key files
wd        <- getwd()
unitDef   <- read.csv(file.path(wd,'www','maak_units.csv'),header=T,skip=1,stringsAsFactors = F)          # defines all units 
playerDef <- read.csv(file.path(wd,'www','maak_spelers.csv'),header=T,skip=1,stringsAsFactors = F)        # defines all players
gameDef   <- read.csv(file.path(wd,'www','maak_speel_settings.csv'),header=T,skip=1,stringsAsFactors = F) # defines all game settings
boardDef  <- read.xlsx(file.path(wd,'www','maak_speelbord.xlsx'),sheetIndex=1,startRow=2)                 # defines size, coördinates, cities & initial boardState
playerDef <- cbind(playerDef,t(col2rgb(playerDef$Color)))                                                 # add player color in RGB
msglog    <- list()

# Basic game settings
for(i in 1:nrow(gameDef)){
  assign(gameDef[i,1],gameDef[i,2])
}

xNames   <- as.character(names(boardDef)[-1])                                               # names of Y axis
yNames   <- as.character(boardDef[,1])                                                      # names of X axis
uNames   <- unitDef$Unit                                                                    # unit names (abbreviated)
pNames   <- playerDef$Player                                                                # names of players (abbreviated)
scrnRes  <- c(scrnResX,scrnResY)                                                            # screen resolution
gridSize <- c(length(xNames),length(yNames))                                                # size of game board 
sprRes   <- rep(floor(scrnRes[1]*0.73/gridSize[1]),2)                                       # resolution of sprites/units (scaled to match screen resolution)
gridRes  <- gridSize*sprRes                                                                 # resolution of gameboard
gameID   <- paste0(LETTERS[sample(1:26,1)],LETTERS[sample(1:26,1)],LETTERS[sample(1:26,1)],sample(111:999,1))
ip       <- system("ipconfig",intern=T)                                                     # get (server) IP address
ip       <- strsplit(ip[grep("IPv4 Address",ip)]," : ")[[1]][2]                             # select (server) IP address
l        <- '------------------------------------------'                                    # used to separate report elements
s        <- " // "                                                                          # used to divide information elements
b        <- '<br/>'                                                                         # used as line break (html)

# Basic board definition
boardDef <- data.frame(xPos     = rep(xNames,each=gridSize[2]),                             # all possible X coördinates (in grid)
                       yPos     = rep(yNames,gridSize[1]),                                  # all possible Y coördinates (in grid)
                       xRes     = rep(1:gridSize[1],each=gridSize[2])*sprRes[1]-sprRes[1],  # all possible X coördinates (in pixels)
                       yRes     = rep(1:gridSize[2],gridSize[1])*sprRes[2]-sprRes[2],       # all possible Y coördinates (in pixels)
                       water    = rep('no',gridSize[1]*gridSize[2]),                        # boolean if location is 'water'
                       stringsAsFactors=F)                                                  # make sure to encode as 'char' instead of 'factor'

# Create initial board state
startIni  <- 15                                                                             # RANDOMLY initialize the board with 15 units
boardState <- data.frame(xPos=sample(1:length(xNames),startIni,replace=T),
                         yPos=sample(1:length(yNames),startIni,replace=T))
boardState$xRes     <- boardState$xPos * sprRes[1]-sprRes[1]
boardState$yRes     <- boardState$yPos * sprRes[2]-sprRes[2]
boardState$xPos     <- xNames[boardState$xPos]
boardState$yPos     <- yNames[boardState$yPos]
boardState$player   <- sample(pNames,startIni,replace=T)
boardState$unit     <- sample(uNames,startIni,replace=T)
boardState$quantity <- sample(1:4,startIni,replace=T)

land <- array(rep(0,gridSize[2]*gridSize[1]*4),c(gridSize[2],gridSize[1],4))               # create land variable after function definition!
for(i in 1:nrow(boardState)){                                                              # assign starting tiles based on existing units
  land <- updateMapOwner(land,boardState$xPos[i],boardState$yPos[i],boardState$player[i])
}

# Initiate game variable
game <- NULL
game[[paste(turn)]][['bs']]   <- boardState
game[[paste(turn)]][['land']] <- land

# Load images & sprites
spr <- list()                                                                              # this will hold all sprites
spr[['board']] <- readPNG(file.path(wd,'www','base_map.png'),native=F)                     # read gameboard
for(i in 1:nrow(unitDef)){                                                                 # loop over units
  sprTmp <- image_read(file.path(wd,'www',paste0('spr_',unitDef$Label[i],'.jpeg')))        # read sprite for current unit
  for(j in 1:nrow(playerDef)){                                                             # loop over players
    spr[[paste0(unitDef$Unit[i],playerDef$Player[j])]] <- sprTmp %>%                       # make copy of unit sprite
      image_fill(color=playerDef$Color[j],fuzz=20) %>%                                     # set background of sprite to player color
      image_scale(paste0("x",sprRes[1]))
  }
}

# KEY talking variables:
actions <- list()
for(i in playerDef$Player){
  for(j in 1:3){
    actions[[paste0(i,j)]] <- ''
  }
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
