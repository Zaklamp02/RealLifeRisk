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
library(readxl)
library(raster)
library(rgeos)

source("global_reports.R")
source("global_actions.R")
source("global_other.R")

# Load key files
wd        <- getwd()
unitDef   <- data.frame(read_excel(file.path(wd,'www','Settings.xlsx'), skip=1, sheet="Units"))    # unit specification
boardDef  <- data.frame(read_excel(file.path(wd,'www','Settings.xlsx'), skip=1, sheet="Bord"))     # board specification
gameDef   <- data.frame(read_excel(file.path(wd,'www','Settings.xlsx'), skip=1, sheet="Settings")) # player specification
playerDef <- data.frame(read_excel(file.path(wd,'www','Settings.xlsx'), skip=1, sheet="Spelers"))  # add colors (in rgb) to player specification
playerDef <- cbind(playerDef,t(col2rgb(playerDef$Color)))                                          # add player color in RGB
msglog    <- list()

# Basic game settings
for(i in 1:nrow(gameDef)){
  assign(gameDef[i,1],gameDef[i,2])
}

scrnRes  <- system("wmic path Win32_VideoController get CurrentHorizontalResolution,CurrentVerticalResolution",intern=T)
scrnRes  <- as.numeric(unlist(regmatches(scrnRes, gregexpr("[[:digit:]]+", scrnRes))))
scrnResX <- ifelse(scrnResX==0,scrnRes[1],scrnResX)
scrnResY <- ifelse(scrnResY==0,scrnRes[2],scrnResY)
xNames   <- as.character(names(boardDef)[-1])                                               # names of Y axis
yNames   <- rev(as.character(boardDef[,1]))                                                 # names of X axis
uNames   <- unitDef$Unit                                                                    # unit names (abbreviated)
pNames   <- playerDef$Player                                                                # names of players (abbreviated)
scrnRes  <- c(scrnResX,scrnResY)                                                            # screen resolution
gridSize <- c(length(xNames),length(yNames))                                                # size of game board 
sprRes   <- rep(floor(scrnRes[1]*0.73/gridSize[1]),2)                                       # resolution of sprites/units (scaled to match screen resolution)
gridRes  <- gridSize*sprRes                                                                 # resolution of gameboard
gameID   <- paste0(LETTERS[sample(1:26,1)],LETTERS[sample(1:26,1)],LETTERS[sample(1:26,1)],sample(111:999,1))
yearStart<- year
ip       <- system("ipconfig",intern=T)                                                     # get (server) IP address
ip       <- ip[grep("IPv4 Address",ip)][1]
ip       <- ifelse(is.na(ip),"unknown IP address",strsplit(ip," : ")[[1]][2])               # select (server) IP address
l        <- paste(rep('-',maxReportWidth*1.4),collapse="")                                  # used to separate report elements (*1.5 as '-' is a bit smaller than usual characters)
s        <- " // "                                                                          # used to divide information elements
b        <- '<br/>'                                                                         # used as line break (html)
newTurn  <- FALSE                                                                           # check if new turn has started

# Create initial board state
boardState <- data.frame(xPos=NA,yPos=NA,xRes=NA,yRes=NA,player=NA,unit=NA,quantity=NA)
land       <- array(rep(0,gridSize[2]*gridSize[1]*4),c(gridSize[2],gridSize[1],4))          # create land variable after function definition!
for(i in 1:nrow(playerDef)){
  loc <- which(boardDef==paste0("P",i),arr.ind=T)
  for(j in 1:nrow(unitDef)){
    pos        <- (j-1) %% nrow(loc)+1
    boardState <- create_unit(boardState,playerDef$Player[i],unitDef$Unit[j],unitDef$StartQuantity[j],xNames[loc[pos,2]],yNames[gridSize[2]-loc[pos,1]+1])
    land       <- updateMapOwner(land,xNames[loc[pos,2]],yNames[gridSize[2]-loc[pos,1]+1],playerDef$Player[i])
  }
}

# Load images & sprites
spr <- list()                                                                              # this will hold all sprites
spr[['board']] <- readPNG(file.path(wd,'www','base_map.png'),native=F)                     # read gameboard
for(i in 1:nrow(unitDef)){                                                                 # loop over units
  sprTmp <- image_read(file.path(wd,'www',paste0(unitDef$Sprite[i])))                      # read sprite for current unit
  for(j in 1:nrow(playerDef)){                                                             # loop over players
    spr[[paste0(unitDef$Unit[i],playerDef$Player[j])]] <- sprTmp %>%                       # make copy of unit sprite
      image_fill(color=playerDef$Color[j],fuzz=20) %>%                                     # set background of sprite to player color
      image_scale(paste0("x",sprRes[1]))
  }
}

# KEY talking variables:
game                          <- NULL
game[[paste(turn)]][['bs']]   <- boardState
game[[paste(turn)]][['land']] <- land


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