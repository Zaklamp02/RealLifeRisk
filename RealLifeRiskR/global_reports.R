#-------------------------------------------#
# 1. GENERATE REPORT
#-------------------------------------------#
#
# This is a key function to provide consolidated
# reports to players. The idea is to use this
# function once each turn. The report provides
# information on action outcomes, results
# of special actions, units lost in battle etc.
#
#-------------------------------------------#

generate_report <- function(session,tbs,tland,player,input,output,msgBattle=''){
  
  bomb <- ''
  for(p in playerDef$Player){
    for(j in 1:3){
      bomb <- paste0(bomb,msglog[[paste0(p,'a',j)]]$bomb)
    }
  }
  bomb   <- ifelse(grepl(playerDef$Label[playerDef$Player==player],bomb),bomb,'')
  battle <- ifelse(grepl(playerDef$Label[playerDef$Player==player],msgBattle),msgBattle,'')
  
  a1 <- ifelse(msglog[[paste0(player,'a1')]]$type == "succes",msglog[[paste0(player,'a1')]]$outcome,msglog[[paste0(player,'a1')]]$type)
  a2 <- ifelse(msglog[[paste0(player,'a2')]]$type == "succes",msglog[[paste0(player,'a2')]]$outcome,msglog[[paste0(player,'a2')]]$type)
  a3 <- ifelse(msglog[[paste0(player,'a3')]]$type == "succes",msglog[[paste0(player,'a3')]]$outcome,msglog[[paste0(player,'a3')]]$type)
  
  special <- ifelse(is.null(msglog[[paste0(player)]]$special),'',paste0(l,b,"Speciale Berichten:",msglog[[paste0(player)]]$special,b))
  msglog[[paste0(player)]]$special <<- NULL
  
  rapport <- paste0(
    l,b,
    'Rapport: ',playerDef$Label[playerDef$Player==player],b,
    sample(1:30,1),' ',month.abb[floor(((turn-1)%%yearCycle+1)/yearCycle*12)],' ',year,' (beurt ',turn-1,')',b
  )
  
  overzicht <- paste0(
    l,b,
    'Overzicht:',b,
    'Goud: ',playerDef$Gold[playerDef$Player==player],b,
    'Land: ',playerDef$Land[playerDef$Player==player],' (',round(100*playerDef$Land[playerDef$Player==player]/prod(gridSize)),'%)',b,
    'Leger: ',calculate_army_value(tbs,player),b
  )
  
  acties <- paste0(
    l,b,
    'Uitslag acties:',b,
    "A1: ", a1,b,
    "A2: ", a2,b,
    "A3: ", a3,b
  )
  
  radar   <- generate_radar_report(player)
  gevecht <- ifelse(battle=='','',paste0(l,b,"Gevechtssituaties:",battle))
  bom     <- ifelse(bomb=='','',paste0(l,b,"Bombardementen:",b,bomb))
  
  unitoverzicht <- generate_unit_report(tbs,player)
  landoverzicht <- generate_land_report(tland,player)
  
  out <- paste(rapport,overzicht,acties,gevecht,bom,radar,unitoverzicht,landoverzicht,special,l,sep='')
  out <- rectify(out,maxReportWidth)
  game[[paste(turn)]][[paste0('report',player)]] <<- out
  
  return(out)
}

#-------------------------------------------#
# 2. SUB-REPORT: LAND
#-------------------------------------------#
generate_land_report <- function(tland,player){
  i   <- which(playerDef$Player==player)
  lnd <- which(tland[,,1]  == playerDef$red[i]/255 & tland[,,2]  == playerDef$green[i]/255 & tland[,,3]  == playerDef$blue[i]/255,arr.ind=T)
  x   <- xNames[lnd[,2]]
  y   <- yNames[gridSize[2] - lnd[,1] + 1]
  msg <- paste(paste(x,y,sep=""),collapse=s)
#  msg <- rectify(msg,maxReportWidth)
  msg <- paste(l,b,"Land overzicht",b,msg,b)
  
  return(msg)
}

#-------------------------------------------#
# 3. SUB-REPORT: UNITS
#-------------------------------------------#
generate_unit_report <- function(tbs,player=NULL){
  
  if(!is.null(player)){tbs <- tbs[tbs$player==player,]}
  
  tbs <- tbs[order(tbs$xPos,tbs$yPos),]
  p1  <- paste(tbs$xPos,tbs$yPos,sep="")
  p2  <- paste(tbs$quantity,tbs$unit,sep="x ")
  msg <- paste(p1,p2,sep=": ",collapse=s)
#  msg <- paste(msg,collapse=s)
  msg <- paste(l,b,"Unit overzicht",b,msg,b)
  
  return(msg)
}

#-------------------------------------------#
# 4. SUB-REPORT: RADAR
#-------------------------------------------#
generate_radar_report <- function(player){
  radar <- paste0(
    msglog[[paste0(player,'a1')]]$radar,
    msglog[[paste0(player,'a2')]]$radar,
    msglog[[paste0(player,'a3')]]$radar
  )
  
  if(length(radar)==0){
    msg <- ''
  } else {
    msg <- paste(l,b,"Radar Scans:",b,radar,b)
  }

  return(msg)
}

#-------------------------------------------#
# 5. CREDITS REPORT
#-------------------------------------------#

credits_report <- function(){
  html_align="center"
  
  title_pre = "<h1 align='center' style='color:red'><b>"
  title_post = "</h1></b>"
  main_pre = "<h2 align='center' style='color:white'><b>"
  main_post = "</h2></b>"
  
  paste0(
    b,
    title_pre,"Spel-ontwerp:",title_post,b,
    main_pre,"Hilbert Vinke | Joachaz van Keulen | Maarten-Marijn Koens | Rick Gerrits",b,"Rutger Schols | Stijn Hautvast | Tijmen Dokter",main_post,b,
    
    title_pre,"Programmeerwerk:",title_post,b,
    main_pre,"Sebastiaan den Boer",main_post,b,
    
    title_pre,"Sprites, Kaart & Visuals:",title_post,b,
    main_pre,"Maarten-Marijn Koens",main_post,b,
    
    
    title_pre,"Origineel spelidee & programmeerwerk",title_post,b,
    main_pre,"Guit-Jan Ridderbos | Peter Niemeijer | Peter Wieringa",main_post,b,
    
    title_pre,"Standing on the shoulders of these giants:",title_post,b,
    main_pre,"Guit-Jan Ridderbos | Peter Niemeijer | Matthijs van Calveen | Niek van den Esker",main_post,b

  )
}

#-------------------------------------------#
# 6. CHEATSHEET
#-------------------------------------------#

cheat_sheet <- function(){

  Actie = c("Verplaats","Verplaats Meerdere","Verplaats in Vrachtwagen","Koop Unit","Bombardement","Radarscan","Geef Goud","Bericht aan allen","Bericht aan player")
  Formaat = c("[player] [unit+aantal] [startcoordinaat] [verplaatsing]","[player] [unit1+aantal1+unit2+aantal2] [startcoordinaat] [verplaatsing]","[player] [unit1+aantal1+unit2+aantal2] [startcoordinaat] [verplaatsing]","[player] [unit+aantal] [doelcoordinaat]","[player] [unit+aantal] [doelcoordinaat]","[player] [unit] [doelcoordinaat]","[player] [GOUD] [hoeveelheid]","[bericht]","[player] [bericht]")
  Voorbeeld = c("HER . PLT3 . K12 . NNO","HER . PLT3TNK1 . K12 . NNO","HER . VRW1PLT3 . K12 . NNOOOO","HER . PLT2 . K12","HER . BOM1 . K12","HER . RAD1 . K12","HER . GOUD . 1000","Testbericht","HRT . Testbericht")
  
  report = data.frame(Actie,Formaat,Voorbeeld)
  return(report)
}

#-------------------------------------------#
# X. HELPER: RECTIFY MESSAGES
#-------------------------------------------#
rectify <- function(msgIn,nc=50){
  
  msgIn <- unlist(strsplit(msgIn,b))
  
  for(i in which(nchar(msgIn)>nc)){
    msgOut <- ""
    s   <- ifelse(grepl(s,msgIn[i]),s," ")                                               # if message does not contain separator, use space as standin
    nc1 <- trimws(unlist(strsplit(msgIn[i],s)))                                          # break into elements  
    nc4 <- floor(cumsum((nchar(nc1))+nchar(s))/nc)                                       # find length of each element (while adding separator-length)
    
    for(j in unique(nc4)){
      msgOut <- paste(msgOut,b,paste(nc1[nc4==j],collapse=s),s,sep="")
    }
    msgIn[i] <- sub(b,"",msgOut)
  }
  
  msgOut <- paste(msgIn,collapse=b)
  
  return(msgOut)  
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